// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.


// Not supported on Windows or 32 bit architectures. When not supported the NIF
// will still build, but the API functions will return {error, einval}, and
// we'd fallback to the regular file API then
//
#if !defined(_WIN32) && defined(__LP64__)
#define COUCH_CFILE_SUPPORTED 1
#endif

#ifdef COUCH_CFILE_SUPPORTED

#include <errno.h>
#include <limits.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>

// erl_driver.h is for erl_errno_id()
#include "erl_driver.h"
#include "erl_nif.h"

static ErlNifResourceType* HANDLE_T;
static ErlNifPid JANITOR_PID;

static ERL_NIF_TERM ATOM_BOF;
static ERL_NIF_TERM ATOM_EOF;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_BADARG;
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_CLOSE;
static ERL_NIF_TERM ATOM_CONTINUE;

typedef int posix_errno_t;

typedef struct {
    // This read-write lock is for the closing state, not for the the
    // operations themselves (pread vs write). The owner process in the
    // #file_descriptor{} record handle in Erlang controls who can read vs
    // write to the file.
    ErlNifRWLock *lock;
    // Monitor for the owner process. If owner exits we'll get notified
    // in the handle_down callback
    ErlNifMonitor monitor;
    // Our file descriptor. If set to -1 it means it's closed
    int fd;
    // This is for the "belt and suspenders" in the sanity_checker function.
    // After we dup a handle we verify that the dup-ed old_fd matcheds the
    // original fd and we still have both open (they were not somehow closed in
    // the meantime).
    int old_fd;
} handle_t;

#define LOCK        enif_rwlock_rwlock(hdl->lock)
#define UNLOCK      enif_rwlock_rwunlock(hdl->lock)
#define READ_LOCK   enif_rwlock_rlock(hdl->lock)
#define READ_UNLOCK enif_rwlock_runlock(hdl->lock)

// The fallback, min and iov defines are for writev function unix_prim_file.c
#define FALLBACK_RW_LENGTH ((1ull << 31) - 1)
#ifndef MIN
    #define MIN(A, B) ((A) < (B) ? (A) : (B))
#endif
#if !defined(IOV_MAX) && defined(UIO_MAXIOV)
    #define IOV_MAX UIO_MAXIOV
#elif !defined(IOV_MAX)
    #define IOV_MAX 16
#endif

static int get_handle(ErlNifEnv *env, ERL_NIF_TERM arg, handle_t** h) {
    return enif_get_resource(env, arg, HANDLE_T, (void**)h);
}

static ERL_NIF_TERM ok_tup(ErlNifEnv* env, ERL_NIF_TERM res) {
  return enif_make_tuple2(env, ATOM_OK, res);
}

static ERL_NIF_TERM err_tup(ErlNifEnv *env, posix_errno_t posix_errno) {
    ERL_NIF_TERM error = enif_make_atom(env, erl_errno_id(posix_errno));
    return enif_make_tuple2(env, ATOM_ERROR, error);
}

static ERL_NIF_TERM badarg(ErlNifEnv *env) {
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_BADARG);
}

// Copy from OTP. For the sake of keeping the same behavior we use
// the same logic. If it changes in OTP, consider updating this as well.
//
static void shift_iov(SysIOVec **iov, int *iovlen, ssize_t shift) {
    SysIOVec *head_vec = (*iov);

    while(shift > 0) {

        if(shift < head_vec->iov_len) {
            head_vec->iov_base = (char*)head_vec->iov_base + shift;
            head_vec->iov_len -= shift;
            break;
        } else {
            shift -= head_vec->iov_len;
            head_vec++;
        }
    }

    (*iovlen) -= head_vec - (*iov);
    (*iov) = head_vec;
}

// Copied form OTP to keep the same logic. Some differences:
//   - Pass file descriptor as int and errno result as a separate arg
//   - Assume preadv exists so skip the check + fallback code
//
static long efile_preadv(int fd, long offset, SysIOVec *iov, int iovlen, posix_errno_t* res_errno) {
    unsigned long bytes_read;
    long result;

    bytes_read = 0;

    do {
        if(iovlen < 1) {
            result = 0;
            break;
        }
        result = preadv(fd, (const struct iovec*)iov, MIN(IOV_MAX, iovlen), offset);
        if(result > 0) {
            shift_iov(&iov, &iovlen, result);
            bytes_read += result;
            offset += result;
        }
    } while(result > 0 || (result < 0 && errno == EINTR));

    *res_errno = errno;

    if(result == 0 && bytes_read > 0) {
        return bytes_read;
    }

    return result;
}

// Copied from OTP just like efile_preadv. Differences are:
//  - Pass file descriptor as int and errno result as a separate arg
//  - Assume writev exists
//
static long efile_writev(int fd, SysIOVec *iov, int iovlen, posix_errno_t* res_errno) {
    long bytes_written;
    ssize_t result;

    bytes_written = 0;

    do {
        int use_fallback = 0;

        if(iovlen < 1) {
            result = 0;
            break;
        }

        result = writev(fd, (const struct iovec *)iov, MIN(IOV_MAX, iovlen));

        /* Fall back to using write(2) if writev(2) reports that the combined
         * size of iov is greater than SSIZE_T_MAX. */
        use_fallback = (result < 0 && errno == EINVAL);

        if(use_fallback) {
            result = write(fd, iov->iov_base, iov->iov_len);

            /* Some OSs (e.g. macOS) does not allow writes greater than 2 GB,
               so if we get EINVAL in the fallback, we try with a smaller length */
            if (result < 0 && errno == EINVAL && iov->iov_len > FALLBACK_RW_LENGTH)
                result = write(fd, iov->iov_base, FALLBACK_RW_LENGTH);
        }

        if(result > 0) {
            shift_iov(&iov, &iovlen, result);
            bytes_written += result;
        }
    } while(result > 0 || (result < 0 && errno == EINTR));

    *res_errno = errno;

    if(result == 0 && bytes_written > 0) {
        return bytes_written;
    }

    return result;
}

// Copied from OTP. Differences are:
//    - File descriptor and return error passed in as separate args
//    - This is for datasync only so don't pass that extra argument in
//
int efile_datasync(int fd, posix_errno_t* res_errno) {

#if !defined(__DARWIN__)
    if(fdatasync(fd) < 0) {
        *res_errno = errno;
        return 0;
    }
    return 1;
#endif

#if defined(__DARWIN__) && defined(F_BARRIERFSYNC)
    if(fcntl(fd, F_BARRIERFSYNC) < 0) {
#elif defined(__DARWIN__) && defined(F_FULLFSYNC)
    if(fcntl(fd, F_FULLFSYNC) < 0) {
#else
    if(fsync(fd) < 0) {
#endif
        *res_errno = errno;
        return 0;
    }
    return 1;
}

// Duplicate a file descriptor. This doesn't open a new "file description"
// entry, just creates an extra light-weight file "descriptor" id for it.
// Position, buffers, permission bits are all shared with the main description
// entry. See https://www.man7.org/linux/man-pages/man2/dup.2.html for details
//
static ERL_NIF_TERM dup_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef COUCH_CFILE_SUPPORTED
   int fd, newfd;
   handle_t* hdl;
   ErlNifRWLock *lock;
   ErlNifPid pid;
   ERL_NIF_TERM res;


   if (argc != 1 || !enif_is_number(env, argv[0])) {
       return badarg(env);
   }
   if (!enif_get_int(env, argv[0], &fd) || fd < 0) {
       return err_tup(env, EINVAL);
   }

   if(!enif_self(env, &pid)){
       // Calling from not a process-bound environment? (highly unlikely)
       return err_tup(env, EINVAL);
   }
   newfd = dup(fd);

   if (newfd < 0) {
       return err_tup(env, errno);
   }
   // From here on close the new dup-ed file descriptors on any failure.

   lock = enif_rwlock_create("couch_cfile:rwlock");
   if(!lock) {
       close(newfd);
       return err_tup(env, ENOMEM);
   }

   hdl = (handle_t*) enif_alloc_resource(HANDLE_T, sizeof(handle_t));
   if (!hdl) {
       close(newfd);
       enif_rwlock_destroy(lock);
       return err_tup(env, ENOMEM);
   }
   // From here on, once we release our resource the destructor will be called.
   // On failures below, we'll let the destructor deallocate the lock, but
   // we'll close the new handle here manually. The descriptors will be set to
   // -1 so the state in the handle will be "closed" until we know everything
   // is good to go

   hdl->lock = lock;
   hdl->fd = -1;
   hdl->old_fd = -1;

   if (enif_monitor_process(env, hdl, &pid, &hdl->monitor) != 0) {
       close(newfd);
       enif_release_resource(hdl);
       return err_tup(env, EINVAL);
   }

   // Everything is good to go. Return the new handle
   hdl->fd = newfd;
   hdl->old_fd = fd;

   res = enif_make_resource(env, hdl);
   enif_release_resource(hdl);
   return ok_tup(env, res);
#else
   return err_tup(env, EINVAL);
#endif
}

static ERL_NIF_TERM close_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef COUCH_CFILE_SUPPORTED
    handle_t* hdl;

    if (argc != 1 || !get_handle(env, argv[0], &hdl)) {
        return badarg(env);
    }

    // ------ Critical section start ------
    LOCK;
    if (hdl->fd < 0) {
         UNLOCK;
         return err_tup(env, EINVAL);
    }
    enif_demonitor_process(env, hdl, &hdl->monitor);
    if (close(hdl->fd) < 0) {
        hdl->fd = -1;
        UNLOCK;
        return err_tup(env, errno);
    }
    hdl->fd = -1;
    UNLOCK;
    // ------ Critical section end ------

    return ATOM_OK;
#else
   return err_tup(env, EINVAL);
#endif
}

// !!! This should be called from the janitor process only !!!
//
static ERL_NIF_TERM close_fd_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef COUCH_CFILE_SUPPORTED
   int fd;

   if (argc != 1 || !enif_is_number(env, argv[0])) {
       return badarg(env);
   }
   if (!enif_get_int(env, argv[0], &fd) || fd < 0) {
       return err_tup(env, EINVAL);
   }

   if (close(fd) < 0) {
        return err_tup(env, errno);
   }

   return ATOM_OK;
#else
   return err_tup(env, EINVAL);
#endif
}


// Follows pread_nif_impl from prim_file_nif.c
//
static ERL_NIF_TERM pread_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef COUCH_CFILE_SUPPORTED
   handle_t* hdl;
   long offset, block_size, bytes_read;
   SysIOVec io_vec[1];
   posix_errno_t res_errno = 0;
   ErlNifBinary result;

   if (argc != 3
       || !get_handle(env, argv[0], &hdl)
       || !enif_is_number(env, argv[1])
       || !enif_is_number(env, argv[2])
   ) {
     return badarg(env);
   }

   if (!enif_get_int64(env, argv[1], &offset)
       || !enif_get_int64(env, argv[2], &block_size)
       || offset < 0
       || block_size < 0
   ) {
        return err_tup(env, EINVAL);
   }

   if (!enif_alloc_binary((size_t) block_size, &result)) {
       return err_tup(env, ENOMEM);
   }

   io_vec[0].iov_base = (char *)result.data;
   io_vec[0].iov_len = result.size;

   // ------ Critical section start ------
   READ_LOCK;
   if (hdl->fd < 0) {
       READ_UNLOCK;
       enif_release_binary(&result);
       return err_tup(env, EINVAL);
   }
   bytes_read = efile_preadv(hdl->fd, offset, io_vec, 1, &res_errno);
   READ_UNLOCK;
   // ------ Critical section end ------

   if (bytes_read < 0) {
       enif_release_binary(&result);
       return err_tup(env, res_errno);
   }
   if (bytes_read == 0) {
       enif_release_binary(&result);
       return ATOM_EOF;
   }
   if (bytes_read < block_size && !enif_realloc_binary(&result, bytes_read)) {
       enif_release_binary(&result);
       return err_tup(env, ENOMEM);
   }
   return ok_tup(env, enif_make_binary(env, &result));
#else
   return err_tup(env, EINVAL);
#endif
}

// Follows implementation from prim_file_nif.c
//
static ERL_NIF_TERM write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef COUCH_CFILE_SUPPORTED
    handle_t* hdl;
    ErlNifIOVec vec, *input = &vec;
    posix_errno_t  res_errno = 0;
    long bytes_written;
    ERL_NIF_TERM tail;

    if (argc != 2
       || !get_handle(env, argv[0], &hdl)
       || !enif_inspect_iovec(env, 64, argv[1], &tail, &input)
    ) {
     return badarg(env);
    }

    // ------ Critical section start ------
    READ_LOCK;
    if (hdl->fd < 0) {
       READ_UNLOCK;
       return err_tup(env, EINVAL);
    }
    bytes_written = efile_writev(hdl->fd, input->iov, input->iovcnt, &res_errno);
    READ_UNLOCK;
    // ------- Critical section end ------

    if(bytes_written < 0) {
        return err_tup(env, res_errno);
    }

    if(!enif_is_empty_list(env, tail)) {
        return enif_make_tuple2(env, ATOM_CONTINUE, tail);
    }

    return ATOM_OK;
#else
    return err_tup(env, EINVAL)
#endif
}

static ERL_NIF_TERM seek_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef COUCH_CFILE_SUPPORTED
    handle_t* hdl;
    long result, offset;
    int whence;

    if (argc != 3
       || !get_handle(env, argv[0], &hdl)
       || !enif_is_atom(env, argv[1])
       || !enif_is_number(env, argv[2])
    ) {
     return badarg(env);
    }

    if (enif_is_identical(argv[1], ATOM_BOF)) {
        whence = SEEK_SET;
    } else if (enif_is_identical(argv[1], ATOM_EOF)) {
        whence = SEEK_END;
    } else {
        return badarg(env);
    }

    if(!enif_get_int64(env, argv[2], &offset) || offset < 0){
        return err_tup(env, EINVAL);
    }

    // ------ Critical section start ------
    READ_LOCK;
    if (hdl->fd < 0) {
        READ_UNLOCK;
        return err_tup(env, EINVAL);
    }
    result = lseek(hdl->fd, offset, whence);
    READ_UNLOCK;
    // ------ Critical section end ------

    // Follow OTP special case here: result < 0 with ernno = 0 is einval
    if(result < 0 && errno == 0) {
        return err_tup(env, EINVAL);
    }

    if(result < 0) {
        return err_tup(env, errno);
    }
    return ok_tup(env, enif_make_uint64(env, result));
#else
    return err_tup(env, EINVAL)
#endif
}

static ERL_NIF_TERM datasync_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef COUCH_CFILE_SUPPORTED
    handle_t* hdl;
    posix_errno_t  res_errno = 0;

    if (argc != 1 || !get_handle(env, argv[0], &hdl)) {
        return badarg(env);
    }

    // ------ Critical section start ------
    READ_LOCK;
    if (hdl->fd < 0) {
        READ_UNLOCK;
        return err_tup(env, EINVAL);
    }
    if(!efile_datasync(hdl->fd, &res_errno)) {
       READ_UNLOCK;
       return err_tup(env, res_errno);
    }
    READ_UNLOCK;
    // ------ Critical section end ------

    return ATOM_OK;
#else
    return err_tup(env, EINVAL)
#endif
}

static ERL_NIF_TERM truncate_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef COUCH_CFILE_SUPPORTED
    handle_t* hdl;
    off_t offset;

    if (argc != 1 || !get_handle(env, argv[0], &hdl)) {
     return badarg(env);
    }

    // ------ Critical section start ------
    READ_LOCK;
    if (hdl->fd < 0) {
        READ_UNLOCK;
        return err_tup(env, EINVAL);
    }
    offset = lseek(hdl->fd, 0, SEEK_CUR);
    if (offset < 0) {
        READ_UNLOCK;
        return err_tup(env, errno);
    }
    if (ftruncate(hdl->fd, offset) < 0){
        READ_UNLOCK;
        return err_tup(env, errno);
    }
    READ_UNLOCK;
    // ------ Critical section end ------

    return ATOM_OK;
#else
    return err_tup(env, EINVAL)
#endif
}

// Return a tuple with info about the handle The fields are:
//   fd : file descriptor (int)
//   old_fd : file descriptor we dup()-ed from (int)
//
static ERL_NIF_TERM info_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef COUCH_CFILE_SUPPORTED
    handle_t* hdl;
    int fd, old_fd;

    if (argc != 1 || !get_handle(env, argv[0], &hdl)) {
        return badarg(env);
    }

    // ------ Critical section start ------
    READ_LOCK;
    if (hdl->fd < 0) {
        READ_UNLOCK;
        return err_tup(env, EINVAL);
    }
    fd = hdl->fd;
    old_fd = hdl->old_fd;
    READ_UNLOCK;
    // ------ Critical section end ------

    return ok_tup(env, enif_make_tuple2(env,
        enif_make_int(env, fd),
        enif_make_int(env, old_fd)
    ));
#else
    return err_tup(env, EINVAL);
#endif
}

// Return the eof marker (the file size). This is a read-only call
// and can be called by any reader process not just the owner
//
static ERL_NIF_TERM eof_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef COUCH_CFILE_SUPPORTED
    handle_t* hdl;
    struct stat data;

    if (argc != 1 || !get_handle(env, argv[0], &hdl)) {
        return badarg(env);
    }

    // ------ Critical section start ------
    READ_LOCK;
    if (hdl->fd < 0) {
        READ_UNLOCK;
        return err_tup(env, EINVAL);
    }
    if (fstat(hdl->fd, &data) < 0) {
        READ_UNLOCK;
        return err_tup(env, errno);
    }
    READ_UNLOCK;
    // ------ Critical section end ------

    return ok_tup(env, enif_make_int64(env, data.st_size));
#else
    return err_tup(env, EINVAL);
#endif
}

// We cannot block the main scheduler in GC to close fds. NFS remote files
// could be delayed indefinitely so we, at least, want to make sure it
// happens on a dirty scheduler. See OTP prim_file_nif as example of this
// pattern.
//
static void send_delay_close(ErlNifEnv *env, int fd)
{
    ERL_NIF_TERM msg;
    msg = enif_make_tuple2(env, ATOM_CLOSE, enif_make_int(env, fd));
    enif_send(env, &JANITOR_PID, NULL, msg);
}

static void handle_dtor(ErlNifEnv* env, void *obj)
{
    handle_t* hdl = (handle_t*) obj;

    // ------ Critical section start ------
    LOCK;

    if (hdl->fd > -1) {
        send_delay_close(env, hdl->fd);
        hdl->fd = -1;
    }
    UNLOCK;
    // ------ Critical section end ------

    enif_rwlock_destroy(hdl->lock);
}

static void handle_down(ErlNifEnv* env, void* obj, ErlNifPid* pid, ErlNifMonitor* mon)
{
    handle_t* hdl = (handle_t*) obj;

    // ------ Critical section start ------
    LOCK;
    if (hdl->fd > -1) {
        send_delay_close(env, hdl->fd);
        hdl->fd = -1;
    }
    UNLOCK;
    // ------ Critical section end ------
}


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM pid)
{
  ErlNifResourceTypeInit init;

  if(!enif_get_local_pid(env, pid, &JANITOR_PID)) {
      return -1;
  }

  init.down    = handle_down;
  init.dtor    = handle_dtor;
  init.stop    = NULL;
  init.dyncall = NULL;
  init.members = 4;
  HANDLE_T = enif_init_resource_type(env, "couch_cfile:hdl", &init, ERL_NIF_RT_CREATE, NULL);
  if(!HANDLE_T) {
      return -1;
  }

  ATOM_BOF      = enif_make_atom(env, "bof");
  ATOM_EOF      = enif_make_atom(env, "eof");
  ATOM_ERROR    = enif_make_atom(env, "error");
  ATOM_OK       = enif_make_atom(env, "ok");
  ATOM_CLOSE    = enif_make_atom(env, "close");
  ATOM_BADARG   = enif_make_atom(env, "badarg");
  ATOM_CONTINUE = enif_make_atom(env, "continue");

  *priv_data = NULL;

  return 0;
}

static ErlNifFunc funcs[] = {
    {"dup_nif",      1, dup_nif,      ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close_nif",    1, close_nif,    ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close_fd_nif", 1, close_fd_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"pread_nif",    3, pread_nif,    ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"eof_nif",      1, eof_nif,      ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"seek_nif",     3, seek_nif,     ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"write_nif",    2, write_nif,    ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"datasync_nif", 1, datasync_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"truncate_nif", 1, truncate_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"info_nif",     1, info_nif}
};

ERL_NIF_INIT(couch_cfile, funcs, load, NULL, NULL, NULL);

#endif
