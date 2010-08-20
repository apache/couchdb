// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

// Do what 2 lines of shell script in couchspawnkillable does...
// * Create a new suspended process with the same (duplicated) standard 
//   handles as us.
// * Write a line to stdout, consisting of the path to ourselves, plus
//   '--kill {pid}' where {pid} is the PID of the newly created process.
// * Un-suspend the new process.
// * Wait for the process to terminate.
// * Terminate with the child's exit-code.

// Later, couch will call us with --kill and the PID, so we dutifully
// terminate the specified PID.

#include <stdlib.h>
#include "windows.h"

char *get_child_cmdline(int argc, char **argv)
{
    // make a new command-line, but skipping me.
    // XXX - todo - spaces etc in args???
    int i;
    char *p, *cmdline;
    int nchars = 0;
    int nthis = 1;
    for (i=1;i<argc;i++)
        nchars += strlen(argv[i])+1;
    cmdline = p = malloc(nchars+1);
    if (!cmdline)
        return NULL;
    for (i=1;i<argc;i++) {
        nthis = strlen(argv[i]);
        strncpy(p, argv[i], nthis);
        p[nthis] = ' ';
        p += nthis+1;
    }
    // Replace the last space we added above with a '\0'
    cmdline[nchars-1] = '\0';
    return cmdline;
}

// create the child process, returning 0, or the exit-code we will
// terminate with.
int create_child(int argc, char **argv, PROCESS_INFORMATION *pi)
{
    char buf[1024];
    DWORD dwcreate;
    STARTUPINFO si;
    char *cmdline;
    if (argc < 2)
        return 1;
    cmdline = get_child_cmdline(argc, argv);
    if (!cmdline)
        return 2;

    memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);
    // depending on how *our* parent is started, we may or may not have
    // a valid stderr stream - so although we try and duplicate it, only
    // failing to duplicate stdin and stdout are considered fatal.
    if (!DuplicateHandle(GetCurrentProcess(),
                       GetStdHandle(STD_INPUT_HANDLE),
                       GetCurrentProcess(),
                       &si.hStdInput,
                       0,
                       TRUE, // inheritable
                       DUPLICATE_SAME_ACCESS) ||
       !DuplicateHandle(GetCurrentProcess(),
                       GetStdHandle(STD_OUTPUT_HANDLE),
                       GetCurrentProcess(),
                       &si.hStdOutput,
                       0,
                       TRUE, // inheritable
                       DUPLICATE_SAME_ACCESS)) {
        return 3;
    }
    DuplicateHandle(GetCurrentProcess(),
                   GetStdHandle(STD_ERROR_HANDLE),
                   GetCurrentProcess(),
                   &si.hStdError,
                   0,
                   TRUE, // inheritable
                   DUPLICATE_SAME_ACCESS);

    si.dwFlags = STARTF_USESTDHANDLES;
    dwcreate = CREATE_SUSPENDED;
    if (!CreateProcess( NULL, cmdline,
                        NULL,
                        NULL,
                        TRUE, // inherit handles
                        dwcreate,
                        NULL, // environ
                        NULL, // cwd
                        &si,
                        pi))
        return 4;
    return 0;
}

// and here we go...
int main(int argc, char **argv)
{
    char out_buf[1024];
    int rc;
    DWORD cbwritten;
    DWORD exitcode;
    PROCESS_INFORMATION pi;
    if (argc==3 && strcmp(argv[1], "--kill")==0) {
        HANDLE h = OpenProcess(PROCESS_TERMINATE, 0, atoi(argv[2]));
        if (!h)
            return 1;
        if (!TerminateProcess(h, 0))
            return 2;
        CloseHandle(h);
        return 0;
    }
    // spawn the new suspended process
    rc = create_child(argc, argv, &pi);
    if (rc)
        return rc;
    // Write the 'terminate' command, which includes this PID, back to couch.
    // *sob* - what about spaces etc?
    sprintf_s(out_buf, sizeof(out_buf), "%s --kill %d\n", 
              argv[0], pi.dwProcessId);
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), out_buf, strlen(out_buf), 
              &cbwritten, NULL);
    // Let the child process go...
    ResumeThread(pi.hThread);
    // Wait for the process to terminate so we can reflect the exit code
    // back to couch.
    WaitForSingleObject(pi.hProcess, INFINITE);
    if (!GetExitCodeProcess(pi.hProcess, &exitcode))
        return 6;
    return exitcode;
}
