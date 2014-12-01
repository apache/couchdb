#!/bin/bash
set -e
# Send SIGTERM to process group on exit, ensures background tasks die
# if script exits
cleanup () {
    kill -TERM 0
    wait
}
HERE=$(pwd)
PLUGIN="couchperuser"
COUCHDB_SRC="${HERE}/.eunit/couchdb"
COUCHDB_VER="1.6.1"
COUCHDB_REL="apache-couchdb-${COUCHDB_VER}"
COUCHDB_URL="http://apache.osuosl.org/couchdb/source/${COUCHDB_VER}/${COUCHDB_REL}.tar.gz"
COUCHDB_PREFIX="${COUCHDB_SRC}/${COUCHDB_REL}-build"
COUCHDB_PLUGINS="${COUCHDB_PREFIX}/lib/couchdb/plugins"
COUCHDB_DATA="${COUCHDB_PREFIX}/var/lib/couchdb"
couch () {
    echo "http://$1:$2@127.0.0.1:5985$3"
}
if [ ! -d "${COUCHDB_SRC}" ]; then
    echo "Downloading and unpacking ${COUCHDB_PREFIX}"
    mkdir -p "${COUCHDB_SRC}"
    pushd "${COUCHDB_SRC}"
    curl -f "${COUCHDB_URL}" | tar zxf -
    popd
fi
if [ ! -e "${COUCHDB_PREFIX}/bin/couchdb" ]; then
    echo "Compiling ${COUCHDB_PREFIX}"
    pushd "${COUCHDB_SRC}/${COUCHDB_REL}"
    ./configure --prefix="${COUCHDB_PREFIX}"
    make
    make install
    popd
fi
if [ ! -d "${COUCHDB_PLUGINS}/${PLUGIN}" ]; then
    echo "Symlinking plugin dir"
    if [ ! -d "${COUCHDB_PLUGINS}" ]; then
        mkdir -p "${COUCHDB_PLUGINS}"
    fi
    ln -sf "${HERE}" "${COUCHDB_PLUGINS}/${PLUGIN}"
fi
PATH="${COUCHDB_PREFIX}/bin:${PATH}"
rebar compile
if [ -d "${COUCHDB_DATA}" ]; then
    echo "Removing existing data files"
    rm -rf "${COUCHDB_DATA}"
fi
echo "Writing local.ini"
cat <<EOF > "${COUCHDB_PREFIX}/etc/couchdb/local.ini"
[couchdb]
uuid = 92a5cfcd7c8ad3a05b225e2fa8aba48f
[httpd]
port = 5985
bind_address = 127.0.0.1
[couch_httpd_auth]
secret = d8c211410a5fb33d2458aba4b7bb593c
[admins]
; admin : password
admin = -pbkdf2-341d6b96564af2f7a1a88fada73dbcd6ab7e061e,8aa4052c63662a4bf8cf575891513595,10
EOF
couchdb &
trap 'cleanup' SIGINT SIGTERM EXIT
while ! (curl -f -s "$(couch)" 2>&1 >/dev/null); do
    sleep 0.1
done
secret_json () {
    echo "{\"secret\":\"$1\"}"
}
to_hex () {
    printf "%s" "$1" | xxd -ps
}
userdb () {
    echo "/userdb-$(to_hex "$1")$2"
}
put_json () {
    curl -f -s \
         -HContent-Type:application/json \
         -XPUT "$1" \
         --data-binary "$2"
}
get_json () {
    curl -f -s "$1"
}
create_user () {
    put_json \
        "$(couch "" "" "/_users/org.couchdb.user:$1")" \
        "{\"_id\": \"org.couchdb.user:$1\",\"name\": \"$1\",\"roles\": [],\"type\": \"user\",\"password\": \"password\"}" \
        >/dev/null
}
fail () {
    printf "FAIL: %s\n" "$(printf "$@")" 1>&2
    exit 1
}
create_user eve
for user in alice bob; do
    # Create users with no authentication
    create_user "${user}"
    # Expect database to exist, but give it some time to trigger the change
    for x in $(seq 10); do
        if ! (get_json "$(couch "${user}" "password" "$(userdb "${user}")")" > /dev/null); then
            if [ "$x" -ge "10" ]; then
                fail "Expected create of user %s to create db %s" "${user}" "$(userdb "{user}")"
            else
                sleep 0.2
            fi
        else
            break
        fi
    done
    # Write doc with correct authentication
    if ! (put_json \
        "$(couch "${user}" "password" $(userdb "${user}" "/secret"))" \
        "$(secret_json "${user}")" \
        >/dev/null); then
        fail "User %s could not PUT %s" "${user}" "$(userdb "${user}" "/secret")"
    fi
    # Read doc with correct authentication
    if ! (get_json "$(couch "${user}" "password" $(userdb "${user}" "/secret"))" >/dev/null); then
        fail "User %s could not GET %s" "${user}" "$(userdb "${user}" "/secret")"
    fi
    # Try to read doc without authentication
    if (get_json "$(couch "" "" $(userdb "${user}" "/secret"))" >/dev/null); then
        fail "Expected unauthenticated read for %s database %s to fail" "${user}" "$(userdb "${user}" "/secret")"
    fi
    # Try to read doc with incorrect authentication
    if (get_json "$(couch "eve" "password" $(userdb "${user}" "/secret"))" >/dev/null); then
        fail "Expected %s read for %s database %s to fail" "eve" "${user}" "$(userdb "${user}" "/secret")"
    fi
    # Try to write doc without authentication
    if (put_json "$(couch "" "" $(userdb "${user}" "/notsecret"))" "{\"secret\":\"oops\"}" >/dev/null); then
        fail "Expected unauthenticated write for %s database %s to fail" "${user}" $(userdb "${user}" "/notsecret")
    fi
    # Try to write doc with incorrect authentication
    if (put_json "$(couch "eve" "password" $(userdb "${user}" "/notsecret"))" "{\"secret\":\"oops\"}" >/dev/null); then
        fail "Expected %s write for %s database %s to fail" "eve" "${user}" $(userdb "${user}" "/notsecret")
    fi
done
trap - SIGINT SIGTERM EXIT
for job in $(jobs -p); do
    kill -SIGTERM $job
done
wait
