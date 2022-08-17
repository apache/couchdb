ARG COUCHDB_IMAGE
FROM ${COUCHDB_IMAGE}

# Install SpiderMonkey 78 and tell CouchDB to use it in configure
ENV SM_VSN=78

USER root

# These lines are necessary if the user has cloned the repo to their local machine
# and clicked the "Reopen in container button"
RUN mkdir -p /workspaces/couchdb
WORKDIR /workspaces/couchdb
COPY . .
