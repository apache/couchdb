# CouchDB and Jenkins CI

In 2019, ASF and CloudBees reached an agreement to allow
the Foundation to use CloudBees Core to have a farm of managed Jenkins
masters. This allows the ASF to give larger projects their own dedicated
Jenkins master, which can be custom configured for the project. They can
readily manage this farm of Jenkins masters centrally, including push
updates to all masters and their plugins. Naturally, this also reduces
contention, as well as providing increased security for project-level
credentials.  CouchDB is the first project to use this setup, via
https://ci-couchdb.apache.org/ (aka https://jenkins-cm1.apache.org/)

Only members of the ASF LDAP group `couchdb-pmc` have write access to
the Jenkins CI server, and all jobs are set to not trust changes to the
Jenkinsfile from forked repos (see below).

Further, IBM is sponsoring CouchDB with cloud-based worker nodes, for
the project's exclusive use. Combined with the FreeBSD and OSX nodes the
project already had, this will provide the necessary compute resources
to meet the needs of the project for some time to come.

# Jenkins Configuration

All jobs on the new Jenkins master are contained in a CouchDB folder.
Credentials for the project are placed within this folder; this is a
unique capability of CloudBees Core at this time.

## Pull Requests

[Blue Ocean link](https://ci-couchdb.apache.org/blue/organizations/jenkins/jenkins-cm1%2FPullRequests/activity/)

To implement build-a-PR functionality in the same way that Travis
performs builds, Jenkins offers the Multibranch Pipeline job. Here's how
it's configured for CouchDB through the GUI:

* Job name: PullRequests (jobs shouldn't have spaces in them, because
  the job name is used for workspace path naming.)
* Display Name: "Pull Requests"
* Description: "This job builds all GitHub pull requests against
  apache/couchdb."
* Branch sources: Github
  * Credentials: a GitHub API key from wohali. These credentials are
    stored in the top-level Jenkins CouchDB folder on the server.
    The API token credentials are `user:email` and `repo:status`.
  * URL https://github.com/apache/couchdb
  * Behaviors
    * Discover branches: Exclude branches that are also filed as PRs
    * Discover PRs from origin: Merging the PR with the current target
      branch revision
    * Discover PRs from works: Merging the PR with the current target
      branch revision, trust Nobody [2]
    * Advanced clone behaviours:
      * Fetch tags
      * Clear "Shallow clone" [1]
    * Clean before checkout [1]
    * Prune stale remote-tracking branches
  * Property strategy: All branches get the same properties
* Build Configuration
  * Mode: by Jenkinsfile
  * Script path: `build-aux/Jenkinsfile.pr`
* Scan Repository Triggers
  * Periodically if not other wise run
  * Interval: 1 day (do not set this too high, GitHub has an API token
  throttle that can cause issues!)
* Orphaned Item Strategy
  * Discard old items
  * Days to keep old items: <blank>
  * Max # of old items to keep: 10
* Everything else set as defaults.

[1]: https://issues.jenkins-ci.org/browse/JENKINS-44598 explains why we have the build set to Clean Before Checkout every time, and why clones are not shallow.

[2]: https://issues.apache.org/jira/browse/INFRA-17449 explains why "Discover pull requests from forks/Trust" has been set to "Nobody."

## Full Platform Builds on `main` and release branches

[![main branch status](https://ci-couchdb.apache.org/job/jenkins-cm1/job/FullPlatformMatrix/job/main/badge/icon?subject=main)](https://ci-couchdb.apache.org/blue/organizations/jenkins/jenkins-cm1%2FFullPlatformMatrix/activity?branch=main)
[![3.x branch status](https://ci-couchdb.apache.org/job/jenkins-cm1/job/FullPlatformMatrix/job/3.x/badge/icon?subject=3.x)](https://ci-couchdb.apache.org/blue/organizations/jenkins/jenkins-cm1%2FFullPlatformMatrix/activity?branch=3.x)

Our original Jenkins job (formerly `/Jenkinsfile`) is now
`build-aux/Jenkinsfile.full`. This builds CouchDB on `main`, all of
our release branches (`2.*`, `3.*`, etc.) as well as any branch prefixed with
`jenkins-` for testing on a wide variety of supported operating systems.

Settings are as follows:

* Job name: FullPlatformMatrix (jobs shouldn't have spaces in them, because
  the job name is used for workspace path naming.)
* Display Name: "Full Platform Builds"
* Description: "This job builds on our master and release branches,
  and builds packages on all."
* Branch sources: Github
  * Credentials: a GitHub API key from wohali. These credentials are
    stored in the top-level Jenkins CouchDB folder on the server.
    The API token credentials are `user:email` and `repo:status`.
  * URL https://github.com/apache/couchdb
  * Behaviors
    * Discover branches: All branches
    * Filter by name (with wildcards): Include: master 2.*.x 3.*.x 4.*.x jenkins-*
    * Advanced clone behaviours:
      * Fetch tags
      * Clear "Shallow clone" [1]
    * Clean before checkout [1]
    * Prune stale remote-tracking branches
  * Property strategy: All branches get the same properties
* Build Configuration
  * Mode: by Jenkinsfile
  * Script path: `build-aux/Jenkinsfile.pull`
* Scan Repository Triggers
  * none
* Orphaned Item Strategy
  * Discard old items
  * Days to keep old items: <blank>
  * Max # of old items to keep: 10
* Everything else set as defaults.

## Other Resources

The [apache/couchdb-ci](https://github.com/apache/couchdb-ci) repo contains the
dockerfiles that we use to generate the container images used for our
container-based builds. These images are hosted on Docker Hub in the following
repos:

* [apache/couchdbci-debian](https://hub.docker.com/r/apache/couchdbci-debian)
* [apache/couchdbci-ubuntu](https://hub.docker.com/r/apache/couchdbci-ubuntu)
* [apache/couchdbci-centos](https://hub.docker.com/r/apache/couchdbci-centos)

The [apache/couchdb-pkg](https://github.com/apache/couchdb-ci) repo contains
a set of helper scripts to build binary packages for Debian / CentOS / Ubuntu
from the contents of a release tarball. The packaging stage of our "Full
Platform Builds" pipeline clones this repo to produces the package artifacts.
