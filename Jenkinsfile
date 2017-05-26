#!groovy
/*
Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy of
the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
License for the specific language governing permissions and limitations under
the License.
*/
pipeline {
  /* no top-level agent; agents must be declared for each stage */
  agent none

  environment {
    COUCHAUTH = credentials('couchdb_vm2_couchdb')
    recipient = 'notifications@couchdb.apache.org'
  }

  stages {
    stage('Build') {
      agent {
        docker {
          /* This image has the oldest Erlang we support, 16B03 */
          image 'couchdbdev/ubuntu-14.04-erlang-default'
          /* We need the jenkins user mapped inside of the image */
          args '-v /etc/passwd:/etc/passwd -v /etc/group:/etc/group'
        }
      }
      steps {
        timeout(time: 15, unit: "MINUTES") {
          /* npm config cache below is required because /home/jenkins doesn't
             ACTUALLY exist in the image */
          /* sh 'git clone --depth 10 https://github.com/apache/couchdb .' */
          sh '''
              export npm_config_cache=$(mktemp -d)
              ./configure --with-curl
              make dist
          '''
          stash includes: 'apache-couchdb-*.tar.gz', name: 'tarball'
          archiveArtifacts artifacts: 'apache-couchdb-*.tar.gz', fingerprint: true
          deleteDir()
        }
      }
    }

    /* TODO rework this once JENKINS-41334 is released
       https://issues.jenkins-ci.org/browse/JENKINS-41334 */
    /* The builddir stuff is to prevent all 10 builds from live syncing
       their build results to each other during the build. Moving the
       build outside of the workdir should speed up the build process too,
       though it does mean we pollute /tmp whenever a build fails. */
    stage('Test') {
      steps {
        parallel(centos6erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 45, unit: "MINUTES") {
              sh 'rm *.tar.gz || true'
              unstash 'tarball'
              sh 'docker pull couchdbdev/centos-6-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/centos-6-erlang-18.3', args: '-e LD_LIBRARY_PATH=/usr/local/bin --user 0:0') {
                sh '''
                  cwd=$(pwd)
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
                  make check || (build-aux/logfile-uploader.py && false)
                '''
              } // withDocker
            } // timeout
          } // node
        },
        centos7erlangdefault: {
          node(label: 'ubuntu') {
            timeout(time: 30, unit: "MINUTES") {
              sh 'rm *.tar.gz || true'
              unstash 'tarball'
              sh 'docker pull couchdbdev/centos-7-erlang-default'
              withDockerContainer(image: 'couchdbdev/centos-7-erlang-default', args: '-e LD_LIBRARY_PATH=/usr/local/bin --user 0:0') {
                sh '''
                  cwd=$(pwd)
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
                  make check || (build-aux/logfile-uploader.py && false)
                '''
              } // withDocker
            } // timeout
          } // node
        },
        centos7erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 30, unit: "MINUTES") {
              sh 'rm *.tar.gz || true'
              unstash 'tarball'
              sh 'docker pull couchdbdev/centos-7-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/centos-7-erlang-18.3', args: '-e LD_LIBRARY_PATH=/usr/local/bin --user 0:0') {
                sh '''
                  cwd=$(pwd)
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
                  make check || (build-aux/logfile-uploader.py && false)
                '''
              } // withDocker
            } // timeout
          } // node
        },
        ubuntu1204erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 30, unit: "MINUTES") {
              sh 'rm *.tar.gz || true'
              unstash 'tarball'
              sh 'docker pull couchdbdev/ubuntu-12.04-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/ubuntu-12.04-erlang-18.3', args: '--user 0:0') {
                sh '''
                  cwd=$(pwd)
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
                  make check || (build-aux/logfile-uploader.py && false)
                '''
              } // withDocker
            } // timeout
          } // node
        },
        ubuntu1404erlangdefault: {
          node(label: 'ubuntu') {
            timeout(time: 30, unit: "MINUTES") {
              sh 'rm *.tar.gz || true'
              unstash 'tarball'
              sh 'docker pull couchdbdev/ubuntu-14.04-erlang-default'
              withDockerContainer(image: 'couchdbdev/ubuntu-14.04-erlang-default', args: '--user 0:0') {
                sh '''
                  cwd=$(pwd)
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
                  make check || (build-aux/logfile-uploader.py && false)
                '''
              } // withDocker
            } // timeout
          } // node
        },
        ubuntu1404erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 30, unit: "MINUTES") {
              sh 'rm *.tar.gz || true'
              unstash 'tarball'
              sh 'docker pull couchdbdev/ubuntu-14.04-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/ubuntu-14.04-erlang-18.3', args: '--user 0:0') {
                sh '''
                  cwd=$(pwd)
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
                  make check || (build-aux/logfile-uploader.py && false)
                '''
              } // withDocker
            } // timeout
          } // node
        },
        ubuntu1604erlangdefault: {
          node(label: 'ubuntu') {
            timeout(time: 30, unit: "MINUTES") {
              sh 'rm *.tar.gz || true'
              unstash 'tarball'
              sh 'docker pull couchdbdev/ubuntu-16.04-erlang-default'
              withDockerContainer(image: 'couchdbdev/ubuntu-16.04-erlang-default', args: '--user 0:0') {
                sh '''
                  cwd=$(pwd)
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
                  make check || (build-aux/logfile-uploader.py && false)
                '''
              } // withDocker
            } // timeout
          } // node
        },
        ubuntu1604erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 30, unit: "MINUTES") {
              sh 'rm *.tar.gz || true'
              unstash 'tarball'
              sh 'docker pull couchdbdev/ubuntu-16.04-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/ubuntu-16.04-erlang-18.3', args: '--user 0:0') {
                sh '''
                  cwd=$(pwd)
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
                  make check || (build-aux/logfile-uploader.py && false)
                '''
              } // withDocker
            } // timeout
          } // node
        },
        debian8erlangdefault: {
          node(label: 'ubuntu') {
            timeout(time: 30, unit: "MINUTES") {
              sh 'rm *.tar.gz || true'
              unstash 'tarball'
              sh 'docker pull couchdbdev/debian-8-erlang-default'
              withDockerContainer(image: 'couchdbdev/debian-8-erlang-default', args: '--user 0:0') {
                sh '''
                  cwd=$(pwd)
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
                  make check || (build-aux/logfile-uploader.py && false)
                '''
              } // withDocker
            } // timeout
          } // node
        },
        debian8erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 30, unit: "MINUTES") {
              sh 'rm *.tar.gz || true'
              unstash 'tarball'
              sh 'docker pull couchdbdev/debian-8-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/debian-8-erlang-18.3', args: '--user 0:0') {
                sh '''
                  cwd=$(pwd)
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
                  make check || (build-aux/logfile-uploader.py && false)
                '''
              } // withDocker
            } // timeout
          } // node
        }
        ) // parallel
      } // steps
    } // stage

    stage('Publish') {
      when {
        branch '*(master|2.0.x|2.1.x)'
      }
      agent any
      steps {
        /* Push it somewhere useful other than Jenkins, maybe? */
        /* echo 'Publishing tarball...'
        unstash 'tarball' */
        echo 'Triggering Debian .deb builds...'
        echo 'Triggering Ubuntu .deb builds...'
        echo 'Triggering Ubuntu snap builds...'
        echo 'Triggering CentOS .rpm builds...'
        echo 'Cleaning workspace...'
        sh 'rm -rf * .[a-zA-Z]*'
      }
    }
  }

  post {
    success {
      mail to: "${env.recipient}",
        subject: "[Jenkins] SUCCESS: ${currentBuild.fullDisplayName}",
        replyTo: "${env.recipient}",
        body: "Yay, we passed. ${env.BUILD_URL}"
    }
    failure {
      mail to: "${env.recipient}",
        subject: "[Jenkins] FAILURE: ${currentBuild.fullDisplayName}",
        replyTo: "${env.recipient}",
        body: "Boo, we failed. ${env.BUILD_URL}"
    }
  }
}
