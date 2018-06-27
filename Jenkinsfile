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
// jenkins user == uid 910 for reference
pipeline {
  // no top-level agent; agents must be declared for each stage
  agent none

  environment {
    COUCHAUTH = credentials('couchdb_vm2_couchdb')
    recipient = 'notifications@couchdb.apache.org'
    COUCHDB_IO_LOG_DIR = '/tmp/couchjslogs'
    // Following fix an issue with git <= 2.6.5 where no committer
    // name or email are present for reflog, required for git clone
    GIT_COMMITTER_NAME = 'Jenkins User'
    GIT_COMMITTER_EMAIL = 'couchdb@apache.org'
  }

  stages {
    stage('Build') {
      agent {
        // Cannot use docker agent type because image will not be pulled fresh
        // each time. Instead, manually insert docker pull then run with the
        // the docker image.
        node {
          label 'ubuntu'
        }
      }
      steps {
        // This image has the oldest Erlang we support, 16B03
        sh 'docker pull couchdbdev/ubuntu-trusty-erlang-default:latest'
        timeout(time: 15, unit: "MINUTES") {
          // https://github.com/jenkins-infra/jenkins.io/blob/master/Jenkinsfile#64
          // We need the jenkins user mapped inside of the image
          // npm config cache below is required because /home/jenkins doesn't
          // ACTUALLY exist in the image
          withDockerContainer(image: 'couchdbdev/ubuntu-trusty-erlang-default', args: '-e npm_config_cache=npm-cache -e HOME=. -v=/etc/passwd:/etc/passwd -v /etc/group:/etc/group') {
            sh '''
              set
              rm -rf apache-couchdb-*
              ./configure --with-curl
              make dist
              chmod -R a+w * .
            '''
          }
        }
      }
      post {
        success {
          stash includes: 'apache-couchdb-*.tar.gz', name: 'tarball'
          archiveArtifacts artifacts: 'apache-couchdb-*.tar.gz', fingerprint: true
          deleteDir()
        }
        failure {
		  deleteDir()
        }
      }
    }

    // TODO rework this once JENKINS-41334 is released
    // https://issues.jenkins-ci.org/browse/JENKINS-41334

    // The builddir stuff is to prevent all the builds from live syncing
    // their build results to each other during the build, which ACTUALLY
    // HAPPENS. Ugh.

    // Build packages on supported platforms using esl's erlang
    stage('Test') {
      steps {
        parallel(freebsd: {
          node(label: 'couchdb && freebsd') {
            timeout(time: 60, unit: "MINUTES") {
              deleteDir()
              unstash 'tarball'
              withEnv(['HOME='+pwd()]) {
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR
  
                  # Build CouchDB from tarball & test
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  gmake check || (build-aux/logfile-uploader.py && false)

                  # No package build for FreeBSD at this time
                '''
              } // withEnv
            } // timeout
            deleteDir()
          } // node
        },
        centos6: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/centos-6-erlang-19.3.6'
              withDockerContainer(image: 'couchdbdev/centos-6-erlang-19.3.6') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball & test
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make check || (build-aux/logfile-uploader.py && false)

                  # Build CouchDB packages
                  cd $builddir
                  git clone https://github.com/apache/couchdb-pkg
                  mkdir couchdb
                  cp $cwd/apache-couchdb-*.tar.gz couchdb
                  tar -xf $cwd/apache-couchdb-*.tar.gz -C couchdb
                  cd couchdb-pkg
                  platform=centos6
                  make $platform PLATFORM=$platform

                  # Cleanup & save for posterity
                  rm -rf $cwd/pkgs/$platform && mkdir -p $cwd/pkgs/$platform
                  mv ../rpmbuild/RPMS/x86_64/*rpm $cwd/pkgs/$platform || true
                '''
              } // withDocker
            } // timeout
            archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
            deleteDir()
          } // node
        },
        centos7: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/centos-7-erlang-19.3.6'
              withDockerContainer(image: 'couchdbdev/centos-7-erlang-19.3.6') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball & test
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make check || (build-aux/logfile-uploader.py && false)

                  # Build CouchDB packages
                  cd $builddir
                  git clone https://github.com/apache/couchdb-pkg
                  mkdir couchdb
                  cp $cwd/apache-couchdb-*.tar.gz couchdb
                  tar -xf $cwd/apache-couchdb-*.tar.gz -C couchdb
                  cd couchdb-pkg
                  platform=centos7
                  make $platform PLATFORM=$platform

                  # Cleanup & save for posterity
                  rm -rf $cwd/pkgs/$platform && mkdir -p $cwd/pkgs/$platform
                  mv ../rpmbuild/RPMS/x86_64/*rpm $cwd/pkgs/$platform || true
                '''
              } // withDocker
            } // timeout
            archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
            deleteDir()
          } // node
        },
        ubuntutrusty: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/ubuntu-trusty-erlang-19.3.6'
              withDockerContainer(image: 'couchdbdev/ubuntu-trusty-erlang-19.3.6') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball & test
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make check || (build-aux/logfile-uploader.py && false)

                  # Build CouchDB packages
                  cd $builddir
                  git clone https://github.com/apache/couchdb-pkg
                  mkdir couchdb
                  cp $cwd/apache-couchdb-*.tar.gz couchdb
                  tar -xf $cwd/apache-couchdb-*.tar.gz -C couchdb
                  cd couchdb-pkg
                  platform=$(lsb_release -cs)
                  make $platform PLATFORM=$platform

                  # Cleanup & save for posterity
                  rm -rf $cwd/pkgs/$platform && mkdir -p $cwd/pkgs/$platform
                  mv ../couchdb/*.deb $cwd/pkgs/$platform || true
                '''
              } // withDocker
            } // timeout
            archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
            deleteDir()
          } // node
        },
        ubuntuxenial: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/ubuntu-xenial-erlang-19.3.6'
              withDockerContainer(image: 'couchdbdev/ubuntu-xenial-erlang-19.3.6') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball & test
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make check || (build-aux/logfile-uploader.py && false)

                  # Build CouchDB packages
                  cd $builddir
                  git clone https://github.com/apache/couchdb-pkg
                  mkdir couchdb
                  cp $cwd/apache-couchdb-*.tar.gz couchdb
                  tar -xf $cwd/apache-couchdb-*.tar.gz -C couchdb
                  cd couchdb-pkg
                  platform=$(lsb_release -cs)
                  make $platform PLATFORM=$platform

                  # Cleanup & save for posterity
                  rm -rf $cwd/pkgs/$platform && mkdir -p $cwd/pkgs/$platform
                  mv ../couchdb/*.deb $cwd/pkgs/$platform || true
                '''
              } // withDocker
            } // timeout
            archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
            deleteDir()
          } // node
        },
        ubuntubionic: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/ubuntu-bionic-erlang-19.3.6'
              withDockerContainer(image: 'couchdbdev/ubuntu-bionic-erlang-19.3.6') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball & test
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make check || (build-aux/logfile-uploader.py && false)

                  # Build CouchDB packages
                  cd $builddir
                  git clone https://github.com/apache/couchdb-pkg
                  mkdir couchdb
                  cp $cwd/apache-couchdb-*.tar.gz couchdb
                  tar -xf $cwd/apache-couchdb-*.tar.gz -C couchdb
                  cd couchdb-pkg
                  platform=$(lsb_release -cs)
                  make $platform PLATFORM=$platform

                  # Cleanup & save for posterity
                  rm -rf $cwd/pkgs/$platform && mkdir -p $cwd/pkgs/$platform
                  mv ../couchdb/*.deb $cwd/pkgs/$platform || true
                '''
              } // withDocker
            } // timeout
            archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
            deleteDir()
          } // node
        },
        debianjessie: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/debian-jessie-erlang-19.3.6'
              withDockerContainer(image: 'couchdbdev/debian-jessie-erlang-19.3.6') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball & test
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make check || (build-aux/logfile-uploader.py && false)

                  # Build CouchDB packages
                  cd $builddir
                  git clone https://github.com/apache/couchdb-pkg
                  mkdir couchdb
                  cp $cwd/apache-couchdb-*.tar.gz couchdb
                  tar -xf $cwd/apache-couchdb-*.tar.gz -C couchdb
                  cd couchdb-pkg
                  platform=$(lsb_release -cs)
                  make $platform PLATFORM=$platform

                  # Cleanup & save for posterity
                  rm -rf $cwd/pkgs/$platform && mkdir -p $cwd/pkgs/$platform
                  mv ../couchdb/*.deb $cwd/pkgs/$platform || true
                '''
              } // withDocker
            } // timeout
            archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
            deleteDir()
          } // node
        },
        debianstretch: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/debian-stretch-erlang-19.3.6'
              withDockerContainer(image: 'couchdbdev/debian-stretch-erlang-19.3.6') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball & test
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make check || (build-aux/logfile-uploader.py && false)

                  # Build CouchDB packages
                  cd $builddir
                  git clone https://github.com/apache/couchdb-pkg
                  mkdir couchdb
                  cp $cwd/apache-couchdb-*.tar.gz couchdb
                  tar -xf $cwd/apache-couchdb-*.tar.gz -C couchdb
                  cd couchdb-pkg
                  platform=$(lsb_release -cs)
                  make $platform PLATFORM=$platform

                  # Cleanup & save for posterity
                  rm -rf $cwd/pkgs/$platform && mkdir -p $cwd/pkgs/$platform
                  mv ../couchdb/*.deb $cwd/pkgs/$platform || true
                '''
              } // withDocker
            } // timeout
            archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
            deleteDir()
          } // node
        }
        ) // parallel
      } // steps
    } // stage

    stage('Publish') {
      when {
        expression { return env.BRANCH_NAME ==~ /master|2.0.x|2.1.x|jenkins-.*/ }
      }
      agent {
        // Cannot use docker agent type because image will not be pulled fresh
        // each time. Instead, manually insert docker pull then run with the
        // the docker image.
        node {
          label 'ubuntu'
        }
      }
      steps {
        sh 'docker pull couchdbdev/debian-stretch-erlang-19.3.6:latest'
        withDockerContainer(image: 'couchdbdev/debian-stretch-erlang-19.3.6:latest', args: '-e npm_config_cache=npm-cache -e HOME=. -v=/etc/passwd:/etc/passwd -v /etc/group:/etc/group') {
          withCredentials([file(credentialsId: 'jenkins-key', variable: 'KEY')]) {
            sh 'rm -rf pkgs *.tar.gz'
            unarchive mapping: ['pkgs/' : '.']
            unstash 'tarball'
            echo 'Retrieving & cleaning current couchdb-vm2 tree...'
            sh '''
              rsync -avz -e "ssh -o StrictHostKeyChecking=no -i $KEY" jenkins@couchdb-vm2.apache.org:/var/www/html/$BRANCH_NAME . || mkdir -p $BRANCH_NAME
              rm -rf $BRANCH_NAME/debian/* $BRANCH_NAME/el6/* $BRANCH_NAME/el7/*
              mkdir -p $BRANCH_NAME/debian $BRANCH_NAME/el6 $BRANCH_NAME/el7 $BRANCH_NAME/source
              rsync -avz -e "ssh -o StrictHostKeyChecking=no -i $KEY" jenkins@couchdb-vm2.apache.org:/var/www/html/js .
            '''
            echo 'Building Debian repo...'
            sh '''
              git clone https://github.com/apache/couchdb-pkg
              cp js/debian-jessie/*.deb pkgs/jessie
              reprepro -b couchdb-pkg/repo includedeb jessie pkgs/jessie/*.deb
              cp js/debian-stretch/*.deb pkgs/stretch
              reprepro -b couchdb-pkg/repo includedeb stretch pkgs/stretch/*.deb
              cp js/ubuntu-trusty/*.deb pkgs/trusty
              reprepro -b couchdb-pkg/repo includedeb trusty pkgs/trusty/*.deb
              cp js/ubuntu-xenial/*.deb pkgs/xenial
              reprepro -b couchdb-pkg/repo includedeb xenial pkgs/xenial/*.deb
              cp js/ubuntu-bionic/*.deb pkgs/bionic
              reprepro -b couchdb-pkg/repo includedeb bionic pkgs/bionic/*.deb
            '''
            echo 'Building CentOS repos...'
            sh '''
              cp js/centos-6/*rpm pkgs/centos6
              cp js/centos-7/*rpm pkgs/centos7
              cd pkgs/centos6 && createrepo --database .
              cd ../centos7 && createrepo --database .
            '''
            echo 'Building tree to upload...'
            sh '''
              mv couchdb-pkg/repo/pool $BRANCH_NAME/debian
              mv couchdb-pkg/repo/dists $BRANCH_NAME/debian
              mv pkgs/centos6/* $BRANCH_NAME/el6
              mv pkgs/centos7/* $BRANCH_NAME/el7
              mv apache-couchdb-*.tar.gz $BRANCH_NAME/source
              cd $BRANCH_NAME/source
              ls -1tr | head -n -10 | xargs -d '\n' rm -f --
              cd ../..
            '''
            echo 'rsyncing tree to couchdb-vm2...'
            sh '''
              rsync -avz --delete -e "ssh -o StrictHostKeyChecking=no -i $KEY" $BRANCH_NAME jenkins@couchdb-vm2.apache.org:/var/www/html
              rm -rf $BRANCH_NAME couchdb-pkg *.tar.gz
            '''
            deleteDir()
          } // withCredentials
        } // withDockerContainer
      } // steps
    } // stage
  } // stages

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
