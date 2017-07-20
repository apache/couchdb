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
    COUCHDB_IO_LOG_DIR = '/tmp/couchjslogs'
    // Following fix an issue with git <= 2.6.5 where no committer
    // name or email are present for reflog, required for git clone
    GIT_COMMITTER_NAME = 'Jenkins User'
    GIT_COMMITTER_EMAIL = 'couchdb@apache.org'
  }

  stages {
    stage('Build') {
      agent {
        docker {
          label 'ubuntu'
          // This image has the oldest Erlang we support, 16B03
          image 'couchdbdev/ubuntu-14.04-erlang-default'
          // https://github.com/jenkins-infra/jenkins.io/blob/master/Jenkinsfile#64
          // We need the jenkins user mapped inside of the image
          // npm config cache below is required because /home/jenkins doesn't
          // ACTUALLY exist in the image
          // We need root here to clean up after previous runs where we used to do everything as root
          args '-e npm_config_cache=npm-cache -e HOME=. -v=/etc/passwd:/etc/passwd -v /etc/group:/etc/group --user 0:0'
        }
      }
      steps {
        timeout(time: 15, unit: "MINUTES") {
          sh '''
            set
            rm -rf apache-couchdb-*
            ./configure --with-curl
            make dist
            chmod -R a+w * .
          '''
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
        parallel(centos6erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/centos-6-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/centos-6-erlang-18.3', args: '-e LD_LIBRARY_PATH=/usr/local/bin') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
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
        centos7erlangdefault: {
          node(label: 'ubuntu') {
            timeout(time: 45, unit: "MINUTES") {
              sh 'docker pull couchdbdev/centos-7-erlang-default'
              withDockerContainer(image: 'couchdbdev/centos-7-erlang-default', args: '-e LD_LIBRARY_PATH=/usr/local/bin') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball
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
            deleteDir()
          } // node
        },
        centos7erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/centos-7-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/centos-7-erlang-18.3', args: '-e LD_LIBRARY_PATH=/usr/local/bin') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
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
        ubuntu1204erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 45, unit: "MINUTES") {
              sh 'docker pull couchdbdev/ubuntu-12.04-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/ubuntu-12.04-erlang-18.3') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR
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
            deleteDir()
          } // node
        },
        ubuntu1404erlangdefault: {
          node(label: 'ubuntu') {
            timeout(time: 45, unit: "MINUTES") {
              sh 'docker pull couchdbdev/ubuntu-14.04-erlang-default'
              withDockerContainer(image: 'couchdbdev/ubuntu-14.04-erlang-default') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR
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
            deleteDir()
          } // node
        },
        ubuntu1404erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/ubuntu-14.04-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/ubuntu-14.04-erlang-18.3') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
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
                  mv ../couchdb/*deb $cwd/pkgs/$platform || true
                '''
              } // withDocker
            } // timeout
            archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
            deleteDir()
          } // node
        },
        ubuntu1604erlangdefault: {
          node(label: 'ubuntu') {
            timeout(time: 45, unit: "MINUTES") {
              sh 'docker pull couchdbdev/ubuntu-16.04-erlang-default'
              withDockerContainer(image: 'couchdbdev/ubuntu-16.04-erlang-default') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR
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
            deleteDir()
          } // node
        },
        ubuntu1604erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/ubuntu-16.04-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/ubuntu-16.04-erlang-18.3') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
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
                  mv ../couchdb/*deb $cwd/pkgs/$platform || true
                '''
              } // withDocker
            } // timeout
            archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
            deleteDir()
          } // node
        },
        debian8erlangdefault: {
          node(label: 'ubuntu') {
            timeout(time: 45, unit: "MINUTES") {
              sh 'docker pull couchdbdev/debian-8-erlang-default'
              withDockerContainer(image: 'couchdbdev/debian-8-erlang-default') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR
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
            deleteDir()
          } // node
        },
        debian8erlang183: {
          node(label: 'ubuntu') {
            timeout(time: 60, unit: "MINUTES") {
              sh 'docker pull couchdbdev/debian-8-erlang-18.3'
              withDockerContainer(image: 'couchdbdev/debian-8-erlang-18.3') {
                sh 'rm -f apache-couchdb-*.tar.gz'
                unstash 'tarball'
                sh '''
                  cwd=$(pwd)
                  mkdir -p $COUCHDB_IO_LOG_DIR

                  # Build CouchDB from tarball
                  builddir=$(mktemp -d)
                  cd $builddir
                  tar -xf $cwd/apache-couchdb-*.tar.gz
                  cd apache-couchdb-*
                  ./configure --with-curl
                  make all
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
                  mv ../couchdb/*deb $cwd/pkgs/$platform || true
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
        docker {
          // This image has the deb AND rpm repo tools installed in it
          image 'couchdbdev/debian-8-base'
          // We need the jenkins user mapped inside of the image
          args '-v /etc/passwd:/etc/passwd -v /etc/group:/etc/group'
          label 'ubuntu'
        }
      }
      steps {
        withCredentials([file(credentialsId: 'jenkins-key', variable: 'KEY')]) {
          sh 'rm -rf pkgs *.tar.gz'
          unarchive mapping: ['pkgs/' : '.']
          unstash 'tarball'
          echo 'Retrieving & cleaning current couchdb-vm2 tree...'
          sh '''
            rsync -avz -e "ssh -o StrictHostKeyChecking=no -i $KEY" jenkins@couchdb-vm2.apache.org:/var/www/html/$BRANCH_NAME .
            rm -rf $BRANCH_NAME/debian/* $BRANCH_NAME/el6/* $BRANCH_NAME/el7/*
          '''
          echo 'Building Debian repo...'
          sh '''
            git clone https://github.com/apache/couchdb-pkg
            reprepro -b couchdb-pkg/repo includedeb jessie pkgs/jessie/*deb
            reprepro -b couchdb-pkg/repo includedeb trusty pkgs/trusty/*deb
            reprepro -b couchdb-pkg/repo includedeb xenial pkgs/xenial/*deb
          '''
          echo 'Building CentOS repos...'
          sh '''
            cd pkgs/centos6 && createrepo --database .
            cd ../centos7 && rm -f js* && createrepo --database .
          '''
          echo 'rsyncing tree to couchdb-vm2...'
          sh '''
            mv couchdb-pkg/repo/pool $BRANCH_NAME/debian
            mv couchdb-pkg/repo/dists $BRANCH_NAME/debian
            mv pkgs/centos6/* $BRANCH_NAME/el6
            mv pkgs/centos7/* $BRANCH_NAME/el7
            mv apache-couchdb-*.tar.gz $BRANCH_NAME/source
            cd $BRANCH_NAME/source
            ls -1tr | head -n -10 | xargs -d '\n' rm -f --
            cd ../..
            rsync -avz --delete -e "ssh -o StrictHostKeyChecking=no -i $KEY" $BRANCH_NAME jenkins@couchdb-vm2.apache.org:/var/www/html
            rm -rf $BRANCH_NAME couchdb-pkg *.tar.gz
          '''
          deleteDir()
        } // withCredentials
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
