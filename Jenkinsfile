#!groovy
//
//
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

// DRYing out the Jenkinsfile...

build_and_test = '''
mkdir -p ${COUCHDB_IO_LOG_DIR}
rm -rf build
mkdir build
cd build
tar -xf ${WORKSPACE}/apache-couchdb-*.tar.gz
cd apache-couchdb-*
./configure --with-curl
make check || (build-aux/logfile-uploader.py && false)
'''

make_packages = '''
git clone https://github.com/apache/couchdb-pkg
rm -rf couchdb
mkdir couchdb
cp ${WORKSPACE}/apache-couchdb-*.tar.gz couchdb
tar -xf ${WORKSPACE}/apache-couchdb-*.tar.gz -C couchdb
cd couchdb-pkg
make ${platform} PLATFORM=${platform}
'''

cleanup_and_save = '''
rm -rf ${WORKSPACE}/pkgs/${platform}
mkdir -p ${WORKSPACE}/pkgs/${platform}
mv ${WORKSPACE}/rpmbuild/RPMS/$(arch)/*rpm ${WORKSPACE}/pkgs/${platform} || true
mv ${WORKSPACE}/couchdb/*.deb ${WORKSPACE}/pkgs/${platform} || true
'''

pipeline {

  // no top-level agent; agents must be declared for each stage
  agent {
    label 'ubuntu'
  }

  environment {
    COUCHAUTH = credentials('couchdb_vm2_couchdb')
    recipient = 'notifications@couchdb.apache.org'
    COUCHDB_IO_LOG_DIR = '/tmp/couchjslogs'
    // Following fix an issue with git <= 2.6.5 where no committer
    // name or email are present for reflog, required for git clone
    GIT_COMMITTER_NAME = 'Jenkins User'
    GIT_COMMITTER_EMAIL = 'couchdb@apache.org'
  }

  options {
    buildDiscarder(logRotator(numToKeepStr: '10', artifactNumToKeepStr: '10'))
    // This fails the build immediately if any parallel step fails
    parallelsAlwaysFailFast()
    preserveStashes(buildCount: 10)
    timeout(time: 3, unit: 'HOURS')
    timestamps()
  }

  stages {
    stage('Build Release Tarball') {
      agent {
        // https://github.com/jenkins-infra/jenkins.io/blob/master/Jenkinsfile#64
        // We need the jenkins user mapped inside of the image
        // npm config cache below is required because /home/jenkins doesn't
        // ACTUALLY exist in the image
        docker {
          image 'couchdbdev/debian-stretch-erlang-19.3.6:latest'
          alwaysPull true
          args '-e npm_config_cache=npm-cache -e HOME=. -v=/etc/passwd:/etc/passwd -v /etc/group:/etc/group'
          label 'ubuntu'
        }
      }
      options {
        timeout(time: 15, unit: "MINUTES")
      }
      steps {
        sh '''
          set
          rm -rf apache-couchdb-*
          ./configure --with-curl
          make dist
          chmod -R a+w * .
        '''
      }
      post {
        success {
          stash includes: 'apache-couchdb-*.tar.gz', name: 'tarball'
          archiveArtifacts artifacts: 'apache-couchdb-*.tar.gz', fingerprint: true
        }
        cleanup {
          // UGH see https://issues.jenkins-ci.org/browse/JENKINS-41894
          sh 'rm -rf ${WORKSPACE}/*'
        }
      }
    } // stage Build Release Tarball

    // TODO Rework once Improved Docker Pipeline Engine is released
    // https://issues.jenkins-ci.org/browse/JENKINS-47962
    // https://issues.jenkins-ci.org/browse/JENKINS-48050

    stage('Test and Package') {

      parallel {

        stage('FreeBSD') {
          agent {
            label 'couchdb && freebsd'
          }
          options {
            skipDefaultCheckout()
            timeout(time: 90, unit: "MINUTES")
          }
          steps {
            // deleteDir is OK here because we're not inside of a Docker container!
            deleteDir()
            unstash 'tarball'
            withEnv(['HOME='+pwd()]) {
              sh '''
                mkdir -p $COUCHDB_IO_LOG_DIR

                # Build CouchDB from tarball & test
                mkdir build
                cd build
                tar -xf $WORKSPACE/apache-couchdb-*.tar.gz
                cd apache-couchdb-*
                ./configure --with-curl
                gmake check || (build-aux/logfile-uploader.py && false)

                # No package build for FreeBSD at this time
              '''
            } // withEnv
          } // steps
          post {
            always {
              junit '**/.eunit/*.xml, **/_build/*/lib/couchdbtest/*.xml'
            }
            cleanup {
              sh 'rm -rf $COUCHDB_IO_LOG_DIR'
            }
          } // post
        } // stage FreeBSD

        stage('CentOS 6') {
          agent {
            docker {
              image 'couchdbdev/centos-6-erlang-19.3.6:latest'
              alwaysPull true
              label 'ubuntu'
              // this keeps builds landing on the same host from clashing with each other
              customWorkspace pwd() + '/centos6'
            }
          }
          options {
            skipDefaultCheckout()
            timeout(time: 90, unit: "MINUTES")
          }
          environment {
            platform = 'centos6'
          }
          stages {
            stage('Build from tarball & test') {
              steps {
                unstash 'tarball'
                sh( script: build_and_test )
              }
              post {
                always {
                  junit '**/.eunit/*.xml, **/_build/*/lib/couchdbtest/*.xml'
                }
              }
            }
            stage('Build CouchDB packages') {
              steps {
                sh( script: make_packages )
                sh( script: cleanup_and_save )
              }
              post {
                success {
                  archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
                }
              }
            }
          } // stages
          post {
            cleanup {
              sh 'rm -rf ${WORKSPACE}/*'
            }
          } // post
        } // stage

        stage('CentOS 7') {
          agent {
            docker {
              image 'couchdbdev/centos-7-erlang-19.3.6:latest'
              alwaysPull true
              label 'ubuntu'
              customWorkspace pwd() + '/centos7'
            }
          }
          options {
            skipDefaultCheckout()
            timeout(time: 90, unit: "MINUTES")
          }
          environment {
            platform = 'centos7'
          }
          stages {
            stage('Build from tarball & test') {
              steps {
                unstash 'tarball'
                sh( script: build_and_test )
              }
              post {
                always {
                  junit '**/.eunit/*.xml, **/_build/*/lib/couchdbtest/*.xml'
                }
              }
            }
            stage('Build CouchDB packages') {
              steps {
                unstash 'tarball'
                sh( script: make_packages )
                sh( script: cleanup_and_save )
              }
              post {
                success {
                  archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
                }
              }
            }
          } // stages
          post {
            cleanup {
              sh 'rm -rf ${WORKSPACE}/*'
            }
          } // post
        } // stage

        stage('Ubuntu Xenial') {
          agent {
            docker {
              image 'couchdbdev/ubuntu-xenial-erlang-19.3.6:latest'
              alwaysPull true
              label 'ubuntu'
              customWorkspace pwd() + '/xenial'
            }
          }
          options {
            skipDefaultCheckout()
            timeout(time: 90, unit: "MINUTES")
          }
          environment {
            platform = 'xenial'
          }
          stages {
            stage('Build from tarball & test') {
              steps {
                unstash 'tarball'
                sh( script: build_and_test )
              }
              post {
                always {
                  junit '**/.eunit/*.xml, **/_build/*/lib/couchdbtest/*.xml'
                }
              }
            }
            stage('Build CouchDB packages') {
              steps {
                sh( script: make_packages )
                sh( script: cleanup_and_save )
              }
              post {
                success {
                  archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
                }
              }
            }
          } // stages
          post {
            cleanup {
              sh 'rm -rf ${WORKSPACE}/*'
            }
          } // post
        } // stage

        stage('Ubuntu Bionic') {
          agent {
            docker {
              image 'couchdbdev/ubuntu-bionic-erlang-19.3.6:latest'
              alwaysPull true
              label 'ubuntu'
              customWorkspace pwd() + '/bionic'
            }
          }
          options {
            skipDefaultCheckout()
            timeout(time: 90, unit: "MINUTES")
          }
          environment {
            platform = 'bionic'
          }
          stages {
            stage('Build from tarball & test') {
              steps {
                unstash 'tarball'
                sh( script: build_and_test )
              }
              post {
                always {
                  junit '**/.eunit/*.xml, **/_build/*/lib/couchdbtest/*.xml'
                }
              }
            }
            stage('Build CouchDB packages') {
              steps {
                sh( script: make_packages )
                sh( script: cleanup_and_save )
              }
              post {
                success {
                  archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
                }
              }
            }
          } // stages
          post {
            cleanup {
              sh 'rm -rf ${WORKSPACE}/*'
            }
          } // post
        } // stage

        stage('Debian Jessie') {
          agent {
            docker {
              image 'couchdbdev/debian-jessie-erlang-19.3.6:latest'
              alwaysPull true
              label 'ubuntu'
              customWorkspace pwd() + '/jessie'
            }
          }
          options {
            skipDefaultCheckout()
            timeout(time: 90, unit: "MINUTES")
          }
          environment {
            platform = 'jessie'
          }
          stages {
            stage('Build from tarball & test') {
              steps {
                unstash 'tarball'
                sh( script: build_and_test )
              }
              post {
                always {
                  junit '**/.eunit/*.xml, **/_build/*/lib/couchdbtest/*.xml'
                }
              }
            }
            stage('Build CouchDB packages') {
              steps {
                sh( script: make_packages )
                sh( script: cleanup_and_save )
              }
              post {
                success {
                  archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
                }
              }
            }
          } // stages
          post {
            cleanup {
              sh 'rm -rf ${WORKSPACE}/*'
            }
          } // post
        } // stage

        stage('Debian Stretch x86_64') {
          agent {
            docker {
              image 'couchdbdev/debian-stretch-erlang-19.3.6:latest'
              alwaysPull true
              label 'ubuntu'
              customWorkspace pwd() + '/stretch'
            }
          }
          options {
            skipDefaultCheckout()
            timeout(time: 90, unit: "MINUTES")
          }
          environment {
            platform = 'stretch'
          }
          stages {
            stage('Build from tarball & test') {
              steps {
                unstash 'tarball'
                sh( script: build_and_test )
              }
              post {
                always {
                  junit '**/.eunit/*.xml, **/_build/*/lib/couchdbtest/*.xml'
                }
              }
            }
            stage('Build CouchDB packages') {
              steps {
                sh( script: make_packages )
                sh( script: cleanup_and_save )
              }
              post {
                success {
                  archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
                }
              }
            }
          } // stages
          post {
            cleanup {
              sh 'rm -rf ${WORKSPACE}/*'
            }
          } // post
        } // stage

        stage('Debian Stretch aarch64') {
          agent {
            docker {
              image 'couchdbdev/aarch64-debian-stretch-erlang-20.3.8.20:latest'
              alwaysPull true
              label 'arm'
              customWorkspace pwd() + '/arm'
            }
          }
          options {
            skipDefaultCheckout()
            timeout(time: 90, unit: "MINUTES")
          }
          environment {
            platform = 'aarch64-debian-stretch'
          }
          stages {
            stage('Build from tarball & test') {
              steps {
                unstash 'tarball'
                sh( script: build_and_test )
              }
              post {
                always {
                  junit '**/.eunit/*.xml, **/_build/*/lib/couchdbtest/*.xml'
                }
              }
            }
            stage('Build CouchDB packages') {
              steps {
                sh( script: make_packages )
                sh( script: cleanup_and_save )
              }
              post {
                success {
                  archiveArtifacts artifacts: 'pkgs/**', fingerprint: true
                }
              }
            }
          } // stages
          post {
            cleanup {
              sh 'rm -rf ${WORKSPACE}/*'
            }
          } // post
        } // stage

      } // parallel
    } // stage "Make Check"

    stage('Publish') {

      when {
        expression { return env.BRANCH_NAME ==~ /master|2.0.x|2.1.x|jenkins-.*/ }
      }

      agent {
        docker {
          image 'couchdbdev/debian-stretch-erlang-19.3.6:latest'
          alwaysPull true
          args '-e npm_config_cache=npm-cache -e HOME=. -v=/etc/passwd:/etc/passwd -v /etc/group:/etc/group'
          label 'ubuntu'
        }
      }
      options {
        skipDefaultCheckout()
        timeout(time: 90, unit: "MINUTES")
      }

      steps {
        withCredentials([file(credentialsId: 'jenkins-key', variable: 'KEY')]) {
          sh 'rm -rf ${WORKSPACE}/*'
          unstash 'tarball'
          unarchive mapping: ['pkgs/' : '.']
  
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
        } // withCredentials
      } // steps
    } // stage
  } // stages

  post {
    success {
      mail to: "${env.recipient}",
        replyTo: "${env.recipient}",
        subject: "[Jenkins] SUCCESS: ${currentBuild.fullDisplayName}",
        body: "Yay, we passed. ${env.RUN_DISPLAY_URL}"
    }
    unstable {
      mail to: "${env.recipient}",
        replyTo: "${env.recipient}",
        subject: "[Jenkins] SUCCESS: ${currentBuild.fullDisplayName}",
        body: "Eep! Build is unstable... ${env.RUN_DISPLAY_URL}"
    }
    failure {
      mail to: "${env.recipient}",
        replyTo: "${env.recipient}",
        subject: "[Jenkins] FAILURE: ${currentBuild.fullDisplayName}",
        body: "Boo, we failed. ${env.RUN_DISPLAY_URL}"
    }
    cleanup {
      sh 'rm -rf ${COUCHDB_IO_LOG_DIR}'
    }
  }

} // pipeline
