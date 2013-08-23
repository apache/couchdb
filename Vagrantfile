# -*- mode: ruby -*-
# vi: set ft=ruby :

BOX_NAME = ENV['BOX_NAME'] || "ubuntu"
BOX_URI = ENV['BOX_URI'] || "http://files.vagrantup.com/precise64.box"
AWS_REGION = ENV['AWS_REGION'] || "us-east-1"
AWS_AMI    = ENV['AWS_AMI']    || "ami-d0f89fb9"

Vagrant::Config.run do |config|
  # Setup virtual machine box. This VM configuration code is always executed.
  config.vm.box = BOX_NAME
  config.vm.box_url = BOX_URI

  # Install couchdb dependencies if deployment was not done
  if Dir.glob("#{File.dirname(__FILE__)}/.vagrant/machines/default/*/id").empty?
    # install build-essential
    pkg_cmd = "apt-get update -qq; apt-get install -q -y build-essential git " 
        "autoconf autoconf-archive gnu-standards help2man texinfo; "

    # Install erlang
    pkg_cmd << "apt-get install -q -y erlang-base-hipe erlang-dev " \
        "erlang-manpages erlang-eunit erlang-nox erlang-xmerl erlang-inets; "

    # couchdb developper dependencies
    pkg_cmd << "apt-get install -q -y libmozjs185-dev libicu-dev " \
        "curl libcurl4-gnutls-dev libtool; "

    # doc dependencies
    pkg_cmd << "apt-get install -q -y help2man texinfo python-sphinx python-pip; " \
        "pip install -U pygments; "

    config.vm.provision :shell, :inline => pkg_cmd
  end
end


# Providers were added on Vagrant >= 1.1.0
Vagrant::VERSION >= "1.1.0" and Vagrant.configure("2") do |config|
  config.vm.provider :aws do |aws, override|
    aws.access_key_id = ENV["AWS_ACCESS_KEY_ID"]
    aws.secret_access_key = ENV["AWS_SECRET_ACCESS_KEY"]
    aws.keypair_name = ENV["AWS_KEYPAIR_NAME"]
    override.ssh.private_key_path = ENV["AWS_SSH_PRIVKEY"]
    override.ssh.username = "ubuntu"
    aws.region = AWS_REGION
    aws.ami    = AWS_AMI
    aws.instance_type = "t1.micro"
  end

  config.vm.provider :rackspace do |rs|
    config.ssh.private_key_path = ENV["RS_PRIVATE_KEY"]
    rs.username = ENV["RS_USERNAME"]
    rs.api_key  = ENV["RS_API_KEY"]
    rs.public_key_path = ENV["RS_PUBLIC_KEY"]
    rs.flavor   = /512MB/
    rs.image    = /Ubuntu/
  end

  config.vm.provider :virtualbox do |vb|
    config.vm.box = BOX_NAME
    config.vm.box_url = BOX_URI
  end

  config.vm.provider :lxc do |lxc|
    config.vm.box = BOX_NAME
    config.vm.box_url = BOX_URI
    lxc.customize 'cgroup.memory.limit_in_bytes', '1024M'
  end
end
