rebar
=====

rebar is an Erlang build tool that makes it easy to compile and test Erlang
applications, port drivers and releases.

[![Build Status](https://secure.travis-ci.org/rebar/rebar.png?branch=master)](http://travis-ci.org/rebar/rebar)

rebar is a self-contained Erlang script, so it's easy to distribute or even
embed directly in a project. Where possible, rebar uses standard Erlang/OTP
conventions for project structures, thus minimizing the amount of build
configuration work. rebar also provides dependency management, enabling
application writers to easily re-use common libraries from a variety of
locations (git, hg, etc).

Building
--------

Information on building and installing [Erlang/OTP](http://www.erlang.org) can
be found [here](https://github.com/erlang/otp/wiki/Installation) ([more
info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

### Dependencies

To build rebar you will need a working installation of Erlang R13B03 (or later).

Should you want to clone the rebar repository, you will also require git.

#### Downloading

You can download a pre-built binary version of rebar from:

https://github.com/rebar/rebar/wiki/rebar

#### Building rebar

```sh
$ git clone git://github.com/rebar/rebar.git
$ cd rebar
$ ./bootstrap
Recompile: src/getopt
...
Recompile: src/rebar_utils
==> rebar (compile)
Congratulations! You now have a self-contained script called "rebar" in
your current working directory. Place this script anywhere in your path
and you can use rebar to build OTP-compliant apps.
```


Contributing to rebar
=====================

Please refer to [CONTRIBUTING](CONTRIBUTING.md).

Community and Resources
-----------------------

In case of problems that cannot be solved through documentation or examples, you
may want to try to contact members of the community for help. The community is
also where you want to go for questions about how to extend rebar, fill in bug
reports, and so on.

The main place to go for questions is the [rebar mailing
list](http://lists.basho.com/pipermail/rebar_lists.basho.com/). If you need
quick feedback, you can try the #rebar channel on
[irc.freenode.net](http://freenode.net). Be sure to check the
[wiki](https://github.com/rebar/rebar/wiki) first, just to be sure you're not
asking about things with well known answers.

For bug reports, roadmaps, and issues, visit the [github issues
page](https://github.com/rebar/rebar/issues).

General rebar community resources and links:

- [Rebar Mailing List](http://lists.basho.com/pipermail/rebar_lists.basho.com/)
- #rebar on [irc.freenode.net](http://freenode.net/)
- [wiki](https://github.com/rebar/rebar/wiki)
- [issues](https://github.com/rebar/rebar/issues)
