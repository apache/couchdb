---
name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: 'Single Source Of Truth (SSOT) system for CouchDBs configuration files'
labels: rfc, discussion
assignees: 'ronny@apache.org'

---

# Introduction

This document describes the design of a Single Source Of Truth (SSOT)
system for generating configuration files and values used by CouchDB. 

## Abstract

Currently, there are two main files for defining and setting default
values of CouchDBs database system. These are `default.ini` and
`local.ini`. In these two files, configuration keys and values can
be set explicitly (they are defined), implicitly (defined, but 
commented out) and sometimes not mentioned at all and defined only in 
the source files (e.g. some internal config values).

There is no easy / no overview at all for administrators, users and 
developers to lookup for defined configuration keys and values. The
documentation provides a [Configuration Help for Administrators][1],
a [Configuration Quick Reference][2] for an overview of hopefully all
documented keys in different configuration sections. It's not clear if
the values in the help are default values or only example values and
looked into the default config files, it's unclear too, if commented out
values are the defaults or not.

To avoid these problems, implement a Single Source Of Truth (SSOT) system
which resolves the named issues and providing features for generating and
checking configuration files at build-time and at run-time.

## Requirements Language

The keywords "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

`SSOT` system: Single Source Of Truth system. Every data element is mastered (or edited) in only 
one place.

---

# Detailed Description

The SSOT-system MUST provide the following features:

1. Define a schema definition for all existing sections, configuration keys
   and default values.
2. Within these definitions, all important values are explicitly defined and
   described. For example the name, default value, description, data type, range value,
   required field value as mentioning a few of it.
3. The default configuration file(s) are checked against the schema definition.

Idea:

1. Define a JSON schema definition for the complete config model
2. Every section is defined in a separate schema definition
3. Default config files are derived from the schema definition
4. Validate the config file against the complete schema definition

Questions:

1. With which language should the schema definition and derivation of
the files implemented. Erlang, Python, ...?
2. Versioning of the schema?
3. What should be done after updating the schema definition with existing
installs?
4. Upgrade path?
5. `default.ini` only readable?

[NOTE]: # ( Describe the solution being proposed in greater detail. )
[NOTE]: # ( Assume your audience has knowledge of, but not necessarily familiarity )
[NOTE]: # ( with, the CouchDB internals. Provide enough context so that the reader )
[NOTE]: # ( can make an informed decision about the proposal. )

[TIP]:  # ( Artwork may be attached to the submission and linked as necessary. )
[TIP]:  # ( ASCII artwork can also be included in code blocks, if desired. )

# Advantages and Disadvantages

[NOTE]: # ( Briefly, list the benefits and drawbacks that would be realized should )
[NOTE]: # ( the proposal be accepted for inclusion into Apache CouchDB. )

# Key Changes

[TIP]: # ( If the changes will affect how a user interacts with CouchDB, explain. )

## Applications and Modules affected

[NOTE]: # ( List the OTP applications or functional modules in CouchDB affected by the proposal. )

## HTTP API additions

[NOTE]: # ( Provide *exact* detail on each new API endpoint, including: )
[NOTE]: # (   HTTP methods [HEAD, GET, PUT, POST, DELETE, etc.] )
[NOTE]: # (   Synopsis of functionality )
[NOTE]: # (   Headers and parameters accepted )
[NOTE]: # (   JSON in [if a PUT or POST type] )
[NOTE]: # (   JSON out )
[NOTE]: # (   Valid status codes and their definitions )
[NOTE]: # (   A proposed Request and Response block )

## HTTP API deprecations

[NOTE]: # ( Provide *exact* detail on the API endpoints to be deprecated. )
[NOTE]: # ( If these endpoints are replaced by new endpoints, list those as well. )
[NOTE]: # ( State the proposed version in which the deprecation and removal will occur. )

# Security Considerations

[NOTE]: # ( Include any impact to the security of CouchDB here. )

# References

[1]: https://docs.couchdb.org/en/stable/config/index.html "Configuration Guide"
[2]: https://docs.couchdb.org/en/stable/config-ref.html "Configuration Quick Reference"

# Acknowledgements

[TIP]:  # ( Who helped you write this RFC? )
