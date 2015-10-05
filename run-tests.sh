#!/bin/sh -ev

# Eunit/Javascript tests
make check

# Test Fauxton
cd src/fauxton
npm install
grunt test
