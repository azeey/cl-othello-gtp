#!/bin/bash

# Script to launch Othello as an engine for Quarry
#
# usage : othello-gtp.sh

cd $(dirname $0)

sbcl --script "othello-gtp.lisp" $@
