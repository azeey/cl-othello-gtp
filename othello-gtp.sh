#!/bin/bash

# Script to launch Othello as an engine for Quarry
#
# usage : othello-gtp.sh [strategy index]

cd $(dirname $0)

sbcl --script "othello-gtp.lisp" $@

# Use this if using clisp
#clisp -C "othello-gtp.lisp" $@

