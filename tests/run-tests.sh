#!/bin/bash

emacs -l simpleproj-tests.el -f ert-run-tests-batch -q -batch
