#!/bin/bash

emacs -l simpleproj-tests.el -l util-test.el -f ert-run-tests-batch -q -batch
