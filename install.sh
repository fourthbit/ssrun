#!/usr/bin/env sh

gsi -f -e '(begin (load "ssrunlib")
                  (include "ssrunlib#.scm")
                  (include "tasks/core.scm")
                  (include "ssrunfile.scm")
                  (task-run all)
                  (exit))'
