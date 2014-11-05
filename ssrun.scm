#!/usr/bin/env gsi-script -f

;;!!! This file is just a little wrapper around the function
;; ssrun in ssrun/common.scm
;; .author Francesco Bracchi
;; .author Alvaro Castro-Castilla

;; Debugging notes: comment last line

(##namespace ("ssrun#"))
(##include "~~/lib/gambit#.scm")
(##include "ssrunlib.scm")
(##include "arguments.scm")

(define current-extensions-path
  (make-parameter "~~lib/ssrun/tasks/"))

(define *help* #<<end-help-string

Usage: ssrun [--file <ssrun-file>] [options] [<initial-task> <tasks>]

Arguments:
    <task> is the first task to be run, run in order
    <task[argument1,argument2...]> provide arguments to a tasks

Options:
    --help
    --file <ssrun-file> is the file containing tasks description (defaults to ssrunfile.scm)
    --path <subtasks-alternative-path> a path to use in place of the default subtasks installation
    

end-help-string
)

(define (main . args)
  (define *options*
    '((#\h 0 "help")
      (#\f 1 "file")
      (#\p 1 "path")
      (#\X 0 "no-extensions")))
  (define (process-tasks explicit-tasks? opts cli-args)
    (define help #f)
    (define file "ssrunfile.scm")
    (define path (current-extensions-path))
    (define no-extensions #f)
    (handle-opts!
     opts
     `(("help"
        ,@(lambda (val)
            (println *help*)
            (exit 0)))
       ("file"
        ,@(lambda (val)
            (set! file (with-input-from-string val read))))
       ("path"
        ,@(lambda (val) (set! path val)))
       ("no-extensions"
        ,@(lambda (val)
            (set! no-extensions #t)))))
    (if explicit-tasks? (ensure-args! cli-args))
    (ssrun file: file
           tasks: (map string->symbol cli-args)
           extensions-path: path
           extensions: (not no-extensions)))
  (parse-arguments
   args
   (lambda (args-sans-opts opts)
     (if (null? args-sans-opts)
         (process-tasks #f
                        opts
                        '("all"))
         (process-tasks #t
                        opts
                        args-sans-opts)))
   *options*))

;;(apply main (cdr (command-line)))

