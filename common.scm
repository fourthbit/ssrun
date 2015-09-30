;;-------------------------------------------------------------------------------
;; Parameters and globals

(define current-build-directory 
  (make-parameter 
   (string-append (current-directory) "build/")))

(define current-source-directory 
  (make-parameter 
   (string-append (current-directory) "src/")))

(define current-lib-directory
  (make-parameter (string-append (current-directory) "lib/")))

(define current-bin-directory
  (make-parameter (string-append (current-directory) "bin/")))

(define current-project-name
  (make-parameter
   (path-strip-directory
    (path-strip-trailing-directory-separator (current-directory)))))

(define current-module-name
  (make-parameter (current-project-name)))

;;-------------------------------------------------------------------------------
;; Output

(define (log type . message)
  (display "*** ")
  (display type)
  (display " -- ")
  (for-each print message)
  (newline))

(define (info . message)
  (apply log (cons "INFO" message)))

(define (info/color color . message)
  (let ((color-string
         (case color
           ((black) "\033[00;30m")
           ((dark-gray) "\033[01;30m")
           ((blue) "\033[00;34m")
           ((light-blue) "\033[01;34m")
           ((green) "\033[00;32m")
           ((light-green) "\033[01;32m")
           ((cyan) "\033[00;36m")
           ((light-cyan) "\033[01;36m")
           ((red) "\033[00;31m")
           ((light-red) "\033[01;31m")
           ((purple) "\033[00;35m")
           ((light-purple) "\033[01;35m")
           ((brown) "\033[00;33m")
           ((yellow) "\033[01;33m")
           ((light-gray) "\033[00;37m")
           ((white) "\033[01;37m")
           (else ""))))
    (apply log (append `("INFO" ,color-string) message))
    (display "\033[00m")))

(define (warn . message)
  (display "\033[00;33m")
  (apply log (cons "WARNING" message))
  (display "\033[00m"))

(define (err . message)
  (display "\033[00;31m")
  (apply log (cons "ERROR" message))
  (display "\033[00m")
  (error "ssrun error, aborting"))

;;-------------------------------------------------------------------------------
;; Util

(##include "minimal.scm")

;;-------------------------------------------------------------------------------
;; Main

(define (ssrun #!key
               (file "ssrunfile.scm")
               (tasks '(all))
               extensions-path
               extensions)
  (let* ((file (path-expand file))
         (dir (path-directory file)))
    (info "entering directory " dir)
    (if (not (file-exists? file))
        (err (string-append "file '" file "' not found in "
                            (if (string=? dir (current-directory))
                                "current directory."
                                dir))))
    (eval `(begin
             ,(if (file-exists? "~/.gambini")
                  '(include "~/.gambini")
                  #!void)
             ,(if (file-exists? "ssrunlib#.scm")
                  '(include "ssrunlib#.scm")
                  '(include "~~lib/ssrun/ssrunlib#.scm"))
             (include ,file)
             ,@(map (lambda (t)
                      (let* ((task&args t)
                             (task&args-str (symbol->string task&args))
                             (task&args-str-len (string-length task&args-str))
                             (found-task-args (string-index task&args-str #\[))
                             (task-str (if found-task-args
                                           (substring task&args-str 0 found-task-args)
                                           task&args-str))
                             (task-str-len (string-length task-str))
                             (arguments (if found-task-args
                                            (map (lambda (s) (with-input-from-string s read))
                                                 ((string-split #\,)
                                                  (substring task&args-str (+ found-task-args 1) (- task&args-str-len 1))))
                                            '())))
                        `(with-exception-catcher
                          (lambda (ex)
                            (define (seems-same-symbol? mangled-name)
                              (let* ((undefined-variable mangled-name)
                                     (undef-str (symbol->string undefined-variable))
                                     (undef-str-len (string-length undef-str))
                                     (diff-lengths (- undef-str-len ,task-str-len)))
                                (if (zero? diff-lengths)
                                    (string=? undef-str ,task-str)
                                    (and (> diff-lengths 0)
                                         (string=? (substring undef-str
                                                              diff-lengths
                                                              undef-str-len)
                                                   ,task-str)
                                         (char=? #\# (string-ref undef-str (- diff-lengths 1)))))))
                            (if (unbound-global-exception? ex)
                                (let ((undefined-variable (unbound-global-exception-variable ex)))
                                  (if (seems-same-symbol? undefined-variable)
                                      (err (string-append "seems like you are calling a task '" ,task-str "' not found in " ,file))
                                      (err (string-append "unbound global variable '"
                                                          (symbol->string undefined-variable) "'."))))
                                (raise ex)))
                          (lambda () (task-run ,(string->symbol task-str) arguments: ',arguments)))))
                    tasks)))
    (info "exiting directory " dir)))
