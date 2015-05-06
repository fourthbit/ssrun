(define gambit-compiler (make-parameter
			 ;; This append fixes an issue with MinGW paths
			 (string-append (path-expand "~~") "/bin/gsc")))

(define c-compiler (make-parameter "gcc"))

(define linker (make-parameter "ld"))

(##define (gambit-compile-file
           name 
           #!key 
           (output (current-build-directory)) 
           (options ""))
  (info "compiling " name)
  (shell-command
   (string-append (gambit-compiler) " -f -o " output " " options " " name)))

(##define (gambit-compile-files
           #!key
           (files (fileset test: (f-and (extension=? ".scm") 
                                        (f-not (ends-with? "#.scm")) 
                                        (newer-than/extension? ".c"))
                           recursive: #t))
           (output (current-build-directory))
           (options ""))
  (for-each
   (lambda (name) (gambit-compile-file name output: output options: options))
   files))

;; (##define (c-compile-file
;;            name 
;;            #!key 
;;            (output (current-build-directory)) 
;;            (options ""))
;;   (shell-command
;;    (string-append (c-compiler) " -o " output " " options " " name)))

;; (##define (c-compile-files
;;            #!key
;;            (files (fileset test: (f-and (extension=? ".scm") 
;;                                         (f-not (ends-with? "#.scm")) 
;;                                         (newer-than/extension? ".c"))
;;                            recursive: #t))
;;            (output (current-build-directory))
;;            (options ""))
;;   (for-each
;;    (lambda (name) (c-compile-file output: output options: options))
;;    files))

(##define (link-files
           #!key
           (files (fileset test: (extension=? ".c") recursive: #t))
           (output (current-build-directory))
           (options "")
           (verbose #f))
  (let ((command-string (string-append (linker) " " options " " (string-join files) " -o " output)))
    (if verbose
        (begin (info "linking:")
               (println command-string)))
    (shell-command command-string)))

(##define (include-files
           #!key
           (files (fileset test: (ends-with? "#.scm")
                           output: (current-build-directory)
                           recursive: #t))
           (dest (string-append (current-build-directory)  (current-module-name) "#.scm")))
  (append-files files dest))

(define (list->escaped-string l)
  (define (flatten x:xs)
    (let* ((result (cons '() '())) (last-elt result))
      (define (f x:xs)
        (cond
         ((null? x:xs)
          result)
         ((pair? (car x:xs))
          (f (car x:xs)) (f (cdr x:xs)))
         (else
          (set-cdr! last-elt (cons (car x:xs) '()))
          (set! last-elt (cdr last-elt))
          (f (cdr x:xs)))))
      (f x:xs)
      (cdr result)))
  (apply
   string-append
   (flatten
    (letrec ((map-to-strings
              (##lambda (l #!optional front)
                (let* ((rest
                        (lambda ()
                          (cons (map-to-strings (car l))
                                (if (null? (cdr l))
                                    (map-to-strings (cdr l))
                                    (map-to-strings (cdr l) " ")))))
                       (next
                        (cond
                         ((null? l) (list ")"))
                         ((not (pair? l)) (object->string l))
                         ((pair? (car l))
                          (list "(" (rest)))
                         (else
                          (rest)))))
                  (if front
                      (list front next)
                      next)))))
      (map-to-strings l "(")))))


(##define (gambit-eval-here code #!key
                            (flags-string #f)
                            (verbose #f))
  ;; This is a hack transforming all ' into ` since they work in all cases and makes
  ;; life easier when passed as a string with ' delimiters to bash
  (let ((quotes->semiquotes
         (lambda (string)
           (let ((length (string-length string))
                 (transformed (string-copy string)))
             (let recur ((n 0))
               (if (< n length)
                   (begin
                     (if (eq? #\' (string-ref string n))
                         (string-set! transformed n #\`))
                     (recur (+ n 1)))
                   transformed))))))
    (let ((code-string (quotes->semiquotes
                        (object->string (cons 'begin code)))))
      (and verbose
           (begin (info "gambit-eval-here input code: ") (pp code)
                  (info "gambit-eval-here string: ") (print code-string)))
      (process-status
       (open-process (list path: (gambit-compiler)
                           ;;arguments: (list flags-string "-e" code-string)
                           ;;arguments: (list "-e" "(pp 1234)")
                           arguments: (if flags-string
                                          (list flags-string "-e" code-string)
                                          (list "-e" code-string))
                           stdout-redirection: #f))))))
