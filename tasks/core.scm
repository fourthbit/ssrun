;;!!! Utilities and procedures to be used within ssrunfiles
;; .author Alvaro Castro-Castilla, 2012-2014

(include "macros.scm")

;;! Generate a C file from a Scheme library
;; .argument version: generate module version with specific features
;;  (compiler options, cond-expand...)
;; .returns the path of the generated file
(define (ssrun#compile-to-c library-or-file
                            #!key
                            (cond-expand-features '())
                            (compiler-options '())
                            (expander 'syntax-case)
                            output
                            verbose)
  (let* ((library (if (string? library-or-file)
                      (err "Handling of libraries as a file is unimplemented")
                      library-or-file))
         (library-path (%find-library-path library))
         (library-sld (%find-library-sld library))
         (output-file (or output (%library-c-path library))))
    (info "compiling library to C using " (symbol->string expander)
          " expander -- " (object->string library))
    (let* ((generate-cond-expand-code
            (lambda (features)
              `((define-syntax syntax-rules-error
                  (syntax-rules () ((_) (0))))
                (define-syntax cond-expand
                  (syntax-rules (and or not else ,@features)
                    ((cond-expand) (syntax-rules-error "Unfulfilled cond-expand"))
                    ((cond-expand (else body ...))
                     (begin body ...))
                    ((cond-expand ((and) body ...) more-clauses ...)
                     (begin body ...))
                    ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
                     (cond-expand
                      (req1
                       (cond-expand
                        ((and req2 ...) body ...)
                        more-clauses ...))
                      more-clauses ...))
                    ((cond-expand ((or) body ...) more-clauses ...)
                     (cond-expand more-clauses ...))
                    ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
                     (cond-expand
                      (req1
                       (begin body ...))
                      (else
                       (cond-expand
                        ((or req2 ...) body ...)
                        more-clauses ...))))
                    ((cond-expand ((not req) body ...) more-clauses ...)
                     (cond-expand
                      (req
                       (cond-expand more-clauses ...))
                      (else body ...)))
                    ,@(map
                       (lambda (cef)
                         `((cond-expand (,cef body ...) more-clauses ...)
                           (begin body ...)))
                       features)
                    ((cond-expand (feature-id body ...) more-clauses ...)
                     (cond-expand more-clauses ...))))))))
      (case expander
        ((syntax-case)
         (receive
          (imports exports includes _)
          (%library-read-syntax library)
          (let* ((includes (or (and library-sld includes)
                               (list (%find-library-scm library))))
                 (merged-input-file (ssrun#merge-files
                                     includes
                                     (%library-merged-scm-path library)
                                     prepend-code: (cons '##begin (%library-make-prelude library))
                                     verbose: verbose))
                 (compilation-environment-code
                  `(,@(generate-cond-expand-code (cons 'compile-to-c cond-expand-features))
                    (%load-library ',library only-syntax: #t))))
            (if verbose
                (begin
                  (info/color 'light-green "compilation environment code:")
                  (for-each pp compilation-environment-code)))
            ;; Compile
            (if verbose (info/color 'light-green "syntax-case expansion:"))
            (or (let ((process-result
                       (gambit-eval-here
                        `(,@compilation-environment-code
                          (syntax-case-debug ,verbose)
                          (or (compile-file-to-target
                               ,merged-input-file
                               output: ,output-file
                               options: ',compiler-options)
                              (exit 1))))))
                  (delete-file merged-input-file)
                  (if (not (zero? process-result))
                      (err "ssrun#compile-to-c: error compiling generated C file in child process")))))))
        ((gambit)
         (let ((input-file (%find-library-scm library))
               (compilation-environment-code
                '((void))))
           (when verbose
                 (info/color 'light-green "compilation environment code:")
                 (for-each pp compilation-environment-code))
           ;; Compile
           (or (zero?
                (gambit-eval-here
                 `(,@compilation-environment-code
                   (or (compile-file-to-target
                        ,input-file
                        output: ,output-file
                        options: ',compiler-options)
                       (exit 1)))
                 flags-string: "-f"))
               (err "ssrun#compile-to-c: error compiling generated C file in child process"))))
        (else (err "Unknown expander"))))
    output-file))

;;! Merge several files into one, optionally prepending/appending code
(define (ssrun#merge-files files output-file #!key prepend-code append-code verbose)
  (if (file-exists? output-file) (delete-file output-file))
  (for-each
   (lambda (input-file)
     (call-with-input-file input-file
       (lambda (ip) (call-with-output-file (list path: output-file append: #t)
                 (lambda (op)
                   ;; Necessary for include macro to be aware of current location
                   (current-directory (path-directory input-file))
                   (if prepend-code (pp prepend-code op))
                   (for-each (lambda (form) (pp form op)) (read-all ip))
                   (if append-code (pp append-code op)))))))
   files)
  (when verbose
        (println "merging files:")
        (for-each println files))
  output-file)

;;! Compile a C file generated by Gambit
;; Shouldn't be used directly, better use ssrun#compile-library
(define (ssrun#compile-c-to-o c-file
                              #!key
                              (options '())
                              (environment-options '())
                              output
                              cc-options
                              ld-options
                              delete-c
                              verbose)
  (if (or (not output) ((newer-than? output) c-file))
      (let* ((env-code
              (if (null? environment-options)
                  '()
                  (begin (map (lambda (e) `(setenv ,(car e) ,(cadr e))) environment-options))))
             (un-env-code
              (if (null? environment-options)
                  '()
                  (begin (map (lambda (e) `(setenv ,(car e) "")) environment-options))))
             (compile-code
              `(,@env-code
                ,(if output ;; output filename is discovered by Gambit if not provided
                     `(compile-file ,c-file
                                    options: ',options
                                    output: ,output
                                    cc-options: ,(or cc-options "")
                                    ld-options: ,(or ld-options ""))
                     `(compile-file ,c-file
                                    options: ',options
                                    cc-options: ,(or cc-options "")
                                    ld-options: ,(or ld-options "")))
                ,@un-env-code)))
        (info "compiling C file to o -- " c-file)
        (if verbose (begin (info/color 'green "Spawning a Gambit instance with this code: ")
                           (pp compile-code)))
        (or (zero? (gambit-eval-here compile-code))
            (err "error compiling C file"))))
  (if delete-c
      (delete-file c-file recursive: #t))
  output)

;;! Compile directly to .o from a Scheme file (high-level API)
(define (ssrun#compile-library lib
                               #!key
                               (cond-expand-features '())
                               (compiler-options '())
                               (expander 'syntax-case)
                               c-output-file
                               o-output-file
                               (environment-options '())
                               cc-options
                               ld-options
                               verbose
                               (delete-c #t)
                               force)
  (let ((c-file (or c-output-file (%library-c-path lib)))
        (compile? (or force (%library-updated? lib))))
    (when compile?
          (ssrun#compile-to-c lib
                              cond-expand-features: cond-expand-features
                              compiler-options: compiler-options
                              expander: expander
                              output: c-output-file
                              verbose: verbose)
          (ssrun#compile-c-to-o c-file
                                output: o-output-file
                                environment-options: environment-options
                                cc-options: cc-options
                                ld-options: ld-options
                                delete-c: delete-c
                                verbose: verbose))))

;;! Compile to exe
;; TODO: pending update
(define (ssrun#compile-to-exe exe-name
                              libraries
                              #!key
                              (cond-expand-features '())
                              (compiler-options '())
                              override-cc-options
                              override-ld-options
                              (strip #t)
                              verbose)
  (err "Pending update")
  ;; Make sure work directories are ready
  (unless (file-exists? (current-build-directory)) (make-directory (current-build-directory)))
  (unless (file-exists? (current-bin-directory)) (make-directory (current-bin-directory)))
  (let ((cc-options (or override-cc-options
                        (%process-cc-options (apply append (map %module-deep-dependencies-cc-options modules)))))
        (ld-options (or override-ld-options
                        (%process-ld-options (apply append (map %module-deep-dependencies-ld-options modules))))))

    (info "compiling modules to exe: ")
    (for-each (lambda (m) (info "    * " (object->string m) "  -> " (object->string (%module-normalize m))))
              modules)
    (let ((c-files (apply
                    append
                    (map (lambda (m)
                           (info "The following dependencies for \033[00;32m"
                                 (object->string (%module-normalize m))
                                 "\033[00m will be linked:")
                           (append (map (lambda (mdep)
                                          (info "    * " (object->string mdep) "")
                                          ;; First try with the default path
                                          (let ((scm-path (string-append
                                                           (%module-path-src mdep)
                                                           (%module-filename-scm mdep)))
                                                (default-path (string-append
                                                               (%module-path-lib mdep)
                                                               (%module-filename-c mdep))))
                                            (if (not ((newer-than? default-path) scm-path))
                                                default-path
                                                (let ((local-path (string-append
                                                                   (current-build-directory)
                                                                   (%module-filename-c mdep))))
                                                  (if (not ((newer-than? local-path) scm-path))
                                                      local-path
                                                      (begin
                                                        (info "Compiling deferred dependency "
                                                              (object->string (%module-normalize mdep)))
                                                        (ssrun#compile-to-c mdep
                                                                            version: version
                                                                            cond-expand-features: cond-expand-features
                                                                            compiler-options: compiler-options
                                                                            verbose: verbose)))))))
                                        (%module-deep-dependencies-to-load m))
                                   (list (ssrun#compile-to-c
                                          m
                                          version: version
                                          cond-expand-features: cond-expand-features
                                          compiler-options: compiler-options
                                          verbose: verbose))))
                         modules))))
      (gambit-eval-here
       `((let* ((link-file (link-incremental ',c-files))
                (gcc-cli (string-append ,(c-compiler)
                                        " " ,@(map (lambda (f) (string-append f " ")) c-files)
                                        " " link-file
                                        " -o" ,output
                                        " -I" (path-expand "~~include") " "
                                        ,cc-options
                                        " -L" (path-expand "~~lib") " -lgambc -lm -ldl -lutil "
                                        ,ld-options)))
           (if (not link-file) (err "error generating link file"))
           (if ,verbose (begin (pp link-file) (pp gcc-cli)))
           (shell-command gcc-cli)
           (if ,strip (shell-command ,(string-append "strip " output)))
           (delete-file link-file)))
       flags-string: "-f"))))

;;! Generate a flat link file
;; TODO: pending update
(define (ssrun#link-flat link-file
                         libraries
                         #!key
                         (dir (current-build-directory))
                         (version '())
                         (verbose #f))
  (err "Pending update")
  (info/color 'green (string-append "generating flat link file: " link-file))
  (let* ((output-file (string-append dir link-file))
         (code
          `((link-flat
             ',(map (lambda (m) (string-append dir (%module-filename-c m version: version))) modules)
             output: ,output-file
             warnings?: ,verbose))))
    (if verbose (pp code))
    (unless (= 0 (gambit-eval-here code))
            (err "error generating Gambit flat link file"))
    output-file))

;;! Generate an incremental link file
;; TODO: pending update
(define (ssrun#link-incremental link-file
                                libraries
                                #!key
                                (dir (current-build-directory))
                                (version '())
                                (verbose #f))
  (err "Pending update")
  (info/color 'green (string-append "generating an incremental link file: " link-file))
  (let* ((output-file (string-append dir link-file))
         (code
          `((link-incremental
             ',(map (lambda (m) (string-append dir (%module-filename-c m version: version))) modules)
             output: ,output-file
             warnings?: ,verbose))))
    (if verbose (pp code))
    (unless (= 0 (gambit-eval-here code))
            (err "error generating Gambit incremental link file"))
    output-file))

;;! Substitute (include <>) by the code in the referenced file
;; Merges the included files recursively, as S-expressions, but respects
;; the input-file as text
(define (ssrun#expand-includes input-file output-file)
  (define (do-expansion form)
    (map** (lambda (e) (if (and (pair? e) (eq? (car e) 'include))
                      (begin
                        (current-directory (path-directory (path-expand (cadr e))))
                        (do-expansion `(begin ,@(with-input-from-file (path-strip-directory (cadr e)) read-all))))
                      e))
           form))
  (let* ((str-list (call-with-input-file input-file (lambda (f) (read-all f read-char))))
         (str (list->string str-list))
         (str-len (string-length str)))
    (with-output-to-file
        output-file
      (lambda ()
        (for-each
         write-char
         (let recur ((i 0)
                     (str-rest str-list))
           (cond ((null? str-rest)
                  '())
                 ((and (char=? (car str-rest) #\()
                       (string=? "include " (substring str (+ i 1) (+ i 9)))) ; find include form
                  (receive (continue-position filename)
                           (let parse-filename ((j (+ i 9))
                                                (chars '())
                                                (status 'searching-first))
                             (case status
                               ((searching-first)
                                (if (and (char=? (string-ref str j) #\")
                                         (not (char=? (string-ref str (- j 1)) #\\))) ; make sure the " is not escaped
                                    (parse-filename (+ j 1) chars 'reading)
                                    (parse-filename (+ j 1) chars 'searching-first)))
                               ((reading)
                                (if (and (char=? (string-ref str j) #\")
                                         (not (char=? (string-ref str (- j 1)) #\\))) ; idem
                                    (parse-filename (+ j 1) chars 'search-next-parenthesis)
                                    (parse-filename (+ j 1)
                                                    (cons (string-ref str j)
                                                          chars)
                                                    'reading)))
                               ((search-next-parenthesis)
                                (if (char=? (string-ref str j) #\))
                                    (values (+ j 1)
                                            (list->string (reverse chars)))
                                    (parse-filename (+ j 1) chars 'search-next-parenthesis)))))
                           (append
                            (parameterize
                             ((current-directory (path-directory (path-expand input-file))))
                             (string->list
                              (with-output-to-string
                                '()
                                (lambda () (for-each (lambda (form) (pp form (current-output-port)))
                                                (do-expansion (with-input-from-file filename read-all)))))))
                            (recur continue-position
                                   (drop str-rest (- continue-position i))))))
                 (else
                  (cons (car str-rest)
                        (recur (+ i 1) (cdr str-rest)))))))))))

;;! Run a file
(define (ssrun#run-file file)
  (gambit-eval-here
   `((include ,file))))

;;! Run all files in a directory
(define (ssrun#run-all-files #!optional (dir (current-directory)))
  (for-each ssrun#run-file
            (fileset dir: dir
                     test: (f-and (extension=? ".scm")
                                  (f-not (ends-with? "#.scm")))
                     recursive: #t)))

;;! Clean all default generated files and directories
(define (ssrun#clean-libraries libraries)
  (for-each (lambda (l)
              (let ((c-file (%library-c-path l)))
                (if (file-exists? c-file) (delete-file c-file)))
              (let ((o-file (%library-object-path l)))
                (if (file-exists? o-file) (delete-file o-file)))
              (let ((merge-file (%library-merged-scm-path l)))
                (if (file-exists? merge-file) (delete-file merge-file))))
            libraries))

;;! Get the host platform
(define (ssrun#host-platform)
  (let ((sys (symbol->string (caddr (system-type)))))
    (cond ((and (> (string-length sys) 4)
                (string=? "linux" (substring sys 0 5)))
           'linux)
          ((and (> (string-length sys) 5)
                (string=? "darwin" (substring sys 0 6)))
           'osx)
	  ((or (string=? sys "mingw32") (string=? sys "mingw64"))
           'windows)
          (else (err (string-append
		      "ssrun#host-platform -> can't detect current platform: "
		      (object->string sys)))))))

;;! Parallel for-each, suitable mainly for parallel compilation, which spawns external processes
(define (ssrun#parallel-for-each
         f l
         #!key
         (max-thread-number
          (case (ssrun#host-platform)
            ((linux)
             (string->number
              (with-input-from-process "nproc"
                                       read-line)))
            ((osx)
             (string->number
              (with-input-from-port
                  (open-process
                   (list path: "sysctl"
                         arguments:
                         '("-n" "hw.logicalcpu")))
                read-line)))
            (else 2))))
  (info "using " max-thread-number " compilation threads")
  (let ((pending-elements l)
        (elements-mutex (make-mutex))
        (results '())
        (results-mutex (make-mutex)))
    (let ((main-thread (current-thread))
          (add-to-results! (lambda (r)
                             (mutex-lock! results-mutex)
                             (set! results (cons r results))
                             (mutex-unlock! results-mutex))))
      (let recur ((n 0)
                  (thread-pool '()))
        (if (< n max-thread-number)
            (recur (+ n 1)
                   (cons (thread-start!
                          (make-thread
                           (lambda ()
                             (let ((current-thread-element #f))
                               (with-exception-catcher
                                (lambda (e)
                                  (thread-send main-thread
                                               (cons e current-thread-element)))
                                (lambda ()
                                  (let recur ((n 0))
                                    (mutex-lock! elements-mutex)
                                    (if (null? pending-elements)
                                        (begin (mutex-unlock! elements-mutex)
                                               'finished-thread)
                                        (let ((next (car pending-elements)))
                                          (set! current-thread-element next)
                                          (set! pending-elements (cdr pending-elements))
                                          (mutex-unlock! elements-mutex)
                                          (add-to-results! (f next))
                                          (recur (fx+ n 1)))))))))))
                         thread-pool)))
        (for-each thread-join! thread-pool)
        (let read-messages ()
          (let ((m (thread-receive 0 'finished)))
            (if (not (eq? m 'finished))
                (begin (err "parallel-for-each exception " (car m) " running procedure on element: " (cdr m))
                       (read-messages)))))))
    (reverse results)))
