;;; Copyright (c) 2012-2014, Álvaro Castro-Castilla
;;; Extensions for Sake (Host platform: Linux / OSX)


(define (fusion#host-run-interpreted main-module #!key
                                     (version '())
                                     (cond-expand-features '()))
  ;; Cond-expand features (relevant within the Sake environment)
  (##cond-expand-features (cons 'host (append cond-expand-features (##cond-expand-features))))
  (let* ((features (##cond-expand-features))
         (code `((define-syntax syntax-rules-error
                   (syntax-rules ()
                     ((_) (0))))
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
                      (cond-expand more-clauses ...))))
                 (##spheres-load ,main-module))))
    (gambit-eval-here code flags-string: "-:dar,h10000")))

;;!! Compile an executable for the host OS
(define (fusion#host-compile-exe exe-name
                                 main-module
                                 #!key
                                 (merge-modules #f)
                                 (compiler-options '())
                                 override-cc-options
                                 override-ld-options
                                 (version '())
                                 (cond-expand-features '())
                                 (verbose #f))
  ;; Cond-expand features (relevant within the Sake environment)
  (##cond-expand-features (cons 'host (append cond-expand-features (##cond-expand-features))))
  ;; Checks
  (when merge-modules (err "fusion#host-compile-exe: merge-modules options is not yet implemented"))
  (sake#compile-to-exe exe-name
                       (list main-module)
                       compiler-options: compiler-options
                       override-cc-options: override-cc-options
                       override-ld-options: override-ld-options
                       version: version
                       cond-expand-features: (cons 'host cond-expand-features)
                       verbose: verbose))

;;! Generate a loadable object from a module and its dependencies for the host OS
(define (fusion#host-compile-loadable-set output-file
                                          main-module
                                          #!key
                                          (merge-modules #f)
                                          (cond-expand-features '())
                                          (compiler-options '())
                                          (version compiler-options)
                                          (precompiled-modules '())
                                          (verbose #f))
  ;; Cond-expand features (relevant within the Sake environment)
  (##cond-expand-features (cons 'host (append cond-expand-features (##cond-expand-features))))
  ;; Make sure work directories are ready
  (unless (file-exists? (current-build-directory)) (make-directory (current-build-directory)))
  (unless (file-exists? (current-bin-directory)) (make-directory (current-bin-directory)))
  ;; Compute dependencies
  (let* ((modules-to-compile (append (%module-deep-dependencies-to-load main-module)
                                     (list main-module)))
         (all-modules (append precompiled-modules modules-to-compile))
         (link-file (string-append output-file ".c"))
         (all-module-c-files (map (lambda (m) (string-append (current-build-directory)
                                                        (%module-filename-c m version: version)))
                                  all-modules))
         (all-c-files/link (append all-module-c-files
                                   (list (string-append (current-build-directory) link-file))))
         (something-generated? #f))
    ;; Generate modules (generates C code)
    (for-each
     (lambda (m)
       (let ((output-c-file (string-append (current-build-directory) (%module-filename-c m version: version))))
         (if ((newer-than? output-c-file)
              (string-append (%module-path-src m) (%module-filename-scm m)))
             (begin
               (set! something-generated? #t)
               (sake#compile-to-c m
                                  cond-expand-features: (cons 'host cond-expand-features)
                                  compiler-options: compiler-options
                                  verbose: verbose
                                  output: output-c-file)))))
     modules-to-compile)
    (when something-generated?
          (info/color 'blue "new C files generated")
          (sake#link-flat (string-append (current-build-directory) link-file)
                          all-modules
                          verbose: verbose))
    ;; Compile objects
    (set! something-generated? #f)
    (let ((o-files
           (map (lambda (f m)
                  (let ((output-o-file (string-append (path-strip-extension f) ".o")))
                    (when ((newer-than? output-o-file) f)
                          (unless something-generated?
                                  (info/color 'blue "compiling updated C files:"))
                          (info/color 'brown (string-append " >>>  " f))
                          (set! something-generated? #t)
                          (sake#compile-c-to-o f
                                               output: output-o-file
                                               options: '(obj)
                                               cc-options: (string-append
                                                            "-D___DYNAMIC "
                                                            ;; Append cc options except for the link file
                                                            (if m (%process-cc-options (%module-shallow-dependencies-cc-options m)) ""))
                                               verbose: verbose)
                          output-o-file)))
                all-c-files/link
                (append all-modules '(#f))))) ;; The modules + a #f for the link file
      ;; Make bundle
      (info/color 'green "compiling C/Scheme code into a loadable object")
      (link-files files: o-files
                  output: (string-append (current-bin-directory) output-file)
                  options: (case (sake#host-platform)
                             ((linux) " -shared")
                             ((osx) (string-append
                                     " -bundle -arch "
                                     (symbol->string (car (system-type)))
                                     " -macosx_version_min 10.8")))
                  verbose: verbose))))
