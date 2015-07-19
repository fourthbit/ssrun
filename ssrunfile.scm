(define-task compile ()
  ;; Compile SSRun program
  (info/color 'green "Compiling SSrun...")
  (ssrun#delete-file "ssrun.c")
  (gambit-compile-file "ssrun.scm" output: "ssrun" options: "-exe"))

(define ssrun-exec "ssrun")
(define ssrun-path "~~/bin/ssrun")
(define mingw? #f)

(define-task install ()
  (case (caddr (system-type))
    ((mingw32 mingw64)
     (set! mingw? #t)
     (set! ssrun-exec (string-append ssrun-exec ".exe"))
     (set! ssrun-path (string-append ssrun-path ".exe"))))
  ;; Install SSRun program
  (info/color 'green "Installing SSRun...")
  (ssrun#delete-file ssrun-path)
  (ssrun#copy-file ssrun-exec ssrun-path)
  ;; Create symbolic link in /usr/bin
  (if (not ;; unless we are under MinGW or Travis environment
       (or mingw?
           (with-exception-handler (lambda (e) #f) (lambda () (getenv "TRAVIS_BUILD_DIR") #t))))
      (begin
	(ssrun#delete-file (string-append "/usr/bin/" ssrun-exec))
	(create-symbolic-link ssrun-path (string-append "/usr/bin/" ssrun-exec))))
  ;; Install basic SSRun tasks
  (info/color 'green "Installing SSRun tasks...")
  (ssrun#make-directory "~~lib/ssrun")
  (ssrun#copy-files (fileset dir: "." test: (extension=? ".scm"))
                    "~~lib/ssrun/")
  (ssrun#make-directory "~~lib/ssrun/tasks")
  (ssrun#copy-files (fileset dir: "tasks" test: (extension=? ".scm"))
                    "~~lib/ssrun/tasks"))

(define-task (test first-param second-param third-param) ()
  (println first-param)
  (println second-param)
  (println third-param)
  'test)

(define-task all (compile install)
  'all)
