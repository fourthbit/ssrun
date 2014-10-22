(define-task compile ()
  ;; Compile SSRun program
  (info/color 'green "Compiling SSrun...")
  (ssrun#delete-file "ssrun.c")
  (gambit-compile-file "ssrun.scm" output: "ssrun" options: "-exe"))

(define-task install ()
  ;; Install SSRun program
  (info/color 'green "Installing SSRun...")
  (ssrun#delete-file "~~/bin/ssrun")
  (ssrun#copy-file "ssrun" "~~/bin/ssrun")
  ;; Create symbolic link in /usr/bin
  (ssrun#delete-file "/usr/bin/ssrun")
  (create-symbolic-link "~~/bin/ssrun" "/usr/bin/ssrun")
  ;; Install basic SSRun tasks
  (info/color 'green "Installing SSRun tasks...")
  (ssrun#make-directory "~~lib/ssrun")
  (ssrun#copy-files (fileset dir: "." test: (extension=? ".scm"))
                    "~~lib/ssrun/")
  (ssrun#make-directory "~~lib/ssrun/tasks")
  (ssrun#copy-files (fileset dir: "tasks" test: (extension=? ".scm"))
                    "~~lib/ssrun/tasks"))
  
(define-task all (compile install)
  'all)
