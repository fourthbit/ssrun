(define-task compile ()
  ;; Compile SSRun program
  (info/color 'green "Compiling SSrun...")
  (gambit-compile-file "ssrun.scm" output: "ssrun" options: "-exe"))

(define-task install ()
  ;; Install SSRun program
  (info/color 'green "Installing SSRun...")
  (delete-file "~~/bin/ssrun")
  (copy-file "ssrun" "~~/bin/ssrun")
  ;; Create symbolic link in /usr/bin
  (delete-file "/usr/bin/ssrun")
  (create-symbolic-link "~~/bin/ssrun" "/usr/bin/ssrun")
  ;; Install some Sake extensions
  (info/color 'green "Installing SSRun definitions...")
  #;
  (copy-file (string-append (current-source-directory) "internal/tiny.scm")
             (string-append (ssrun-extensions-path) "tiny.scm"))
  #;
  (copy-file (string-append (current-source-directory) "ssrun/extensions/core-macros.scm")
             (string-append (ssrun-extensions-path) "aaaa_core-macros.scm"))
  #;
  (copy-file (string-append (current-source-directory) "ssrun/extensions/core.scm")
             (string-append (ssrun-extensions-path) "core.scm")))

(define-task all (compile install)
  'all)
