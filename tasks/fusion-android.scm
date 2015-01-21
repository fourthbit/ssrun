;;; Copyright (c) 2012-2014, Álvaro Castro-Castilla
;;; Extensions for Sake (Android)

;;------------------------------------------------------------------------------
;;!! Setup and initialization

(define android-supported-platforms
  '(android-3
    android-4
    android-8
    android-12
    android-13
    android-14
    android-15
    android-16
    android-17
    android-18
    android-19))

(define android-directory
  (make-parameter "android/"))

(define android-jni-directory-suffix
  (make-parameter "jni/"))

(define android-jni-directory
  (make-parameter
   (string-append (android-directory) (android-jni-directory-suffix))))

(define android-jni-generator-directory-suffix
  (make-parameter "jni-generator/"))

(define android-jni-generator-directory
  (make-parameter
   (string-append (android-directory) (android-jni-generator-directory-suffix))))

(define android-src-directory-suffix
  (make-parameter "src/"))

(define android-src-directory
  (make-parameter
   (string-append (android-directory) (android-src-directory-suffix))))

(define android-src-generator-directory-suffix
  (make-parameter "src-generator/"))

(define android-src-generator-directory
  (make-parameter
   (string-append (android-directory) (android-src-generator-directory-suffix))))

(define android-build-directory-suffix
  (make-parameter "build/"))

(define android-build-directory
  (make-parameter
   (string-append (android-directory) (android-jni-directory-suffix) (android-build-directory-suffix))))

(define android-assets-directory-suffix
  (make-parameter "assets/"))

(define android-assets-directory
  (make-parameter
   (string-append (android-directory) (android-assets-directory-suffix))))

(define android-toolchain-directory-suffix
  (make-parameter "toolchain/"))

(define android-toolchain-directory
  (make-parameter (string-append (android-directory) (android-toolchain-directory-suffix))))

(define android-arm-gcc-path
  (make-parameter (string-append (android-toolchain-directory) "bin/arm-linux-androideabi-gcc")))

(define android-arm-g++-path
  (make-parameter (string-append (android-toolchain-directory) "bin/arm-linux-androideabi-g++")))

(define android-arm-ld-path
  (make-parameter (string-append (android-toolchain-directory) "bin/arm-linux-androideabi-ld")))

(define android-arm-ar-path
  (make-parameter (string-append (android-toolchain-directory) "bin/arm-linux-androideabi-ar")))

(define android-arm-strip-path
  (make-parameter (string-append (android-toolchain-directory) "bin/arm-linux-androideabi-strip")))

(define android-arm-ranlib-path
  (make-parameter (string-append (android-toolchain-directory) "bin/arm-linux-androideabi-ranlib")))

;;------------------------------------------------------------------------------
;;!! Host programs

(define host-ant-path
  (make-parameter
   (if (zero? (shell-command "ant -version &>/dev/null"))
       "ant"
       #f)))

;;------------------------------------------------------------------------------
;;!! Android SDK

;;!! Android SDK path
(define android-sdk-path
  (make-parameter
   (with-exception-catcher
    (lambda (e)
      (if (unbound-os-environment-variable-exception? e)
          #f
          (error "internal error in android-sdk-path")))
    (lambda () (path-add-trailing-directory-separator (getenv "ANDROID_SDK_PATH"))))))

;;! Program: android
(define android-android-path
  (make-parameter
   (and (android-sdk-path)
        (let ((path (string-append (android-sdk-path)
                                   "/tools/android")))
          (if (file-exists? path) path #f)))))

;;! Program: adb
(define android-adb-path
  (make-parameter
   (and (android-sdk-path)
        (let ((path (string-append (android-sdk-path)
                                   "/platform-tools/adb")))
          (if (file-exists? path) path #f)))))

;;------------------------------------------------------------------------------
;; Android NDK

;;! Android NDK path
(define android-ndk-path
  (make-parameter
   (with-exception-catcher
    (lambda (e)
      (if (unbound-os-environment-variable-exception? e)
          #f
          (error "internal error in android-ndk-path")))
    (lambda () (path-add-trailing-directory-separator (getenv "ANDROID_NDK_PATH"))))))

;;! Program: ndk-build
(define android-ndk-build-path
  (make-parameter
   (and (android-ndk-path)
        (let ((path (string-append (android-ndk-path) "/ndk-build")))
          (if (file-exists? path) path #f)))))

;;------------------------------------------------------------------------------
;;!! Android tasks

;;! Check if Android seems to be installed in the system
(define (fusion#android-installed?)
  (unless (android-sdk-path)
          (err "Cannot find Android SDK path. Is ANDROID_SDK_PATH environment variable set?"))
  (unless (android-ndk-path)
          (err "Cannot find Android NDK path. Is ANDROID_NDK_PATH environment variable set?"))
  (unless (android-android-path)
          (err "Cannot find \"android\" program in " (android-sdk-path)  " -- Is ANDROID_SDK_PATH set to the right directory?"))
  (unless (android-adb-path)
          (err "Cannot find \"adb\" program in " (android-sdk-path)  " -- Is ANDROID_SDK_PATH set to the right directory?"))
  (unless (android-ndk-build-path)
          (err "Cannot find \"ndk-build\" program in " (android-ndk-path)  " -- Is ANDROID_NDK_PATH set to the right directory?"))
  (unless (host-ant-path)
          (err "Cannot find \"ant\" program. Is it installed?")))

;;! Check whether the project seems to be prepared for Android
(define (fusion#android-project-supported?)
  (unless (file-exists? (android-directory))
          (err "Android directory doesn't exist. Please run Android setup task."))
  (unless (or (file-exists? (android-jni-directory))
              (file-exists? (android-jni-generator-directory)))
          (err "Android 'jni' or 'jni-generator' directory doesn't exist. Please run Android setup task."))
  (unless (or (file-exists? (android-src-directory))
              (file-exists? (android-src-generator-directory)))
          (err "Android 'src' or 'src-generator' directory doesn't exist. Please run Android setup task."))
  (unless (file-exists? (string-append (android-directory) "build.xml"))
          (err "Android build.xml file doesn't exist. Please run Android setup task."))
  (unless (file-exists? (string-append (android-directory) "AndroidManifest.xml"))
          (err "AndroidManifest.xml file doesn't exist. Please run Android setup task."))
  (unless (file-exists? (string-append (android-directory) "default.properties"))
          (err "Android default.properties file doesn't exist. Please run Android setup task."))
  (unless (file-exists? (string-append (android-directory) "project.properties"))
          (err "Android project.properties file doesn't exist. Please run Android setup task."))
  (unless (and (file-exists? (string-append (android-directory) "local.properties"))
               (file-exists? (string-append (android-directory) "toolchain")))
          (err "Android local.properties file doesn't exist. Please run (fusion#android-project-setup <target>) in a setup task.")))

;;! Clean up all generated files
(define (fusion#android-clean)
  (define (delete-if-exists dir)
    (when (file-exists? dir)
          (sake#delete-directory dir recursive: #t force: #t)))
  ;;(delete-if-exists (android-assets-directory))
  (delete-if-exists (android-src-directory))
  (delete-if-exists (android-jni-directory))
  (delete-if-exists (string-append (android-directory) "bin/"))
  (delete-if-exists (string-append (android-directory) "gen/"))
  (delete-if-exists (string-append (android-directory) "libs/"))
  (delete-if-exists (string-append (android-directory) "obj/"))
  (delete-if-exists (string-append (android-directory) "toolchain/"))
  (delete-if-exists (android-build-directory)))

;;! Check update project
(define (fusion#android-project-setup android-platform)
  ;; Checks
  (fusion#android-installed?)
  (unless (any (lambda (p) (eq? android-platform p)) android-supported-platforms)
          (err "fusion#android-project-setup: unsupported target "))
  ;; Setup project target, then make standalone toolchain
  (let ((android-platform-str (symbol->string android-platform)))
    (shell-command
     (string-append (android-android-path)
                    " update project --path " (android-directory)
                    " --target " android-platform-str))
    (when (file-exists? (android-toolchain-directory))
          (sake#delete-directory (android-toolchain-directory) recursive: #t))
    (shell-command
     (string-append (android-ndk-path) "build/tools/make-standalone-toolchain.sh"
                    " --platform=\"" android-platform-str "\""
                    " --install-dir=\"" (android-toolchain-directory) "\""))))

;;! Process a directory containing templates
(define (fusion#process-directory-with-templates destination-path source-path parameters values #!key
                                                 (overwrite-all #f)
                                                 (overwrite-files '()))
  (let recur ((relative-path ""))
    (for-each
     (lambda (file)
       (case (file-info-type (file-info (string-append source-path relative-path file)))
         ((regular)
          (let* ((source-file (string-append source-path relative-path file))
                 (is-source-a-template? (string=? ".sct" (path-extension source-file)))
                 (destination-file ((if is-source-a-template? path-strip-extension (lambda (x) x))
                                    (string-append destination-path relative-path file))))
            ;; Do the work if:
            (if (or (not (file-exists? destination-file)) ; the file doesn't exist
                    overwrite-all ; the source file is newer than the destination file
                    (if (member destination-file overwrite-files)
                        (begin (info/color 'yellow "forced file update: " destination-file) #t)
                        #f)
                    (if ((newer-than? destination-file) source-file) ; the source-file has changed.
                        (begin (info/color 'yellow "file updated from generator: " destination-file) #t)
                        #f))
                ;; Test whether it's a template file to process or a simple file to copy
                (if is-source-a-template?
                    (call-with-output-file
                        destination-file ; we already stripped the extension before
                      (lambda (f) (display
                              (apply
                               (apply
                                build-template-from-file
                                (cons (string-append source-path relative-path file)
                                      parameters))
                               values)
                              f)))
                    (sake#copy-file source-file destination-file force: #t)))))
         ((directory)
          (make-directory (string-append destination-path relative-path file))
          (recur (string-append relative-path file "/")))))
     (directory-files (string-append source-path relative-path)))))

;;! Runs the compiler in the Android toolchain and environment
(define (fusion#android-run-compiler #!key
                                     arch ;; unused
                                     arguments
                                     (android-platform 'android-15)
                                     (compiler 'gcc)
                                     (verbose #f))
  ;; Checks
  (unless (or (eq? compiler 'gcc) (eq? compiler 'g++))
          (err "fusion#android-run-compiler: wrong compiler"))
  (unless (any (lambda (p) (eq? p android-platform)) android-supported-platforms)
          (err "fusion#android-run-compiler: unsupported Android platform: " android-platform))
  (let* ((android-platform-str (symbol->string android-platform))
         (platforms-dir (string-append (android-ndk-path) "platforms/"))
         (android-platform-dir (string-append platforms-dir android-platform-str))
         (compiler-path (case compiler
                          ((gcc) (android-arm-gcc-path))
                          ((g++) (android-arm-g++-path))
                          (else (err "fusion#android-run-compiler: wrong compiler " compiler))))
         (arguments (cons "-fno-short-enums" arguments)))
    (unless (file-exists? android-platform-dir)
            (err "fusion#android-run-compiler: requested platform can't be found in NDK " android-platform))
    (when verbose
          (info/color 'green "Compiler command:")
          (println compiler-path)
          (info/color 'green "Compiler args:")
          (println (string-join arguments)))
    (let ((compilation-process
           (open-process (list path: compiler-path
                               arguments: arguments))))
      (unless (zero? (process-status compilation-process))
              (err "fusion#android-run-compiler: error running command"))
      (close-port compilation-process))))

;;! Runs the linker in the Android toolchain and environment
(define (fusion#android-run-linker #!key
                                   arch ;; unused
                                   arguments
                                   (android-platform 'android-15)
                                   (verbose #f))
  ;; Checks
  (unless (any (lambda (p) (eq? p android-platform)) android-supported-platforms)
          (err "fusion#android-run-linker: unsupported Android platform: " android-platform))
  ;; Find linker
  (let* ((android-platform-str (symbol->string android-platform))
         (platforms-dir (string-append (android-ndk-path) "platforms/"))
         (android-platform-dir (string-append platforms-dir android-platform-str)))
    (unless (file-exists? android-platform-dir)
            (err "fusion#android-run-linker: requested platform can't be found in NDK " android-platform))
    (when verbose
          (info/color 'green "Linker command:")
          (println (android-arm-ld-path))
          (info/color 'green "Linker args:")
          (println (string-join arguments)))
    (let ((compilation-process
           (open-process (list path: (android-arm-ld-path)
                               arguments: arguments))))
      (unless (zero? (process-status compilation-process))
              (err "fusion#android-run-compiler: error running command"))
      (close-port compilation-process))))

;;! Compile app
;; .parameter main-module main-module of the Android App
;; .parameter import-modules modules already generated to be linked as well
(define (fusion#android-compile-app app-name
                                    main-module
                                    #!key
                                    (target 'debug)
                                    (cond-expand-features '())
                                    (compiler-options '())
                                    (version compiler-options)
                                    (compiled-modules '())
                                    (jni-files '())
                                    (num-threads 1)
                                    (merge-modules #f)
                                    (verbose #f))
  ;; cond-expand features for Sake environment
  (##cond-expand-features (cons 'android (##cond-expand-features)))
  ;; Checks
  (fusion#android-installed?)
  (fusion#android-project-supported?)
  (when merge-modules
        (err "fusion#android-compile-loadable-set: merge-modules options is not yet implemented"))
  (unless (number? num-threads) (err "fusion#android-compile-app: num-threads must be a positive number, or +inf.0"))
  ;; Compute dependencies
  (let* ((modules-to-compile (append (%module-deep-dependencies-to-load main-module) (list main-module)))
         (all-modules (append compiled-modules modules-to-compile))
         (link-file "linkfile_.c"))
    ;; List files generated by compiling modules, C files from the JNI, and the linkfile
    (let ((all-c-files
           (append (map (lambda (m) (string-append "../" (android-build-directory-suffix)
                                              (%module-filename-c m version: version)))
                        all-modules)
                   (list (string-append "../" (android-build-directory-suffix) link-file))
                   jni-files)))
      ;; Create Android build directory if it doesn't exist
      (unless (file-exists? (android-build-directory))
              (make-directory (android-build-directory)))
      ;; Create Android assets directory if it doesn't exist
      (unless (file-exists? (android-assets-directory-suffix))
              (make-directory (android-assets-directory-suffix)))
      (unless (file-exists? (android-assets-directory))
              (make-directory (android-assets-directory)))
      (let ((arguments '(app-name main-module-name c-files-string))
            (arg-values `("main"
                          "main"
                          ,(apply string-append (map (lambda (file) (string-append file " ")) all-c-files)))))
        ;; Process 'assets'
        (fusion#process-directory-with-templates (android-assets-directory)
                                                 "assets/"
                                                 arguments
                                                 arg-values)
        ;; Process 'src'
        (fusion#process-directory-with-templates (android-src-directory)
                                                 (string-append (android-directory) (android-src-generator-directory-suffix))
                                                 arguments
                                                 arg-values)
        ;; Process 'jni'. Make sure Android.mk is updated if config.scm is more recent than Android.mk
        (fusion#process-directory-with-templates (android-jni-directory)
                                                 (string-append (android-directory) (android-jni-generator-directory-suffix))
                                                 arguments
                                                 arg-values
                                                 ;; force overwriting Android.mk if config.scm is newer
                                                 overwrite-files:
                                                 (let ((android-mk
                                                        (string-append (android-directory) "jni/src/Android.mk")))
                                                   (if ((newer-than? android-mk)
                                                        (string-append (current-directory) "config.scm"))
                                                       (begin
                                                         (info/color 'yellow "Android.mk updated due to changes in config.scm")
                                                         (list android-mk))
                                                       '()))))
      ;; Generate modules (generates C code)
      (let ((something-generated? #f))
        (for-each (lambda (m)
                    (if ((newer-than? (string-append (android-build-directory)
                                                     (%module-filename-c m version: version)))
                         (string-append (%module-path-src m) (%module-filename-scm m)))
                        (begin
                          (set! something-generated? #t)
                          (sake#compile-to-c m
                                             cond-expand-features: (cons 'android cond-expand-features)
                                             compiler-options: compiler-options
                                             verbose: verbose))))
                  modules-to-compile)
        (if something-generated?
            (info/color 'blue "generating C files")
            (info/color 'blue "no Scheme files need C recompilation"))
        ;; Copy C files from both compiled and imported modules into android build directory (if updated)
        (for-each
         (lambda (m) (let ((source (string-append (current-build-directory)
                                             (%module-filename-c m version: version)))
                      (destination (string-append (android-build-directory)
                                                  (%module-filename-c m version: version))))
                  (if ((newer-than? destination) source)
                      (copy-file source destination))))
         all-modules)
        ;; Generate link file
        (if something-generated?
            (sake#link-incremental link-file all-modules
                                   dir: (android-build-directory)
                                   version: version
                                   verbose: verbose))))
    (info/color 'blue (string-append "compiling JNI C/Scheme code (with "
                                     (if (= num-threads +inf.0) "MAX" (number->string num-threads))
                                     " threads)"))
    ;; Call "ndk-build". We dump the output to a file, as Gambit has issues with the "-j" flag for concurrent compilation
    (let ((ndk-command-str
           (string-append (android-ndk-build-path)
                          (cond ((< num-threads 1) (err "fusion#android-compile-app: num-threads must larger than 1 or +inf.0"))
                                ((= num-threads 1) "")
                                ((= num-threads +inf.0) " -j ")
                                ((> num-threads 1) (string-append " -j " (number->string num-threads))))
                          " -C " (android-directory) " &>ndk-log")))
      (if verbose (println ndk-command-str))
      (unless (zero? (shell-command ndk-command-str))
              (err "error building native code\n\n" (sake#read-file "ndk-log"))))
    (if verbose (display (sake#read-file "ndk-log")))
    (delete-file "ndk-log")
    (info/color 'blue "compiling Java code")
    ;; Call "ant"
    (let ((ant-command-str
           (string-append (host-ant-path) " -s " (android-directory) "build.xml "
                          (cond ((eq? 'debug target) "debug")
                                ((eq? 'release target) "release")
                                (else (err "Unknown target parameter: fusion#android-compile-app"))))))
      (if verbose (println ant-command-str))
      (unless (zero? (shell-command ant-command-str))
              (err "error building Java code")))))

;;! Generate loadable library
(define (fusion#android-compile-loadable-set output-file
                                             main-module
                                             #!key
                                             (target 'debug)
                                             (cond-expand-features '())
                                             (compiler-options '())
                                             (cc-options '())
                                             (ld-options '())
                                             (version compiler-options)
                                             (compiled-modules '())
                                             (merge-modules #f)
                                             (verbose #f))
  ;; Cond-expand features for Sake environment
  (##cond-expand-features (cons 'android (##cond-expand-features)))
  ;; Checks
  (fusion#android-project-supported?)
  (when merge-modules
        (err "fusion#android-compile-loadable-set: merge-modules options is not yet implemented"))
  (unless (file-exists? (android-jni-directory) "deps/gambit")
                (err "fusion#android-compile-loadable-set: you should have a compiled app prior to building a loadable object"))
  ;; Compute dependencies
  (let* ((modules-to-compile (append (%module-deep-dependencies-to-load main-module) (list main-module)))
         (all-modules (append compiled-modules modules-to-compile))
         (link-file (string-append output-file ".c")))
    ;; List files generated by compiling modules and the linkfile
    (let ((all-c-files/link
           (append (map (lambda (m) (string-append (android-build-directory) (%module-filename-c m version: version))) all-modules)
                   (list (string-append (android-build-directory) link-file)))))
      ;; Create Android build directory if it doesn't exist
      (unless (file-exists? (android-build-directory))
              (make-directory (android-build-directory)))
      ;; Create Android assets directory if it doesn't exist
      (unless (file-exists? (android-assets-directory))
              (make-directory (android-assets-directory)))
      ;; Generate modules (generates C code)
      (let ((something-generated? #f))
        (for-each
         (lambda (m)
           (let ((output-c-file (string-append (android-build-directory) (%module-filename-c m version: version))))
             (when ((newer-than? output-c-file)
                    (string-append (%module-path-src m) (%module-filename-scm m)))
                   (set! something-generated? #t)
                   (sake#compile-to-c m
                                      cond-expand-features: (cons 'android cond-expand-features)
                                      compiler-options: compiler-options
                                      verbose: verbose
                                      output: output-c-file))))
         modules-to-compile)
        (when (or something-generated? (not (file-exists? (string-append (android-build-directory) link-file))))
              (info/color 'blue "new C files generated")
              (sake#link-flat link-file all-modules
                              dir: (android-build-directory)
                              version: version
                              verbose: verbose))
        ;; Compile objects
        (set! something-generated? #f)
        (let ((o-files
               (map (lambda (f)
                      (let* ((output-o-file (string-append (path-strip-extension f) ".o"))
                             (args `(,(string-append "-I" (android-jni-directory) "deps/gambit/")
                                     "-D___DYNAMIC"
                                     ,@cc-options
                                     "-c" ,f
                                     "-o" ,output-o-file)))
                        (when ((newer-than? output-o-file) f)
                              (unless something-generated? (info/color 'blue "compiling updated C files:"))
                              (info/color 'brown (string-append " >>>  " f))
                              (set! something-generated? #t)
                              (fusion#android-run-compiler arch: 'arm
                                                           compiler: 'gcc
                                                           arguments: args
                                                           verbose: verbose))
                        output-o-file))
                    all-c-files/link)))
          ;; Make bundle
          (info/color 'green "compiling C/Scheme code into a loadable object")
          (fusion#android-run-linker
           arguments: `("-shared"
                        "-o"
                        ,(string-append (android-assets-directory) output-file)
                        ,@o-files
                        ,(string-append "-L" (android-toolchain-directory) "sysroot/usr/lib")
                        ,@ld-options)
           verbose: verbose))))))

;;! Install App in Android device
(define (fusion#android-install version)
  (let ((version-str (case version ((debug) "d") ((release) "r") (else (err "Wrong version argument: fusion#android-install")))))
    (unless (zero? (shell-command (string-append (host-ant-path) " -s " (android-directory)
                                                 "build.xml install" version-str)))
            (err "App couldn't be installed in Android device."))))

;;! Run App in Android device
(define (fusion#android-run activity)
  (unless (zero? (shell-command (string-append (android-adb-path) " shell am start -n " activity)))
          (err "App couldn't be run in Android device.")))
