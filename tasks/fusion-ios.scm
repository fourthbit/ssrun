;;; Copyright (c) 2014 by Álvaro Castro Castilla
;;; Extensions for Sake, to use with Fusion projects

(define ios-directory
  (make-parameter "ios/"))

(define ios-source-directory-suffix
  (make-parameter "src/"))

(define ios-source-directory
  (make-parameter
   (string-append (ios-directory) (ios-source-directory-suffix))))

(define ios-build-directory-suffix
  (make-parameter "build/"))

(define ios-build-directory
  (make-parameter
   (string-append (ios-source-directory) (ios-build-directory-suffix))))

(define ios-assets-directory-suffix
  (make-parameter "assets/"))

(define ios-assets-directory
  (make-parameter (string-append (ios-directory) (ios-assets-directory-suffix))))

(define ios-spheres-directory-suffix
  (make-parameter "spheres/"))

(define ios-spheres-directory
  (make-parameter (string-append (ios-directory) (ios-spheres-directory-suffix))))


(define ios-device-sdk-directory
  (make-parameter
   (when (eq? 'darwin (sake#host-platform))
         (let* ((sdk-dir-process
                 (open-process (list path: (string-append (%sphere-path 'fusion) "tools/get-ios-sdk-dir")
                                     arguments: '("iPhoneOS"))))
                (result (read-line sdk-dir-process)))
           (unless (zero? (process-status sdk-dir-process))
                   (err "fusion#compile-ios-app: error running script tools/get_ios_sdk_dir"))
           (close-port sdk-dir-process)
           result))))

(define ios-simulator-sdk-directory
  (make-parameter
   (when (eq? 'darwin (sake#host-platform))
         (let* ((sdk-dir-process
                 (open-process (list path: (string-append (%sphere-path 'fusion) "tools/get-ios-sdk-dir")
                                     arguments: '("iPhoneSimulator"))))
                (result (read-line sdk-dir-process)))
           (unless (zero? (process-status sdk-dir-process))
                   (err "fusion#compile-ios-app: error running script tools/get_ios_sdk_dir"))
           (close-port sdk-dir-process)
           result))))

;;------------------------------------------------------------------------------
;;!! Toolchain

(define xcodebuild-path
  (make-parameter
   (when (eq? 'darwin (sake#host-platform))
         (if (zero? (shell-command "xcodebuild -usage &>/dev/null"))
             "xcodebuild"
             #f))))

(define ios-sim-path
  (make-parameter
   (when (eq? 'darwin (sake#host-platform))
         (if (zero? (shell-command "ios-sim --version &>/dev/null"))
             "ios-sim"
             #f))))

;;------------------------------------------------------------------------------
;;!! iOS support procedures

;;! Check whether the project seems to be prepared for Android
(define (fusion#ios-project-supported?)
  (unless (file-exists? (ios-directory))
          (err "iOS directory doesn't exist. Please run iOS setup task."))
  (when (null? (directory-files (ios-directory)))
        (err "iOS directory doesn't seem to have anything. Please run iOS setup task."))
  (unless (file-exists? (ios-source-directory))
          (err "iOS source directory doesn't exist. Please run iOS setup task."))
  (when (null? (fileset dir: (ios-directory) test: (extension=? "xcodeproj")))
        (err "iOS Xcode project doesn't exist. Please run iOS setup task.")))

;;! Default clean for iOS
(define (fusion#ios-clean)
  (define (delete-if-exists dir)
    (when (file-exists? dir)
          (sake#delete-file dir recursive: #t force: #t)))
  ;;(delete-if-exists (ios-assets-directory))
  (delete-if-exists (ios-build-directory))
  (delete-if-exists (string-append (ios-source-directory) "libspheres.a"))
  (delete-if-exists (string-append (ios-directory) "build/")))

;;! Runs the compiler with the iOS environment
(define (fusion#ios-run-compiler #!key
                                 arch
                                 platform-type
                                 arguments
                                 (compiler 'gcc)
                                 (verbose #f))
  (let ((arch-str (symbol->string arch))
        (sdk-name (case platform-type ((device) "iphoneos") ((simulator) "iphonesimulator")))
        (ios-sdk-directory (case platform-type
                             ((device) (ios-device-sdk-directory))
                             ((simulator) (ios-simulator-sdk-directory)))))
    ;; Checks
    (unless (or (eq? platform-type 'simulator) (eq? platform-type 'device))
            (err "fusion#compile-ios-app: wrong platform-type"))
    (unless (or (eq? compiler 'gcc) (eq? compiler 'g++))
            (err "fusion#compile-ios-app: wrong compiler"))
    ;; Construct compiler arguments
    (let ((compiler-cli (list "-sdk" sdk-name
                              (symbol->string compiler)
                              "-isysroot" ios-sdk-directory
                              "-arch" arch-str
                              "-miphoneos-version-min=7.1")))
      (when verbose
            (info/color 'green "Compiler command:")
            (println (string-join compiler-cli))
            (info/color 'green "Compiler args:")
            (println (string-join arguments)))
      (let ((compilation-process
             (open-process
              (list path: "xcrun"
                    arguments: (append compiler-cli arguments)))))
        (unless (zero? (process-status compilation-process))
                (err "fusion#ios-run-compiler: error running command"))
        (close-port compilation-process)))))

;;! Runs the linker with the iOS environment
(define (fusion#ios-run-linker #!key
                                 arch
                                 platform-type
                                 arguments
                                 (verbose #f))
  (let ((arch-str (symbol->string arch))
        (sdk-name (case platform-type ((device) "iphoneos") ((simulator) "iphonesimulator")))
        (ios-sdk-directory (case platform-type
                             ((device) (ios-device-sdk-directory))
                             ((simulator) (ios-simulator-sdk-directory)))))
    ;; Checks
    (unless (or (eq? platform-type 'simulator) (eq? platform-type 'device))
            (err "fusion#ios-run-linker: wrong platform-type"))
    ;; Construct compiler strings
    (let ((ios-ld-cli (list
                       "-sdk" sdk-name
                       "ld"
                       "-syslibroot" ios-sdk-directory
                       "-arch" arch-str
                       "-ios_simulator_version_min" "7.1.0")))
      (when verbose
            (info/color 'green "Linker command:")
            (println (string-join ios-ld-cli))
            (info/color 'green "Linker args:")
            (println (string-join arguments)))
      (let ((compilation-process
             (open-process
              (list path: "xcrun"
                    arguments: (append ios-ld-cli arguments)))))
        (unless (zero? (process-status compilation-process))
                (err "fusion#ios-run-linker: error running command"))
        (close-port compilation-process)))))

;;! Create an archive (static library) for iOS given a set of object files
(define (fusion#ios-create-library-archive lib-name o-files #!key (verbose #f))
  (shell-command (string-append "ar r" (if verbose "cv " " ") lib-name " " (string-join o-files))))

;;! Compile App
;; .parameter main-module main-module of the Android App
;; .parameter arch: target architecture
(define (fusion#ios-compile-app main-module
                                #!key
                                arch
                                (target 'debug)
                                (merge-modules #f)
                                (cond-expand-features '())
                                (compiler-options '())
                                (cc-options '())
                                (ld-options '())
                                (version compiler-options)
                                (compiled-modules '())
                                (verbose #f))
  ;; Cond-expand features (relevant within the Sake environment)
  (##cond-expand-features (cons 'ios (append cond-expand-features (##cond-expand-features))))
  ;; Checks
  (fusion#ios-project-supported?)
  (unless arch (err "fusion#ios-compile-app: arch argument is mandatory"))
  (unless (or (eq? arch 'i386) (eq? arch 'armv7) (eq? arch 'armv7s))
          (err "fusion#ios-compile-app: wrong arch argument"))
  (when merge-modules
        (err "fusion#ios-compile-app: merge-modules options is not yet implemented"))
  ;; Compute dependencies
  (let* ((modules-to-compile (append (%module-deep-dependencies-to-load main-module) (list main-module)))
         (all-modules (append compiled-modules modules-to-compile))
         (platform-type (case arch
                          ((i386) 'simulator)
                          ((armv7 armv7s) 'device)))
         (link-file "linkfile_.c"))
    ;; List files generated by compiling modules and the linkfile
    (let ((all-c-files
           (append (map (lambda (m) (string-append (ios-build-directory) (%module-filename-c m version: version))) all-modules)
                   (list (string-append (ios-build-directory) link-file)))))
      ;; Create iOS build directory if it doesn't exist
      (unless (file-exists? (ios-build-directory))
              (make-directory (ios-build-directory)))
      ;; Create iOS assets directory if it doesn't exist
      (unless (file-exists? (ios-assets-directory))
              (make-directory (ios-assets-directory)))
      ;; Generate modules (generates C code)
      (let ((something-generated? #f))
        (for-each
         (lambda (m)
           (let ((output-c-file (string-append (ios-build-directory) (%module-filename-c m version: version))))
             (if ((newer-than? output-c-file)
                  (string-append (%module-path-src m) (%module-filename-scm m)))
                 (begin
                   (set! something-generated? #t)
                   (sake#compile-to-c m
                                      cond-expand-features: (cons 'ios cond-expand-features)
                                      compiler-options: compiler-options
                                      verbose: verbose
                                      output: output-c-file)))))
         modules-to-compile)
        (when something-generated?
              (info/color 'blue "new C files generated")
              (sake#link-incremental link-file all-modules
                                           version: version
                                           dir: (ios-build-directory)))
        (set! something-generated? #f)
        (info/color 'green "compiling C/Scheme code into a static lib")
        (let ((o-files
               (map (lambda (f)
                      (let ((output-o-file (string-append (path-strip-extension f) ".o")))
                        (when ((newer-than? output-o-file) f)
                              (unless something-generated?
                                      (info/color 'blue "compiling updated C files:"))
                              (info/color 'browns (string-append " >>>  " f))
                              (set! something-generated? #t)
                              (fusion#ios-run-compiler
                               arch: arch
                               platform-type: platform-type
                               arguments: `("-x" "objective-c"
                                            ,(string-append "-I" (ios-directory) "gambit/include")
                                            "-D___LIBRARY"
                                            ,(string-append "-I" (ios-source-directory))
                                            "-Wno-trigraphs"
                                            "-Wreturn-type"
                                            "-Wunused-variable"
                                            ,@cc-options
                                            "-c" ,f
                                            "-o" ,output-o-file)
                               compiler: 'gcc
                               verbose: verbose))
                        output-o-file))
                    all-c-files)))
          (fusion#ios-create-library-archive (string-append (ios-source-directory) "libspheres.a")
                                             o-files
                                             verbose: verbose))))
    (info/color 'blue "compiling iOS app")
    (parameterize
     ((current-directory (ios-directory)))
     (shell-command (string-append
                     (xcodebuild-path) " build -configuration Debug -sdk "
                     (case platform-type ((device) "iphoneos") ((simulator) "iphonesimulator"))
                     " -arch " (symbol->string arch))))))

;;! Generate a loadable object from a module and its dependencies for iOS
;; Warning! This only works on the simulator on iOS
(define (fusion#ios-compile-loadable-set output-file
                                         main-module
                                         #!key
                                         arch
                                         (target 'debug)
                                         (merge-modules #f)
                                         (cond-expand-features '())
                                         (compiler-options '())
                                         (cc-options '())
                                         (ld-options '())
                                         (version compiler-options)
                                         (compiled-modules '())
                                         (verbose #f))
  ;; Cond-expand features (relevant within the Sake environment)
  (##cond-expand-features (cons 'ios (append cond-expand-features (##cond-expand-features))))
  ;; Checks
  (fusion#ios-project-supported?)
  (unless arch (err "fusion#ios-compile-loadable-set: arch argument is mandatory"))
  (unless (or (eq? arch 'i386) (eq? arch 'armv7) (eq? arch 'armv7s))
          (err "fusion#ios-compile-loadable-set: wrong arch argument"))
  (when merge-modules
        (err "fusion#ios-compile-loadable-set: merge-modules options is not yet implemented"))
  ;; Compute dependencies
  (let* ((modules-to-compile (append (%module-deep-dependencies-to-load main-module) (list main-module)))
         (all-modules (append compiled-modules modules-to-compile))
         (platform-type (case arch ((i386) 'simulator) ((armv7 armv7s) 'device)))
         (link-file (string-append output-file ".c")))
    ;; List files generated by compiling modules and the linkfile
    (let ((all-c-files
           (append (map (lambda (m) (string-append (ios-build-directory)
                                              (%module-filename-c m version: version)))
                        all-modules)
                   (list (string-append (ios-build-directory) link-file)))))
      ;; Create iOS build directory if it doesn't exist
      (unless (file-exists? (ios-build-directory))
              (make-directory (ios-build-directory)))
      ;; Create iOS assets directory if it doesn't exist
      (unless (file-exists? (ios-assets-directory))
              (make-directory (ios-assets-directory)))
      ;; Generate modules (generates C code)
      (let ((something-generated? #f))
        (for-each
         (lambda (m)
           (let ((output-c-file (string-append (ios-build-directory) (%module-filename-c m version: version))))
             (if ((newer-than? output-c-file)
                  (string-append (%module-path-src m) (%module-filename-scm m)))
                 (begin
                   (set! something-generated? #t)
                   (sake#compile-to-c m
                                      cond-expand-features: (cons 'ios cond-expand-features)
                                      compiler-options: compiler-options
                                      verbose: verbose
                                      output: output-c-file)))))
         modules-to-compile)
        (when something-generated?
              (info/color 'blue "new C files generated")
              (sake#link-flat link-file all-modules
                              version: version
                              dir: (ios-build-directory)))
        ;; Compile objects
        (set! something-generated? #f)
        (let ((o-files
               (map (lambda (f)
                      (let* ((output-o-file (string-append (path-strip-extension f) ".o"))
                             (args `("-x" "objective-c"
                                            ,(string-append "-I" (ios-directory) "gambit/include")
                                            "-D___DYNAMIC"
                                            ,(string-append "-I" (ios-source-directory))
                                            "-Wno-trigraphs"
                                            "-Wreturn-type"
                                            "-Wunused-variable"
                                            ,@cc-options
                                            "-c" ,f
                                            "-o" ,output-o-file)))
                        (when ((newer-than? output-o-file) f)
                              (unless something-generated?
                                      (info/color 'blue "compiling updated C files:"))
                              (info/color 'brown (string-append " >>>  " f))
                              (set! something-generated? #t)
                              (fusion#ios-run-compiler
                               arch: arch
                               platform-type: platform-type
                               compiler: 'gcc
                               arguments: args
                               verbose: verbose))
                        output-o-file))
                    all-c-files)))
          ;; Make bundle
          (info/color 'green "compiling C/Scheme code into a loadable object")
          (fusion#ios-run-linker
           arch: arch
           platform-type: platform-type
           ;; Flags not included:
           ;; "-lcrt1.o" This one is for the executables
           arguments: `("-bundle"
                        "-demangle"
                        "-dynamic"
                        "-ObjC"
                        "-all_load"
                        "-dead_strip"
                        ;; We hard-code these three, specific to the iOS Simulator
                        "-objc_abi_version" "2"
                        "-no_implicit_dylibs"
                        ;; Essential libraries
                        "-framework" "Foundation"
                        "-framework" "UIKit"
                        "-lobjc"
                        "-lSystem"
                        ,@o-files "-o"
                        ,(string-append (ios-spheres-directory) output-file))
           verbose: verbose))))))

;;! Copy the local source dependencies to the iOS spheres directory, so they can be loaded
;; at runtime
(define (fusion#ios-copy-foreign-dependencies module
                                              #!key
                                              (cond-expand-features '()))
  (##cond-expand-features (cons 'ios (append cond-expand-features (##cond-expand-features))))
  ;; Spheres for the REPL
  ;; (let ((spheres-file (string-append (ios-spheres-directory) "core/src/spheres.scm")))
  ;;   (unless (file-exists? spheres-file)
  ;;           (sake#copy-file (string-append (%module-path-src '(core: spheres))
  ;;                                          (%module-filename-scm '(core: spheres)))
  ;;                           spheres-file)))
  ;; Syntax-case for the REPL
  (let ((syntax-case-file (string-append (ios-spheres-directory) "core/lib/syntax-case.o1")))
    (unless (file-exists? syntax-case-file)
            (sake#copy-file (string-append (%sphere-path 'fusion) "ios/syntax-case.o1")
                            syntax-case-file)))
  ;; Copy dependencies in source form so they can be accessed from the REPL
  (let* ((current-sphere (%current-sphere))
         (current-sphere-remover
          (lambda (m) (not (eq? (%module-sphere m) current-sphere))))
         (selected-modules
          (append
           (filter current-sphere-remover (%module-deep-dependencies-to-include module))
           (filter current-sphere-remover (%module-deep-dependencies-to-load module)))))
    (for-each
     (lambda (m)
       (let* ((filename (%module-filename-scm m))
              (target-source (string-append (ios-spheres-directory)
                                            (symbol->string (%module-sphere m)) "/src/"
                                            filename))
              (origin-source-directory (%module-path-src m))
              (origin-source (string-append origin-source-directory filename)))
         (when ((newer-than? target-source) origin-source)
               (if (file-exists? target-source)
                   (info/color 'brown "updated file " filename)
                   (begin (info/color 'brown "new file " filename)
                          (sake#make-directory origin-source-directory)))
               (sake#copy-file origin-source target-source))))
     selected-modules)))
