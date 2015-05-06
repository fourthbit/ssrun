(define-structure task
  name
  parameters
  (description unprintable:)
  (depends read-only: unprintable:)
  (handler read-only: unprintable:)
  (executed? unprintable: init: #f))

(define (task-force-run task #!key arguments)
  (let ((fixed-arguments
         (let recur ((expected (task-parameters task))
                     (provided arguments))
           (cond ((null? expected)
                  '())
                 ((null? provided)
                  (cons #f
                        (recur (cdr expected) '())))
                 (else
                  (cons (car provided)
                        (recur (cdr expected) (cdr provided))))))))
    (apply (task-handler task) fixed-arguments))
  (task-executed?-set! task #t)
  #t)

(define (task-run task #!key arguments)
  (or (task-executed? task)
      (let* ((expected (task-parameters task))
             (provided arguments)
             (no-arguments? (and (null? expected) (null? provided))))
        (map (lambda (t) (task-run t arguments: '())) (task-depends task))
        (display (string-append "*** INFO -- \033[01;34mrunning task "
                                (symbol->string (task-name task))
                                "...\033[00m "))
        ;; Print arguments
        (if no-arguments?
            (newline)
            (begin
              (display (string-append " ("))
              (let recur ((expected expected)
                          (provided provided))
                (cond ((null? expected)
                       '())
                      ((null? provided)
                       (display (car expected)) (display ": ") (display "<not provided>")
                       (if (not (null? (cdr expected))) (display ", "))
                       (recur (cdr expected) '()))
                      (else
                       (display (car expected)) (display ": ") (display (car provided))
                       (if (not (null? (cdr expected))) (display ", "))
                       (recur (cdr expected) (cdr provided)))))
              (display ") \n")))
        (task-force-run task arguments: arguments)
        (display "\033[01;34mdone\033[00m\n")
        #t)))

;; bound dynamically to the local task
(define current-task (make-parameter #f))

;; bound to task that have to be run as a default task
(define main-task (make-parameter #f))
