(##namespace ("ssrun#" 
              make-task
              task?
              task-name
              task-parameters
              task-descriptino
              task-depends 
              task-handler
              task-executed?
              task 
              task-run
              define-task
              current-task
              main-task
              ))

;; (define-macro (task depends . body)
;;   (let((name (gensym 'task))
;;        (self '(gensym 'self)))
;;     `(let((,self '()))
;;        (set! ,self 
;;              (make-task
;;               ,name 
;;               (list ,@depends) 
;;               ,(if (and (pair? body) (string? (car body))) (car body) (symbol->string name))
;;               (lambda () 
;;                 (parameterize
;;                  ((current-task ,self))
;;                  ,@body))))
;;        ,self)))

(define-macro (define-task name&args depends . body)
  (let ((name (if (pair? name&args)
                  (car name&args)
                  name&args))
        (params (if (pair? name&args)
                    (cdr name&args)
                    '())))
    `(begin
       (define ,name
         (make-task (quote ,name)
                    ',params
                    ,(if (and (pair? body) (string? (car body)))
                         (car body)
                         (symbol->string name))
                    (list ,@depends)
                    (lambda ,params
                      (parameterize ((current-task ,name))
                                    ,@body))))
       (main-task ,name))))
