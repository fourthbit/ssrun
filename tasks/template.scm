;;! Core template building procedure, builds the code to be compiled or interpreted
;; Arguments are passed to sub-templates in the variable "args"
;; ((eval
;;   (build-template "
;; <title>=(let-template (a b) (string-append \"Hello\" \" world.\n\" (a) (b 8585858)))=
;; -(try )-
;; -(this =((car args))= times!)-
;; </title>
;; <h1>Header =(input-var)=</h1>
;; " '(input-var)))
;;  "passed variable!")
(define (generate-template-code template-char-list input-parameters)
  (letrec
      ((template-code-begin?
        (lambda (strl) (and (not (null? strl))
                       (char=? #\= (car strl))
                       (not (null? (cdr strl)))
                       (char=? #\( (cadr strl)))))
       (template-code-end?
        (lambda (strl) (and (not (null? strl))
                       (char=? #\) (car strl))
                       (not (null? (cdr strl)))
                       (char=? #\= (cadr strl)))))
       (template-argstring-begin?
        (lambda (strl last-token-type)
          (if (or (eq? last-token-type 'code)
                  (eq? last-token-type 'arg-string))
              (and (not (null? strl))
                   (char=? #\( (car strl)))
              (and (not (null? strl))
                   (char=? #\- (car strl))
                   (not (null? (cdr strl)))
                   (char=? #\( (cadr strl))))))
       (template-argstring-end?
        (lambda (strl) (and (not (null? strl))
                       (char=? #\) (car strl))
                       (not (null? (cdr strl)))
                       (char=? #\- (cadr strl)))))
       (parse
        (lambda (template-char-list)
          (receive (tokens rest)
                   (let token-recur ((strl template-char-list)
                                     (last-token-type 'none))
                     (cond
                      ((null? strl) (values '() '()))
                      ;; Code tokens
                      ((template-code-begin? strl)
                       (receive (chars rest)
                                (let char-recur ((strl (cdr strl))
                                                 (inside-code-string? #f))
                                  (cond ((null? strl) (values '() '()))
                                        ((and (template-code-end? strl)
                                              (not inside-code-string?))
                                         (values '(#\)) (cddr strl)))
                                        (else (receive (chars rest)
                                                       (char-recur (cdr strl)
                                                                   (if inside-code-string?
                                                                       (not (char=? #\" (car strl)))
                                                                       (char=? #\" (car strl))))
                                                       (values (cons (car strl)
                                                                     chars)
                                                               rest)))))
                                (receive (tokens rest)
                                         (token-recur rest 'code)
                                         (values (cons (cons 'code (list->string chars))
                                                       tokens)
                                                 rest))))
                      ;; Argument string tokens
                      ((template-argstring-begin? strl last-token-type)
                       (receive (chars rest)
                                (let char-recur ((strl (if (or (eq? last-token-type 'code)
                                                               (eq? last-token-type 'arg-string))
                                                           (cdr strl)
                                                           (cddr strl))))
                                  (cond ((null? strl) (values '() '()))
                                        ((template-argstring-end? strl)
                                         (values '() (cddr strl)))
                                        (else (receive (chars rest)
                                                       (char-recur (cdr strl))
                                                       (values (cons (car strl)
                                                                     chars)
                                                               rest)))))
                                (receive (tokens rest)
                                         (token-recur rest 'arg-string)
                                         (values (cons (cons 'arg-string (parse chars))
                                                       tokens)
                                                 rest))))
                      ;; Free string tokens
                      (else
                       (receive (chars-for-string rest)
                                (let char-recur ((strl strl))
                                  (cond ((null? strl) (values '() '()))
                                        ((template-code-begin? strl)
                                         (values '() strl))
                                        ((template-argstring-end? strl)
                                         (values '() (cddr strl)))
                                        (else (receive (chars-for-string rest)
                                                       (char-recur (cdr strl))
                                                       (values (cons (car strl)
                                                                     chars-for-string)
                                                               rest)))))
                                (receive (tokens rest)
                                         (token-recur rest 'free-string)
                                         (values (cons (cons 'free-string (list->string chars-for-string))
                                                       tokens)
                                                 rest))))))
                   tokens)))
       (generate-code
        (lambda (tokens)
          (letrec
              ((generate-template-level
                (lambda (tokens)
                  `(string-append
                    ,@(let recur ((tok tokens))
                        (cond ((null? tok) '())
                              ;; Generate free strings
                              ((eq? 'free-string (caar tok))
                               (if (zero? (string-length (cdar tok)))
                                   (recur (cdr tok)) ; Remove if zero-length
                                   (cons (cdar tok)
                                         (recur (cdr tok)))))
                              ;; Generate code part and its arguments
                              ((eq? 'code (caar tok))
                               (let ((template-processor (with-input-from-string (cdar tok) read)))
                                 (receive (template-args rest)
                                          (generate-template-sublevels (cdr tok))
                                          (cond ((eq? (car template-processor) 'let-template)
                                                 (if (not (= (length (cadr template-processor))
                                                             (length template-args)))
                                                     (error "Number of template processor parameters and given sub-templates don't match"))
                                                 (cons `(,(cons 'lambda (cdr template-processor))
                                                         ,@(map (lambda (arg) `(lambda args (string-append ,@(recur arg)))) template-args))
                                                       (recur rest)))
                                                (else
                                                 (cons `((lambda (o) (if (string? o) o (object->string o))) ,(car template-processor))
                                                       (recur rest)))))))
                              ((eq? 'arg-string (caar tok))
                               (error "Nested templates must come after a template processor"))
                              (else
                               (error "error generating code")))))))
               (generate-template-sublevels
                (lambda (tokens)
                  (let recur ((tok tokens))
                    (cond ((null? tok) (values '() '()))
                          ((eq? (caar tok) 'arg-string)
                           (receive (template-args rest)
                                    (recur (cdr tok))
                                    (values (cons (cdar tok)
                                                  template-args)
                                            rest)))
                          (else
                           (values '()
                                   tok)))))))
            (generate-template-level tokens)))))
    `(lambda ,input-parameters ,(generate-code (parse template-char-list)))))

;;! Build a template, use a string as input
(define (build-template-from-string template . parameters)
  (eval (generate-template-code (string->list template) parameters)))


;;! Build a template, use a file as input
(define (build-template-from-file file . parameters)
  (eval (generate-template-code (call-with-input-file file (lambda (p) (read-all p read-char))) parameters)))
