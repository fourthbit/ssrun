;;------------------------------------------------------------------------------
;; Minimal toolkit for avoiding dependencies but still enjoy some goodies

;;! The accumulator represents the rightmost value to tack onto the end of
;; the list, after you've finished recursing down it.
(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

;;! The accumulator represents the completed calculation for the leftmost
;; part of the list. Tail-recursive, more efficient than foldr.
(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

;;! Reduce
(define (reduce f i l)
  (let reduce ((i i) (l l))
    (if (null? l) i
        (reduce (f i (car l)) (cdr l)))))

;;! Unfold
(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

;;! Filter
(define (filter pred lst)
  (foldr (lambda (x y) (if (pred x) (cons x y) y))
         '()
         lst))

;;! Any
(define (any pred lst)
  (let recur ((rest lst))
    (cond ((null? rest) #f)
          ((pred (car rest)) #t)
          (else (recur (cdr rest))))))

;;! Every
(define (every pred lst)
  (let recur ((rest lst))
    (cond ((null? rest) #t)
          ((pred (car rest)) (recur (cdr rest)))
          (else #f))))

;;! Drop
(define (drop lis k)
    (let iter ((lis lis) (k k))
      (if (zero? k) lis (iter (cdr lis) (- k 1)))))

;;! Run the function at the leaves of the tree
(define (map* f l)
  (cond
   ((null? l) '())
   ((not (pair? l)) (f l))
   (else
    (cons (map** f (car l)) (map** f (cdr l))))))

;;! Run the function at every node of the tree
(define (map** f l)
  (cond
   ((null? l) '())
   ((not (pair? l)) (f l))
   (else
    (cons (f (map** f (car l))) (f (map** f (cdr l)))))))

;;! Curry
(define (curry func arg1)
  (lambda (arg) (apply func (cons arg1 arg))))

;;! Compose
(define (compose f g)
  (lambda (arg) (f (apply g arg))))

;;! Complement
(define (complement f)
  (lambda args (not (apply f args))))

;;! Non-tail recursive Quick Sort
(define (quicksort l gt?)
  (if (null? l)
      '()
      (append (quicksort (filter (lambda (x) (gt? (car l) x)) (cdr l)) gt?)
              (list (car l))
              (quicksort (filter (lambda (x) (not (gt? (car l) x))) (cdr l)) gt?))))

;;! Split a string using a separator
(define (string-split sep)
  (lambda (str)
    (call-with-input-string
     str
     (lambda (p)
       (read-all p (lambda (p) (read-line p sep)))))))

;; Concatenate strings
(define (string-concatenate strings)
  (define (%string-copy! to tstart from fstart fend)
    (if (> fstart tstart)
        (do ((i fstart (+ i 1))
             (j tstart (+ j 1)))
            ((>= i fend))
          (string-set! to j (string-ref from i)))

        (do ((i (- fend 1)                    (- i 1))
             (j (+ -1 tstart (- fend fstart)) (- j 1)))
            ((< i fstart))
          (string-set! to j (string-ref from i)))))
  (let* ((total (do ((strings strings (cdr strings))
                     (i 0 (+ i (string-length (car strings)))))
                    ((not (pair? strings)) i)))
         (ans (make-string total)))
    (let lp ((i 0) (strings strings))
      (if (pair? strings)
          (let* ((s (car strings))
                 (slen (string-length s)))
            (%string-copy! ans i s 0 slen)
            (lp (+ i slen) (cdr strings)))))
    ans))

;; Join strings
(define (string-join strings #!key (delim " ") (grammar 'infix))
  (let ((buildit (lambda (lis final)
                   (let recur ((lis lis))
                     (if (pair? lis)
                         (cons delim (cons (car lis) (recur (cdr lis))))
                         final)))))
    (cond ((pair? strings)
           (string-concatenate
            (case grammar
              ((infix strict-infix)
               (cons (car strings) (buildit (cdr strings) '())))
              ((prefix) (buildit strings '()))
              ((suffix)
               (cons (car strings) (buildit (cdr strings) (list delim))))
              (else (error "Illegal join grammar"
                           grammar string-join)))))
          ((not (null? strings))
           (error "STRINGS parameter not list." strings string-join))
          ((eq? grammar 'strict-infix)
           (error "Empty list cannot be joined with STRICT-INFIX grammar."
                  string-join))
          (else ""))))

;; string-contains    s1 s2 [start1 end1 start2 end2] -> integer or false
;; string-contains-ci s1 s2 [start1 end1 start2 end2] -> integer or false
;;     Does string s1 contain string s2?
;;     Return the index in s1 where s2 occurs as a substring, or false. The
;;     optional start/end indices restrict the operation to the indicated
;;     substrings.
;; We do not support the optional arguments
(define (string-contains str pattern)
  (let* ((pat-len (string-length pattern))
         (search-span (- (string-length str) pat-len))
         (c1 (if (zero? pat-len) #f (string-ref pattern 0)))
         (c2 (if (<= pat-len 1) #f (string-ref pattern 1))))
    (cond
     ((not c1) 0)                     ; empty pattern, matches upfront
     ((not c2) (string-index str c1)) ; one-char pattern
     (else                  ; matching a pattern of at least two chars
      (let outer ((pos 0))
        (cond
         ((> pos search-span) #f) ; nothing was found thru the whole str
         ((not (char=? c1 (string-ref str pos)))
          (outer (+ 1 pos)))	; keep looking for the right beginning
         ((not (char=? c2 (string-ref str (+ 1 pos))))
          (outer (+ 1 pos)))     ; could've done pos+2 if c1 == c2....
         (else                   ; two char matched: high probability
				   	; the rest will match too
          (let inner ((i-pat 2) (i-str (+ 2 pos)))
            (if (>= i-pat pat-len) pos  ; whole pattern matched
                (if (char=? (string-ref pattern i-pat)
                            (string-ref str i-str))
                    (inner (+ 1 i-pat) (+ 1 i-str))
                    (outer (+ 1 pos))))))))))))	; mismatch after partial match

;; Return the index of the first occurence of a-char in str, or #f
;; This is a subset of the corresponding SRFI-13 function.
;; The latter is more generic.
(define (string-index str a-char)
  (let loop ((pos 0))
    (cond
     ((>= pos (string-length str)) #f) ; whole string has been searched, in vain
     ((char=? a-char (string-ref str pos)) pos)
     (else (loop (+ pos 1))))))

;; Return the index of the last occurence of a-char in str, or #f
;; This is a subset of the corresponding SRFI-13 function.
;; The latter is more generic.
(define (string-index-right str a-char)
  (let loop ((pos (dec (string-length str))))
    (cond
     ((negative? pos) #f)    ; whole string has been searched, in vain
     ((char=? a-char (string-ref str pos)) pos)
     (else (loop (- pos 1))))))

