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

;; -- procedure+: string-split STRING
;; -- procedure+: string-split STRING '()
;; -- procedure+: string-split STRING '() MAXSPLIT
;; Returns a list of whitespace delimited words in STRING.
;; If STRING is empty or contains only whitespace, then the empty list
;; is returned. Leading and trailing whitespaces are trimmed.
;; If MAXSPLIT is specified and positive, the resulting list will
;; contain at most MAXSPLIT elements, the last of which is the string
;; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
;; non-positive, the empty list is returned. "In time critical
;; applications it behooves you not to split into more fields than you
;; really need."
;;
;; -- procedure+: string-split STRING CHARSET
;; -- procedure+: string-split STRING CHARSET MAXSPLIT
;; Returns a list of words delimited by the characters in CHARSET in
;; STRING. CHARSET is a list of characters that are treated as delimiters.
;; Leading or trailing delimeters are NOT trimmed. That is, the resulting
;; list will have as many initial empty string elements as there are
;; leading delimiters in STRING.
;;
;; If MAXSPLIT is specified and positive, the resulting list will
;; contain at most MAXSPLIT elements, the last of which is the string
;; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
;; non-positive, the empty list is returned. "In time critical
;; applications it behooves you not to split into more fields than you
;; really need."
;;
;; (string-split " abc d e f  ") ==> ("abc" "d" "e" "f")
;; (string-split " abc d e f  " '() 1) ==> ("abc d e f  ")
;; (string-split " abc d e f  " '() 0) ==> ()
;; (string-split ":abc:d:e::f:" '(#\:)) ==> ("" "abc" "d" "e" "" "f" "")
;; (string-split ":" '(#\:)) ==> ("" "")
;; (string-split "root:x:0:0:Lord" '(#\:) 2) ==> ("root" "x:0:0:Lord")
;; (string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:))
;; ==> ("/usr/local/bin" "/usr/bin" "/usr/ucb/bin")
;; (string-split "/usr/local/bin" '(#\/)) ==> ("" "usr" "local" "bin")
;; (define (string-split str . rest)
;;   ;; maxsplit is a positive number
;;   (define (split-by-whitespace str maxsplit)
;;     (define (skip-ws i yet-to-split-count)
;;       (cond
;;        ((>= i (string-length str)) '())
;;        ((char-whitespace? (string-ref str i))
;;         (skip-ws (inc i) yet-to-split-count))
;;        (else (scan-beg-word (inc i) i yet-to-split-count))))
;;     (define (scan-beg-word i from yet-to-split-count)
;;       (cond
;;        ((zero? yet-to-split-count)
;;         (cons (substring str from (string-length str)) '()))
;;        (else (scan-word i from yet-to-split-count))))
;;     (define (scan-word i from yet-to-split-count)
;;       (cond
;;        ((>= i (string-length str))
;;         (cons (substring str from i) '()))
;;        ((char-whitespace? (string-ref str i))
;;         (cons (substring str from i) 
;;               (skip-ws (inc i) (- yet-to-split-count 1))))
;;        (else (scan-word (inc i) from yet-to-split-count))))
;;     (skip-ws 0 (- maxsplit 1)))
;;   ;; maxsplit is a positive number
;;   ;; str is not empty
;;   (define (split-by-charset str delimeters maxsplit)
;;     (define (scan-beg-word from yet-to-split-count)
;;       (cond
;;        ((>= from (string-length str)) '(""))
;;        ((zero? yet-to-split-count)
;;         (cons (substring str from (string-length str)) '()))
;;        (else (scan-word from from yet-to-split-count))))
;;     (define (scan-word i from yet-to-split-count)
;;       (cond
;;        ((>= i (string-length str))
;;         (cons (substring str from i) '()))
;;        ((memq (string-ref str i) delimeters)
;;         (cons (substring str from i) 
;;               (scan-beg-word (inc i) (- yet-to-split-count 1))))
;;        (else (scan-word (inc i) from yet-to-split-count))))
;;     (scan-beg-word 0 (- maxsplit 1)))
;;   ;; resolver of overloading...
;;   ;; if omitted, maxsplit defaults to
;;   ;; (inc (string-length str))
;;   (if (string-null? str) '()
;;       (if (null? rest) 
;;           (split-by-whitespace str (inc (string-length str)))
;;           (let ((charset (car rest))
;;                 (maxsplit
;;                  (if (pair? (cdr rest)) (cadr rest) (inc (string-length str)))))
;;             (cond 
;;              ((not (positive? maxsplit)) '())
;;              ((null? charset) (split-by-whitespace str maxsplit))
;;              (else (split-by-charset str charset maxsplit)))))))


;; make-char-quotator QUOT-RULES
;; Given QUOT-RULES, an assoc list of (char . string) pairs, return
;; a quotation procedure. The returned quotation procedure takes a string
;; and returns either a string or a list of strings. The quotation procedure
;; check to see if its argument string contains any instance of a character
;; that needs to be encoded (quoted). If the argument string is "clean",
;; it is returned unchanged. Otherwise, the quotation procedure will
;; return a list of string fragments. The input straing will be broken
;; at the places where the special characters occur. The special character
;; will be replaced by the corresponding encoding strings.
;;
;; For example, to make a procedure that quotes special HTML characters,
;; do
;; 	(make-char-quotator
;; 	    '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")))
(define (make-char-quotator char-encoding)
  (define (inc i) (+ i 1))
  (let ((bad-chars (map car char-encoding)))
    ;; Check to see if str contains one of the characters in charset,
    ;; from the position i onward. If so, return that character's index.
    ;; otherwise, return #f
    (define (index-cset str i charset)
      (let loop ((i i))
	(and (< i (string-length str))
	     (if (memv (string-ref str i) charset) i
		 (loop (inc i))))))
    ;; The body of the function
    (lambda (str)
      (let ((bad-pos (index-cset str 0 bad-chars)))
	(if (not bad-pos) str           ; str had all good chars
	    (let loop ((from 0) (to bad-pos))
	      (cond
	       ((>= from (string-length str)) '())
	       ((not to)
		(cons (substring str from (string-length str)) '()))
	       (else
		(let ((quoted-char
		       (cdr (assv (string-ref str to) char-encoding)))
		      (new-to 
		       (index-cset str (inc to) bad-chars)))
		  (if (< from to)
		      (cons
		       (substring str from to)
		       (cons quoted-char (loop (inc to) new-to)))
		      (cons quoted-char (loop (inc to) new-to))))))))))))

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
  (let loop ((pos (- (string-length str) 1)))
    (cond
     ((negative? pos) #f)    ; whole string has been searched, in vain
     ((char=? a-char (string-ref str pos)) pos)
     (else (loop (- pos 1))))))


