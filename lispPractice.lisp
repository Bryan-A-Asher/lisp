(setq N (list 0 1 2 3 4 5 6 7 8 9))
(setq M (list 'a 'b 'c 'd 'e 'f 'g 'h))
(setq O (list 'a))
(setq B (list))
(set 'temp 10)

(defun listMember(L)
  (if (eq L '())
    ()
    (and (print (car L)) (listMember (cdr L))))
)

(defun isMember(x L)
  (cond
    ((eq (cdr L) '()) 'L)
    ((eq x (car L)) t)
    (t (isMember x (cdr L))))
)

(defun factorial (n)
  (if (<= n 0)
    1
    (* n (factorial (- n 1))))
)

(defun conEx(x)
  (cond
  ((= x 10) (print "Hi" ))
  ((= x 11) (print "Bye"))
  ((= x 12) (print "Huh")))
)

(defun lengthList(L)
  (if (eq L '())
  0
  (+ 1 (lengthList (cdr L))))
)

(print (car '(a b c)))
(print (cons 'a '(b c)))
(print (cdr '(a b c)))
(print (cdr (cdr '(a b c))))
(print (list 'a 'b 'c))
(print (cons 'a '(b c)))
(print (atom N))
(print (cons (atom 'a)(eq 'x 'y)))
(print (list 'a '(b c) (atom '(a))))
(print M)
(print(if (eq (car M) 'a)
       (print (list '(y e s)))
       (print(cdr '(a n o)))))
(print ((lambda(L)
          (car (cdr L)))
            '(a b c)))
(print (funcall #'listMember N))
(print (funcall #'factorial 10))
(funcall #'conEx 12)
(print (funcall #'lengthList N))
(print (funcall #'isMember 9 N))
