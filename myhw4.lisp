; Programmer: Bryan A. Asher
; Date : April 30, 2018
; Description: lisp utilities
(setf *print-length* 25)


; Evaluates to the last element of list L.
; eg.  (myLast ‘(p a e g))  →  g
(defun myLast(L)
  (if (eq (cdr L) '())
    (car L)
    (myLast (cdr L))
  )
)


; Evaluates to the number of occurrences of atom X in list L.
; eg.  (myCount ‘a ‘(p k a t p a e g))  →  2
(defun myCount(x L)
  (if (eq L '())
    0
    (if (or (eq x (car L)) (listp (car L)))
      (+  (if (eq x (car L)) 
            (+ 1 (myCount x (cdr L)))
            0
          ) 
          (if (listp (car L)) 
            (+ (myCount x (car L)) (myCount x (cdr L))) 
            0
          )
      )
      (myCount x (cdr L))
    ) 
  )  
)


; Evaluates to ‘true’ if X is an atom in list L, ‘false’ otherwise.
; eg.  (myMember ‘a ‘(p a e g))  →  t
(defun myMember(x L)
  (defvar temp nil)
  (if (eq L '())
    temp
    (if (or (eq x (car L)) (listp (car L)))
      (or (if (eq x (car L)) 
            (setq temp t)
            (myMember x (cdr L))
          ) 
          (if (listp (car L))
            (or (myMember x (car L)) (myMember x (cdr L)))
            temp
          )
      )
      (myMember x (cdr L))
    ) 
  )  
)


; Evaluates to a list with all elements of  L without repetition.
; eg.  (myPurge ‘(p a c e p c))  →  (a e p c)
(defun myPurge(L)
  (if (eq L '())
    ()
    (if (= (myCount (car L) L) 1)
      (cons (car L) (myPurge(cdr L)))
      (myPurge(cdr L))
    )
  )
)

; (defun myPurge(L)
;   (if (eq L '())
;     ()
;     (if (and (print (car L) ) (myMember (car L) L))
;       (myPurge (cdr L))
;       (cons (car L) (myPurge (cdr L)))
;     )
;   )
; )


; Evaluates to a list of elements that are common in both lists L1 and L2.
; Assume L1 and L2 have no repeated elements.
; eg.  (myCommon ‘(p a e g) ‘(a q r e))  →  (a e)
(defun myCommon(L1 L2)
  (if(OR (eq L1 '()) (eq L2 '()))
    ()
    (if (myMember (car L1) L2)
      (cons (car L1) (myCommon (cdr L1) L2))
      (myMember (car L1) L2)
    )
  )
)


; Given integers X and Y, evaluates to the list of increasing integers between 
;X and Y inclusive. 
;( or to  nil  if such list does not exist )
; eg.  (myGen 3 11)  →  (3 4 5 6 7 8 9 10 11)
; eg.  (myGen 4 4)  →  (4)
; eg.  (myGen 11 3)  →  ()
(defun myGen(x y)
  (if (eq x y)
    (cons x ())
    (if(> x y)
      ()
      (cons x (myGen (+ x 1) y))
    )
  )
)


; Evaluates to the list which results from applying function F to
;every element of list L.
; eg.  (myMap (lambda (x) (* 2 x)) ‘(1 2 3 4) )  →  (2 4 6 8)
(defun myMap(F L)
  (if (eq L '())
    ()
    (cons (funcall F (car L)) (myMap F (cdr L)))
  )
)


(defun mySize(L)
  (if(eq L '())
    0
    (+ 1 (mySize(cdr L)))
  )
)


; Evaluates to the the results of applying aggregate function F
; to the elements of L.
; L will be of size >= 2.
; F will be a commutative function.
; eg.  (myReduce (lambda (x y) (+ x y)) ‘(1 2 3 4 5))  →  15
(defun myReduce(F L)
  (if(eq (funcall #'mySize L) 2)
    (funcall F (car L)(car (cdr L)))
    (funcall F (car L) (myReduce F (cdr L)))
  )
)
 
 
;///////////////////////////////////////////////////////////////////////////////
(defvar testList0 (list '()))
(defvar testList1 (list 'a 'b 'c 'd 'e 'f))
(defvar testList2 (list (list 'a 'b 'c) (list 'a 'e 'f) (list 'a 'g 'h 'i)))
(defvar testList3 (list 'p 'a 'c 'e 'p 'c))
(defvar testList4 (list 'a 'b 'c 'd 'e 'f 'g))
(defvar testList5 (list 'p 'a 'c 'e 'p 'c))
(defvar testList6 (list 'a 'a 'a 'a 'a 'a))
(defvar testList7 (list 'a 'a 'a 'a 'a 'a 'a (list 'a 'b 'c)))
(defvar testList8 (list 'p 'c 'e 'p 'c))
(defvar testList9 (list 'b 'b 'b 'b 'b 'b (list 'a 'b 'c)))
(defvar testList10 (list (list 'a 'b 'c) (list 'a 'b 'c) (list 'a 'g 'h 'i)))

;test for myLast()
; (print (funcall #'myLast testList0)) ;return ()
; (print (funcall #'myLast testList1)) ;return F
; (print (funcall #'myLast testList2)) ;return (G H I)


;test for myCount()
; (print (funcall #'myCount 'a ()))        ;return 0
; (print (funcall #'myCount 'a testList1)) ;return 1
; (print (funcall #'myCount 'a testList2)) ;return 3
; (print (funcall #'myCount 'a testList6)) ;return 6
; (print (funcall #'myCount 'a testList7)) ;return 7


;test for myMember()
; (print (funcall #'myMember 'a ()))        ;return false
; (print (funcall #'myMember 'a testList1)) ;return true
; (print (funcall #'myMember 'a testList2)) ;return true
; (print (funcall #'myMember 'a testList6)) ;return true
; (print (funcall #'myMember 'a testList8)) ;return false
; (print (funcall #'myMember 'a testList9)) ;return true


(print"///////////////////////////////////////////////////////////////////////")
;test for myPurge()
(and (print "0")  (print (funcall #'myPurge testList0)))
(and (print "1")  (print (funcall #'myPurge testList1)))
(and (print "2")  (print (funcall #'myPurge testList2)))
(and (print "3")  (print (funcall #'myPurge testList3)))
(and (print "4")  (print (funcall #'myPurge testList4)))
(and (print "5")  (print (funcall #'myPurge testList5)))
(and (print "6")  (print (funcall #'myPurge testList6)))
(and (print "7")  (print (funcall #'myPurge testList7)))
(and (print "8")  (print (funcall #'myPurge testList8)))
(and (print "9")  (print (funcall #'myPurge testList9)))
(and (print "10") (print (funcall #'myPurge testList10)))
(print"///////////////////////////////////////////////////////////////////////")















; Evaluates to a list with all elements of  L without repetition.
; eg.  (myPurge ‘(p a c e p c))  →  (a e p c)
; (defun myPurge(L)
;   (if (eq L '())
;     L
;     (if (= (funcall #'myCount (car L) L) 1)
;       (cons (car L) (myPurge(cdr L)))
;       (myPurge(cdr L))
;     )
;   )
; )



