;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; P09 (**) Pack consecutive duplicates of list elements into sublists.
;;    If a list contains repeated elements they should be placed in separate sublists.
;;    Example:
;;    * (pack '(a a a a b c c a a d e e e e))
;;    ((A A A A) (B) (C C) (A A) (D) (E E E E))

(defun my-pack (l)
  (defun my-pack-iter (l tmp res c)
    (cond ((null l)
	   (reverse (cons tmp res)))
	  ((eq (car l) c)
	   (my-pack-iter (cdr l)
			 (cons c tmp)
			 res
			 c))
	  (t
	   (my-pack-iter (cdr l)
			 (list (car l))
			 (cons tmp res)
			 (car l)))))
  (my-pack-iter (cdr l)
		(list (car l))
		nil
		(car l)))
(my-pack '(a a a a b c c a a d e e e e)) ; '((a a a a) (b) (c c) (a a) (d) (e e e e))

