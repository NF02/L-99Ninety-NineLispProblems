;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; P08 (**) Eliminate consecutive duplicates of list elements.
;;    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
;;    Example:
;;    * (compress '(a a a a b c c a a d e e e e))
;;    (A B C A D E)

(defun my-compress (l)
  (defun my-compress-iter (l res c)
    (cond ((null l) (reverse res)) 
	  ((eq (car l) c)
	   (my-compress-iter (cdr l) res c))
	  (t
	   (my-compress-iter (cdr l)
			     (cons (car l) res)
			     (car l)))))
  (if (null l) ; empty list
      nil
  (my-compress-iter (cdr l)
		    (list (car l))
		    (car l))))
(my-compress '(a a a a b c c a a d e e e e)) ; (a b c d e)
