;; L-99: Ninety-Nine Lisp Problems
;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; P07 (**) Flatten a nested list structure.
;; Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
;;    Example:
;;    * (my-flatten '(a (b (c d) e)))
;;    (A B C D E)

(defun my-flatten (l)
  (cond ((null l) nil)
	((atom l) (list l))
	(t
	 (append (my-flatten (car l))
	         (my-flatten (cdr l))))))

(my-flatten '(a (b (c d) e)))
