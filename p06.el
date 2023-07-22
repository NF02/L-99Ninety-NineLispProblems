;; L-99: Ninety-Nine Lisp Problems
;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; P06 (*) Find out whether a list is a palindrome.
;;    A palindrome can be read forward or backward; e.g. (x a m a x).

;; (a n n a)

(defun my-equal-list (l1 l2)
  (cond ((and (null l1) (null l2))
	 t)
	((or (and (null l1) (not (null l2)))
	     (and (not (null l1)) (null l2)))
	 nil)
	((eq (car l1) (car l2))
	 (my-equal-list (cdr l1) (cdr l2)))
	(t nil)))
(my-equal-list '(a a) '(a a))

(defun my-check-palindrome (l)
  (my-equal-list l (reverse l)))
(my-check-palindrome '(a n n a))
