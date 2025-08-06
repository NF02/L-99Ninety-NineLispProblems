;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; P10 (*) Run-length encoding of a list.
;;    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
;;    Example:
;;    * (encode '(a a a a b c c a a d e e e e))
;;    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

;; refer to p09
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

;; p10 ex
(defun my-encode (l)
  (defun my-encode-rec (l res)
    (if (null l)
	(reverse res) ;; ((4 e) (1 d) (2 a) (2 c) (1 b) (4 a)) -> ((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))
      (my-encode-rec (cdr l)
		     (cons
		      (list (length (car l)) (caar l)) ;; the caar is a car of the car, the car shows the first
		      ;; element of the list in the same way as the cdr and cddr which show the last element of
		      ;; the list and the last element of the sublist.
		      res))))
  (my-encode-rec (my-pack l) nil))

(my-encode '(a a a a b c c a a d e e e e)) ;; ((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))
