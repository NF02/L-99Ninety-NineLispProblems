;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; P12 (**) Decode a run-length encoded list.
;;    Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.

(defun my-flatten (l)
  (cond ((null l) nil)
	((atom l) (list l))
	(t
	 (append (my-flatten (car l))
	         (my-flatten (cdr l))))))

(defun my-decode (l)
  (defun generate-list (c n res)
    (if (eq n 0)
	res
      (generate-list c (- n 1) (cons c res))))
  (defun my-decode-rec (l res)
    (cond ((null l) (my-flatten (reverse res)))
	  ((atom (car l))
	   (my-decode-rec (cdr l)
			  (cons (car l) res)))
	  ;; recursive step
	  (t
	   (my-decode-rec (cdr l)
			  (cons
			   (generate-list (cadar l)
					  (caar l)
					  nil)
			   res)))))
  (my-decode-rec l nil))


(my-decode '((4 A) B (2 C) (2 A) D (4 E))) ;; (A A A A B C C A A D E E E E)
