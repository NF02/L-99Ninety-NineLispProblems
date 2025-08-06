;; P11 (*) Modified run-length encoding.
;;    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
;;    Example:
;;    * (encode-modified '(a a a a b c c a a d e e e e))
;;    ((4 A) B (2 C) (2 A) D (4 E))
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

(defun my-encode-modified (l)
  (defun my-encode-modified-rec (l res)
    (if (null l)
	(reverse res)
      (let* ((len (length (car l)))
	     (new_elem (if (eq len 1)
			   (caar l)
			 (list len (caar l)))))
	(my-encode-modified-rec (cdr l)
				(cons new_elem res)))))
	(my-encode-modified-rec (my-pack l) nil))

(my-encode-modified '(a a a a b c c a a d e e e e))
