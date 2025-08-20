(defun istree (expr)
  ""
  (cond
    ((null expr) t) ; nil is a valid tree
    ((not (listp expr)) nil) ; if not a list, not a tree
    ((/= (length expr) 3) nil) ; if list length is not 3, not a tree
    (t (and (istree (cadr expr)) (istree (caddr expr)))))) ; check left and right subtrees

;; example usage
(istree '(a (b nil nil) nil)) ;; => t
(istree '(a (b nil nil))) ;; => nil
