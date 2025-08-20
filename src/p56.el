(defun mirror? (left right)
  "mirror function"
  (cond
    ((and (null left) (null right)) t)
    ((or (null left) (null right)) nil)
    (t (and (mirror? (cadr left) (caddr right))
            (mirror? (caddr left) (cadr right))))))

(defun symmetric (tree)
  "Symmetric binary trees"
  (if (null tree)
      t
    (mirror? (cadr tree) (caddr tree))))

;; example usage
(symmetric '(x (x nil nil) (x nil nil))) ;; => t
(symmetric '(x (x nil nil)))             ;; => f
