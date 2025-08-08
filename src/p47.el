(defun and* (a b) (and a b))
(defun or* (a b) (or a b))
(defun nand* (a b) (not (and a b)))
(defun nor* (a b) (not (or a b)))
(defun xor* (a b) (or (and a (not b)) (and (not a) b)))
(defun impl* (a b) (or (not a) b))
(defun equ* (a b) (equal a b))

(defun infix-to-prefix (expr)
  (cond
   ((atom expr) expr)
   ((and (listp expr) (= (length expr) 3))
    (let ((op (cadr expr)))
      (list (intern (concat (symbol-name op) "*")) 
            (infix-to-prefix (car expr)) 
            (infix-to-prefix (caddr expr)))))
   ((and (listp expr) (= (length expr) 2) (eq (car expr) 'not))
    (list 'not (infix-to-prefix (cadr expr))))
   (t expr)))

(defun table (var1 var2 expr)
  (let ((prefix-expr (infix-to-prefix expr)))
    (dolist (a '(t nil))
      (dolist (b '(t nil))
        (let ((result (eval `(let ((,var1 ,a) (,var2 ,b)) ,prefix-expr))))
          (message "%s %s %s" 
                   (if a "true" "nil") 
                   (if b "true" "nil") 
                   (if result "true" "nil")))))))
;; example usage
(table 'A 'B '(A and (A or (not B)))) ;; => true true true
                                      ;;    true nil true
                                      ;;    nil true nil
                                      ;;    nil nil nil
                                      ;;    nil
