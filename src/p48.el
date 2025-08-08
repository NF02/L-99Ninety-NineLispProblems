;; Define logical operators as functions with * suffix to avoid name conflicts
(defun and* (a b) (and a b))          ; Logical AND
(defun or* (a b) (or a b))            ; Logical OR
(defun nand* (a b) (not (and a b)))   ; Logical NAND
(defun nor* (a b) (not (or a b)))     ; Logical NOR
(defun xor* (a b) (or (and a (not b)) (and (not a) b)))  ; Logical XOR
(defun impl* (a b) (or (not a) b))    ; Logical implication
(defun equ* (a b) (equal a b))        ; Logical equivalence

;; Convert infix expressions to prefix notation (Lisp style)
(defun infix-to-prefix (expr)
  (cond
   ((atom expr) expr)  ; If it's an atom (variable or constant), return as-is
   ((and (listp expr) (eq (car expr) 'not)) ; Handle NOT operator
    (list 'not (infix-to-prefix (cadr expr))))
   ((and (listp expr) (= (length expr) 3))  ; Handle binary operators
    (let ((op (cadr expr)))
      (list (intern (concat (symbol-name op) "*"))  ; Append * to operator name
            (infix-to-prefix (car expr))            ; Convert left operand
            (infix-to-prefix (caddr expr)))))       ; Convert right operand
   (t expr)))  ; For any other case, return the expression as-is

;; Generate all possible combinations of true/nil for given variables
(defun generate-combinations (vars)
  (if (null vars)
      '(())  ; Base case: one combination with no variables
    (let ((rest (generate-combinations (cdr vars))))  ; Recursively get combinations for remaining vars
      (append (mapcar (lambda (comb) (cons t comb)) rest)    ; Prepend true to each combination
              (mapcar (lambda (comb) (cons nil comb)) rest)))))  ; Prepend nil to each combination

  ;; Create variable-value bindings by pairing variables with their values
  (defun zip-vars-values (vars values)
    (if (or (null vars) (null values))
	'()  ; Base case: empty list
      (cons (list (car vars) (car values))  ; Pair first var with first value
            (zip-vars-values (cdr vars) (cdr values)))))  ; Recursively process rest

  ;; Evaluate the expression with given variable values
  (defun evaluate-expr (expr vars values)
    (let ((prefix-expr (infix-to-prefix expr))  ; Convert to prefix notation
          (bindings (zip-vars-values vars values)))  ; Create variable bindings
      (eval `(let ,bindings ,prefix-expr))))  ; Evaluate the expression in this context

  ;; Generate and print a truth table for the expression with given variables
  (defun table (vars expr)
    (let ((combinations (generate-combinations vars)))  ; Get all possible value combinations
      (dolist (combination combinations)  ; For each combination
	(let ((result (evaluate-expr expr vars combination)))  ; Evaluate the expression
          (dolist (val combination)  ; Print each value in the combination
            (princ (if val "true " "nil ")))
          (princ (if result "true\n" "nil\n"))))))  ; Print the result

;; example usage
(table '(A B C) '((A and (B or C)) equ ((A and B) or (A and C)))) ;; => true true true true
                                                                  ;;    true true nil true
                                                                  ;;    true nil true true
                                                                  ;;    true nil nil true
                                                                  ;;    nil true true true
                                                                  ;;    nil true nil true
                                                                  ;;    nil nil true true
                                                                  ;;    nil nil nil true
                                                                  ;;    nil
