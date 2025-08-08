(defun huffman (frequencies)
  "Generate Huffman codes for symbols with given frequencies.
   FREQUENCIES is a list of (SYMBOL FREQ) pairs.
   Returns a list of (SYMBOL CODE) pairs."
  (let ((tree (build-huffman-tree frequencies)))
    (generate-codes tree)))

(defun build-huffman-tree (nodes)
  "Build Huffman tree from list of NODES (symbol frequency pairs)."
  (if (= (length nodes) 1)
      (car nodes)
    (let* ((sorted (sort (copy-sequence nodes) 
                         (lambda (a b) (< (cadr a) (cadr b)))))
           (left (pop sorted))
           (right (pop sorted))
           (new-node (list (intern (format "NODE-%d" (+ (cadr left) (cadr right))))
                              (+ (cadr left) (cadr right))
                              left right)))
      (build-huffman-tree (cons new-node sorted)))))

(defun generate-codes (tree &optional path codes)
  "Generate Huffman codes from TREE, accumulating PATH and CODES."
  (cond
   ;; Leaf node - symbol case
   ((and (consp tree) (symbolp (car tree)) (numberp (cadr tree)) (null (cddr tree)))
    (cons (list (car tree) (apply 'string (reverse path))) codes))
   ;; Internal node case
   ((consp tree)
    (let ((left-codes (generate-codes (caddr tree) (cons ?0 path) codes))
          (right-codes (generate-codes (cadddr tree) (cons ?1 path) codes)))
      (append left-codes right-codes)))
   (t codes)))

;; Example usage:
(huffman '((a 45) (b 13) (c 12) (d 16) (e 9) (f 5))) ;; => ((a "0") (c "100") (b "101") (f "1100") (e "1101") (d "111"))
