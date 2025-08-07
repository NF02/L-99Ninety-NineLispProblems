;; a) Sorting by Sublist Length
(defun lsort (lst)
  "Sort a list of lists by their length in ascending order."
  (sort lst (lambda (a b) (< (length a) (length b)))))

;; Example usage:
(lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o))) ;; => ((o) (d e) (d e) (m n) (a b c) (f g h) (i j k l))

;; b) Sorting by Length Frequency
(defun lfsort (lst)
  "Sort a list of lists by the frequency of their lengths in ascending order."
  (let ((length-freq (make-hash-table :test 'equal)))
    ;; Count frequency of each length
    (dolist (sublist lst)
      (let ((len (length sublist)))
        (puthash len (1+ (or (gethash len length-freq) 0)) length-freq)))
    
    ;; Sort by frequency then by length
    (sort lst (lambda (a b)
                (let ((freq-a (gethash (length a) length-freq))
                      (freq-b (gethash (length b) length-freq)))
                  (if (= freq-a freq-b)
                      (< (length a) (length b))
                    (< freq-a freq-b)))))))
  
;; Example usage:
(lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o))) ;; => ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
