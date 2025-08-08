;; Define a hash table to cache computed Gray codes for efficiency
(defvar gray-cache (make-hash-table :test 'equal))

(defun gray (n)
  "Generate the n-bit Gray code."
  (or (gethash n gray-cache)
      (let ((result (if (= n 1)
                       '("0" "1")
                     (let* ((prev (gray (1- n))))
                            (append (mapcar (lambda (s) (concat "0" s)) prev)
                                  (mapcar (lambda (s) (concat "1" s)) (reverse prev)))))))
        (puthash n result gray-cache)
        result)))

;; example usage:
(gray 1) ;; => ("0" "1")
(gray 2) ;; => ("00" "01" "11" "10").
(gray 3) ;; => ("000" "001" "011" "010" "110" "111" "101" "100")
