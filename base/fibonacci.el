;; Fibonacci sequence
(defun fib (n)
  "A simple recursive function for the Fibonacci sequence"
  (if (< n 2) ;; if the number is less than two return n for fib(1) = 1
      n
    (+ (fib (- n 1)) (fib (- n 2)))))

;; prints the element at position n fib(6) = 8
(fib 6)

