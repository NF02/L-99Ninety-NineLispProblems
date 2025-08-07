(defun is-prime (n &optional d)
  "Check if N is a prime number (recursive helper)."
  (let ((d (or d 2)))  ; Default d to 2 if not provided
  (cond
   ((<= n 1) nil)                     ; 1 or less is not prime
   ((= n 2) t)                        ; 2 is prime
   ((= (mod n d) 0) nil)              ; divisible by d → not prime
   ((> (* d d) n) t)                  ; no divisor found → prime
   (t (is-prime n (+ d 1))))))         ; check next divisor

(defun primes-in-range (a b)
  "Recursively generate a list of primes in [A, B]."
  (cond
   ((> a b) nil)                      ; base case: empty range
   ((is-prime a)                      ; if A is prime
    (cons a (primes-in-range (+ a 1) b))) ; include A and recurse
   (t (primes-in-range (+ a 1) b))))  ; skip A and recurse

;; Example usage:
(primes-in-range 10 20)  ; => (11 13 17 19)
