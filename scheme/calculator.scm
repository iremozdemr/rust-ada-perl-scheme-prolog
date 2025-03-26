(define variables '())

(define (lookup-variable var)
  (let ((pair (assoc var variables)))
    (if pair
        (cdr pair)
        (error "Unbound variable:" var))))

(define (assign-variable var val)
  (let ((pair (assoc var variables)))
    (if pair
        (set-cdr! pair val)
        (set! variables (cons (cons var val) variables)))))

(define (eval expr)
  (cond ((number? expr) expr)
        ((symbol? expr) (lookup-variable expr))
        ((list? expr)
         (case (car expr)
           ((+) (+ (eval (cadr expr)) (eval (caddr expr))))
           ((-) (- (eval (cadr expr)) (eval (caddr expr))))
           ((*) (* (eval (cadr expr)) (eval (caddr expr))))
           ((/) (/ (eval (cadr expr)) (eval (caddr expr))))
           ((set!) (assign-variable (cadr expr) (eval (caddr expr))))
           (else (error "Unknown operator:" (car expr)))))
        (else (error "Invalid expression:" expr))))

(define (calculator)
  (display "Enter an expression (or 'exit'): ")
  (let ((input (read)))
    (if (eq? input 'exit)
        (display "Exiting calculator.\n")
        (begin
          (display "Result: ")
          (display (eval input))
          (newline)
          (calculator)))))