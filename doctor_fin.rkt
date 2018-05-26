#lang scheme/base
(define (fifty-fifty)
  (= (random 2) 0))

(define (prob n1 n2)
  (< (random n2) n1))

(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (many-replace replacement-pairs lst)
        (if (null? replacement-pairs)
            lst
            (map (lambda (x) (replace replacement-pairs x)) lst)))

(define (change-person phrase)
        (many-replace '((i you) (me you) (am are) (my your) (are am) (you i) (your my)) phrase))

(define (replace replacement-pairs lst)
  (cond ((null? replacement-pairs) lst)
        ((equal?  lst (caar replacement-pairs))
         (cadar replacement-pairs))
        (else
         (replace (cdr replacement-pairs) lst))))

(define (hedge)
        (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       (it's a common problem)
                       (don't worry,what esle)
                       (unusual situation)
                       (it's normal to worry about that)
                       (tell me more)
                       (don't be shy))))

(define (qualifier)
        (pick-random '((you seem to think)
                       (you feel that)
                       (why do you believe)
                       (why do you say)
                       (why do you think)
                       (you seem to be sure that)
                       (are you sure that)
                       (you want to tell me that))))

(define keys '(((depressed suicide)
                  ((when you feel depressed, go out for ice cream)
                   (depression is a disease that can be treated)))
                 ((mother father parents)
                  ((tell me more about your *)
                   (why do you feel that way about your *)))))

(define (contains lst keyword) (ormap (lambda (x) (equal? x keyword)) lst))

(define (has-keywords lst dict)
  (define (has-group group) (ormap (lambda (x) (contains lst x)) (car group)))
  (ormap has-group keys))

(define (reply-keyword lst dict)
  (define (find keywords)
    (let ((match (filter (lambda (x) (contains lst x)) (car keywords))))
      (if (null? match) #f
          (list (pick-random match) (cadr keywords)))))
  (let ((fin (pick-random (filter (lambda (x) (not (eq? x #f))) (map find keys)))))
    (many-replace (list(list '* (car fin))) (pick-random (cadr fin)))))

(define (has-dict lst dict) (not (null? dict)))

(define (reply-dict lst dict)
  (append '(earlier you said that) (change-person (list-ref dict (random (length dict))))))

(define (true-func str history) #t)

(define (possible-replies lst dict)
  ;(let ((key (has-keywords lst))
  ;      (dic (has-dict dict)))
    (list
     (list 1 has-keywords reply-keyword)
     (list 1 has-dict reply-dict)
     (list 1 true-func
           (lambda (lst dict) (append (qualifier) (change-person lst)))
           )
     (list 1 true-func (lambda (lst dict) (hedge)))))

(define (weighted-random lst)
  (define (choise prob lst)
    (cond
      ((null? lst) #f)
      ((< prob (caar lst)) (cdar lst))
      (else (choise (- prob (caar lst)) (cdr lst)))))
  
  (let ((sum (foldl (lambda (x acc) (+ acc (car x))) 0 lst)))
    (choise (* sum (random)) lst)))
  
(define (reply lst dict)
  (let ((correct (filter (lambda (x) ((cadr x) lst dict))  (possible-replies lst dict))))
    ;(print correct)
    ((cadr (weighted-random correct)) lst dict)))

(define (visit-doctor)
  (println'(Who is next?))
  (define name (read))
  (define (doctor-driver-loop name dict)
    (newline)
    (print '**)
    (let ((user-response (read)))
      (cond ((equal? user-response '(goodbye))
             (printf "Goodbye, ~a!\n" name)
             (println '(see you next week))
             (visit-doctor))
            (else (print (reply user-response dict))
                  (doctor-driver-loop name (cons user-response dict))))))
  (cond((equal? name 'supertime)
        (println'(Time is over)))
       (else (printf "Hello, ~a!\n" name)
             (print '(what seems to be the trouble?))
             (doctor-driver-loop name '()))))




(visit-doctor)