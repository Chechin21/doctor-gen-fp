#lang scheme/base
(require scheme/list)
(require scheme/math)
(require compiler/find-exe)

;(println #f)
(define (cmd)  
  (call-with-input-file (car (vector->list (current-command-line-arguments)));чтение входа из командной строки
  ;(call-with-input-file "/Users/ivan/Desktop/fp/path_tests_6.in"
    (lambda (in)
      (list (read in) (read in)))))

(define edges (list->vector (caar (list (cmd))))); граф
(define max-colors (cadar (list (cmd)))); количество цветов
;(println edges)
;(println max-colors)

;(define edges (list->vector (read)))
;(define max-colors (read))
(define len (vector-length edges))

(define (gather-points i ht)
  (cond ((< i 0) ht)
        (else (let ((edge (vector-ref edges i)))
                (gather-points (- i 1) (hash-set* ht (car edge) '() (cadr edge) '()))))))

(define points (list->vector (sort (hash-keys (gather-points (sub1 len) (make-immutable-hash)))
                                   string<? #:key symbol->string)))

(define n_points (vector-length points));количество вершин

(define max_generations 300)
(define population_size 100)
(define mutation_probability 0.6)
(define mutation_severity 0.05)
(define cross_probability 0.4)
(define crossing_quota (exact-round (* 0.4 population_size)))
(define selection_quota (exact-round (* 0.4 population_size)))
(define keep_quota (max 2 (exact-round (* 0.1 population_size))))



(define (random-specimen colors)
  (build-vector n_points (lambda (i) (+ (random colors) 1))))

(define (random-population colors)
  (build-vector population_size (lambda (i) (random-specimen colors))))

(define (get-score specimen);функция оценки - количетсво конфликтов для входной раскраски 
  (define (get_s spec i table ans)
    (cond ((< i 0) ans)
          (else (let* ((first (hash-ref table (car (vector-ref edges i)) 0))
                       (second (hash-ref table (cadr (vector-ref edges i)) 0)))
                  (cond ((equal? first second) (get_s spec (sub1 i) table (add1 ans)))
                        (else (get_s spec (sub1 i) table ans)))))))
  (get_s specimen  (sub1 len) (make-immutable-hash (map (lambda (x y) (cons x y)) (vector->list points) (vector->list specimen))) 0))

                   
(define (build-scores population)
  (build-vector (vector-length population)
                (lambda (i) (get-score (vector-ref population i)))))


(define (cross-specimen a b)
    (let ((cross_point (random n_points)))
        (build-vector n_points (lambda (i)
                (if (< i cross_point)
                    (vector-ref b i)
                    (vector-ref a i))))))

(define (mutate-specimen colors specimen)
  (build-vector n_points (lambda (i)
                           (cond ((< (random) mutation_severity)
                                  (+ 1 (random colors)))
                                 (else (vector-ref specimen i))))))

(define (find-best scores)
  (define (find-best_ scores i j)
  (cond ((<= i 0) j)
        (else (find-best_ scores (- i 1)
                          (if (< (vector-ref scores i) (vector-ref scores j)) i j)))))
  (find-best_ scores (- (vector-length scores) 1) 0))



(define (next-generation col population old_best remaining_generations)
  (let*((scores (build-scores population))
        (best (find-best scores))
        (bestscore (vector-ref scores best)))

    
    ;(print bestscore) (print  "   "  ) 
    ;(print (quotient (foldl + 0 (vector->list scores)) (vector-length scores))) (print "   ")
    ;(println col)
    (cond((<= remaining_generations 0)                                            
           (if (= 0 bestscore)
               (vector-ref population best)
               '()))
          ((> bestscore 0)

           (let* ((sorted
                   (list->vector (sort (range (vector-length population)) <
                                       #:key (lambda (i) (vector-ref scores i)))));сортировка по оценочной функции по возрастанию(т.е. лучшие особи на первых местах)
                  (next (build-vector population_size (lambda (i); новая популяция
                                                      (let((chromosome
                                                            (cond ((< i keep_quota)           
                                                                   (vector-ref population (vector-ref sorted i)))
                                                                  ((< (random) cross_probability);скрещивание    
                                                                   (cross-specimen
                                                                    (vector-ref population (vector-ref sorted (random crossing_quota)))
                                                                    (vector-ref population (vector-ref sorted (random crossing_quota)))))
                                                                  (else                               
                                                                   (vector-ref population (vector-ref sorted (random selection_quota)))))))                                               
                                                        (if (and (>= i keep_quota) (< (random) mutation_probability));мутация
                                                            (mutate-specimen col chromosome)
                                                            chromosome))))))
            
             ;(write bestscore) (write '()) (write (quotient (foldl + 0 (vector->list scores)) (vector-length scores))) (write'()) (write col)(newline) ;(write scores)(newline)
             (if (< bestscore old_best)
                  (next-generation col next bestscore max_generations)        ;сброс счетчика
                  (next-generation col next old_best (- remaining_generations 1)))))

         (else (vector-ref population best)))));)

(define (genetic colors)
    (next-generation colors (random-population colors) +inf.0 max_generations))

(define (gen-step colors prev_answer)
    (let*((answer (genetic colors)))
        (if (null? answer)
            (cons #t prev_answer)
            (gen-step (- colors 1) (cons colors answer)))))


(define geller ( quotient (* n_points n_points) ( - (* n_points n_points) (* 2 len))))

(define (main)
  (if (>= max-colors n_points)
      (bin-step   geller n_points (cons n_points (genetic n_points)))
      ;(gen-step n_points (cons n_points (genetic n_points)))
      (let ((answer (genetic max-colors)))
        (if (null? answer)
            '(#f)
            (bin-step geller max-colors (cons max-colors answer))))))
            ;(gen-step (- max-colors 1) (cons max-colors answer))))))

(define (bin-step min_colors max-colors upper_answer)
  (if (>=   min_colors max-colors)
      (cons #t upper_answer)
      (let* ((mid-colors (quotient (+ min_colors max-colors) 2))
             (answer (genetic mid-colors)))
        ;(write mid-colors)
        (if (null? answer)
            (bin-step (+ 1 mid-colors) max-colors upper_answer)
            (bin-step min_colors mid-colors (cons mid-colors answer))))))


(cond ((equal? edges #() )
       (write "Пустой граф")(newline))
      ((<= max-colors 0)
       (write "Ошибка цветов")(newline))
      (else
       (let ((answer (main)))
         (write (car answer))(newline)
         (cond ((car answer)
                (write (cadr answer))(newline)
                (write (build-list n_points (lambda (i) (list (vector-ref points i) (vector-ref (cddr answer) i)))))(newline))))))
