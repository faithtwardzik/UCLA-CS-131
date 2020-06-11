#lang racket

(define (lambda? x)
  (member x '(lambda λ)))

(define (NE x y)
  (list 'if '% x y))

(define (zip-map l1 l2 map rev)
    (cond 
        [(or (empty? l1) (empty? l2)) map]
        [else
            (let*
                ([a (first l1)]
                 [b (first l2)]
                 [a-str (symbol->string a)]
                 [b-str (symbol->string b)]
                 [concatenated_symbol (if rev (string->symbol (string-append b-str "!" a-str)) (string->symbol (string-append a-str "!" b-str)))]
                 [ab (if (equal? a b) a concatenated_symbol) ]
                 [new-map (hash-set map a ab)]
                )
                (zip-map (rest l1) (rest l2) new-map rev)
            )
        ]
    )
)

(define (hash-no-key? map key) (not (hash-has-key? map key)))

(define (reset-to-identity-map existing-map keys-to-reset)
    (cond 
    [(list? keys-to-reset) 
        (if (empty? keys-to-reset)
            existing-map
            (let* 
                ([first_key (first keys-to-reset)]
                 [new-map 
                    (if (hash-has-key? existing-map first_key)
                        (hash-set existing-map first_key first_key)
                        existing-map
                    )
                 ]
                )
                (reset-to-identity-map new-map (rest keys-to-reset))
            )
        )
    ]
    [else 
        ;; if keys-to-reset is not a list we wrap it in a list
        (let 
            ([keys (list keys-to-reset)])
            (reset-to-identity-map existing-map keys)
        )
    ]
    )
)

;; switched the order of the argument for consistency with transform-identifier-recursively
(define (transform-params param-mapping formal-params)
    (let 
        ([transform 
            (lambda (param) 
                (if (hash-has-key? param-mapping param) 
                    (hash-ref param-mapping param) 
                    param
                )
            )
         ]
        )
        (if (list? formal-params)
            (map transform formal-params)
            (transform formal-params)
        )
    )
)

(define (transform-identifier-rec mapping expr)
    (if (and (list? expr) (not (empty? expr)))
        (let*
            ([first_sym (first expr)]
             [is_lambda (and (lambda? first_sym) (equal? 3 (length expr)))]
             [lambda_in_map (or (hash-has-key? mapping 'lambda) (hash-has-key? mapping 'λ))]
            )
            (if (and is_lambda (not lambda_in_map))
                ;; formal params should overwrite the existing map keys
                (let*
                    ([formal_params (first (rest expr))]
                     [new_map (reset-to-identity-map mapping formal_params)]
                    )
                    (cons first_sym 
                        (map (curry transform-identifier-rec new_map) (rest expr))
                    )
                )
                ;; otherwise either the expr is not a lambda 
                ;; or the lambda has been overshadowed to become a normal identifier
                (map (curry transform-identifier-rec mapping) expr)
            )
        )
        (transform-params mapping expr)
    )
)


;need two maps, one for first program, one for second
(define (expr-compare-hash map1 map2 x y)
    (let 
    ([transformed_x (transform-identifier-rec map1 x)]
     [transformed_y (transform-identifier-rec map2 y)]
    )
    (cond
        [(equal? transformed_x transformed_y) transformed_x]
        [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
        [(or (not (list? x)) (not (list? y))) (NE transformed_x transformed_y)]
        [(or (> (length x) (length y)) (< (length x) (length y))) (NE transformed_x transformed_y)]
        [else 
            (let*
            ([first_x (first transformed_x)]
             [first_y (first transformed_y)]
             [x_is_if (equal? 'if first_x)]
             [y_is_if (equal? 'if first_y)]
             [x_or_y_is_if (or x_is_if y_is_if)]
             [x_y_both_if (and x_is_if y_is_if)]
             [x_is_lambda (equal? 'lambda first_x)]
             [y_is_lambda (equal? 'lambda first_y)]
             [x_is_greek_lambda (equal? 'λ first_x)]
             [y_is_greek_lambda (equal? 'λ first_y)]
             [x_not_y_lambdas (and (lambda? first_x) (not (lambda? first_y)))]
             [y_not_x_lambdas (and (lambda? first_y) (not (lambda? first_x)))]
             [x_y_two_lambda_flavors (or (and x_is_greek_lambda y_is_lambda) (and x_is_lambda y_is_greek_lambda))]
             [both_greek_lambdas (and x_is_greek_lambda y_is_greek_lambda)]
             [both_english_lambdas (and x_is_lambda y_is_lambda)]
             [first_x_in_map1 (hash-has-key? map1 first_x)]
             [first_y_in_map2 (hash-has-key? map2 first_y)]
            )
            (cond
	            [(or (equal? 'quote first_x) (equal? 'quote first_y)) (NE transformed_x transformed_y)]
                [x_or_y_is_if 
                    (if x_y_both_if
                        (cons 'if (expr-compare-hash map1 map2 (rest x) (rest y)))
                        (NE transformed_x transformed_y)
                    )
                ]
                [(or x_y_two_lambda_flavors both_greek_lambdas both_english_lambdas)
                    (cond 
                        [(or (empty? (rest x)) (empty? (rest y)))
                            (cons (expr-compare-hash map1 map2 first_x first_y) (expr-compare-hash map1 map2 (rest x) (rest y)))]
                        [(and (not (equal? (length (first (rest x))) (length (first (rest y))))))
                            (NE (cons first_x (cons (first (rest x)) (list (first (rest (rest x))))))
                            (cons first_y (cons (first (rest y)) (list (first (rest (rest y)))))))]
                        [else 
                            (let*
                            ([x_formal_params (first (rest x))]
                             [y_formal_params (first (rest y))]
                             [zmap1 (zip-map x_formal_params y_formal_params map1 #f)]
                             [zmap2 (zip-map y_formal_params x_formal_params map2 #t)]
                             [lambda_symbol (if (or x_y_two_lambda_flavors both_greek_lambdas) 'λ 'lambda)]
                             [transformed_formal_params (transform-params zmap1 x_formal_params)]
                             [x_body (rest (rest x))]
                             [y_body (rest (rest y))]
                            )
                            (cons lambda_symbol 
                                    (cons transformed_formal_params
                                        (expr-compare-hash zmap1 zmap2 x_body y_body)
                                    )
                                )
                            )
                        ]
                    )
                ]
                ;; could've reduced code duplication for these two cases, but probably not worth it for this homework...
                [x_not_y_lambdas 
                    (let* 
                        ([x_formal_params (first (rest x))]
                         [new_map1 (reset-to-identity-map map1 x_formal_params)]
                         [new_transformed_rest_x (transform-params new_map1 (rest x))]
                        )
                        (NE (cons first_x new_transformed_rest_x) transformed_y)
                    )
                ]
                [y_not_x_lambdas
                      (let* 
                        ([y_formal_params (first (rest y))]
                         [new_map2 (reset-to-identity-map map2 y_formal_params)]
                         [new_transformed_rest_y (transform-params new_map2 (rest y))]
                        )
                        (NE transformed_x (cons first_y new_transformed_rest_y))
                    )
                ]
	            ; recursive call, append result of first elements compared to result of rest of elements compared
            	[else (map (curry expr-compare-hash map1 map2) x y)]
            )
            )
        ]
    )
    )
)

(define (expr-compare x y)
  (define map1 (make-immutable-hash))
  (define map2 (make-immutable-hash))
  (expr-compare-hash map1 map2 x y)
)

; from the hint code
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

; uses constant literals, variables, procedure calls, quote, lambda, if, and more
(define test-expr-x '(cons "hello" (cons ''(if a b c) (cons (if #f #t 265) (cons ((lambda (b) (* b 1)) (quote 2)) (let ((a 479) (b "never_free")) '(b a)))))))
(define test-expr-y  '(cons "not_hello" (cons ''(if b c d) (cons (if #t #f 265) (cons ((λ (a) (+ a a)) (quote 3)) (let ((a 469) (b "free")) '(a b)))))))