#lang racket/base
(require txexpr sugar/define racket/string) 

(define/contract+provide (make-default-tag-function . ids)
  (() #:rest txexpr-tags? . ->* . procedure?)
  (define (make-one-tag id)
    (λ x 
      (define reversed-pieces ; list of attribute pairs, and last element holds a list of everything else, then reversed
        (reverse (let chomp ([x x])
                   (define result+regexp (and ((length x) . >= . 2) 
                                              (symbol? (car x)) 
                                              ;; accept strings only
                                              ;; numbers are difficult because they don't parse as cleanly.
                                              ;; string will read as a string even if there's no space to the left.
                                              (or (string? (cadr x))) 
                                              ;; Looking for symbol ending with a colon
                                              (regexp-match #rx"^(.*?):$" (symbol->string (car x)))))
                   (if result+regexp
                       ; reuse result value. cadr is first group in match. 
                       (cons (list (string->symbol (cadr result+regexp))(cadr x)) (chomp (cddr x)))
                       (list x)))))
      
      (define-values (body attrs) (if (equal? null reversed-pieces)
                                      (values null null)
                                      (values (car reversed-pieces) (cdr reversed-pieces))))
      
      ;; make this behave similar to ->html, where it's OK to use a list as the last arg (e.g., result of `map`)
      ;; this way, we can avoid having to use `apply` in those cases.
      (define body-unpacked (if (and (= (length body) 1)
                                     (list? (car body))
                                     (not (txexpr? (car body)))
                                     (andmap txexpr-element? (car body)))
                                (car body)
                                body))
      
      `(,id ,@(if (equal? attrs null) null (list (reverse attrs))) ,@body-unpacked)))
  
  (procedure-rename (apply compose1 (map make-one-tag ids)) (string->symbol (format "pollen-tag:~a" (string-join (map symbol->string ids) "+")))))



(define/contract+provide (split-attributes parts)
  (list? . -> . (values txexpr-attrs? txexpr-elements?))
   (define dummy-tag (gensym))
   (define dummy-txexpr (apply (make-default-tag-function dummy-tag) parts))
   (define-values (tag attrs elements) (txexpr->values dummy-txexpr))
   (values attrs elements))