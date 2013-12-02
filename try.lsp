#!/usr/local/bin/newlisp

(load "mustache.lsp")
(load "lib/nl-unittest.lsp")

(context 'MAIN)

(println)


;;(define template "The band {{band}} has members {{#members}}{{name}} on {{instrument}}{{/members}}, and I like them.")
;;(define tags (mustache:next-tag template (list)))
;;(define parsed (mustache:nest-sections tags (list (mustache:mk-tag "section" "root" (list)))))
(define template "The band name is {{band}}.")
(define tags (mustache:next-tag template (list)))
(define parsed (mustache:nest-sections tags (list (mustache:mk-tag "section" "root" (list)))))
(set 'vals '((band "The Grateful Dead")))


(define (apply-template template vals val-stack)
        (setq ret (list))
	(dolist (chunk template)
	  (if (= (lookup 'mustache:type chunk) "section")
	         (apply-template (lookup 'mustache:content chunk) vals) 
	      (= (lookup 'mustache:type chunk) "text")    (setq ret (cons ret (lookup 'mustache:content chunk)))
	      (= (lookup 'mustache:type chunk) "escaped") (setq ret (cons ret (lookup (sym (lookup 'mustache:key chunk)) vals)))))
 	(join (flat ret) ""))
	
(println (apply-template parsed vals (list)))

(println)

(exit)
