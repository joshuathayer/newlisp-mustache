#!/usr/local/bin/newlisp

(load "mustache.lsp")
(load "lib/nl-unittest.lsp")

(context 'MAIN)

(println)

(define template "The band name is {{band}}.")
(define tags (mustache:next-tag template (list)))
(define parsed (mustache:nest-sections
                tags
                (list (mustache:mk-tag "section" "root" (list)))))

(set 'vals '(("root" (("band" "The Grateful Dead")
                      ("goodyears" ((("year" "1969"))
                                    (("year" "1977"))
                                    (("year" "1981"))))
                      ("members" ((("name" "Jerry") ("instrument" "guitar"))
                                  (("name" "Phil")  ("instrument" "bass"))
                                  (("name" "Pig Pen") ("instrument" "harmonica"))))
                      ))))


(println (join (mustache:apply-template parsed (list vals)) ""))

(define template "The band {{band}} has members {{#members}}{{name}} on {{instrument}} {{/members}}, and I like them.")
(define tags (mustache:next-tag template (list)))
(define parsed (mustache:nest-sections
                tags
                (list (mustache:mk-tag "section" "root" (list)))))
(println (join (mustache:apply-template parsed (list vals)) ""))

(define template "The band {{band}} has members {{#members}}{{name}} on {{instrument}} {{/members}}. Their good years were {{#goodyears}}{{year}} {{/goodyears}}.")
(define tags (mustache:next-tag template (list)))
(define parsed (mustache:nest-sections
                tags
                (list (mustache:mk-tag "section" "root" (list)))))
(println (join (mustache:apply-template parsed (list vals)) ""))

;; make a context to hold templates
(define Templates:Templates)

  
;; given a template string, tokenize and parse it and put it in a context         
(mustache:add-template Templates "band" template )
(mustache:add-template Templates "uncool" "The band {{band}} is terribly uncool!")

(println (mustache:run-template Templates "band"
                                '(("band" "The Grateful Dead")
                                  ("goodyears" ((("year" "1969"))
                                                (("year" "1977"))
                                                (("year" "1981"))))
                                  ("members" ((("name" "Jerry") ("instrument" "guitar"))
                                              (("name" "Phil")  ("instrument" "bass"))
                                              (("name" "Pig Pen") ("instrument" "harmonica"))))
                                  )))

(set 'uncool-vals '(("band" "Nickleback")))
(mustache:add-template Templates "uncool" "The band {{band}} is terribly uncool!")
(println (mustache:run-template Templates "uncool" uncool-vals))

(exit)
