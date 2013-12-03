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

(define Vals:Vals)
(Vals "vals" '(("root" (("band" "The Grateful Dead")
                        ("goodyears" ((("year" "1969"))
                                      (("year" "1977"))
                                      (("year" "1981"))))
                        ("members" ((("name" "Jerry") ("instrument" "guitar"))
                                    (("name" "Phil")  ("instrument" "bass"))
                                    (("name" "Pig Pen") ("instrument" "harmonica"))))
                        ))))

(println (join (mustache:apply-template parsed (list (Vals "vals")))
               ""))

(define template "The band {{band}} has members {{#members}}{{name}} on {{instrument}} {{/members}}, and I like them.")
(define tags (mustache:next-tag template (list)))
(define parsed (mustache:nest-sections
                tags
                (list (mustache:mk-tag "section" "root" (list)))))
(println (join 
          (mustache:apply-template parsed (list (Vals "vals")))
          ""))

(define template "The band {{band}} has members {{#members}}{{name}} on {{instrument}} {{/members}}. Their good years were {{#goodyears}}{{year}} {{/goodyears}}.")
(define tags (mustache:next-tag template (list)))
(define parsed (mustache:nest-sections
                tags
                (list (mustache:mk-tag "section" "root" (list)))))
(println (join (mustache:apply-template parsed (list (Vals "vals")))
               ""))

(exit)
