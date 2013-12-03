#!/usr/local/bin/newlisp

(load "mustache.lsp")
(load "lib/nl-unittest.lsp")

(context 'MAIN)

(println)

;; -------------- test tag identification
(define-test (test_find-tag)
  (assert= (mustache:find-tag-type "{{foo}}")
           '((mustache:type "escaped") (mustache:key "foo") (mustache:content nil)))
  (assert= (mustache:find-tag-type "{{#foo}}")
           '((mustache:type "section") (mustache:key "foo") (mustache:content nil)))
  (assert= (mustache:find-tag-type "{{&foo}}")
           '((mustache:type "unescaped") (mustache:key "foo") (mustache:content nil)))
  (assert= (mustache:find-tag-type "{{!foo}}")
           '((mustache:type "comment") (mustache:key "foo") (mustache:content nil)))
  (assert= (mustache:find-tag-type "{{>foo}}")
           '((mustache:type "partial") (mustache:key "foo") (mustache:content nil)))
  (assert= (mustache:find-tag-type "{{^foo}}")
           '((mustache:type "inverted") (mustache:key "foo") (mustache:content nil)))
  (assert= (mustache:find-tag-type "{{/foo}}")
           '((mustache:type "close") (mustache:key "foo") (mustache:content nil)))
  (assert= (mustache:find-tag-type "{{ foo}}")
           '((mustache:type "escaped") (mustache:key "foo") (mustache:content nil)))
  (assert= (mustache:find-tag-type "{{ foo }}")
           '((mustache:type "escaped") (mustache:key "foo") (mustache:content nil)))
  (assert= (mustache:find-tag-type "{{foo }}")
           '((mustache:type "escaped") (mustache:key "foo") (mustache:content nil)))
  (assert= (mustache:find-tag-type "{{{foo}}}")
           '((mustache:type "unescaped") (mustache:key "foo") (mustache:content nil))))
;; -------------- /test tag identification


;; -------------- test tokenization
(define template "The band {{band}} has members {{#members}}{{name}} on {{instrument}}{{/members}}, and I like them.")
(define tags (mustache:next-tag template (list)))
(define result (mustache:nest-sections tags (list (mustache:mk-tag "section" "root" (list)))))

(define-test (test_tokens)
  (assert= tags
           '(((mustache:type "text") (mustache:key nil) (mustache:content "The band "))
             ((mustache:type "escaped") (mustache:key "band") (mustache:content nil))
             ((mustache:type "text") (mustache:key nil) (mustache:content " has members "))
             ((mustache:type "section") (mustache:key "members") (mustache:content nil))
             ((mustache:type "escaped") (mustache:key "name") (mustache:content nil))
             ((mustache:type "text") (mustache:key nil) (mustache:content " on "))
             ((mustache:type "escaped") (mustache:key "instrument") (mustache:content nil))
             ((mustache:type "close") (mustache:key "members") (mustache:content nil))
             ((mustache:type "text") (mustache:key nil) (mustache:content ", and I like them.")))))
;; -------------- /test tokenization

;; -------------- test parsing from tokens
(define-test (test_parse)
  (assert= result
           '(((mustache:type "section")
             (mustache:key "root")
             (mustache:content
              (((mustache:type "text")
                (mustache:key nil)
                (mustache:content "The band "))
               ((mustache:type "escaped")
                (mustache:key "band")
                (mustache:content nil))
               ((mustache:type "text")
                (mustache:key nil)
                (mustache:content " has members "))
               ((mustache:type "section")
                (mustache:key "members")
                (mustache:content
                 (((mustache:type "escaped")
                   (mustache:key "name")
                   (mustache:content nil))
                  ((mustache:type "text")
                   (mustache:key nil)
                   (mustache:content " on "))
                  ((mustache:type "escaped")
                   (mustache:key "instrument")
                   (mustache:content nil)))))
               ((mustache:type "text")
                (mustache:key nil)
                (mustache:content ", and I like them."))))))))
;; -------------- /test parsing


;; -------------- test basic application
(set 'vals '(("root" (("band" "The Grateful Dead")
                      ("goodyears" ((("year" "1969"))
                                    (("year" "1977"))
                                    (("year" "1981"))))
                      ("members" ((("name" "Jerry") ("instrument" "guitar"))
                                  (("name" "Phil")  ("instrument" "bass"))
                                  (("name" "Pig Pen") ("instrument" "harmonica"))))
                      ))))

(define apply-template "The band {{band}} has members {{#members}}{{name}} on {{instrument}} {{/members}}. Their good years were {{#goodyears}}{{year}} {{/goodyears}}.")
(define apply-tags (mustache:next-tag apply-template (list)))
(define apply-parsed (mustache:nest-sections
                      apply-tags
                      (list (mustache:mk-tag "section" "root" (list)))))
(define apply-result
  (join (mustache:apply-template apply-parsed (list vals))
        ""))

(define-test (test_apply)
  (assert= apply-result
           "The band The Grateful Dead has members Jerry on guitar Phil on bass Pig Pen on harmonica . Their good years were 1969 1977 1981 ."))
;; -------------- /test basic application


(UnitTest:run-all 'MAIN)
(println)
(exit)
