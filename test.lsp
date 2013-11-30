#!/usr/local/bin/newlisp

(load "mustache.lsp")
(load "lib/nl-unittest.lsp")

(context 'MAIN)

(println)

(define template "The band {{band}} has members {{#members}}{{name}} on {{instrument}}{{/members}}, and I like them.")
(define tags (mustache:next_tag template (list)))
(println tags)
(define result (mustache:nest-sections tags (list (mustache:new-section "root" "root"))))
(define-test (test_one)
  (assert= tags
           '(((mustache:type "text") (mustache:key nil) (mustache:content "The band "))
             ((mustache:type "escaped") (mustache:key "band") (mustache:content nil))
             ((mustache:type "text") (mustache:key nil) (mustache:content " has members "))
             ((mustache:type "section") (mustache:key "members") (mustache:content nil))
             ((mustache:type "escaped") (mustache:key "name") (mustache:content nil))
             ((mustache:type "text") (mustache:key nil) (mustache:content " on "))
             ((mustache:type "escaped") (mustache:key "instrument") (mustache:content nil))
             ((mustache:type "close") (mustache:key "members") (mustache:content nil))
             ((mustache:type "text") (mustache:key nil) (mustache:content ", and I like them."))))
  (assert= result
           '(((mustache:section-key "root")
              (mustache:section-type "root")
              (mustache:section-content
               (((mustache:type "text")
                 (mustache:key nil)
                 (mustache:content "The band "))
                ((mustache:type "escaped")
                 (mustache:key "band")
                 (mustache:content nil))
                ((mustache:type "text")
                 (mustache:key nil)
                 (mustache:content " has members "))
                ((mustache:section-key "members")
                 (mustache:section-type "section")
                 (mustache:section-content
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

(UnitTest:run-all 'MAIN)

(println)

;; (setq template "I'd like to make a comment: {{! this is some kind of comment }} also may I insert a partial? {{>whynot}}")
;; (define tags (mustache:next_tag template (list)))
;; (println (mustache:nest-sections tags (list (mustache:new-section "root" "root"))))

(exit)
