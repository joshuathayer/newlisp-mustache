(context 'mustache)

(setq otag "{{")
(setq ctag "}}")

(setq otag-len (length otag))
(setq ctag-len (length ctag))

;; make a little associative list to represent tags
(define (mk-tag type key content)
  (list (list 'type    type)
        (list 'key     key)
        (list 'content content)))

;; given some text, determine what kind of tag we're looking at
(define (find-tag-type tag-text)
  (letn (tag-text  (replace "[\ \t]" tag-text "" 1)
         ;; remove opening and closing braces
         tag-text  (otag-len (- 0 ctag-len) tag-text)    
         type_flag (first tag-text))
    (if (= type_flag "#") (mk-tag "section"   (rest tag-text))
        (= type_flag "^") (mk-tag "inverted"  (rest tag-text))
        (= type_flag "/") (mk-tag "close"     (rest tag-text))
        (= type_flag "&") (mk-tag "unescaped" (rest tag-text))
        (= type_flag "!") (mk-tag "comment"   (rest tag-text))
        (= type_flag ">") (mk-tag "partial"   (rest tag-text))
        (and (= type_flag "{") (= (last tag-text) "}"))
                          (mk-tag "unescaped" (1 -1 tag-text))
        true              (mk-tag "escaped"   tag-text))))

;; given a template string, tokenize into a 1D list of tags
(define (next-tag template acc)
  (let (start_index (find otag template))
    (if (nil? start_index)
        (if (empty? template)
            acc
            (append acc (list (mk-tag "text" nil template))))
        (letn (before_tag  (0 start_index template)
               acc         (if (empty? before_tag)
                               acc
                               (append acc (list (mk-tag "text" nil before_tag))))
               ;; we wish to match {{foo}} but also {{{foo}}}
               closing     (regex (append "}*" ctag) template 0 start_index))
          (if (nil? closing)
              (throw-error "Template is missing closing tag")
              (letn (close_index (nth 1 closing)
                     close_len   (nth 2 closing)
                     ;; in the case of "{{{foo}}}", index points to the 1st "}",
                     ;; but we want it at the 2nd one (assuming ctag == "}}")
                     close_index (+ close_index (- close_len ctag-len))
                     tag-text   (start_index (- (+ close_index ctag-len) start_index) template)
                     tag_type   (find-tag-type tag-text)
                     remainder  ((+ close_index ctag-len) template))
                (next-tag remainder (append acc (list tag_type)))))))))

;; Given a 1D list of tags, make a nested structure of sections
;; sections are an ordered list of text nodes, variable nodes, and subsections.
;; "The band {{band}} has members {{#members}}{{name}} on {{instrument}}{{/members}}" ->
;; ( (type: text,    content: "The band"),
;;   (type: escaped, key: "band"),
;;   (type: text,    content: "has members"),
;;   (type: section, key: "members", content: (
;;     (type: escaped, key: "name")
;;     (type: text,    content: "on")
;;     (type: escaped, key: "instrument")))

;; Step through the list of tags, create a nested list of sections.
;; We build structure this structure iteratively.
;; Section-stack is the list of sections we've discovered. When we find
;; an open-section tag, we create a section assoc and push it on this stack.
;; The current section is always the head of that stack.
;; We accumulate text and var tags into the current section.
;; Whe we discover a close-section tag, we pop that stack, and add the section
;; we just popped into the contents of the enclosing section.

(define (add-content-to-section current-section new-data)
  (letn (current-content (lookup 'content current-section))
    (setf (assoc 'content current-section)
          (list  'content (push new-data current-content -1)))
    current-section))

(define (nest-sections tags section-stack)
  (if (empty? tags) section-stack
      (letn (tag             (first tags)
             remainder       (rest tags)
             tag-type        (lookup 'type tag)
             current-section (first section-stack))
        
        (if (or  (= tag-type "text")
                 (= tag-type "unescaped")
                 (= tag-type "escaped")
                 (= tag-type "partial"))
            ;; easy case: a text or variable tag.
            ;; we add the tag to the content of the first section in the stack.
            (nest-sections remainder (cons
                                      (add-content-to-section current-section tag)
                                      (rest section-stack)))

            (or (= tag-type "section")
                (= tag-type "inverted"))
            ;; we're opening a new section.
            ;; create a new section, cons it to the section stack
            (nest-sections remainder (cons tag section-stack))

            (= tag-type "close")
            ;; we're closing a section. pop the section stack, saving the head
            ;; into the content of the enclosing section
            (letn (closed-section  current-section
                   section-stack   (rest section-stack)
                   current-section (first section-stack))

              (nest-sections remainder (cons
                                        (add-content-to-section
                                         current-section closed-section)
                                        (rest section-stack))))

            ;; some other tag
            (nest-sections remainder section-stack)))))

(define (find-in-val-stack key stack) 
  (if (empty? stack) nil
      (letn (found (lookup key (first stack)))
        (if (not (nil? found)) found
            (find-in-val-stack key (rest stack))))))

(define (apply-template template val-stack)
  (let (ret (list))
    (dolist (template-part template)
            (letn (template-part-type    (lookup 'type    template-part)
                   template-part-content (lookup 'content template-part)
                   template-part-key     (lookup 'key     template-part))
              
              (if (= template-part-type "section")
                  
                  ;; it's a section. we do things depending on the nature of the
                  ;; value found in the user-supplied table of values
                  (letn (val (find-in-val-stack
                              template-part-key
                              val-stack))
                    
                    ;; this is not the best. we need to determine if the value
                    ;; provided by the user is a list of associative lists, or a
                    ;; single associative list, or a single value.
                    (if (and (list? val)
                             (list? (first val))
                             (list? (first (first val))))
                        ;; it is a list of associations.
                        ;; we want to loop over these.
                        (dolist (singleval val)
                                (extend ret
                                        (apply-template
                                         template-part-content
                                         (cons singleval (list val-stack)))))
                        
                        ;; it is a single association structure.
                        (or (true? val) (list? val))
                        (extend ret
                                (apply-template template-part-content
                                                (cons val val-stack)))))
                  
                  ;; if the template part is text, we just cons it on...
                  (= template-part-type "text")
                  (extend ret (list template-part-content))
                  
                  ;; we do some variable substitution...
                  (= template-part-type "escaped")
                  (extend ret (list (find-in-val-stack
                                     template-part-key
                                     val-stack))))))))
