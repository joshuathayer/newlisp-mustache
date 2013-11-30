(context 'mustache)

(setq otag "{{")
(setq ctag "}}")

(setq otag-len (length otag))
(setq ctag-len (length ctag))

(define (mk-tag type key content)
  (list (list 'type    type)
        (list 'key     key)
        (list 'content content)))

;; given some text, determine what kind of tag we're looking at
(define (find_tag_type tag_text)
  (letn (tag_text (replace "[\ \t]" tag_text "" 1)      ;; normalize: remove all spaces
         tag_text (otag-len (- 0 ctag-len) tag_text)    ;; remove opening and closing braces
         type_flag (first tag_text))
    (if (= type_flag "#") (mk-tag "section"   (rest tag_text))
        (= type_flag "^") (mk-tag "inverted"  (rest tag_text))
        (= type_flag "/") (mk-tag "close"     (rest tag_text))
        (= type_flag "&") (mk-tag "unescaped" (rest tag_text))
        (= type_flag "!") (mk-tag "comment"   (rest tag_text))
        (= type_flag ">") (mk-tag "partial"   (rest tag_text))
        (and (= type_flag "{") (= (last tag_text) "}"))
                          (mk-tag "unescaped" (1 -1 tag_text))
        true              (mk-tag "escaped"   tag_text))))

;; given a template string, tokenize into a 1D list of tags
(define (next_tag template acc)
  (let (start_index (find otag template))
    (if (nil? start_index)
        (if (empty? template) acc (append acc (list (mk-tag "text" nil template))))
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
                     tag_text   (start_index (- (+ close_index ctag-len) start_index) template)
                     tag_type   (find_tag_type tag_text)
                     remainder  ((+ close_index ctag-len) template))
                (next_tag remainder (append acc (list tag_type)))))))))

;; given a 1D list of tags, make a nested structure of sections
;; sections are an ordered list of text nodes, variable nodes, and subsections
;; "The band {{band}} has members {{#members}}{{name}} on {{instrument}}{{/members}}" ->
;; ( (type: text,    content: "The band"),
;;   (type: escaped, key: "band"),
;;   (type: text,    content: "has members"),
;;   (type: section, key: "members", content: (
;;     (type: escaped, key: "name")
;;     (type: text,    content: "on")
;;     (type: escaped, key: "instrument")))

;; create an association list representing a section
(define (new-section type key)
  (list (list 'section-key key)
        (list 'section-type type)
        (list 'section-content (list))))

;; Step through the list of tags. Create a nested list of sections.
;; We build structure iteratively.
;; Section-stack is the list of sections we've discovered. When we find 
;; an open-section tag, we create a section assoc and push it on this stack.
;; The current section is always the head of that stack.
;; We accumulate text and var tags into the current section.
;; Whe we discover a close-section tag, we pop that stack, and add the section
;; we just popped into the acc of the enclosing section.

(define (add-content-to-section current-section new-data)
  (letn (current-content (lookup 'section-content current-section))
    (setf (assoc 'section-content current-section)
          (list  'section-content (push new-data current-content -1)))
    current-section))

(define (nest-sections tags section-stack)
  (if (empty? tags) section-stack
      (letn (tag             (first tags)
             remainder       (rest tags)
             tag-type        (lookup 'type tag)
             tag-key         (lookup 'key tag)
             current-section (first section-stack))
        
        (if (or  (= tag-type "text")
                 (= tag-type "unescaped")
                 (= tag-type "escaped")
                 (= tag-type "partial"))
            ;; easy case: a text or variable tag.
            ;; this updates (in place) current-section, to include the new tag.
            (nest-sections remainder (cons
                                      (add-content-to-section current-section tag)
                                      (rest section-stack)))
            (or (= tag-type "section")
                (= tag-type "inverted"))
            ;; we're opening a new section.
            ;; create a new section, cons it to the section stack
            (nest-sections remainder (cons (new-section tag-type tag-key) section-stack))

            (= tag-type "close")
            (letn (closed-section  current-section
                   section-stack   (rest section-stack)
                   current-section (first section-stack))

              (nest-sections remainder (cons
                                        (add-content-to-section current-section closed-section)
                                        (rest section-stack))))

            ;; some other tag
            (nest-sections remainder section-stack)))))
