;;; godoc-mode.el

;; what I have available:
;; go-builtins
;; go-mode-keywords

;; what i really need:

(define-derived-mode godoc-mode fundamental-mode "Godoc"
  "Something useful"
  (set (make-local-variable 'font-lock-defaults)
       '(go--build-font-lock-keywords))
  ;; set comments?
  ;; (set (make-local-variable 'comment-start) "// ")
  ;; (set (make-local-variable 'comment-end)   "")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")

  (view-mode 1))

(defun godoc--selectively-fontify ()
  (save-excursion
    (while (search-forward-regexp godoc-font-lock-regexp nil t)
      (goto-char (match-beginning 0))
      (let ((beg (point)))
        (move-end-of-line 1)
        (font-lock-fontify-region beg (point)))
      (next-line))))

;; returns '(ident line col)
(defun godoc--lex-ident ()
  (let (ident line col)
    (setq col (point))
    (forward-word)
    (setq ident (buffer-substring-no-properties col (point)))
    (save-excursion
      (beginning-of-line)
      (setq line (1+ (count-lines 1 (point)))))
    (list ident line col)))

;; godoc--lex returns a list, with the structure
;; (state ident line col)
;; will return nil for ident, line, and col if it has not found it yet
(defun godoc--lex (state)
  ;; states:
  ;; start
  ;; in-block
  ;; done
  (let ((non-whitespace "[^ \n\t]")
        (non-ident "[ \n\t\(]")
        (in-block-p (if (eq state :in-block) t nil)))
    (cond ((eq state :start)
           (search-forward-regexp non-whitespace) ;; TODO will give us errors if it can't
           ;; find it, refactor later?
           (backward-char) ;; we are now on the beginning of an identifier
           ;; if we see a '(', do weird shit TODO make this comment better
           (if (= (char-after) ?\( )
               (progn (setq in-block-p t)
                      (forward-char) ;; move past ')'
                      (search-forward-regexp non-whitespace) ;; move to ident
                      (backward-char))) ;; put point on ident
           (if in-block-p
               ;; check to see if we are still in the block
               (let ((ident (godoc--lex-ident)))
                 (search-forward-regexp "\n")
                 (if (eq (char-after) ?\) )
                     (cons :done ident)
                   (cons :in-block ident)))
             (cons :done (godoc--lex-ident))))
          ((eq state :in-block)
           (search-forward-regexp non-whitespace)
           (backward-char)
           (let ((ident (godoc--lex-ident)))
             (search-forward-regexp "\n")
             (if (eq (char-after) ?\) )
                 (cons :done ident)
               (cons :in-block ident))))
          ((eq state :done)
           nil))))
                 
(defun godoc--build-plist (keyword)
  (let ((ident-plist nil))     ; stored as ident:(line col) mapping
    (while (let ((case-fold-search nil)) (search-forward-regexp keyword nil t))
      (let* ((state :start)
             (lex-struct (godoc--lex state))) ; in form (state ident line col)
        (while lex-struct
          (setq ident-plist (plist-put ident-plist
                                       (cadr lex-struct) (cddr lex-struct)))
          (setq state (car lex-struct))
          (setq lex-struct (godoc--lex state)))))
    ident-plist))

;; only matches keyword at beginning of line
(defconst godoc-mode-keywords '("^const" "^func" "^type" "^var"))
(defconst godoc-font-lock-regexp "\\(^const \\|^func \\|^type \\|^var \\)")

;; probably won't need this
(defconst godoc-mode-delimiters '("CONSTANTS" "FUNCTIONS"))
