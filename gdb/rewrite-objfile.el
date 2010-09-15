#!/usr/bin/emacs --script

(setq stack-trace-on-error t)

(defvar source-tree "/home/tromey/gnu/archer/archer/gdb/")
(defvar build-tree "/home/tromey/gnu/archer/build/gdb/")

(defun file-error (text)
  (error "%s:%d:%d: error: expected %s"
	 buffer-file-name (line-number-at-pos (point))
	 (current-column)
	 text))

(defun assert-looking-at (exp)
  (unless (looking-at exp)
    (file-error exp)))

(defun get-field-name ()
  (save-excursion
    (assert-looking-at "\\(\\.\\|->\\)\\([A-Za-z0-9_]+\\)\\_>")
    (prog1
	(match-string 2)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun skip-backward-lhs ()
  (skip-chars-backward " \t\n")
  (cond
   ((eq (char-before) ?\])
    (file-error "array ref!")
    ;; fixme
    )
   ((eq (char-before) ?\))
    ;; A paren expression is preceding.
    ;; See if this is just a paren expression or whether it is a
    ;; function call.
    ;; For now assume that there are no function-calls-via-expr.
    (backward-sexp)
    (skip-chars-backward " \t\n")
    (if (save-excursion
	  (backward-char)
	  (looking-at "[A-Za-z0-9_]"))
	(backward-sexp)))
   ((save-excursion
      (backward-char)
      (looking-at "[A-Za-z0-9_]"))
    (backward-sexp))
   (t
    (file-error "unhandled case!"))))

(defun move-if-arrow-before ()
  (let ((save (point)))
    (skip-chars-backward " \t\n")
    (backward-char 2)
    (if (looking-at "->")
	t
      (goto-char save)
      nil)))

(defun do-fix-instance ()
  (cond
   ((looking-at "->")
    (let ((field-name (get-field-name))
	  (keep-going t))
      (insert ")")
      (backward-char)
      (while keep-going
	(skip-backward-lhs)
	(setq keep-going (move-if-arrow-before)))
      (insert "OBJFILE_" (upcase field-name) " (")))
   ((eq (char-after) ?.)
    (file-error "unhandled `.'!"))
   (t
    (message "%s:%d:%d: warning: did not see -> or ., probably macro"
	     buffer-file-name (line-number-at-pos (point))
	     (current-column)))))

(defun fix-one-instance (filename line column)
  (unless (string-match "[.]h$" filename)
    (message "%s:%d:%d: info: fixing instance" filename line column)
    (find-file (expand-file-name filename build-tree))
    (goto-char (point-min))
    (forward-line (- line 1))
    ;; (move-to-column (- column 1))
    (forward-char (- column 1))
    (do-fix-instance)))

(defvar make-accumulation "")

(defvar last-error-file nil)
(defvar last-error-line nil)
(defvar error-list nil)

(defun make-filter (process string)
  (setq make-accumulation (concat make-accumulation string))
  (while (string-match "^[^\n]*\n" make-accumulation)
    (let ((line (substring (match-string 0 make-accumulation) 0 -1)))
      (setq make-accumulation (substring make-accumulation
					 (match-end 0)))
      (message "%s" line)
      (if (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)+: error:"
			line)
	  (save-excursion
	    (let ((file-name (match-string 1 line))
		  (line-no (string-to-number (match-string 2 line)))
		  (col-no (string-to-number (match-string 3 line))))
	      (if (and (string= last-error-file file-name)
		       (eq line-no last-error-line))
		  ;; We want to process all errors on a given line in
		  ;; reverse order, so we defer this entry.
		  nil
		;; Process the pending list.
		(dolist (one-item error-list)
		  (apply #'fix-one-instance one-item))
		(setq error-list nil)
		(setq last-error-line line-no)
		(setq last-error-file file-name))
	      (message "%s:%d:%d: pushing on the stack"
		       file-name line-no col-no)
	      (push (list file-name line-no col-no) error-list)))))))

(defvar make-done nil)

(defun make-sentinel (process string)
  (dolist (one-item error-list)
    (apply #'fix-one-instance one-item))
  (setq make-done t))

(defun recompile-gdb ()
  (find-file "/tmp/Log")
  (goto-char (point-min))
  (while (not (eobp))
    (let ((here (point)))
      (forward-line 1)
      (save-excursion
	(make-filter nil (buffer-substring here (point))))))
  (make-sentinel nil ""))

(recompile-gdb)
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when buffer-file-name
      (message "Saving %s" buffer-file-name)
      (save-buffer))))
