;;;
;;; testunit-mode.el
;;;
;;; Andrew Kappen
;;;
;;; based on rspec-mode.el by Pat Maddox

(define-derived-mode testunit-mode ruby-mode
  "Toggle Testunit mode"
  nil
  " Testunit")
(add-to-list 'auto-mode-alist '("_test.rb$" . testunit-mode))

(defun run-tests ()
  "Run tests and display results in same buffer"
  (interactive)
  (do-run-testunit))

(defun run-focused-test ()
  "Run the example defined on the current line"
  (interactive)
  (do-run-testunit (concat "--name=" (testunit-example-at-point))))

(defun ruby-command ()
  "ruby")

(defvar testunit-testcase-re
  "^[ \\t]*def[ \\t]+\\(test[_a-z0-9]*\\)")

(defun testunit-example-at-point ()
  (save-excursion
    (if (re-search-backward testunit-testcase-re nil t)
        (match-string 1))))

(require 'linkify)
(defun do-run-testunit (&rest args)
  (setq testunit-results (get-buffer-create "testunit-results"))
  (save-excursion
    (set-buffer testunit-results)
    (erase-buffer)
    (setq linkify-regexps '("\\(/.*\\):\\([0-9]*\\):")))
  (setq proc (apply #'start-process "testunit" testunit-results (ruby-command) (buffer-file-name) args))
  (set-process-filter proc 'linkify-filter)
  (display-buffer testunit-results))
(provide 'testunit-mode)
