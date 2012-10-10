
;;; no tests for
;;;
;;;    unicode-fonts-debug-info-at-point
;;;    unicode-fonts-debug-change-font-for-block
;;;    unicode-fonts-debug-change-all-fonts
;;;    unicode-fonts-debug-interactively
;;;    unicode-fonts-setup
;;;    unicode-fonts--setup-1
;;;

;;; todo
;;;
;;; why does unicode-fonts-debug-insert-block-01 set off
;;; the very lengthy duplicates check?
;;;
;;; add coverage for
;;;     unicode-fonts-compute-skipped-fonts
;;;     unicode-fonts-debug-check-duplicate-fonts
;;;     unicode-fonts-first-existing-font

;;; requires and setup

(when load-file-name
  (setq pcache-directory (expand-file-name "test_output/" (file-name-directory load-file-name)))
  (setq package-enable-at-startup nil)
  (setq package-load-list '((pcache t)
                            (persistent-soft t)
                            (ucs-utils t)
                            (font-utils t)
                            (string-utils t)
                            (alert t)))
  (when (fboundp 'package-initialize)
    (package-initialize)))

(require 'persistent-soft)
(require 'ucs-utils)
(require 'font-utils)
(require 'string-utils)
(require 'alert)
(require 'unicode-fonts)

;;; unicode-fonts-font-exists-p

(ert-deftest unicode-fonts-font-exists-p-01 nil
  :tags '(:interactive)
  (should
   (unicode-fonts-font-exists-p "Courier")))

(ert-deftest unicode-fonts-font-exists-p-02 nil
  :tags '(:interactive)
  (should-not
   (unicode-fonts-font-exists-p "__nonexistent-font__")))

(ert-deftest unicode-fonts-font-exists-p-03 nil
  :tags '(:interactive)
  (should
   (unicode-fonts-font-exists-p "Courier" 12)))

(ert-deftest unicode-fonts-font-exists-p-04 nil
  :tags '(:interactive)
  (should-not
   (let ((unicode-fonts-restrict-to-fonts '("Courier New")))
     (unicode-fonts-font-exists-p "Courier"))))

(ert-deftest unicode-fonts-font-exists-p-05 nil
  :tags '(:interactive)
  (should
   (let ((unicode-fonts-restrict-to-fonts '("Courier" "Arial")))
     (unicode-fonts-font-exists-p "Courier"))))

;;; unicode-fonts-read-block-name

(ert-deftest unicode-fonts-read-block-name-01 nil
  :tags '(:interactive)
  (should (equal "Mathematical Operators"
                 (let ((cursor-in-echo-area t))
                   (read-char "Press a key, then enter \"Mathematical Operators\" at the next prompt (with completions).")
                   (unicode-fonts-read-block-name)))))

(ert-deftest unicode-fonts-read-block-name-02 nil
  :tags '(:interactive)
  (should (equal "Mathematical Operators"
                 (let ((cursor-in-echo-area t))
                   (read-char "Press a key, then enter \"Mathematical Operators\" at the next prompt (with ido completions).")
                   (unicode-fonts-read-block-name 'ido)))))

;;; unicode-fonts-debug-insert-block

(ert-deftest unicode-fonts-debug-insert-block-01 nil
  (should (equal (decode-char 'ucs #x2201)
                 (with-temp-buffer
                   (delete-region (point-min) (point-max))
                   (goto-char (point-min))
                   (unicode-fonts-debug-insert-block "Mathematical Operators")
                   (goto-char (point-min))
                   (forward-line 3)
                   (prog1
                       (char-after)
                     (delete-region (point-min) (point-max)))))))

;;; unicode-fonts-debug-validate-data

(ert-deftest unicode-fonts-debug-validate-data-01 nil
  (should
   (with-temp-buffer
     (delete-region (point-min) (point-max))
     (goto-char (point-min))
     (unicode-fonts-debug-validate-data 'insert)
     (goto-char (point-min))
     (when (re-search-forward "^ERROR[: \t]*\\([^\n]*\\)" nil t)
       (error "data failed validation: %s" (match-string 1)))
     (delete-region (point-min) (point-max))
     t)))

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;

;;; unicode-fonts-test.el ends here
