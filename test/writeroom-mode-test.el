;;; writeroom-mode-test.el --- Tests for writeroom-mode

(require 'writeroom-mode)

(ert-deftest writeroom--match-major-mode-test ()
  ;; Match major mode symbol.
  (should (eq (let ((major-mode 'mock-mode))
                (writeroom--match-major-mode '(text-mode mock-mode)))
              t))
  ;; Match derived mode.
  (should (eq (let ((major-mode 'mock-mode))
                (put 'mock-mode 'derived-mode-parent 'text-mode)
                (writeroom--match-major-mode '(text-mode) t))
              t))
  ;; Fail when major mode not present.
  (should (eq (let ((major-mode 'mock-mode))
                (writeroom--match-major-mode '(text-mode)))
              nil))
  ;; Match partial mode name if a string.
  (should (eq (let ((major-mode 'emacs-lisp-mode))
                (writeroom--match-major-mode '(text-mode "lisp-mode")))
              t))
  ;; Fail when partial mode name is not a string.
  (should (eq (let ((major-mode 'emacs-lisp-mode))
                (writeroom--match-major-mode '(text-mode lisp-mode)))
              nil)))

;;; writeroom-mode-test.el ends here
