;;; safety.el --- Hardening tweaks -*- lexical-binding: t; -*-
;;; Commentary:
;;;  Avoid common runtime issues (e.g., nil face attrs).
;;; Code:

;; Convert nil face color attrs to 'unspecified to avoid warnings
(defun v5--normalize-face-attrs (args)
  (let ((xs args)
        (i 0))
    (while (< i (length xs))
      (when (memq (nth i xs) '(:foreground :background))
        (let ((val (nth (1+ i) xs)))
          (when (null val)
            (setf (nth (1+ i) xs) 'unspecified))))
      (setq i (+ i 2)))
    xs))

(advice-add 'set-face-attribute :filter-args #'v5--normalize-face-attrs)

;; Silence noisy ELPA byte-compile warnings (docstrings width, obsolete, etc.)
;; Only while compiling packages to avoid clutter during startup.
(with-eval-after-load 'package
  (when (fboundp 'package--compile)
    (defun v5--silence-package-bytecomp (orig &rest args)
      (let ((warning-suppress-types (cons '(bytecomp) warning-suppress-types)))
        (apply orig args)))
    (advice-add 'package--compile :around #'v5--silence-package-bytecomp))
  ;; Async compile path on newer Emacs
  (when (fboundp 'package--compile-async)
    (defun v5--silence-package-bytecomp-async (orig &rest args)
      (let ((warning-suppress-types (cons '(bytecomp) warning-suppress-types)))
        (apply orig args)))
    (advice-add 'package--compile-async :around #'v5--silence-package-bytecomp-async)))

(provide 'safety)
;;; safety.el ends here
