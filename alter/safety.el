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

(provide 'safety)
;;; safety.el ends here
