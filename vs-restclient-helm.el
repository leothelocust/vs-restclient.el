;;; vs-restclient-helm.el --- helm interface for vs-restclient.el
;;
;; Public domain.
;;
;; Author: Levi Olson <https://github.com/leothelocust>
;;
;; Forked From `https://github.com/pashky/restclient.el'
;; Author: Pavel Kurnosov <pashky@gmail.com>
;; Maintainer: Pavel Kurnosov <pashky@gmail.com>
;; Created: 01 Apr 2016
;; Keywords: http helm
;; Package-Requires: ((vs-restclient "0") (helm "1.9.4"))

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is a companion to vs-restclient.el to add helm sources for requests and variables in current vs-restclient-mode buffer.

;;; Code:
;;
(require 'helm)
(require 'helm-utils)
(require 'vs-restclient)

(defun vs-restclient-helm-find-candidates-matching (regexp process)
  (let ((result '()))
    (with-helm-current-buffer
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings
          (font-lock-fontify-buffer)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (setq result (cons (cons (funcall process) (line-number-at-pos)) result))))
      result)))

(defun vs-restclient-helm-find-requests ()
  (vs-restclient-helm-find-candidates-matching
   vs-restclient-method-url-regexp
   '(lambda () (match-string 0))))

(defun vs-restclient-helm-find-variables ()
  (vs-restclient-helm-find-candidates-matching
   vs-restclient-var-regexp
   '(lambda () (match-string 1))))

(defun vs-restclient-helm-goto (candidate)
  (switch-to-buffer helm-current-buffer)
  (helm-goto-line candidate))

(defconst vs-restclient-helm-requests-source
  (helm-build-sync-source "Variables"
    :action '(("Go to declaration" . vs-restclient-helm-goto))
    :candidates 'vs-restclient-helm-find-variables))

(defconst vs-restclient-helm-variables-source
  (helm-build-sync-source "Requests"
    :action '(("Go to" . vs-restclient-helm-goto))
    :candidates 'vs-restclient-helm-find-requests))

;;;###autoload
(defun helm-vs-restclient ()
  "Helm for Vs-Restclient."
  (interactive)
  (helm :sources '(vs-restclient-helm-requests-source vs-restclient-helm-variables-source)))

(provide 'vs-restclient-helm)

(eval-after-load 'vs-restclient
  '(progn
     (define-key vs-restclient-mode-map (kbd "C-c C-g") #'helm-vs-restclient)))

;;; vs-restclient-helm.el ends here
