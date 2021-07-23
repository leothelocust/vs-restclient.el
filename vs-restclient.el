;;; vs-restclient.el --- An interactive HTTP client for Emacs
;;
;; Public domain.
;;
;; Author: Levi Olson <https://github.com/leothelocust>
;;
;; Forked From `https://github.com/pashky/restclient.el'
;; Author: Pavel Kurnosov <pashky@gmail.com>
;; Maintainer: Pavel Kurnosov <pashky@gmail.com>
;; Created: 01 Apr 2012
;; Keywords: http
;;
;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.
;;
;;; Commentary:
;;
;; VS Code Compatable REST Client
;;  --- An interactive HTTP client for Emacs
;;
;; This is a tool to manually explore and test HTTP REST
;; webservices.  Runs queries from a plain-text query sheet, displays
;; results as a pretty-printed XML, JSON and even images.
;;
;;; Code:

(require 'url)
(require 'json)
(require 'outline)
(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl))

(defgroup vs-restclient nil
  "An interactive HTTP client for Emacs."
  :group 'tools)

(defcustom vs-restclient-log-request t
  "Log vs-restclient requests to *Messages*."
  :group 'vs-restclient
  :type 'boolean)

(defcustom vs-restclient-same-buffer-response t
  "Re-use same buffer for responses or create a new one each time."
  :group 'vs-restclient
  :type 'boolean)

(defcustom vs-restclient-same-buffer-response-name "*HTTP Response*"
  "Name for response buffer."
  :group 'vs-restclient
  :type 'string)

(defcustom vs-restclient-info-buffer-name "*Vs-Restclient Info*"
  "Name for info buffer."
  :group 'vs-restclient
  :type 'string)

(defcustom vs-restclient-inhibit-cookies nil
  "Inhibit vs-restclient from sending cookies implicitly."
  :group 'vs-restclient
  :type 'boolean)

(defcustom vs-restclient-content-type-modes '(("text/xml" . xml-mode)
                                              ("text/plain" . text-mode)
                                              ("application/xml" . xml-mode)
                                              ("application/json" . js-mode)
                                              ("image/png" . image-mode)
                                              ("image/jpeg" . image-mode)
                                              ("image/jpg" . image-mode)
                                              ("image/gif" . image-mode)
                                              ("text/html" . html-mode))
  "An association list mapping content types to buffer modes"
  :group 'vs-restclient
  :type '(alist :key-type string :value-type symbol))

(defgroup vs-restclient-faces nil
  "Faces used in Vs-Restclient Mode"
  :group 'vs-restclient
  :group 'faces)

(defface vs-restclient-variable-name-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for variable name."
  :group 'vs-restclient-faces)

(defface vs-restclient-variable-string-face
  '((t (:inherit font-lock-string-face)))
  "Face for variable value (string)."
  :group 'vs-restclient-faces)

(defface vs-restclient-variable-elisp-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for variable value (Emacs lisp)."
  :group 'vs-restclient-faces)

(defface vs-restclient-variable-multiline-face
  '((t (:inherit font-lock-doc-face)))
  "Face for multi-line variable value marker."
  :group 'vs-restclient-faces)

(defface vs-restclient-variable-usage-face
  '((t (:inherit vs-restclient-variable-name-face)))
  "Face for variable usage (only used when headers/body is represented as a single variable, not highlighted when variable appears in the middle of other text)."
  :group 'vs-restclient-faces)

(defface vs-restclient-method-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for HTTP method."
  :group 'vs-restclient-faces)

(defface vs-restclient-url-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for variable value (Emacs lisp)."
  :group 'vs-restclient-faces)

(defface vs-restclient-file-upload-face
  '((t (:inherit vs-restclient-variable-multiline-face)))
  "Face for highlighting upload file paths."
  :group 'vs-restclient-faces)

(defface vs-restclient-header-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTTP header name."
  :group 'vs-restclient-faces)

(defface vs-restclient-header-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for HTTP header value."
  :group 'vs-restclient-faces)

(defface vs-restclient-request-hook-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for single request hook indicator."
  :group 'vs-restclient-faces)

(defface vs-restclient-request-hook-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for single request hook type names."
  :group 'vs-restclient-faces)

(defface vs-restclient-request-hook-args-face
  '((t (:inherit font-lock-string-face)))
  "Face for single request hook type arguments."
  :group 'vs-restclient-faces)


(defvar vs-restclient-within-call nil)

(defvar vs-restclient-request-time-start nil)
(defvar vs-restclient-request-time-end nil)

(defvar vs-restclient-var-overrides nil
  "An alist of vars that will override any set in the file,
  also where dynamic vars set on callbacks are stored.")

(defvar vs-restclient-result-handlers '()
  "A registry of available completion hooks.
   Stored as an alist of name -> (hook-creation-func . description)")

(defvar vs-restclient-curr-request-functions nil
  "A list of functions to run once when the next request is loaded")

(defvar vs-restclient-response-loaded-hook nil
  "Hook run after response buffer is formatted.")

(defvar vs-restclient-http-do-hook nil
  "Hook to run before making request.")

(defvar vs-restclient-response-received-hook nil
  "Hook run after data is loaded into response buffer.")

(defcustom vs-restclient-vars-max-passes 10
  "Maximum number of recursive variable references. This is to prevent hanging if two variables reference each other directly or indirectly."
  :group 'vs-restclient
  :type 'integer)

(defconst vs-restclient-comment-separator "#")
(defconst vs-restclient-comment-start-regexp (concat "^" vs-restclient-comment-separator))
(defconst vs-restclient-comment-not-regexp (concat "^[^" vs-restclient-comment-separator "]"))
(defconst vs-restclient-empty-line-regexp "^\\s-*$")

(defconst vs-restclient-method-url-regexp
  "^\\(GET\\|POST\\|DELETE\\|PUT\\|HEAD\\|OPTIONS\\|PATCH\\) \\(.*\\)$")

(defconst vs-restclient-header-regexp
  "^\\([^](),/:;@[\\{}= \t]+\\): \\(.*\\)$")

(defconst vs-restclient-use-var-regexp
  "^\\(@[^@ \n]+\\)$")

(defconst vs-restclient-var-regexp
  (concat "^@\\([^:= ]+\\)[ \t]*\\(:?\\)=[ \t]*\\(<<[ \t]*\n\\(\\(.*\n\\)*?\\)" vs-restclient-comment-separator "\\|\\([^<].*\\)$\\)"))

(defconst vs-restclient-svar-regexp
  "^\\(@[^:= ]+\\)[ \t]*=[ \t]*\\(.+?\\)$")

(defconst vs-restclient-evar-regexp
  "^\\(@[^: ]+\\)[ \t]*:=[ \t]*\\(.+?\\)$")

(defconst vs-restclient-mvar-regexp
  "^\\(@[^: ]+\\)[ \t]*:?=[ \t]*\\(<<\\)[ \t]*$")

(defconst vs-restclient-file-regexp
  "^<[ \t]*\\([^<>\n\r]+\\)[ \t]*$")

(defconst vs-restclient-content-type-regexp
  "^Content-[Tt]ype: \\(\\w+\\)/\\(?:[^\\+\r\n]*\\+\\)*\\([^;\r\n]+\\)")

(defconst vs-restclient-response-hook-regexp
  "^\\(->\\) \\([^[:space:]]+\\) +\\(.*\\)$")

;; The following disables the interactive request for user name and
;; password should an API call encounter a permission-denied response.
;; This API is meant to be usable without constant asking for username
;; and password.
(defadvice url-http-handle-authentication (around vs-restclient-fix)
  (if vs-restclient-within-call
      (setq ad-return-value t)
    ad-do-it))
(ad-activate 'url-http-handle-authentication)

(defadvice url-cache-extract (around vs-restclient-fix-2)
  (unless vs-restclient-within-call
    ad-do-it))
(ad-activate 'url-cache-extract)

(defadvice url-http-user-agent-string (around vs-restclient-fix-3)
  (if vs-restclient-within-call
      (setq ad-return-value nil)
    ad-do-it))
(ad-activate 'url-http-user-agent-string)

(defun vs-restclient-http-do (method url headers entity &rest handle-args)
  "Send ENTITY and HEADERS to URL as a METHOD request."
  (if vs-restclient-log-request
      (message "HTTP %s %s Headers:[%s] Body:[%s]" method url headers entity))
  (let ((url-request-method (encode-coding-string method 'us-ascii))
        (url-request-extra-headers '())
        (url-request-data (encode-coding-string entity 'utf-8))
        (url-mime-charset-string (url-mime-charset-string))
        (url-mime-language-string nil)
        (url-mime-encoding-string nil)
        (url-mime-accept-string nil)
        (url-personal-mail-address nil))

    (dolist (header headers)
      (let* ((mapped (assoc-string (downcase (car header))
                                   '(("from" . url-personal-mail-address)
                                     ("accept-encoding" . url-mime-encoding-string)
                                     ("accept-charset" . url-mime-charset-string)
                                     ("accept-language" . url-mime-language-string)
                                     ("accept" . url-mime-accept-string)))))

        (if mapped
            (set (cdr mapped) (encode-coding-string (cdr header) 'us-ascii))
          (let* ((hkey (encode-coding-string (car header) 'us-ascii))
                 (hvalue (encode-coding-string (cdr header) 'us-ascii)))
            (setq url-request-extra-headers (cons (cons hkey hvalue) url-request-extra-headers))))))

    (setq vs-restclient-within-call t)
    (setq vs-restclient-request-time-start (current-time))
    (run-hooks 'vs-restclient-http-do-hook)
    (url-retrieve url 'vs-restclient-http-handle-response
                  (append (list method url (if vs-restclient-same-buffer-response
                                               vs-restclient-same-buffer-response-name
                                             (format "*HTTP %s %s*" method url))) handle-args) nil vs-restclient-inhibit-cookies)))

(defun vs-restclient-prettify-response (method url)
  (save-excursion
    (let ((start (point)) (guessed-mode) (end-of-headers))
      (while (and (not (looking-at vs-restclient-empty-line-regexp))
                  (eq (progn
                        (when (looking-at vs-restclient-content-type-regexp)
                          (setq guessed-mode
                                (cdr (assoc-string (concat
                                                    (match-string-no-properties 1)
                                                    "/"
                                                    (match-string-no-properties 2))
                                                   vs-restclient-content-type-modes
                                                   t))))
                        (forward-line)) 0)))
      (setq end-of-headers (point))
      (while (and (looking-at vs-restclient-empty-line-regexp)
                  (eq (forward-line) 0)))
      (unless guessed-mode
        (setq guessed-mode
              (or (assoc-default nil
                                 ;; magic mode matches
                                 '(("<\\?xml " . xml-mode)
                                   ("{\\s-*\"" . js-mode))
                                 (lambda (re _dummy)
                                   (looking-at re))) 'js-mode)))
      (let ((headers (buffer-substring-no-properties start end-of-headers)))
        (when guessed-mode
          (delete-region start (point))
          (unless (eq guessed-mode 'image-mode)
            (apply guessed-mode '())
            (if (fboundp 'font-lock-flush)
                (font-lock-flush)
              (with-no-warnings
                (font-lock-fontify-buffer))))

          (cond
           ((eq guessed-mode 'xml-mode)
            (goto-char (point-min))
            (while (search-forward-regexp "\>[ \\t]*\<" nil t)
              (backward-char) (insert "\n"))
            (indent-region (point-min) (point-max)))

           ((eq guessed-mode 'image-mode)
            (let* ((img (buffer-string)))
              (delete-region (point-min) (point-max))
              (fundamental-mode)
              (insert-image (create-image img nil t))))

           ((eq guessed-mode 'js-mode)
            (let ((json-special-chars (remq (assoc ?/ json-special-chars) json-special-chars))
                  ;; Emacs 27 json.el uses `replace-buffer-contents' for
                  ;; pretty-printing which is great because it keeps point and
                  ;; markers intact but can be very slow with huge minimalized
                  ;; JSON.  We don't need that here.
                  (json-pretty-print-max-secs 0))
              (ignore-errors (json-pretty-print-buffer)))
            (vs-restclient-prettify-json-unicode)))

          (goto-char (point-max))
          (or (eq (point) (point-min)) (insert "\n"))
          (let ((hstart (point)))
            (insert method " " url "\n" headers)
            (insert (format "Request duration: %fs\n" (float-time (time-subtract vs-restclient-request-time-end vs-restclient-request-time-start))))
            (unless (member guessed-mode '(image-mode text-mode))
              (comment-region hstart (point)))))))))

(defun vs-restclient-prettify-json-unicode ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\[Uu]\\([0-9a-fA-F]\\{4\\}\\)" nil t)
      (replace-match (char-to-string (decode-char 'ucs (string-to-number (match-string 1) 16))) t nil))))

(defun vs-restclient-http-handle-response (status method url bufname raw stay-in-window)
  "Switch to the buffer returned by `url-retreive'.
The buffer contains the raw HTTP response sent by the server."
  (setq vs-restclient-within-call nil)
  (setq vs-restclient-request-time-end (current-time))
  (if (= (point-min) (point-max))
      (signal (car (plist-get status :error)) (cdr (plist-get status :error)))
    (when (buffer-live-p (current-buffer))
      (with-current-buffer (vs-restclient-decode-response
                            (current-buffer)
                            bufname
                            vs-restclient-same-buffer-response)
        (run-hooks 'vs-restclient-response-received-hook)
        (unless raw
          (vs-restclient-prettify-response method url))
        (buffer-enable-undo)
        (vs-restclient-response-mode)
        (run-hooks 'vs-restclient-response-loaded-hook)
        (if stay-in-window
            (display-buffer (current-buffer) t)
          (switch-to-buffer-other-window (current-buffer)))))))

(defun vs-restclient-decode-response (raw-http-response-buffer target-buffer-name same-name)
  "Decode the HTTP response using the charset (encoding) specified in the Content-Type header. If no charset is specified, default to UTF-8."
  (let* ((charset-regexp "^Content-Type.*charset=\\([-A-Za-z0-9]+\\)")
         (image? (save-excursion
                   (search-forward-regexp "^Content-Type.*[Ii]mage" nil t)))
         (encoding (if (save-excursion
                         (search-forward-regexp charset-regexp nil t))
                       (intern (downcase (match-string 1)))
                     'utf-8)))
    (if image?
        ;; Dont' attempt to decode. Instead, just switch to the raw HTTP response buffer and
        ;; rename it to target-buffer-name.
        (with-current-buffer raw-http-response-buffer
          ;; We have to kill the target buffer if it exists, or `rename-buffer'
          ;; will raise an error.
          (when (get-buffer target-buffer-name)
            (kill-buffer target-buffer-name))
          (rename-buffer target-buffer-name)
          raw-http-response-buffer)
      ;; Else, switch to the new, empty buffer that will contain the decoded HTTP
      ;; response. Set its encoding, copy the content from the unencoded
      ;; HTTP response buffer and decode.
      (let ((decoded-http-response-buffer
             (get-buffer-create
              (if same-name target-buffer-name (generate-new-buffer-name target-buffer-name)))))
        (with-current-buffer decoded-http-response-buffer
          (setq buffer-file-coding-system encoding)
          (save-excursion
            (erase-buffer)
            (insert-buffer-substring raw-http-response-buffer))
          (kill-buffer raw-http-response-buffer)
          (condition-case nil
              (decode-coding-region (point-min) (point-max) encoding)
            (error
             (message (concat "Error when trying to decode http response with encoding: "
                              (symbol-name encoding)))))
          decoded-http-response-buffer)))))

(defun vs-restclient-current-min ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at vs-restclient-comment-start-regexp)
        (if (re-search-forward vs-restclient-comment-not-regexp (point-max) t)
            (point-at-bol) (point-max))
      (if (re-search-backward vs-restclient-comment-start-regexp (point-min) t)
          (point-at-bol 2)
        (point-min)))))

(defun vs-restclient-current-max ()
  (save-excursion
    (if (re-search-forward vs-restclient-comment-start-regexp (point-max) t)
        (max (- (point-at-bol) 1) 1)
      (progn (goto-char (point-max))
             (if (looking-at "^$") (- (point) 1) (point))))))

(defun vs-restclient-replace-all-in-string (replacements string)
  (if replacements
      (let ((current string)
            (pass vs-restclient-vars-max-passes)
            (continue t))
        (while (and continue (> pass 0))
          (setq pass (- pass 1))
          (setq current (replace-regexp-in-string (regexp-opt (mapcar 'car replacements))
                                                  (lambda (key)
                                                    (setq continue t)
                                                    (cdr (assoc key replacements)))
                                                  current t t)))
        current)
    string))

(defun vs-restclient-replace-all-in-header (replacements header)
  (cons (car header)
        (vs-restclient-replace-all-in-string replacements (cdr header))))

(defun vs-restclient-chop (text)
  (if text (replace-regexp-in-string "\n$" "" text) nil))

(defun vs-restclient-find-vars-before-point ()
  (let ((vars nil)
        (bound (point)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp vs-restclient-var-regexp bound t)
        (let ((name (concat "{{" (match-string-no-properties 1) "}}"))
              (should-eval (> (length (match-string 2)) 0))
              (value (or (vs-restclient-chop (match-string-no-properties 4)) (match-string-no-properties 3))))
          (setq vars (cons (cons name (if should-eval (vs-restclient-eval-var value) value)) vars))))
      (append vs-restclient-var-overrides vars))))

(defun vs-restclient-eval-var (string)
  (with-output-to-string (princ (eval (read string)))))

(defun vs-restclient-make-header (&optional string)
  (cons (match-string-no-properties 1 string)
        (match-string-no-properties 2 string)))

(defun vs-restclient-parse-headers (string)
  (let ((start 0)
        (headers '()))
    (while (string-match vs-restclient-header-regexp string start)
      (setq headers (cons (vs-restclient-make-header string) headers)
            start (match-end 0)))
    headers))

(defun vs-restclient-read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun vs-restclient-parse-body (entity vars)
  (if (= 0 (or (string-match vs-restclient-file-regexp entity) 1))
      (vs-restclient-read-file (match-string 1 entity))
    (vs-restclient-replace-all-in-string vars entity)))

(defun vs-restclient-parse-hook (cb-type args-offset args)
  (if-let ((handler (assoc cb-type vs-restclient-result-handlers)))
      (funcall (cadr handler) args args-offset)
    `(lambda ()
       (message "Unknown vs-restclient hook type %s" ,cb-type))))

(defun vs-restclient-register-result-func (name creation-func description)
  (let ((new-cell (cons name (cons creation-func description))))
    (setq vs-restclient-result-handlers (cons new-cell vs-restclient-result-handlers))))

(defun vs-restclient-remove-var (var-name)
  (setq vs-restclient-var-overrides (assoc-delete-all var-name vs-restclient-var-overrides)))

(defun vs-restclient-set-var (var-name value)
  (vs-restclient-remove-var var-name)
  (setq vs-restclient-var-overrides (cons (cons var-name value) vs-restclient-var-overrides)))

(defun vs-restclient-get-var-at-point (var-name buffer-name buffer-pos)
  (message (format "getting var %s form %s at %s" var-name buffer-name buffer-pos))
  (let* ((vars-at-point  (save-excursion
                           (switch-to-buffer buffer-name)
                           (goto-char buffer-pos)
                           ;; if we're called from a vs-restclient buffer we need to lookup vars before the current hook or evar
                           ;; outside a vs-restclient buffer only globals are available so moving the point wont matter
                           (re-search-backward "^:\\|->" (point-min) t)
                           (vs-restclient-find-vars-before-point))))
    (vs-restclient-replace-all-in-string vars-at-point (cdr (assoc var-name vars-at-point)))))

(defmacro vs-restclient-get-var (var-name)
  (lexical-let ((buf-name (buffer-name (current-buffer)))
                (buf-point (point)))
    `(vs-restclient-get-var-at-point ,var-name ,buf-name ,buf-point)))

(defun vs-restclient-single-request-function ()
  (dolist (f vs-restclient-curr-request-functions)
    (ignore-errors
      (funcall f)))  
  (setq vs-restclient-curr-request-functions nil)
  (remove-hook 'vs-restclient-response-loaded-hook 'vs-restclient-single-request-function))


(defun vs-restclient-http-parse-current-and-do (func &rest args)
  (save-excursion
    (goto-char (vs-restclient-current-min))
    (when (re-search-forward vs-restclient-method-url-regexp (point-max) t)
      (let ((method (match-string-no-properties 1))
            (url (match-string-no-properties 2))
            (vars (vs-restclient-find-vars-before-point))
            (headers '()))
        (forward-line)
        (while (cond
                ((looking-at vs-restclient-response-hook-regexp)
                 (when-let (hook-function (vs-restclient-parse-hook (match-string-no-properties 2)
                                                                    (match-end 2)
                                                                    (match-string-no-properties 3)))
                   (push hook-function vs-restclient-curr-request-functions)))
                ((and (looking-at vs-restclient-header-regexp) (not (looking-at vs-restclient-empty-line-regexp)))
                 (setq headers (cons (vs-restclient-replace-all-in-header vars (vs-restclient-make-header)) headers)))
                ((looking-at vs-restclient-use-var-regexp)
                 (setq headers (append headers (vs-restclient-parse-headers (vs-restclient-replace-all-in-string vars (match-string 1)))))))
          (forward-line))
        (when (looking-at vs-restclient-empty-line-regexp)
          (forward-line))
        (when vs-restclient-curr-request-functions
          (add-hook 'vs-restclient-response-loaded-hook 'vs-restclient-single-request-function))
        (let* ((cmax (vs-restclient-current-max))
               (entity (vs-restclient-parse-body (buffer-substring (min (point) cmax) cmax) vars))
               (url (vs-restclient-replace-all-in-string vars url)))
          (apply func method url headers entity args))))))

(defun vs-restclient-copy-curl-command ()
  "Formats the request as a curl command and copies the command to the clipboard."
  (interactive)
  (vs-restclient-http-parse-current-and-do
   '(lambda (method url headers entity)
      (let ((header-args
             (apply 'append
                    (mapcar (lambda (header)
                              (list "-H" (format "%s: %s" (car header) (cdr header))))
                            headers))))
        (kill-new (concat "curl "
                          (mapconcat 'shell-quote-argument
                                     (append '("-i")
                                             header-args
                                             (list (concat "-X" method))
                                             (list url)
                                             (when (> (string-width entity) 0)
                                               (list "-d" entity)))
                                     " "))))
      (message "curl command copied to clipboard."))))


(defun vs-restclient-elisp-result-function (args offset)
  (goto-char offset)
  (lexical-let ((form (macroexpand-all (read (current-buffer)))))
    (lambda ()
      (eval form))))

(vs-restclient-register-result-func
 "run-hook" #'vs-restclient-elisp-result-function
 "Call the provided (possibly multi-line) elisp when the result
  buffer is formatted. Equivalent to a vs-restclient-response-loaded-hook
  that only runs for this request.
  eg. -> on-response (message \"my hook called\")" )

;;;###autoload
(defun vs-restclient-http-send-current (&optional raw stay-in-window)
  "Sends current request.
Optional argument RAW don't reformat response if t.
Optional argument STAY-IN-WINDOW do not move focus to response buffer if t."
  (interactive)
  (vs-restclient-http-parse-current-and-do 'vs-restclient-http-do raw stay-in-window))

;;;###autoload
(defun vs-restclient-http-send-current-raw ()
  "Sends current request and get raw result (no reformatting or syntax highlight of XML, JSON or images)."
  (interactive)
  (vs-restclient-http-send-current t))

;;;###autoload
(defun vs-restclient-http-send-current-stay-in-window ()
  "Send current request and keep focus in request window."
  (interactive)
  (vs-restclient-http-send-current nil t))

(defun vs-restclient-jump-next ()
  "Jump to next request in buffer."
  (interactive)
  (let ((last-min nil))
    (while (not (eq last-min (goto-char (vs-restclient-current-min))))
      (goto-char (vs-restclient-current-min))
      (setq last-min (point))))
  (goto-char (+ (vs-restclient-current-max) 1))
  (goto-char (vs-restclient-current-min)))

(defun vs-restclient-jump-prev ()
  "Jump to previous request in buffer."
  (interactive)
  (let* ((current-min (vs-restclient-current-min))
         (end-of-entity
          (save-excursion
            (progn (goto-char (vs-restclient-current-min))
                   (while (and (or (looking-at "^\s*\\(#.*\\)?$")
                                   (eq (point) current-min))
                               (not (eq (point) (point-min))))
                     (forward-line -1)
                     (beginning-of-line))
                   (point)))))
    (unless (eq (point-min) end-of-entity)
      (goto-char end-of-entity)
      (goto-char (vs-restclient-current-min)))))

(defun vs-restclient-mark-current ()
  "Mark current request."
  (interactive)
  (goto-char (vs-restclient-current-min))
  (set-mark-command nil)
  (goto-char (vs-restclient-current-max))
  (backward-char 1)
  (setq deactivate-mark nil))

(defun vs-restclient-show-info ()
  ;; vs-restclient-info-buffer-name
  (interactive)
  (let ((vars-at-point (vs-restclient-find-vars-before-point)))
    (cl-labels ((non-overidden-vars-at-point ()
                                             (seq-filter (lambda (v)
                                                           (null (assoc (car v) vs-restclient-var-overrides)))
                                                         vars-at-point))
                (sanitize-value-cell (var-value)
                                     (replace-regexp-in-string "\n" "|\n| |"
                                                               (replace-regexp-in-string "\|" "\\\\vert{}"
                                                                                         (vs-restclient-replace-all-in-string vars-at-point var-value))))
                (var-row (var-name var-value)
                         (insert "|" var-name "|" (sanitize-value-cell var-value) "|\n"))
                (var-table (table-name)
                           (insert (format "* %s \n|--|\n|Name|Value|\n|---|\n" table-name)))
                (var-table-footer ()
                                  (insert "|--|\n\n")))
      
      (with-current-buffer (get-buffer-create vs-restclient-info-buffer-name)
        ;; insert our info
        (erase-buffer)

        (insert "\Vs-Restclient Info\ \n\n")

        (var-table "Dynamic Variables")
        (dolist (dv vs-restclient-var-overrides)
          (var-row (car dv) (cdr dv)))
        (var-table-footer)

        ;;    (insert ":Info:\n Dynamic vars defined by request hooks or with calls to vs-restclient-set-var\n:END:")

        (var-table "Vars at current position")
        (dolist (dv (non-overidden-vars-at-point))
          (var-row (car dv) (cdr dv)))
        (var-table-footer)


        ;; registered callbacks
        (var-table "Registered request hook types")
        (dolist (handler-name (delete-dups (mapcar 'car vs-restclient-result-handlers)))
          (var-row handler-name (cddr (assoc handler-name vs-restclient-result-handlers))))
        (var-table-footer)

        (insert "\n\n'q' to exit\n")
        (org-mode)
        (org-toggle-pretty-entities)
        (org-table-iterate-buffer-tables)
        (outline-show-all)
        (vs-restclient-response-mode)
        (goto-char (point-min))))
    (switch-to-buffer-other-window vs-restclient-info-buffer-name)))

(defun vs-restclient-narrow-to-current ()
  "Narrow to region of current request"
  (interactive)
  (narrow-to-region (vs-restclient-current-min) (vs-restclient-current-max)))

(defun vs-restclient-toggle-body-visibility ()
  (interactive)
  ;; If we are not on the HTTP call line, don't do anything
  (let ((at-header (save-excursion
                     (beginning-of-line)
                     (looking-at vs-restclient-method-url-regexp))))
    (when at-header
      (save-excursion
        (end-of-line)
        ;; If the overlays at this point have 'invisible set, toggling
        ;; must make the region visible. Else it must hide the region
        
        ;; This part of code is from org-hide-block-toggle method of
        ;; Org mode
        (let ((overlays (overlays-at (point))))
          (if (memq t (mapcar
                       (lambda (o)
                         (eq (overlay-get o 'invisible) 'outline))
                       overlays))
              (outline-flag-region (point) (vs-restclient-current-max) nil)
            (outline-flag-region (point) (vs-restclient-current-max) t)))) t)))

(defun vs-restclient-toggle-body-visibility-or-indent ()
  (interactive)
  (unless (vs-restclient-toggle-body-visibility)
    (indent-for-tab-command)))

(defconst vs-restclient-mode-keywords
  (list (list vs-restclient-method-url-regexp '(1 'vs-restclient-method-face) '(2 'vs-restclient-url-face))
        (list vs-restclient-svar-regexp '(1 'vs-restclient-variable-name-face) '(2 'vs-restclient-variable-string-face))
        (list vs-restclient-evar-regexp '(1 'vs-restclient-variable-name-face) '(2 'vs-restclient-variable-elisp-face t))
        (list vs-restclient-mvar-regexp '(1 'vs-restclient-variable-name-face) '(2 'vs-restclient-variable-multiline-face t))
        (list vs-restclient-use-var-regexp '(1 'vs-restclient-variable-usage-face))
        (list vs-restclient-file-regexp '(0 'vs-restclient-file-upload-face))
        (list vs-restclient-header-regexp '(1 'vs-restclient-header-name-face t) '(2 'vs-restclient-header-value-face t))
        (list vs-restclient-response-hook-regexp '(1 ' vs-restclient-request-hook-face t)
              '(2 'vs-restclient-request-hook-name-face t)
              '(3 'vs-restclient-request-hook-args-face t))))

(defconst vs-restclient-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

(defvar vs-restclient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'vs-restclient-http-send-current)
    (define-key map (kbd "C-c C-r") 'vs-restclient-http-send-current-raw)
    (define-key map (kbd "C-c C-v") 'vs-restclient-http-send-current-stay-in-window)
    (define-key map (kbd "C-c C-n") 'vs-restclient-jump-next)
    (define-key map (kbd "C-c C-p") 'vs-restclient-jump-prev)
    (define-key map (kbd "C-c C-.") 'vs-restclient-mark-current)
    (define-key map (kbd "C-c C-u") 'vs-restclient-copy-curl-command)
    (define-key map (kbd "C-c n n") 'vs-restclient-narrow-to-current)
    (define-key map (kbd "C-c C-i") 'vs-restclient-show-info)
    map)
  "Keymap for vs-restclient-mode.")

(define-minor-mode vs-restclient-outline-mode
  "Minor mode to allow show/hide of request bodies by TAB."
  :init-value nil
  :lighter nil
  :keymap '(("\t" . vs-restclient-toggle-body-visibility-or-indent)
            ("\C-c\C-a" . vs-restclient-toggle-body-visibility-or-indent))
  :group 'vs-restclient)

(define-minor-mode vs-restclient-response-mode
  "Minor mode to allow additional keybindings in vs-restclient response buffer."
  :init-value nil
  :lighter nil
  :keymap '(("q" . (lambda ()
                     (interactive)
                     (quit-window (get-buffer-window (current-buffer))))))
  :group 'vs-restclient)

;;;###autoload
(define-derived-mode vs-restclient-mode fundamental-mode "REST Client"
  "Turn on vs-restclient mode."
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "# *")
  (set (make-local-variable 'comment-column) 48)

  (set (make-local-variable 'font-lock-defaults) '(vs-restclient-mode-keywords))
  ;; We use outline-mode's method outline-flag-region to hide/show the
  ;; body. As a part of it, it sets 'invisibility text property to
  ;; 'outline. To get ellipsis, we need 'outline to be in
  ;; buffer-invisibility-spec
  (add-to-invisibility-spec '(outline . t)))

(add-hook 'vs-restclient-mode-hook 'vs-restclient-outline-mode)

(provide 'vs-restclient)

(eval-after-load 'helm
  '(ignore-errors (require 'vs-restclient-helm)))

(eval-after-load 'jq-mode
  '(ignore-errors (require 'vs-restclient-jq)))

;;; vs-restclient.el ends here
