;;; company-cracker.el --- company-mode backend for Crystal (using cracker)

;; Copyright (C) 2017-2018
;; Keywords: languages
;; Package-Version: 20180115.2233
;; Package-Requires: ((company "0.9.0") (crystal-mode "0.1.0"))
;;; Commentary:

;;; Code:

(require 'crystal-mode)
(require 'company)
(require 'company-template)
(require 'json)

(defgroup company-cracker nil
  "Code completion, goto-definition and docs browsing for Crystal via cracker."
  :link '(url-link "https://github.com/TechMagister/emacs-racer/")
  :group 'company)

(defcustom company-cracker-cmd
     (or (executable-find "cracker")
                "/usr/local/bin/cracker")
     "Path to the cracker binary."
     :type 'file
     :group 'company-cracker)

(defcustom company-cracker-lib-path nil
  "Path to the crystal source"
  :type 'string
  :group 'company-cracker)

(defvar company-cracker--source-paths '())

(defcustom company-cracker-begin-after-member-access t
  "When non-nil, automatic completion will start whenever the current
symbol is preceded by a \".\", ignoring `company-minimum-prefix-length'."
  :type 'boolean
  :group 'company-cracker)

(defcustom company-cracker-insert-arguments t
  "When non-nil, insert function or method arguments as a template after completion."
  :type 'boolean
  :group 'company-cracker)

(defcustom company-cracker-show-annotation nil
  "Show an annotation inline with the candidate."
  :type 'boolean
  :group 'company-cracker)

(defun company-cracker--insert-arguments (meta)
  "Insert arguments when META is a function or a method."
  (when (string-match "^[^(]+\\(.*\\)" meta)
    (let ((args (company-cracker--extract-arguments (match-string 1 meta))))
      (insert args)
      (company-template-c-like-templatify args))))

(defun company-cracker--extract-arguments (str)
  "Extract arguments with parentheses from STR."
  (let ((len (length str))
        (pos 1)
        (pirs-paren 1))
    (while (and (/= pirs-paren 0) (< pos len))
      (let ((c (substring-no-properties str pos (1+ pos))))
        (cond
         ((string= c "(") (setq pirs-paren (1+ pirs-paren)))
         ((string= c ")") (setq pirs-paren (1- pirs-paren))))
        (setq pos (1+ pos))))
    (substring-no-properties str 0 pos)))

(defun company-cracker--format-meta (candidate)
  (let ((name (plist-get candidate :name))
        (file (plist-get candidate :file))
        (location (plist-get candidate :location))
        (type (plist-get candidate :type)))
    (if (string-equal type "Function")
        (progn
          (setq split (split-string name "#"))
          (if (eq (length split) 1)
              (setq split (split-string name "\\.")))
          (nth 1 split))
      name)))

(defun company-cracker--invoke-autocomplete ()
  (let ((temp-buffer (generate-new-buffer "*cracker*")))
    (prog2
        (apply #'call-process-region
               (point-min)
               (+ (point) 1)
               company-cracker-cmd
               nil
               temp-buffer
               nil
               '("client" "--context"))
        (with-current-buffer temp-buffer (buffer-string))
      (kill-buffer temp-buffer))))

(defun company-cracker--add-path ()
  (when (and (derived-mode-p 'crystal-mode)
             (process-live-p (get-process "cracker-server")))
    (let ((source-path (file-truename (crystal-find-project-root))))
      (message source-path)
      (unless (member source-path company-cracker--source-paths)
        (call-process
         company-cracker-cmd nil "*cracker-client*" nil
         "client" "--add-path" source-path)
        (when (zerop )
          (setq company-cracker--source-paths
                (append company-cracker--source-paths '(source-path))))))))

(defun company-cracker--make-candidate (candidate)
  "Prepare and format CANDIDATE."
  (setq name (plist-get candidate :name))
  (setq type (plist-get candidate :type))
  (setq contents (if (string-equal type "Function")
                     (progn
                       (setq split (split-string name "#"))
                       (if (eq (length split) 1)
                           (setq split (split-string name "\\.")))
                       (nth 1 split))
                   name))
  (setq contents (nth 0 (split-string contents "(")))
  ;;(if (and (string-equal type "Function")
  ;;         (not (string-match "()" name)))
  ;;    (setq contents (concat contents "(")))
  (propertize contents
              'meta (company-cracker--format-meta candidate)))

(defun company-cracker--syntax-highlight (str)
  "Apply syntax highlighting to STR."
  ;; If the user has disabled font-lock, respect that.
  (if global-font-lock-mode
      (with-temp-buffer
        (insert str)
        (delay-mode-hooks (crystal-mode))
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))
        (buffer-string))
    str))

(defun company-cracker--candidates ()
  (let ((json-object-type 'plist))
    (setq raw (json-read-from-string
                  (company-cracker--invoke-autocomplete))))
  (setq results (append (plist-get raw :results) nil))
  (setq final (list))
  (dolist (candidate results)
    (push (company-cracker--make-candidate candidate) final))
  final)

(defun company-cracker--prefix ()
  "Return the symbol to complete.
Also, if point is on a dot, triggers a completion immediately."
  (if company-cracker-begin-after-member-access
      (company-grab-symbol-cons "\\." 1)
    (company-grab-symbol)))

(defun company-cracker--annotation (meta)
  "Do some stuff with META."
  (if (string-match "()" meta)
      (if (string-match ":" meta)
          (concat ":" (nth 1 (split-string meta ":")))
        "")
    (if (string-match "(" meta)
        (concat "(" (nth 1 (split-string meta "(")))
      "")))

;;;###autoload
(defun company-cracker (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-cracker))
    (prefix (and (derived-mode-p 'crystal-mode)
                 (not (company-in-string-or-comment))
                 (or (company-cracker--prefix) 'stop)))
    (meta
     (company-cracker--syntax-highlight (get-text-property 1 'meta arg)))
    (candidates (company-cracker--candidates))
    (annotation
     (when company-cracker-show-annotation
       (company-cracker--annotation (get-text-property 0 'meta arg))))
    (sorted t)
    (post-completion
     (when (and company-cracker-insert-arguments
                (not (char-equal ?\( (following-char))))
       (let ((meta (get-text-property 0 'meta arg)))
         (unless (string-match "()" meta)
           (company-cracker--insert-arguments meta)))))))

;;;###autoload
(defun company-cracker-init ()
  "Start cracker server."
  (interactive)
  (unless (process-live-p (get-process "cracker-server"))
    (let ((crystal-source-path
           (if company-cracker-lib-path
               company-cracker-lib-path
             (nth 0 (split-string (shell-command-to-string "crystal env CRYSTAL_PATH") ":")))))
      (start-process "cracker-server" "*cracker-server*"
                     company-cracker-cmd "server" crystal-source-path))))

(provide 'company-cracker)
;;; company-cracker.el ends here
