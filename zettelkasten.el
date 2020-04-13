;;; zettelkasten.el --- Functions to maintain a zettel-archive -*- lexical-binding: t -*-

;; Copyright (C) 2017 Jan Ole Bangen.

;; Author: Jan Ole Bangen <jobangen@gmail.com>
;; URL:
;; Package-Version: 20170918.2122
;; Version: 0.5.0
;; Package-Requires: hydra
;; Keywords: Archive

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Zettelkasten
;;
;;; Code:
(require 'deadgrep)
(require 'hydra)
(require 's)
(require 'org-el-cache)
(require 'org-element)

(defcustom zettelkasten-main-directory
  (expand-file-name (convert-standard-filename "zettelkasten/") user-emacs-directory)
  "Path for main directory"
  :group 'zettelkasten
  :type '(string))

(defcustom zettelkasten-zettel-directory
  (expand-file-name (convert-standard-filename "zettel/") zettelkasten-main-directory)
  "Path for Zettel"
  :group 'zettelkasten
  :type '(string))

(defcustom zettelkasten-temp-directory
  (expand-file-name (convert-standard-filename "temp/") zettelkasten-main-directory)
  "Path for Zettel"
  :group 'zettelkasten
  :type '(string))

(defcustom zettelkasten-tags-directory
  (expand-file-name (convert-standard-filename "tags/") zettelkasten-temp-directory)
  "Path for temporary files with tags"
  :group 'zettelkasten
  :type '(string))

(defcustom zettelkasten-similarities-directory
  (expand-file-name (convert-standard-filename "similarities/") zettelkasten-temp-directory)
  "Path for files with lists of similarities"
  :group 'zettelkasten
  :type '(string))

(defcustom zettelkasten-bibliography-file "~/biblio.bib"
  "Path to bibfile"
  :group 'zettelkasten
  :type '(string))

;; Creation and (re)naming of zettel
(push '("z" "Zettel append" plain
        (file (lambda ()
                (read-file-name "Append to: " zettelkasten-zettel-directory
                                "zettelkasten-inbox.org")))
        "** TODO %?
%i")
      org-capture-templates)

(push '("Z" "Zettel" plain
        (file (lambda ()
                (let ((name (or zettel-capture-filename (read-string "Name: "))))
                  (expand-file-name
                   (concat zettelkasten-zettel-directory
                           (format-time-string "%Y-%m-%d-%H%M-")
                           name
                           ".txt")))))
        (function zettelkasten-zettel-template)
        :immediate-finish t
        :jump-to-captured t)
      org-capture-templates)


(defun zettelkasten--title-to-fname (title)
  (s-replace-all
   '((" " . "-")
     (":" . "") ("." . "") ("," . "") (";" . "")
     ("?" . "") ("!" . "")
     ("\"" . "") ("'" . "")
     ("ß" . "ss") ("ä" . "ae")
     ("ü" . "ue") ("ö" . "oe"))
   (s-downcase title)))
  
;;;###autoload
(defun zettelkasten-new-zettel ()
  "Capture a Zettel with org-capture"
  (interactive)
  (let ((zettel-title
         (read-string "Title: ")))
    (setq zettel-capture-filename
          (zettelkasten--title-to-fname zettel-title))
    (org-capture nil "Z")
    (end-of-line)
    (insert zettel-title))
  (setq zettel-capture-filename nil)
  (save-buffer))

(defun zettelkasten-zettel-template ()
  "#+TITLE: 
#+DATE: %U

* Schlagwörter
tags: %^{Type|@index|@content|@proj},

* 
%i

* Refile
")

;;;###autoload
(defun zettelkasten-subtree-refile ()
  "Refile subtree to Zettel"
  (interactive)
  (org-back-to-heading)
  (org-todo "TODO")
  (org-set-tags '("refile"))
  (org-cut-subtree)
  (if (equal (buffer-name) "zettelkasten-inbox.org")
      (find-file (cdr (zettelkasten--select-zettel)))
    (ivy-read "Links: "
              (zettelkasten-links-in-buffer)
              :action
              (lambda (selection)
                (find-file (cdr selection)))))
  (goto-char (point-max))
  (org-paste-subtree 2)
  (save-buffer)
  (previous-buffer))

;;;###autoload
(defun zettelkasten-subtree-refile-current-file ()
  (interactive)
  (counsel-outline)
  (org-todo "")
  (org-set-tags '())
  (org-cut-subtree)
  (counsel-outline)
  (end-of-line)
  (newline)
  (org-paste-subtree))

;; obsolete
;;;###autoload
(defun zettelkasten-rename-zettel-upd-links ()
  (interactive)
  (if (equal major-mode 'dired-mode)
      (dired-find-file))
  (kill-new (file-name-base))
  (let ((old-name-base
         (file-name-base))
        (new-name-base
         (read-string "New Name: ")))
    (rename-file (concat old-name-base ".txt") (concat new-name-base ".txt"))
    (kill-buffer)
    (grep-compute-defaults)
    (lgrep old-name-base "*.txt" zettelkasten-zettel-directory nil)
    (other-window 1)
    (if (y-or-n-p "Do you want to replace basenames? ")
        (progn
          (wgrep-change-to-wgrep-mode)
          (search-forward "txt:" nil t)
          (condition-case nil
              (while (search-forward old-name-base nil t)
                (replace-match new-name-base))
            (error nil))
          (wgrep-finish-edit))
      (message "Changed filename without updating links."))
    (find-file (concat zettelkasten-main-directory "/zettelkasten-log.csv"))
    (goto-char (point-min))
    (insert (format-time-string "%Y-%m-%d-%H%M") ", " old-name-base ".txt, " new-name-base ".txt\n")
    (bury-buffer)
    (save-some-buffers)))

;;; Open from Zettel
(org-link-set-parameters "zk" :follow #'org-zettelkasten-open)

(defun org-zettelkasten-open (path)
  (find-file (concat zettelkasten-zettel-directory path "*") t))

;;;###autoload
(defun zettelkasten-deadgrep-backlinks ()
  (interactive)
  (let ((zettel (file-name-base)))
    (counsel-ag
     (car (split-string zettel "\s")))))

(defun zettelkasten-link-hint-loop ()
  (interactive)
  (goto-char (point-min))
  (ignore-errors
    (while t
      (sleep-for 1)
      (link-hint-open-link)
      )))


;;; https://emacs.stackexchange.com/questions/21713/how-to-get-property-values-from-org-file-headers
;;; refactor!
(defun org-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

;;;###autoload
(defun zettelkasten-insert-link (&optional file)
  (interactive)
  (let* ((zettel
          (or file (read-file-name "Zettel: " zettelkasten-zettel-directory)))
         (zettel-id
          (s-left 15 (file-name-base zettel)))
         (zettel-title
          (with-temp-buffer
            (insert-file-contents zettel)
            (org-element-property :value (car (org-global-props "TITLE"))))))
    (insert (concat "[[zk:" zettel-id "][" zettel-title "]]"))))

;;;###autoload
(defun zettelkasten-create-zettel-insert-link-at-point ()
  (interactive)
  (zettelkasten-new-zettel)
  (let ((link-target
         buffer-file-name))
    (previous-buffer)
    (zettelkasten-insert-link link-target))
  (left-char)
  (org-open-at-point))

;;;###autoload
(defun zettelkasten-push-link-to-current ()
  (interactive)
  (let ((link-target (buffer-file-name)))
    (find-file (cdr (zettelkasten--select-zettel)))
    (goto-char (point-max))
    (org-insert-heading)
    (zettelkasten-insert-link link-target)
    (org-todo "TODO")
    (org-set-tags '("refile")))
  (previous-buffer))


;;;###autoload
(defun zettelkasten-zettel-open-similarities ()
  (interactive)
  (find-file
   (concat zettelkasten-similarities-directory "sim-" (buffer-name))))

;; Dirs and Queries
;;;###autoload
(defun zettelkasten-open-dir ()
  (interactive)
  (find-file (read-file-name "Zettel: " zettelkasten-zettel-directory)))

;;;###autoload
(defun zettelkasten-ag-query ()
   (interactive)
   (counsel-ag nil zettelkasten-zettel-directory nil))

;;;###autoload
(defun zettelkasten-ag-query-symbol-at-point ()
  (interactive)
  (counsel-ag (thing-at-point 'symbol) zettelkasten-zettel-directory nil))

;; Handling of Tags
(defvar zettelkasten-tags-values)

;;;###autoload
(defun zettelkasten-parse-tags-values ()
  (interactive)
  (setq zettelkasten-tags-values (zettelkasten-collect-tags-values)))

(defun zettelkasten-collect-tags-values (&optional regexp)
  "Collect values in keywords fields of all BibTeX entries.
Maybe restrict the values to those matching REGEXP. Keywords may be phrases
separated by commas. Multiple spaces or newlines within a keyword will be
removed before collection."
  (save-excursion
    (goto-char (point-min))
    (let (zk-tags kstring)
      (while (re-search-forward "^tags:\\s-*\\(.*\\),$" nil t)
        ;; TWS - remove newlines/multiple spaces:
        (setq kstring (replace-regexp-in-string "[ \t\n]+" " "
                                                (match-string-no-properties 1)))
        (mapc
         (lambda (v)
           (if regexp (if (string-match regexp v)
                          (add-to-list 'zk-tags v t))
             (add-to-list 'zk-tags v t)))
         (split-string kstring ",[ \n]*\\|{\\|}" t)))
      zk-tags)))

;;;###autoload
(defun zettelkasten-parse-values-combined ()
  (interactive)
  (zettelkasten-combine-zettel)
  (find-file (expand-file-name
              (concat zettelkasten-main-directory "/zettel-combined.txt")))
  (zettelkasten-parse-tags-values)
  (kill-current-buffer))

;; Edit zettel
;;;###autoload
(defun zettelkasten-add-tags (&optional arg)
  "Make a keywords field.
If ARG is nil, ask for each keyword and offer completion over
keywords that are already available in the buffer.  Inserting
the empty string will quit the prompt. If the keyword is not already
present in the buffer, it will be added to the local variable
bu-keywords-values. Note that if you use ido-ubiquitous, the value of
  `ido-ubiquitous-enable-old-style-default' is temporarily set to t within
the body of this command."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (search-forward "tags:" nil t)
    (end-of-line)
    (insert " ")
    (let ((elist (save-excursion))
          append)
      (if (assoc "zk-tags" elist)
          (progn (setq append t)))
      (unless arg
        (let ((cnt 0)
              k)
          (while (and (setq k (completing-read
                               "Tags (RET to quit): " zettelkasten-tags-values nil))
                      (not (equal k "")))
            (when append (insert " ")
                  (setq append nil))
            (setq cnt (1+ cnt))
            (insert (format "%s%s," (if (> cnt 1) " " "") k))
            (add-to-list 'zettelkasten-tags-values k)))))))

;;;###autoload
(defun zettelkasten-insert-tags (&optional arg)
  "Make a keywords field.
If ARG is nil, ask for each keyword and offer completion over
keywords that are already available in the buffer.  Inserting
the empty string will quit the prompt. If the keyword is not already
present in the buffer, it will be added to the local variable
bu-keywords-values. Note that if you use ido-ubiquitous, the value of
  `ido-ubiquitous-enable-old-style-default' is temporarily set to t within
the body of this command."
  (interactive "P")
  (if (boundp 'zettelkasten-tags-values)
      nil
    (zettelkasten-parse-values-combined))
  (let ((elist (save-excursion))
        append)
    (goto-char (point-min))
    ;; goto tags
    (search-forward "tags: " nil nil)
    ;; goto last 'formschlagwort'
    (end-of-line)
    (insert " ")
    (if (assoc "zk-tags" elist)
        (progn (setq append t)))
    (unless arg
      (let ((cnt 0)
            k)
        (while (and (setq k (completing-read
                             "Tags (RET to quit): " zettelkasten-tags-values nil))
                    (not (equal k "")))
          (when append (insert " ")
                (setq append nil))
          (setq cnt (1+ cnt))
          (insert (format "%s%s," (if (> cnt 1) " " "") k))
          (zettelkasten-sort-tags)
          (goto-char (point-min))
          ;; goto tags
          (search-forward "tags: " nil nil)
          ;; goto last 'formschlagwort'
          (end-of-line)
          (add-to-list 'zettelkasten-tags-values k))))))

;;;###autoload
(defun zettelkasten-sort-tags ()
  (interactive "*")
  (text-mode)
  (goto-char (point-min))
  (search-forward "tags: " nil nil)
  (end-of-line)
  (setq my-end (point))
  (search-backward "@" nil nil)
  (setq my-beg (point))
  (save-restriction
    (narrow-to-region my-beg my-end)
    (while (search-forward ", " nil t)
      (replace-match ",
"))
    (goto-char (point-min))
    (forward-line)
    (sort-lines nil (point) my-end)
    (my/unfill-region my-beg my-end)
    (delete-trailing-whitespace))
  (org-mode))

;;;###autoload
(defun zettelkasten-finish-zettel ()
  "Zettelkasten: delete whitespace, save, kill buffer."
  (interactive)
  (zettelkasten-sort-tags)
  (delete-trailing-whitespace)
  (save-buffer)
  (kill-buffer))

;;;###autoload
(defun zettelkasten-zettel-store-link ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (right-char 9)
    (kill-ring-save (point) (line-end-position))
    (call-interactively 'org-store-link)))

;; Zettel output
;;;###autoload
(defun zettelkasten-tangle-combined ()
  (interactive)
  (org-babel-tangle-file
   (concat zettelkasten-main-directory "/zettel-combined.txt")))

;; Shell-interaction
;;;###autoload
(defun zettelkasten-combine-zettel ()
  (interactive)
  (shell-command-to-string
   (concat "cat " zettelkasten-zettel-directory "*.txt > " zettelkasten-main-directory "/zettel-combined.txt")))

;;;###autoload
(defun zettelkasten-gitstats ()
  (interactive)
  (shell-command-to-string (concat "cd ~/Dropbox/db/zk/zettel &&"
                                   "gitstats .git gitstats &&"
                                   "firefox 'gitstats/index.html'")))

;; convenience functions
;;;###autoload
(defun zettelkasten-name-of-the-file ()
  "Gets the name of the file the current buffer is based on."
  (interactive)
  (insert
   (file-name-base (buffer-file-name (window-buffer (minibuffer-selected-window))))))

;;;###autoload
(defun zk-link-wrapper ()
  (interactive)
  (ivy-read "Links"
            (zettelkasten-links-in-buffer "zk:[0-9-]+\\|file:.+.txt\\|autocite:[0-9a-zA-Z-]+")
            :sort nil
            :action 'zettelkasten-open-file-from-linklist))


(defun zettelkasten-open-file-from-linklist (link)
  (let ((zk-list
         (split-string link ":")))
    (find-file (concat (nth 1 zk-list) "*") t)))

;;;###autoload
(defun zettelkasten-links-in-buffer (regexp &optional buffer)
  "return a list of matches of REGEXP in BUFFER or the current buffer if not given."
  (interactive)
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp regexp nil t 1)
              (push (match-string-no-properties 0) matches)))))
      matches)))

;;; Cache
(defun zettelkasten--extract-title (filename el)
  (org-element-map el 'keyword
    (lambda (kw)
      (if (string= (org-element-property :key kw) "TITLE")
          (org-element-property :value kw)))
    :first-match t))

(def-org-el-cache
  zettelkasten-cache
  (list zettelkasten-zettel-directory)
  (lambda (filename el)
    (list
     :file filename
     :title (zettelkasten--extract-title filename el)
     :id (s-left 15 (file-name-base filename))))
)

;; Update / Initialize the cache
(add-hook 'after-save-hook (lambda () (org-el-cache-update zettelkasten-cache)))

;;;###autoload
(defun zettelkasten-select-zettel ()
  (interactive)
  (ivy-read
   "Zettel: "
   (org-el-cache-map
    zettelkasten-cache
    (lambda (filename entry)
      (cons entry filename)))
   :action
   (lambda (selection)
     (find-file (cdr selection)))))

(defun org-el-cache--find (paths &optional include-archives)
  "Generate shell code to search PATHS for org files.
If INCLUDE-ARCHIVES is non-nil, org_archive files are included,
too."
  (if include-archives
      (format
       "find %s -name \"[a-Z0-9_]*.org\" -o -name \"[a-Z0-9_]*.org_archive\" "
       (mapconcat 'identity paths " "))
    (format
     "find %s -name \"[a-Z0-9_]*.txt\" -o -name \"[a-Z0-9_]*.org\""
     (mapconcat 'identity paths " "))))









;;;###autoload
(defhydra hydra-zettelkasten (:hint t
                                    :color pink)
  "
 ^Zettelkasten^ ^ ^              ^Zettel^              ^Var^
-^---^---------------------------^-^-------------------^-^-------------------
 _d_: zk         _z_: new zettel  _s_: sort tags        _r_: remem
 _b_: bibtex     _q_: query       _t_: add tags         _b_: Open Bibkey
 _k_: kill bfs  _xq_: txt query  _xs_: finish           _f_: Open Files
 _r_: recentf   ^ ^               _o_: org-noter        _i_: Open Similarities
 ^ ^            ^ ^               _l_: store link       _j_: join line
"
  ;;General
  ("C-s" counsel-grep-or-swiper)
  ("C-m" job/open-at-point)
  ("<tab>" org-next-link "next link")
  ("C-<tab>" org-previous-link "prev link")
  ("b" zettelkasten-zettel-open-bibkey)
  ("d" zettelkasten-open-dir)
  ("f" zettelkasten-zettel-open-files)
  ("g" nil "Quit")
  ("j" join-line)
  ("i" zettelkasten-zettel-open-similarities)
  ("k" projectile-kill-buffers) ;;proj
  ("l" zettelkasten-zettel-store-link)
  ("o" org-noter)
  ("q" zettelkasten-ag-query)                 ;;proj
  ("Q" zettelkasten-ag-query-symbol-at-point) ;;(proj)
  ("r" remem-toggle)                          ;var
  ("s" zettelkasten-sort-tags)                ;zet
  ("r" projectile-recentf)
  ("t" zettelkasten-add-tags)           ;zet
  ("xq" zettelkasten-txt-query)         ;;var
  ("xs" zettelkasten-finish-zettel)     ;zet
  ("z" zettelkasten-new-zettel)
  )

(provide 'zettelkasten)
;;; zettelkasten.el ends here
