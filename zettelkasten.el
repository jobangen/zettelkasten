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

(defun zettelkasten-zettel-template ()
  "#+TITLE: 
#+DATE: %U
#+COLLECTION: %^{Type|index|content|proj}
#+DESCRIPTOR:

* 
%i

* Refile")

(defcustom zettelkasten-descriptor-chain-sep ">"
  "Char that separates chained descriptors"
  :group 'zettelkasten
  :type '(string))

(defcustom zettelkasten-descriptor-cycle-threshold 5
  "Number of remaining Zettel, that break the descriptor cycle"
  :group 'zettelkasten
  :type 'integer)

(defcustom zettelkasten-context-filter-list '()
  "List of context filter"
  :group 'zettelkasten
  :type 'list)

(defun zettelkasten-context-work-fun (entry)
  (and (not (member "journal" (plist-get entry :collections)))
       (not (member "Rezept" (plist-get entry :collections)))))


(setq zettelkasten-context-filter-list
      '(("All" . (lambda (entry) t))
        ("Work" . zettelkasten-context-work-fun)))

(defvar zettelkasten-context-filter '("All" . (lambda (entry) t)))

;;;###autoload
(defun zettelkasten-set-context-filter ()
  (interactive)
  (ivy-read "Filter" zettelkasten-context-filter-list
            :action
            (lambda (selection)
              (setq zettelkasten-context-filter selection))))


;; Creation and (re)naming of zettel
(push '("z" "Zettel append" plain
        (file (lambda ()
                (cdr (zettelkasten--select-zettel (zettelkasten--get-all-zettel)))))
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
                           ".org")))))
        (function zettelkasten-zettel-template)
        :immediate-finish t
        :jump-to-captured t)
      org-capture-templates)

(defun zettelkasten--title-to-fname (title)
  (s-replace-all
   '((" " . "-") ("/". "-")
     (":" . "") ("." . "") ("," . "") (";" . "")
     ("?" . "") ("!" . "")
     ("\"" . "") ("'" . "")
     ("&" . "und")
     ("\(" . "") ("\)" . "")
     ("ß" . "ss") ("ä" . "ae")
     ("ü" . "ue") ("ö" . "oe")
     ("é" . "e") ("ó" . "o"))
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

(defun zettelkasten--linked-in-zettel (zettel)
  (let ((link-list
         (plist-get (org-el-cache-get zettelkasten-cache zettel) :links)))
    (org-el-cache-select
     zettelkasten-cache
     (lambda (filename entry)
       (member filename link-list)))))

;;;###autoload
(defun zettelkasten-refile ()
  (interactive)
  (let ((zettel-linked (zettelkasten--linked-in-zettel (buffer-file-name)))
        (zettel-all (zettelkasten--get-all-zettel)))
    (cond ((equal (buffer-name) "zettelkasten-inbox.org")
           (zettelkasten-refile-to-zettel zettel-all))
          ((not zettel-linked)
           (zettelkasten-refile-to-headline))
          ((yes-or-no-p "Refile to headline?")
           (zettelkasten-refile-to-headline))
          (t (zettelkasten-refile-to-zettel zettel-linked)))))

(defun zettelkasten-refile-to-headline ()
  (org-back-to-heading)
  (org-todo "")
  (org-set-tags '())
  (org-cut-subtree)
  (counsel-outline)
  (end-of-line)
  (newline)
  (org-paste-subtree))

(defun zettelkasten-refile-to-zettel (zettel-lst)
  (org-back-to-heading)
  (org-todo "TODO")
  (org-set-tags '("refile"))
  (org-cut-subtree)
  (ivy-read "Links: "
            (zettelkasten--get-cons-title-fname zettel-lst)
            :action
            (lambda (selection)
              (find-file (cdr selection))))
  (goto-char (point-max))
  (org-paste-subtree 2)
  (zettelkasten-refile)
  (save-buffer))


;;; Open from Zettel
(org-link-set-parameters "zk" :follow #'org-zettelkasten-open)

(defun org-zettelkasten-open (path)
  (let ((zettel-entry
         (car (org-el-cache-select
               zettelkasten-cache
               (lambda (filename entry)
                 (string= path (plist-get entry :id)))))))
    (find-file (plist-get zettel-entry :file))))

;;; https://emacs.stackexchange.com/questions/21713/how-to-get-property-values-from-org-file-headers
;;; refactor!
(defun org-global-props (&optional property buffer)
  "Get the alists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

;;;###autoload
(defun zettelkasten-insert-link-at-point (&optional link-target)
  (interactive)
  (let* ((zettel
          (or link-target
              (cdr (zettelkasten--select-zettel (zettelkasten--get-all-zettel)))))
         (zettel-data
          (zettelkasten-cache-query-filename zettel))
         (zettel-id
          (plist-get zettel-data :id))
         (zettel-title
          (read-string "Title: "
                       (plist-get zettel-data :title))))
    (insert (format "[[zk:%s][%s]]" zettel-id zettel-title))))

;;;###autoload
(defun zettelkasten-new-zettel-insert-link-at-point ()
  (interactive)
  (zettelkasten-new-zettel)
  (let ((link-target
         buffer-file-name))
    (previous-buffer)
    (kill-region
     (region-beginning)
     (region-end))
    (zettelkasten-insert-link-at-point link-target))
  (left-char)
  (org-open-at-point))

;;;###autoload
(defun zettelkasten-push-link-to-current ()
  (interactive)
  (let ((link-target (buffer-file-name)))
    (find-file (cdr (zettelkasten--select-zettel (zettelkasten--get-all-zettel))))
    (goto-char (point-max))
    (org-insert-heading)
    (zettelkasten-insert-link-at-point link-target)
    (org-todo "TODO")
    (org-set-tags '("refile"))
    (org-cycle-level))
  (previous-buffer))

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

;;;###autoload
(defun zettelkasten-sort-tags ()
  (interactive "*")
  (text-mode)
  (goto-char (point-min))
  (unless (search-forward "#+DESCRIPTOR:" nil t)
    (goto-char (point-max))
    (search-backward-regexp "^#\\+")
    (end-of-line)
    (newline)
    (insert "#+DESCRIPTOR:"))
  (end-of-line)
  (setq my-end (point))
  (beginning-of-line)
  (setq my-beg (point))
  (save-restriction
    (narrow-to-region my-beg my-end)
    (while (search-forward " " nil t)
      (replace-match "
"))
    (goto-char (point-min))
    (forward-line)
    (sort-lines nil (point) my-end)
    (my/unfill-region my-beg my-end)
    (delete-trailing-whitespace))
  (org-mode))

;;;###autoload
(defun zettelkasten-zettel-add-collection (&optional collection)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^#\\+COLLECTION:" nil t)
        (end-of-line)
      (search-forward-regexp "^#\\+DATE:")
      (end-of-line)
      (newline)
      (insert "#+COLLECTION:"))
    (insert (concat " "
                    (or collection
                        (completing-read
                         "Collection: "
                         (zettelkasten-cache-get-collection-values))))))
  (unless collection
    (zettelkasten-zettel-add-collection)))

;;;###autoload
(defun zettelkasten-zettel-add-descriptor (&optional descriptor)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward "#+DESCRIPTOR:" nil t)
      (goto-char (point-max))
      (search-backward-regexp "^#\\+")
      (end-of-line)
      (newline)
      (insert "#+DESCRIPTOR:"))
    (insert (concat " "
                    (or descriptor
                        (completing-read
                         "Descriptor: "
                         (zettelkasten-cache-get-descriptor-values))))))
  (zettelkasten-sort-tags)
  (unless descriptor
    (zettelkasten-zettel-add-descriptor)))

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
   (concat zettelkasten-main-directory "/zettel-combined.org")))

;;;###autoload
(defun zettelkasten-gitstats ()
  (interactive)
  (shell-command-to-string (concat "cd ~/Dropbox/db/zk/zettel &&"
                                   "gitstats .git gitstats &&"
                                   "firefox 'gitstats/index.html'")))

;;;###autoload
(defun zettelkasten-links-in-file (filename)
  "Return list of linked zettel-ids."
  (let ((matches))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char 1)
      (while (search-forward-regexp "zk:[0-9-]+" nil t 1)
        (push (plist-get
               (car
                (zettelkasten-cache-query-key-value
                 :id
                 (s-chop-prefix
                  "zk:"
                  (match-string-no-properties 0))))
               :file)
              matches)))
    matches))

;;; Cache
(defun zettelkasten-extract-value (data type keyword)
  (org-element-map data type
    (lambda (kw)
      (if (string= (org-element-property :key kw) keyword)
          (org-element-property :value kw)))
    :first-match t))

(defun zettelkasten-extract-title (filename data)
  (zettelkasten-extract-value data 'keyword "TITLE"))

(defun zettelkasten-extract-collections (filename data)
  (ignore-errors
    (let* ((collection-string
            (zettelkasten-extract-value data 'keyword "COLLECTION"))
           (collection-split
            (split-string collection-string))
           (collection-list nil))
      (dolist (descriptor collection-split)
        (if (s-contains? zettelkasten-descriptor-chain-sep descriptor)
            (progn
              (let* ((chain-split
                      (split-string descriptor zettelkasten-descriptor-chain-sep))
                     (chain-part (car chain-split)))
                (push chain-part collection-list)
                (pop chain-split)
                (dolist (descriptor chain-split)
                  (setq chain-part
                        (concat
                         chain-part zettelkasten-descriptor-chain-sep descriptor))
                  (push chain-part collection-list))))
          (push descriptor collection-list)))
      collection-list)))

(defun zettelkasten-extract-descriptors (filename data)
  (ignore-errors
    (let* ((descriptor-string
            (zettelkasten-extract-value data 'keyword "DESCRIPTOR"))
           (descriptor-split
            (split-string descriptor-string))
           (descriptor-list nil))
      (dolist (descriptor descriptor-split)
        (if (s-contains? zettelkasten-descriptor-chain-sep descriptor)
            (progn
              (let* ((chain-split
                      (split-string descriptor zettelkasten-descriptor-chain-sep))
                     (chain-part (car chain-split)))
                (push chain-part descriptor-list)
                (pop chain-split)
                (dolist (descriptor chain-split)
                  (setq chain-part
                        (concat
                         chain-part zettelkasten-descriptor-chain-sep descriptor))
                  (push chain-part descriptor-list))))
          (push descriptor descriptor-list)))
      descriptor-list)))

(defun zettelkasten-extract-id (filename)
  (let ((fname-chop
         (s-chop-prefix
          zettelkasten-zettel-directory
          (s-chop-suffix ".org" filename))))
    (cond ((s-prefix? "txt" fname-chop)
           (s-chop-prefix "txt/" fname-chop))
          ((s-prefix? "jr" fname-chop)
           (s-chop-prefix "jr/" fname-chop))
          (t (s-left 15 fname-chop)))))


(def-org-el-cache
  zettelkasten-cache
  (list zettelkasten-zettel-directory)
  (lambda (filename data)
    (list
     :file filename
     :title (zettelkasten-extract-title filename data)
     :id (zettelkasten-extract-id filename)
     :collections (zettelkasten-extract-collections filename data)
     :descriptors (zettelkasten-extract-descriptors filename data)
     :links (zettelkasten-links-in-file filename))))

;; Update / Initialize the cache
(add-hook 'after-save-hook (lambda () (org-el-cache-update zettelkasten-cache)))


(defun zettelkasten--get-all-zettel ()
  (org-el-cache-select
   zettelkasten-cache
   (lambda (filename entry)
     (funcall (cdr zettelkasten-context-filter) entry))))

(defun zettelkasten--get-collection-zettel ()
  (let ((collection
         (completing-read "Collection: "
                          (zettelkasten-cache-get-collection-values))))
    (org-el-cache-select
     zettelkasten-cache
     (lambda (filename entry)
       (member collection (plist-get entry :collections))))))

(defun zettelkasten--get-descriptor-zettel (&optional entries)
  (let* ((descriptor
          (completing-read
           (format "Descriptor [%s]: " (if entries
                                           (safe-length entries)
                                         nil))
           (if entries
               (append (zettelkasten-cache-get-descriptor-values entries)
                       '("Break"))
             (zettelkasten-cache-get-descriptor-values))))
         (zettel
          (if entries
              (-filter
               (lambda (entry)
                 (member descriptor (plist-get entry :descriptors)))
               entries)
            (org-el-cache-select
             zettelkasten-cache
             (lambda (filename entry)
               (member descriptor (plist-get entry :descriptors)))))))
    (if (string= descriptor "Break")
        entries
      (if (<= (safe-length zettel) zettelkasten-descriptor-cycle-threshold)
          zettel
        (zettelkasten--get-descriptor-zettel zettel)))))

(defun zettelkasten--select-zettel (zettel)
  (ivy-read
   "Zettel: "
   (mapcar (lambda (arg)
             (cons
              (plist-get arg :title)
              (plist-get arg :file)))
           zettel)
   :preselect "Inbox"
   :action
   (lambda (selection)
     (setq zettelkasten-zettel-selected selection)))
  zettelkasten-zettel-selected)

(defun zettelkasten-cache-get-collection-values ()
  (let ((collections nil))
    (org-el-cache-each
     zettelkasten-cache
     (lambda (filename data)
       (setq collections
             (cons
              (plist-get data :collections)
              collections))))
    (delete-dups (-flatten collections))))

(defun zettelkasten-cache-get-descriptor-values (&optional entries)
  (delete-dups
   (-flatten
    (if (not entries)
        (org-el-cache-map
         zettelkasten-cache
         (lambda (filename entry)
           (plist-get entry :descriptors)))

      (mapcar
       (lambda (arg)
         (plist-get arg :descriptors))
       entries)))))


(defun zettelkasten-cache-query-filename (filename)
  (org-el-cache-get zettelkasten-cache filename))

(defun zettelkasten-cache-query-key-value (key value)
  (org-el-cache-select
   zettelkasten-cache
   (lambda (filename entry)
     (--any
      (string= (plist-get entry key) value)
      entry))))


(defun zettelkasten-open-zettel ()
  (interactive)
  (ivy-read
   (format "Zettel [%s]: " (car zettelkasten-context-filter))
   (zettelkasten--get-cons-title-fname
    (zettelkasten--get-all-zettel))
   :preselect "Inbox"
   :action
   (lambda (selection)
     (find-file
      (cdr selection)))))

(defun zettelkasten-open-zettel-collection ()
  (interactive)
  (ivy-read
   (format "Zettel [%s]: " (car zettelkasten-context-filter))
   (zettelkasten--get-cons-title-fname
    (zettelkasten--get-collection-zettel))
   :preselect "Inbox"
   :action
   (lambda (selection)
     (find-file
      (cdr selection)))))

(defun zettelkasten-open-zettel-descriptor ()
  (interactive)
  (ivy-read
   "Zettel: " (zettelkasten--get-cons-title-fname
                (zettelkasten--get-descriptor-zettel))
   :preselect "Inbox"
   :action
   (lambda (selection)
     (find-file
      (cdr selection)))))

(defun zettelkasten--get-cons-title-fname (plist)
  (mapcar (lambda (arg)
            (cons
             (plist-get arg :title)
             (plist-get arg :file)))
          plist))


(defun zettelkasten-backlinks-to-file (file)
  "Files linking to FILE."
  (let ((links)
        (file (expand-file-name file)))
    (org-el-cache-each
     zettelkasten-cache
     (lambda (filename data)
       (dolist (link (plist-get data :links))
         (if (string= file link)
             (push (cons (plist-get data :title) filename) links)))))
    links))

;;;###autoload
(defun zettelkasten-open-backlink ()
  (interactive)
  (ivy-read
   "Zettel: "
   (zettelkasten-backlinks-to-file (buffer-file-name))
   :action
   (lambda (selection)
     (find-file (cdr selection))
     ))
  )

(defun org-el-cache--find (paths &optional include-archives)
  "Generate shell code to search PATHS for org files.
If INCLUDE-ARCHIVES is non-nil, org_archive files are included,
too."
  (if include-archives
      (format
       "find %s -name \"[a-Z0-9_]*.org\" -o -name \"[a-Z0-9_]*.org_archive\" "
       (mapconcat 'identity paths " "))
    (format
     "find %s -name \"[a-Z0-9_]*.org\" -o -name \"[a-Z0-9_]*.org\""
     (mapconcat 'identity paths " "))))

;;;###autoload
(defun zettelkasten-force-update-cache ()
  (interactive)
  (org-el-cache-force-update zettelkasten-cache))

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

(defun zettelkasten-each-file ()
  (interactive)
  (let ((files
         (directory-files "/home/job/Dropbox/db/zk/zettel/txt/" nil ".txt$")))
    (dolist (zettel files)
      (let ((fullfname
             (concat "/home/job/Dropbox/db/zk/zettel/txt/" "/" zettel)))
        (find-file fullfname)
        (zettelkasten-repl-space)
        (zettelkasten-repl-double)
        (zettelkasten-repl-para)
        (zettelkasten-repl-at)
        (save-buffer)
        (kill-current-buffer)))))



(defun zettelkasten-repl-space ()
  (interactive "*")
  (goto-char (point-min))
  (search-forward "#+DESCRIPTOR:" nil nil)
  (end-of-line)
  (setq my-end (point))
  (beginning-of-line)
  (setq my-beg (point))
  (save-restriction
    (narrow-to-region my-beg my-end)
    (while (search-forward " " nil t)
      (replace-match " #")))
  )

(defun zettelkasten-repl-at ()
  (interactive "*")
  (goto-char (point-min))
  (search-forward "#+DESCRIPTOR:" nil nil)
  (end-of-line)
  (setq my-end (point))
  (beginning-of-line)
  (setq my-beg (point))
  (save-restriction
    (narrow-to-region my-beg my-end)
    (while (search-forward "#@" nil t)
      (replace-match "@")))
  )


(defun zettelkasten-repl-double ()
  (interactive "*")
  (goto-char (point-min))
  (search-forward "#+DESCRIPTOR:" nil nil)
  (end-of-line)
  (setq my-end (point))
  (beginning-of-line)
  (setq my-beg (point))
  (save-restriction
    (narrow-to-region my-beg my-end)
    (while (search-forward "##" nil t)
      (replace-match "#")))
  )

(defun zettelkasten-repl-para ()
  (interactive "*")
  (goto-char (point-min))
  (search-forward "#+DESCRIPTOR:" nil nil)
  (end-of-line)
  (setq my-end (point))
  (beginning-of-line)
  (setq my-beg (point))
  (save-restriction
    (narrow-to-region my-beg my-end)
    (while (search-forward "#§" nil t)
      (replace-match "@")))
  )




(defun zettelkasten-replace-tags ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "[\n]*\\* Schlagwörter\ntags:" nil t)
    (replace-match "\n#+DESCRIPTOR:")))



(defun zettelkasten-tmp (path tag)
  (let ((files
         (directory-files path nil ".txt$"))
        (collection
         (s-chop-prefix "§" tag)))
    (dolist (file files)
      (let ((fullfname
             (concat path "/" file)))
        (find-file fullfname)
        (if (search-forward tag nil t)
            (zettelkasten-zettel-add-collection collection))
        (while (search-forward (concat " " tag ",") nil t)
          (replace-match ""))
        (save-buffer)
        (kill-current-buffer)))))


(defun zettelkasten-replace-tmp (path tag repl)
  (let ((files
         (directory-files path nil ".txt$"))
        ;; (collection
        ;;  (s-chop-prefix "§" tag))
        )
    (dolist (file files)
      (let ((fullfname
             (concat path "/" file)))
        (find-file fullfname)
        ;; (if (search-forward tag nil t)
        ;;     (zettelkasten-zettel-add-collection collection))
        (while (search-forward tag nil t)
          (replace-match repl))
        (save-buffer)
        (kill-current-buffer)))))





(provide 'zettelkasten)
;;; zettelkasten.el ends here
