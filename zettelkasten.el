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
(require 'zettelkasten-cache)
(require 'zettelkasten-visualization)

(defgroup zettelkasten nil
  "Quick manipulation of textual checkboxes."
  :group 'convenience)

(defcustom zettelkasten-main-directory
  (expand-file-name (convert-standard-filename "zettelkasten/") user-emacs-directory)
  "Path for main directory"
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-zettel-directory
  (expand-file-name (convert-standard-filename "zettel/") zettelkasten-main-directory)
  "Path for Zettel"
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-bibliography-file "~/biblio.bib"
  "Path to bibfile"
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-inbox-file (concat zettelkasten-zettel-directory "zettelkasten-inbox.org")
  "Path to bibfile"
  :group 'zettelkasten
  :type 'string)


(defun zettelkasten-zettel-template ()
  "#+TITLE: 
#+DATE: %U
#+COLLECTION: %^{Type|index|index>t|content|proj}
#+DESCRIPTOR: @%\\1

* Inhalt
%i

* Refile")

(defcustom zettelkasten-descriptor-chain-sep ">"
  "Char that separates chained descriptors"
  :group 'zettelkasten
  :type '(string))

(defcustom zettelkasten-descriptor-cycle-threshold 5
  "Threshold for remaining Zettel, to breaks the selection cycle"
  :group 'zettelkasten
  :type 'integer)

(defcustom zettelkasten-context-filter-list '()
  "List of context filter"
  :group 'zettelkasten
  :type 'list)

(defcustom zettelkasten-org-agenda-integration nil
  "If non-nil, add zettel with todos to `org-agenda-files'"
  :type 'boolean)


(defun zettelkasten-context-work-fun (entry)
  (and (not (member "journal" (plist-get entry :collections)))
       (not (member "@Rezept" (plist-get entry :descriptors)))
       (not (member "priv" (plist-get entry :collections)))))

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
        (file+headline zettelkasten-inbox-file "Refile")
        "\n** TODO :refile:zkt:
:PROPERTIES:
:CATEGORY: zkt
:END:
%?
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

(push '("e" "Zettel ephemera" plain
        (file (lambda ()
                (let ((name (or zettel-capture-filename (read-string "Name: "))))
                  (expand-file-name
                   (concat zettelkasten-zettel-directory "/eph/"
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
(defun zettelkasten-capture-elfeed ()
  (interactive)
  (let ((title
         (progn
           (goto-char (point-min))
           (search-forward "Title: ")
           (s-replace "Title:" "" (thing-at-point 'line)))))
    (mark-whole-buffer)
    (org-capture nil "z")
    (org-edit-headline (s-truncate 51 title))
    (org-priority)
    (org-capture-finalize)
    (elfeed-show-next)))
  
;;;###autoload
(defun zettelkasten-new-zettel (&optional title)
  "Capture a Zettel with org-capture"
  (interactive)
  (let ((zettel-title
         (or title (read-string "Title: "))))
    (setq zettel-capture-filename
          (zettelkasten--title-to-fname zettel-title))
    (org-capture nil "Z")
    (end-of-line)
    (insert zettel-title)
    (setq zettel-capture-filename nil)
    (save-buffer)))


;;; Open from Zettel
(org-link-set-parameters "zk" :follow #'org-zettelkasten-open)

(defun org-zettelkasten-open (path)
  (when zettelkasten-capture-state
    (kill-current-buffer))
  (if (= (length path) 15)
      (let ((zettel-entry
             (car (zettelkasten-cache-entry-ids (list path)))))
        (find-file (plist-get zettel-entry :file)))
    (let ((zettel-entry
           (car (zettelkasten-cache-entries-where-member path :custom-ids))))
      (find-file (plist-get zettel-entry :file))
      (goto-char (point-min))
      (search-forward path nil t)
      (org-back-to-heading)))
  (when zettelkasten-capture-state
    (zettelkasten-capture-mode)))

(defun zettelkasten-org-zk-export (path desc format)
  "Format zk links for export."
  (let* ((entry
          (car (zettelkasten-cache-entry-ids (list path))))
         (filename
          (s-replace-all '(("/home/job/Dropbox/db/zk/zettel/" . "")
                          (".org" . ".html"))
                         (plist-get entry :file))))
    (when (eq format 'html)
      (format "<a href=\"./%s\">%s</a>"
              filename
              desc))))

(org-link-set-parameters "zk" :export 'zettelkasten-org-zk-export)


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
          (zettelkasten-cache-entry-filename zettel))
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
    (org-set-tags '("refile" "zkt"))
    (org-set-property "CATEGORY" "zkt")
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
                         (zettelkasten-cache-values-collection))))))
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
                         (zettelkasten-cache-values-descriptor))))))
  (zettelkasten-sort-tags)
  (unless descriptor
    (zettelkasten-zettel-add-descriptor)))

;;;###autoload
(defun zettelkasten-headline-add-descriptor ()
  "Add descriptor to current headline."
  (interactive)
  (let* ((current
          (split-string
           (or (org-entry-get nil "DESCRIPTOR") "")))
         (add
          (ivy-read "Descriptor [Headline]: "
                    (zettelkasten-cache-values-descriptor)))
         (join
          (sort (append current (list add)) 'string<))
         (string
          (mapconcat 'identity join " ")))
    (org-set-property "DESCRIPTOR" string))
  (zettelkasten-headline-add-descriptor))

;;;###autoload
(defun zettelkasten-headline-add-collection ()
  "Add descriptor to current headline."
  (interactive)
  (let* ((current
          (split-string
           (or (org-entry-get nil "COLLECTION") "")))
         (add
          (ivy-read "Collection [Headline]: "
                    (zettelkasten-cache-values-collection)))
         (join
          (sort (append current (list add)) 'string<))
         (string
          (mapconcat 'identity join " ")))
    (org-set-property "COLLECTION" string))
  (zettelkasten-headline-add-collection))


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


(defun zettelkasten--get-all-zettel ()
  (org-el-cache-select
   zettelkasten-cache
   (lambda (filename entry)
     (funcall (cdr zettelkasten-context-filter) entry))))

(defun zettelkasten--get-collection-zettel ()
  (let ((collection
         (completing-read "Collection: "
                          (zettelkasten-cache-values-collection))))
    (zettelkasten-cache-entries-where-member collection :collections)))

(defun zettelkasten--get-descriptor-zettel (&optional entries)
  "Select descriptor and show shared descriptors or zettel."
  (let* ((descriptor
          (completing-read
           (format "Descriptor [%s]: " (when entries (safe-length entries)))
           (if entries
               (append (zettelkasten-cache-values-descriptor entries)
                       '("Break"))
             (zettelkasten-cache-values-descriptor))))
         (zettel
          (if entries
              (-filter
               (lambda (entry)
                 (member descriptor (plist-get entry :descriptors)))
               entries)
            (zettelkasten-cache-entries-where-member descriptor :descriptors))))
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

;;;###autoload
(defun zettelkasten-open-zettel ()
  (interactive)
  (ivy-read
   (format "Zettel [%s]: " (car zettelkasten-context-filter))
   (zettelkasten--get-cons-title-fname
    (zettelkasten--get-all-zettel))
   :preselect "Inbox"
   :action
   (lambda (selection)
     (if (listp selection)
         (find-file (cdr selection))
       (zettelkasten-new-zettel selection)))))

;;;###autoload
(defun zettelkasten-open-zettel-random ()
  (interactive)
  (let* ((zettel
          (zettelkasten--get-cons-title-fname
           (zettelkasten--get-all-zettel)))
         (rand-element
          (random (safe-length zettel))))
    (find-file (cdr (nth rand-element zettel)))))

;;;###autoload
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


(defun zettelkasten--get-cons-title-fname (plist)
  (mapcar (lambda (arg)
            (cons
             (plist-get arg :title)
             (plist-get arg :file)))
          plist))


;;;###autoload
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

(defun zettelkasten-open-zettel-todo ()
  (interactive)
  (ivy-read
   "Zettel: " (zettelkasten--get-cons-title-fname
               (org-el-cache-select
                zettelkasten-cache
                (lambda (filename entry)
                  (plist-get entry :todo))))
   :action
   (lambda (selection)
     (find-file (cdr selection)))))


(defun zettelkasten--get-backlinks (file)
  "Files linking to FILE."
  (let ((file-id
         (plist-get (zettelkasten-cache-entry-filename) :id)))
    (org-el-cache-select
     zettelkasten-cache
     (lambda (filename entry)
       (member file-id (plist-get entry :links))))))

  
;;;###autoload
  (defun zettelkasten-open-backlink ()
    (interactive)
    (ivy-read
     "Zettel: "
     (zettelkasten--get-cons-title-fname
      (zettelkasten--get-backlinks (buffer-file-name)))
     :action
     (lambda (selection)
       (find-file (cdr selection)))))

;;;###autoload
(defun zettelkasten-update-org-agenda-files ()
  (interactive)
  (when zettelkasten-org-agenda-integration
    (let ((not-zettelkasten-agenda-files
           (seq-filter
            (lambda (filename)
              (or (not (string-match "/home/job/Dropbox/db/zk/zettel/.*" filename))
                  (string-match "/home/job/Dropbox/db/zk/zettel/jr/.*" filename)))
            (org-agenda-files)))
          (zettelkasten-agenda-files
           (mapcar
            (lambda (arg)
              (plist-get arg :file))
            (org-el-cache-select
             zettelkasten-cache
             (lambda (filename entry)
               (and (plist-get entry :todo)
                    (not (member "journal" (plist-get entry :collections)))))))))
      (setq org-agenda-files (append not-zettelkasten-agenda-files
                                     zettelkasten-agenda-files)))))


;;;###autoload
(defun zettelkasten-headline-set-followup ()
  (interactive)
  (org-todo "TODO")
  (org-set-property "CATEGORY" "zkt")
  (org-set-tags '("zkt" "followup")))

;;;###autoload
(defun zettelkasten-headline-reset ()
  (interactive)
  (org-todo "")
  (org-set-tags '()))

(defhydra hydra-zettelkasten (:columns 4 :color blue)
  "Zettelkasten"
  ("C-ä" zettelkasten-open-zettel "Open Zettel" :column "Open")
  ("R" zettelkasten-open-zettel-random "Open random")
  ("ä" zettelkasten-open-zettel-collection "Open collection")
  ("d" zettelkasten-open-zettel-descriptor "Open descriptor")
  ("t" zettelkasten-open-zettel-todo "Open todo")
  ("b" zettelkasten-open-backlink "Backlinks")

  ("z" zettelkasten-new-zettel "New Zettel" :column "Zettelkasten")
  ("L" zettelkasten-new-zettel-insert-link-at-point "Link new Zettel")
  ("p" zettelkasten-push-link-to-current "Push Link")
  ("f" zettelkasten-set-context-filter "Set filter")
  ("D" zettelkasten-replace-descriptor "Replace Desc.")
  ("I" zettelkasten-info "Info")

  ("c" zettelkasten-zettel-add-collection "Add Collection" :column "Zettel")
  ("#" zettelkasten-zettel-add-descriptor "Add descriptor")
  ("l" zettelkasten-insert-link-at-point "Insert Link")
  ("i" zettelkasten-zettel-info "Info")
  ("v" zettelkasten-vis-buffer "visualize")

  ("hc" zettelkasten-headline-add-collection "Add collection" :column "Headline")
  ("h#" zettelkasten-headline-add-descriptor "Add descriptor headline")
  ("r" zettelkasten-refile "Refile" :color amaranth)
  ("hf" zettelkasten-headline-set-followup "Followup")
  ("hr" zettelkasten-headline-reset "Reset")

  ("sl" zetteldeft-avy-link-search "search link" :column "Search")
  ("sf" zetteldeft-search-current-id "seach current file")
  ("sd" zetteldeft-deft-new-search "search deft ")
  ("st" zetteldeft-avy-tag-search "search tag")
  ("n" org-noter "noter" :column "Other")
  ("Q" zettelkasten-force-update-cache "Reset Cache")
  ("u" zettelkasten-update-org-agenda-files "Update agenda")
  ("q" nil "Quit"))

(defun zettelkasten-each-file ()
  (interactive)
  (let* ((path "/home/job/Dropbox/db/zk/zettel/txt/")
         (files
         (directory-files path nil ".org$")))
    (dolist (zettel files)
      (let ((fullfname
             (concat path "/" zettel)))
        (find-file fullfname)
        (zettelkasten-zettel-add-collection "txt")
        (goto-char (point-min))
        (while (search-forward "txt txt" nil t)
          (replace-match "txt"))
        (save-buffer)
        (kill-current-buffer)))))

(provide 'zettelkasten)
;;; zettelkasten.el ends here
