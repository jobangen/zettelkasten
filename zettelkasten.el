;;; zettelkasten.el --- Functions to maintain a zettel-archive -*- lexical-binding: t -*-

;; Copyright (C) 2017 Jan Ole Bangen.

;; Author: Jan Ole Bangen <jobangen@gmail.com>
;; URL:
;; Package-Version: 20170918.2122
;; Version: 0.0.1
;; Package-Requires:
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
(push '("z" "Zettel" plain
        (file (lambda ()
                (capture-report-date-file
                 (expand-file-name zettelkasten-zettel-directory))))
        (function zettelkasten-zettel-template)
        :immediate-finish t
        :jump-to-captured t)
      org-capture-templates)

;;;###autoload
(defun zettelkasten-new-zettel ()
  "Capture a Zettel with org-capture"
  (interactive)
  (org-capture  nil "z"))

(defun zettelkasten-zettel-template ()
  "#+TITLE: %^{Title}
#+DATE: %U

* Schlagwörter
tags: %^{Type|§index|§content|§proj},

* Inhalt

* Literatur

* Links & Files

* Data
")

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
    (beginning-of-buffer)
    (insert (format-time-string "%Y-%m-%d-%H%M") ", " old-name-base ".txt, " new-name-base ".txt\n")
    (bury-buffer)
    (save-some-buffers)))

;;; Open from Zettel
;;;###autoload
(defun zettelkasten-zettel-open-bibkey ()
  (interactive)
  (org-open-link-from-string
   (concat "file:" zettelkasten-bibliography-file "::" (file-name-base))))

;;;###autoload
(defun zettelkasten-zettel-open-files ()
  (interactive)
  (org-open-link-from-string
   (concat "file:" zettelkasten-texts-directory (file-name-base) "*.pdf")))

;;;###autoload
(defun zettelkasten-zettel-open-similarities ()
  (interactive)
  (find-file
   (concat zettelkasten-similarities-directory "sim-" (buffer-name))))

;; Dirs and Queries
;;;###autoload
(defun zettelkasten-open-dir ()
  (interactive)
  (find-file zettelkasten-zettel-directory))

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
  (zettelkasten-parse-tags-values))

(add-hook 'after-init-hook 'zettelkasten-parse-values-combined)


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
    (search-forward "tags: " nil t)
    (search-forward-regexp ",$" nil t)
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
(defun zettelkasten-sort-tags ()
  (interactive)
  (goto-char (point-min))
  (search-forward "tags: " nil t)
  (search-forward "§" nil t)
  (search-forward ", " nil t)
  (deactivate-mark)
  (sort-symbols nil (point) (search-forward-regexp ",$" nil nil)))

;;;###autoload
(defun zettelkasten-finish-zettel ()
  "Zettelkasten: delete whitespace, save, kill buffer."
  (interactive)
  (delete-trailing-whitespace)
  (zettelkasten-sort-tags)
  (save-buffer)
  (kill-buffer))


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
  (insert (file-name-base (buffer-file-name (window-buffer (minibuffer-selected-window))))))


;;;###autoload
(defhydra hydra-zettelkasten (:hint nil
                                    :color pink)
  "
 ^Zettelkasten^ ^ ^             ^Zettel^             ^ ^              ^Var^
-^---^--------------------------^-^-----------------^-^-------------^-^---------------
 _d_: zk        _z_: zettel     _m_: mark tags     _v_: visualize   _s_: sort symbols
 _b_: bibtex    _q_: query      _t_: insert tags   _cp_: parent     _R_: remem
 _k_: kill b.   _K_: kill bfs   _f_: finish        _cc_: child      _x_: txt query
 ^ ^            ^ ^             ^ ^                _cf_: friend     _j_: join line
"
  ;;General
  ("C-s" counsel-grep-or-swiper)
  ("C-m" job/open-at-point)
  ("<tab>" org-next-link "next link")
  ("C-<tab>" org-previous-link "prev link")
  ("b" ivy-bibtex)
  ("d" zettelkasten-open-dir)
  ("k" kill-this-buffer)
  ("K" projectile-kill-buffers)               ;;proj
  ("q" zettelkasten-ag-query)                 ;;proj
  ("Q" zettelkasten-ag-query-symbol-at-point) ;;(proj)
  ("v" hydra-brain-visualize/body :color blue) ;;brain
  ("x" zettelkasten-txt-query)                 ;;var
  ("z" zettelkasten-new-zettel :color blue)    ;;proj
  ;; Zettel
  ("cp" org-brain-add-parent)           ;brain
  ("cc" org-brain-add-child)            ;brain
  ("cf" org-brain-add-friend)           ;brain
  ("f" zettelkasten-finish-zettel)      ;zet
  ("m" zettelkasten-mark-tags)          ;zet
  ("R" remem-toggle)                    ;var
  ("s" sort-symbols)                    ;var
  ("j" join-line)
  ("t" zettelkasten-insert-tags)        ;zet
  )


(provide 'zettelkasten)
;;; zettelkasten.el ends here
