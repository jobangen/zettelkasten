;;; zettelkasten.el --- Functions to maintain a semantic zettel-archive -*- lexical-binding: t -*-

;; Copyright (C) 2017-2021 Jan Ole Bangen.

;; Author: Jan Ole Bangen <jobangen@gmail.com>
;; Version: 0.5.0

;; This file is not part of GNU Emacs.

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
(require 's)
(require 'zettelkasten-db)

(defgroup zettelkasten nil
  "Quick manipulation of textual checkboxes."
  :group 'convenience)

(defcustom zettelkasten-main-directory
  (expand-file-name (convert-standard-filename "zettelkasten/") user-emacs-directory)
  "Path to main zettelkasten directory."
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-zettel-directory
  (expand-file-name (convert-standard-filename "zettel/") zettelkasten-main-directory)
  "Path for zettelkasten directory that houses zettel."
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-inbox-file (concat zettelkasten-zettel-directory "zettelkasten-inbox.org")
  "Path of zettelkasten inbox file."
  :group 'zettelkasten
  :type 'string)

(defun zettelkasten-zettel-template ()
  "Template for zettel."
  "#+TITLE:\n#+DATE: %U

* Meta

* Inhalt
%i

* Refile")

(defcustom zettelkasten-descriptor-chain-sep ">"
  "Character that separates chained descriptors."
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-descriptor-cycle-threshold 5
  "Threshold for remaining Zettel to break the selection cycle."
  :group 'zettelkasten
  :type 'integer)

(defcustom zettelkasten-org-agenda-integration nil
  "If non-nil, add zettel with todos to `org-agenda-files'."
  :type 'boolean)

(defcustom zettelkasten-classes nil
  "List of available classes for ressources."
  :group 'zettelkasten
  :type 'list)

(defcustom zettelkasten-predicates nil
  "List of available predicates."
  :group 'zettelkasten
  :type 'list)

(defcustom zettelkasten-predicate-domain-range nil
  "List of lists with domain and range for each predicate."
  :group 'zettelkasten
  :type 'list)

(defcustom zettelkasten-descriptor-predicate "skos:subject"
  "Default predicate for descriptor keyword and filetag."
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-collection-predicate ""
  "Default predicate for collection keyword and filetag."
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-file-id-format "%Y-%m-%d-%H%M"
  "Format for id part of filename. To be interpreted by `format-time-string'."
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-filename-to-id-func #'zettelkasten-default-filename-to-id
  "Function for extracting id from filename."
  :group 'zettelkasten
  :type 'function)


(defun zettelkasten-default-filename-to-id (filenamepart)
  "Default func to extract id from FILENAMEPART.
Used in `zettelkasten--filename-to-id' to process last part of filename."
  (let ((id-length (length (format-time-string zettelkasten-file-id-format))))
    (s-left id-length filename)))

(defun zettelkasten--filename-to-id (filename)
  "Extract id from FILENAME. Return id string."
  (let* ((fname-chop
          (s-chop-prefix
           zettelkasten-zettel-directory
           (s-chop-suffix ".org" filename))))
    (funcall zettelkasten-filename-to-id-func fname-chop)))

;; Creation and (re)naming of zettel
(push '("z" "Zettel inbox" plain
        (file+headline zettelkasten-inbox-file "Inbox")
        "\n** TODO :refile:zkt:
:PROPERTIES:
:CATEGORY: zkt
:END:
%?
%i")
      org-capture-templates)

(push '("Z" "Zettel" plain
        (file (lambda ()
                (let ((name (or zettel-capture-filename (read-string "Name: ")))
                      (time-string (format-time-string
                                    zettelkasten-file-id-format)))
                  (while (zettelkasten-db-query [:select [zkid]
                                                 :from nodes
                                                 :where (= zkid $s1)]
                                                time-string)
                    (let ((base-string (s-left 11 time-string))
                          (hmin (+ 1 (string-to-number (s-right 4 time-string)))))
                      (setq time-string (concat base-string
                                                (number-to-string hmin)))))
                  (expand-file-name
                   (concat zettelkasten-zettel-directory
                           time-string "-" name ".org")))))
        (function zettelkasten-zettel-template)
        :immediate-finish t
        :jump-to-captured t)
      org-capture-templates)

(defun zettelkasten--title-to-fname (title)
  "Replace charakters in TITLE to create filename."
  (s-replace-all
   '((" " . "-") ("/" . "-")
     (":" . "") ("." . "") ("," . "") (";" . "")
     ("?" . "") ("!" . "")
     ("\"" . "") ("'" . "")
     ("&" . "und")
     ("\(" . "") ("\)" . "")
     ("{" . "") ("}" . "")
     ("ß" . "ss") ("ä" . "ae")
     ("ü" . "ue") ("ö" . "oe")
     ("é" . "e") ("ó" . "o")
     ("–" . "")
     )
   (s-chop-suffix "-" (s-left 59 (s-downcase title)))))

(defun zettelkasten--get-all-files ()
  "Return all files in zettelkasten-dir recursively."
  (directory-files-recursively zettelkasten-zettel-directory "org$"))

;;;###autoload
(defun zettelkasten-new-zettel (&optional title type)
  "Capture a Zettel with `org-capture' and use TITLE and TYPE."
  (interactive)
  (let ((zettel-title
         (or title (read-string "Title: "))))
    (setq zettel-capture-filename
          (zettelkasten--title-to-fname zettel-title))
    (org-capture nil "Z")
    (end-of-line)
    (insert zettel-title)
    (setq zettel-capture-filename nil)
    (if type
        (zettelkasten-set-type type)
      (zettelkasten-set-type))
    (save-buffer)))

;;; Open from Zettel
(org-link-set-parameters "zk" :follow #'org-zettelkasten-open)

(defun org-zettelkasten-open (path)
  "Open file at PATH."
  (let* ((path (nreverse (split-string path "::")))
         (target (car path))
         (target-label (concat "%" target " %")))
    (find-file (caar (zettelkasten-db-query
                      [:select filename
                               :from nodes
                               :where (= zkid $s1)
                               :or (= label $s2)
                               :or (like label $s3)] ;; multiple labels?
                      target target target-label)))
    (goto-char (point-min))
    (search-forward target nil t)
    (ignore-error (org-back-to-heading))))

(defun zettelkasten-flat-predicates ()
  "Return predicates as uniquified, flat list."
  (delete-dups (-flatten zettelkasten-predicates)))


;;;###autoload
(defun zettelkasten-set-type (&optional type)
  "Set TYPE for zettel."
  (interactive)
  (let ((type-sel (or type (completing-read "Type: "
                                            (-flatten zettelkasten-classes)))))
    (zettelkasten--zettel-ensure-keyword "RDF_TYPE")
    (delete-region (point) (line-end-position))
    (insert (format " %s" type-sel))))

;;;###autoload
(defun zettelkasten-set-type-headline (&optional type)
  "Set TYPE for headling."
  (interactive)
  (let ((type-sel (or (when (stringp type)
                        type)
                      (completing-read "Type: "
                                       (or type
                                           (-flatten zettelkasten-classes))))))
    (org-set-property "RDF_TYPE" type-sel)))

;;;###autoload
(defun zettelkasten-set-label (&optional label)
  "Set LABEL for zettel."
  (interactive)
  (let* ((title (zettelkasten--extract-title))
         (title-proc (s-replace-all '((" " . "-")
                                      ("," . "")
                                      ("." . ""))
                                    (zettelkasten--extract-title)))
         (label-old
          (zettelkasten-extract-value "ZK_LABEL" (org-element-parse-buffer)))
         (label-sel (or label (read-string "Label: " (or label-old title-proc)))))
    (when (and label-sel (stringp label-sel) (string-match "\\S-" label-sel))
      (zettelkasten--zettel-ensure-keyword "ZK_LABEL")
      (delete-region (point) (line-end-position))
      (insert (format " %s" label-sel)))))

;;;###autoload
(defun zettelkasten-set-type-and-label ()
  "Set type and label for zettel."
  (interactive)
  (zettelkasten-set-type)
  (unless (or (s-contains? "/txt/" (buffer-file-name))
              (s-contains? "/jr/" (buffer-file-name)))
    (zettelkasten-set-label)))


;;;###autoload
(defun zettelkasten-insert-link-at-point (&optional link-predicate link-target)
  "Insert semantic link at point. Use LINK-PREDICATE and LINK-TARGET if provieded."
  (interactive)
  (let* ((filename (buffer-file-name))
         (element (org-element-parse-buffer))
         (zk-id (org-entry-get nil "CUSTOM_ID"))
         (zettel-target link-target)
         ;; List of node types in heading or file
         (node-types
          (unless link-predicate
            (save-excursion
              (zettelkasten-get-property-or-filetag-upwards filename
                                                            element
                                                            "RDF_TYPE"))))
         ;; List of types in hierarchy upwards
         (node-hierarchy (unless link-predicate
                           (delete-dups ;;todo?
                            (mapcan
                             (lambda (type)
                               (zettelkasten--tree-parents-rec
                                type
                                zettelkasten-classes))
                             node-types))))
         (predicate-by-domain (unless link-predicate
                                (remove
                                 nil
                                 (mapcar (lambda (entry)
                                           (when (member (caadr entry)
                                                         node-hierarchy)
                                             (car entry)))
                                         zettelkasten-predicate-domain-range))))
         (predicate (or (when (stringp link-predicate)
                          link-predicate)
                        (completing-read "Predicate: "
                                         (or link-predicate
                                             predicate-by-domain))))
         ;; List of possible target classes
         (classes-by-pred-range (cadr (cadr (assoc
                                             predicate
                                             zettelkasten-predicate-domain-range))))
         ;; List of classes including ones lower in class hierarchy
         (target-classes (-flatten
                          (mapcan (lambda (rangeitem)
                                    (zettelkasten--tree-children-rec
                                     rangeitem zettelkasten-classes))
                                  classes-by-pred-range)))
         (turtle (s-starts-with? ":TURTLE" (thing-at-point 'line t)))
         (zettel
          (save-excursion
            (unless link-target
              (ivy-read "Link Zettel: "
                        (mapcar
                         (lambda (node)
                           (list
                            (format "%s [%s]"
                                    (s-pad-right 120 " " (s-truncate 120 (car node)))
                                    (s-truncate 40 (file-name-base (cadr node))))
                            (caddr node)))
                         (if target-classes
                             (let ((edges
                                    (-flatten
                                     (zettelkasten-db-query
                                      [:select [subject]
                                               :from edges
                                               :where (= predicate "rdf:type")
                                               :and (in object $v1)]
                                      (vconcat target-classes)))))
                               (zettelkasten-db-query [:select [title filename zkid]
                                                               :from nodes
                                                               :where (in zkid $v1)]
                                                      (vconcat edges)))
                           (zettelkasten-db-query [:select [title filename zkid]
                                                           :from nodes])))
                        :action
                        (lambda (selection)
                          (if (listp selection)
                              (setq zettel-target (cadr selection))
                            (if (s-starts-with? "val:" selection)
                                (setq zettel-target selection)
                              (other-window 1)
                              (zettelkasten-new-zettel selection)
                              (setq zettel-target filename))))))))
         (zettel-id (if (s-starts-with? "val:" zettel-target)
                        (s-chop-prefix "val:" zettel-target)
                      (car
                       (split-string
                        (car
                         (remove nil
                                 (car (zettelkasten-db-query
                                       [:select [label zkid]
                                                :from nodes
                                                :where (= zkid $s1)]
                                       zettel-target))))))))
         (zettel-title
          (if (s-starts-with? "val:" zettel-target)
              (s-chop-prefix "val:" zettel-target)
            (caar (zettelkasten-db-query [:select title
                                                  :from nodes
                                                  :where (= zkid $s1)]
                                         zettel-target)))))
    (if predicate
        (if (and zk-id (stringp zk-id) (string-match "\\S-" zk-id))
            (if (equal turtle t)
                (insert (format "%s::%s" predicate zettel-id))
              (insert (format "[[zk:%s::%s::%s][%s]]"
                              zk-id predicate zettel-id zettel-title)))
          (insert (format "[[zk:%s::%s][%s]]"
                          predicate zettel-id zettel-title)))
      (insert (format "[[zk:%s][%s]]" zettel-id zettel-title)))))

;;;###autoload
(defun zettelkasten-insert-link-loop ()
  "Insert Links as a org-list."
  (interactive)
  (insert "- ")
  (zettelkasten-insert-link-at-point)
  (newline)
  (zettelkasten-insert-link-loop))

(defun zettelkasten-heading-set-relation-to-context (&optional predicate)
  "Set relate of heading to parent resource. Use PREDICATE if provided."
  (let* ((predicate (or pred
                        (completing-read
                         "Predicate: "
                         (zettelkasten-flat-predicates)
                         nil nil "dct:isPartOf")))
         (target (completing-read
                  "Target:"
                  (zettelkasten-db-query [:select zkid
                                          :from nodes
                                          :where (= filename $s1)]
                                         (buffer-file-name))))
         (turtle (format "%s::%s" predicate target)))
    (org-set-property "TURTLE" turtle)))

;;;###autoload
(defun zettelkasten-heading-to-node (&optional rdftype prov-id predicate)
  "Create node form heading by adding RDFTYPE, PROV-ID and set relation to parent ressource with PREDICATE if provided."
  (interactive)
  (zettelkasten-set-type-headline rdftype)
  (zettelkasten-id-get-create prov-id)
  (zettelkasten-heading-set-relation-to-context predicate))

(defun zettelkasten-id-get-create (&optional prov-id)
  "Select and set ID for heading. Use PROV-ID if proviede."
  (let ((zk-id (org-entry-get nil "CUSTOM_ID")))
    (unless (and zk-id (stringp zk-id) (string-match "\\S-" zk-id))
      (let* ((org-id-method 'ts)
             (org-id-ts-format "%Y-%m-%d-%H%M%S.%6N")
             (myid
              (or prov-id
                  (completing-read
                   "ID: "
                   `(,(zettelkasten-extract-value "ZK_LABEL")
                     ,(concat "zk-" (s-chop-suffix ".org" (buffer-name)))
                     ,(file-name-base)
                     ,(concat "zk-" (s-left 17 (org-id-new)))))))
             (editid (read-string "Edit ID: " myid)))
        (setq zk-id editid))
      (org-set-property "CUSTOM_ID" zk-id))
    zk-id))

;; Dirs and Queries

(defun zettelkasten--zettel-ensure-keyword (keyword)
  "Make sure KEYWORD is present."
  (let ((key-format (format "#+%s:" keyword)))
    (goto-char (point-min))
    (unless (search-forward key-format nil t)
      (goto-char (point-min))
      (next-line 2)
      (search-backward-regexp "^#\\+")
      (end-of-line)
      (newline)
      (insert key-format))))


;;;###autoload
(defun zettelkasten-sort-tags ()
  "Sort descriptors alphabetically."
  (interactive "*")
  (text-mode)
  (zettelkasten--zettel-ensure-keyword "DESCRIPTOR")
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
(defun zettelkasten-zettel-add-descriptor (&optional descriptor)
  "Add DESCRIPTOR to zettel."
  (interactive)
  (let ((desc
         (or descriptor
             (completing-read
              "Descriptor: "
              (append
               '("#Break#")
               (-flatten
                (zettelkasten-db-query
                 [:select :distinct [object]
                  :from edges
                  :where (in predicate ["skos:subject"
                                        "skos:primarySubject"])])))))))
    (unless (equal desc "#Break#")
      (save-excursion
        (zettelkasten--zettel-ensure-keyword "DESCRIPTOR")
        (insert (concat " " desc))
        (zettelkasten-sort-tags)))
    (unless (or descriptor (equal desc "#Break#"))
      (zettelkasten-zettel-add-descriptor))))

;;;###autoload
(defun zettelkasten-headline-add-descriptor ()
  "Add descriptor to current headline."
  (interactive)
  (let* ((current
          (split-string
           (or (org-entry-get nil "DESCRIPTOR") "")))
         (add
          (completing-read
           "Descriptor [Headline]: "
           (-flatten
            (zettelkasten-db-query [:select :distinct [object]
                                            :from edges
                                            :where (= predicate "skos:subject")]))))
         (join
          (sort (append current (list add)) 'string<))
         (string
          (mapconcat 'identity join " ")))
    (org-set-property "DESCRIPTOR" string))
  (zettelkasten-headline-add-descriptor))

;;;###autoload
(defun zettelkasten-finish-zettel ()
  "Zettelkasten: delete whitespace, save, kill buffer."
  (interactive)
  (zettelkasten-sort-tags)
  (delete-trailing-whitespace)
  (save-buffer)
  (kill-buffer))

;;;###autoload
(defun zettelkasten-open-zettel (&optional nodes)
  "Select and open zettel. Restrict selection to NODES if provided."
  (interactive)
  (ivy-read
   (format "Zettel: ")
   (if nodes
       (zettelkasten-db-title-filename-nodes)
     (zettelkasten-db--title-filename))
   :action
   (lambda (selection)
     (if (listp selection)
         (progn
           (find-file (cadr selection))
           (when nodes
             (goto-char (point-min))
             (search-forward (caddr selection) nil t)))
       (zettelkasten-new-zettel selection)))))

;;;###autoload
(defun zettelkasten-open-zettel-descriptor ()
  "Select descriptor and open resource that match."
  (interactive)
  (ivy-read
   "Zettel: " (zettelkasten-db--title-filename
               (zettelkasten-db--files-matching-descriptor))
   :action
   (lambda (selection)
     (find-file
      (cadr selection)))))

(defun zettelkasten--tree-children-rec (key tree)
  "Accepts string (KEY) and nested list (TREE)
to return all children recursively."
  (when (consp tree)
    (destructuring-bind (x . y) tree
      (if (equal x key)
          tree ;; fct of comparing
        (or (zettelkasten--tree-children-rec key x)
            (zettelkasten--tree-children-rec key y))))))

(defun zettelkasten--tree-parents-rec (key tree &optional path-in)
  "Accepts string (KEY) and nested list (TREE) to return all parents.
Uses PATH-IN internally to return path."
  (let ((path (or path-in nil)))
    (when (consp tree)
      (destructuring-bind (x . y) tree
        (if (equal x key)
            path
          (if (listp x)
              (push (car x) path)
            (push x path))
          (or (progn
                (zettelkasten--tree-parents-rec key x path))
              (progn
                (when path-in (pop path))
                (zettelkasten--tree-parents-rec key y path))))))))

(defun zettelkasten-predicate-hierachy (predicate)
  "Return vector of children for PREDICATE."
  (vconcat
   (-flatten
    (zettelkasten--tree-children-rec predicate
                             zettelkasten-predicates))))

(defun zettelkasten-db--nodes-semantic (&optional input-nodes)
  "Query resources by narrowing resources semantically. Start with INPUT-NODES if provided."
  (let* ((output-nodes input-nodes)
         ;; select predicate
         (predicate (completing-read "Predicate: "
                                     (if input-nodes
                                         ;; replace with db query
                                         (append
                                          (zettelkasten-db-query
                                           [:select :distinct [predicate]
                                            :from edges
                                            :where (in subject $v1)]
                                           input-nodes)
                                          '("#Break"))
                                       (zettelkasten-flat-predicates)))))
    (unless (equal predicate "#Break#")
      (let* ;; lookup predicate hierarchy
          ((predicates (zettelkasten-predicate-hierachy predicate))
           ;; get object label or id with selected predicate
           (objects-edges (vconcat
                           (-flatten
                            (if input-nodes
                                (zettelkasten-db-query
                                 [:select :distinct [object]
                                  :from edges
                                  :where (in subject $v1)
                                  :and (in predicate $v2)]
                                 input-nodes predicates )
                              (zettelkasten-db-query
                               [:select :distinct [object]
                                :from edges
                                :where (in predicate $v1)]
                               predicates)))))
           ;; match ids and label and return nodes
           (objects
            (zettelkasten-db-query [:select :distinct [title zkid label]
                                    :from nodes
                                    :where (in zkid $v1)
                                    :or (in label $v2)]
                                   objects-edges objects-edges))
           ;; select object
           (object (cdr (assoc (completing-read "Object: " objects) objects)))
           ;; lookup all subjects that link to object with predicate
           (subjects (-flatten (if input-nodes
                                   (zettelkasten-db-query
                                    [:select :distinct [subject]
                                     :from edges
                                     :where (in subject $v1)
                                     :and (in predicate $v2)
                                     :and (in object $v3)]
                                    input-nodes predicates (vconcat object))
                                 (zettelkasten-db-query
                                  [:select :distinct [subject]
                                   :from edges
                                   :where (in predicate $v1)
                                   :and (in object $v2)]
                                  predicates (vconcat object))))))
        (setq output-nodes subjects)))
    (if (or (equal predicate "#Break#")
            (< (length output-nodes) 11))
        output-nodes
      (zettelkasten-db--nodes-semantic (vconcat output-nodes)))))

;;;###autoload
(defun zettelkasten-open-semantic ()
  "Select and resource by semantic relations and open it."
  (interactive)
  (let* ((completions
          (mapcar
           (lambda (node)
             (list
              (format "%s [%s]" (car node) (file-name-base (cadr node)))
              (cadr node)
              (caddr node)))
           (zettelkasten-db-query
            [:select [title filename zkid]
             :from nodes
             :where (in zkid $v1)]
            (vconcat (zettelkasten-db--nodes-semantic)))))
         ;; (selection (assoc (completing-read "Node: " completions) completions))
         )
    (ivy-read "Node: " completions
              :action
              (lambda (selection)
                (find-file (cadr selection))
                (search-forward (caddr selection) nil t)
                )
              )
    ;; (find-file (cadr selection))
    ;; (search-forward (caddr selection) nil t)
    ))

(defun zettelkasten-get-property-or-filetag-upwards (filename element property &optional skip)
  "For ELEMENT of FILENAME get PROPERTY of next parent resource. SKIP one level if non nil."
  (when skip
    (outline-up-heading 1))
  (let ((return-value))
    (while (not return-value)
      (let ((prop-value (cdr (assoc property (org-entry-properties)))))
        (condition-case nil
            (if prop-value
                (setq return-value (split-string prop-value))
              (outline-up-heading 1))
          (error
           (if (equal property "CUSTOM_ID")
               ;; corner case for ids and labes
               (setq return-value (append
                                   (ignore-errors
                                     (split-string
                                      (zettelkasten-extract-value "ZK_LABEL"
                                                                  element)))
                                   (list (zettelkasten--filename-to-id
                                          filename))))
             (setq return-value (split-string
                                 (zettelkasten-extract-value
                                  property
                                  element))))))))
    return-value))

(defun zettelkasten--get-backlinks (filename)
  "Files linking to nodes of FILENAME. Return list links."
  (let* ((current-ids ;; gets next id upwards
          (zettelkasten-get-property-or-filetag-upwards
           filename
           (org-element-parse-buffer)
           "CUSTOM_ID"))
         (backlinks
          (zettelkasten-db-query [:select [e:predicate n:title n:filename]
                                  :from edges e
                                  :inner-join nodes n
                                  :on (= e:subject n:zkid)
                                  :where (in object $v1)]
                                 (vconcat current-ids)))
         (len (car (sort
                    (mapcar (lambda (link)
                              (length (car link)))
                            backlinks)
                    '>))))
    (mapcar (lambda (link)
              (list (concat " <- "
                            (s-pad-right (+ 2 len) " " (car link))
                            (s-pad-right 80 " " (s-left 80 (cadr link)))
                            "  "
                            (s-pad-right 40 " "
                                         (s-left 40 (file-name-base (caddr link))))
                            " ")
                    (caddr link)))
            backlinks)))

;;;###autoload
(defun zettelkasten-open-backlink ()
  "Select and open backlink."
  (interactive)
  (let* ((zettel (zettelkasten--get-backlinks (buffer-file-name)))
         (selected (assoc (completing-read "Zettel: " zettel) zettel)))
    (find-file (cadr selected))))

;;;###autoload
(defun zettelkasten-update-org-agenda-files ()
  "Update `org-agenda' files and exclude files that are handled by `org-journal'."
  (interactive)
  (when zettelkasten-org-agenda-integration
    (let ((not-zettelkasten-agenda-files
           (seq-filter
            (lambda (filename)
              (or (not (string-match "/home/job/Dropbox/db/zk/zettel/.*" filename))
                  (and (string-match "/home/job/Dropbox/db/zk/zettel/jr/.*" filename)
                       (= 10 (length (file-name-base filename))))))
            (org-agenda-files)))
          (zettelkasten-agenda-files
           (-flatten (zettelkasten-db-query
                      [:select [filename]
                       :from nodes
                       :where (= todo 't)]))))
      (setq org-agenda-files (append not-zettelkasten-agenda-files
                                     zettelkasten-agenda-files)))))

;;;###autoload
(defun zettelkasten-rename-file ()
  "Rename file and update db accordingly."
  (interactive)
  (let* ((old (buffer-file-name))
         (new (read-string "New name: " old)))
    (zettelkasten-db-query [:delete-from nodes
                            :where (= filename $s1)]
                           old)
    (zettelkasten-db-query [:delete-from edges
                            :where (= filename $s1)]
                           old)
    (rename-file old new)
    (kill-buffer (current-buffer))
    (find-file new)))

;;;###autoload
(defun zettelkasten-delete-file (&optional filename)
  "Delete current zettel or with FILENAME if provided."
  (interactive)
  (let ((filename (file-truename (or filename (buffer-file-name)))))
    (when (yes-or-no-p "Delete zettel? ")
      (zettelkasten-db-query [:delete-from nodes
                              :where (= filename $s1)]
                             filename)
      (zettelkasten-db-query [:delete-from edges
                              :where (= filename $s1)]
                             filename)
      (kill-current-buffer)
      (delete-file filename))))

(defun zettelkasten-extract-value (keyword &optional element)
  "Extract value of KEYWORD while using ELEMENT."
  (org-element-map
      (or element (org-element-parse-buffer 'greater-element))
      'keyword
    (lambda (kw)
      (if (string= (org-element-property :key kw) keyword)
          (org-element-property :value kw)))
    :first-match t))


(provide 'zettelkasten)
;;; zettelkasten.el ends here
