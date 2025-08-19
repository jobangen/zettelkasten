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
  "#+TITLE: \n#+DATE: %U

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

(defcustom zettelkasten-subject-predicate "skos:subject"
  "Default predicate for descriptor keyword and filetag."
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-collection-predicate ""
  "Default predicate for collection keyword and filetag."
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-edges-mappings
  `(("RDF_TYPE" "rdf:type")
    ("COLLECTION" ,zettelkasten-collection-predicate zettelkasten--process-chain)
    ("DESCRIPTOR" ,zettelkasten-subject-predicate zettelkasten--process-chain)
    ("GENERATED_AT_TIME" "prov:generatedAtTime")
    ("INVALIDATED" "prov:invalidatedAtTime"))
  "Mapping org to linked data dm."
  :group 'zettelkasten
  :type 'list)

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

(defun zettelkasten-journal-capture ()
  (let* ((target (org-read-date))
         (time (org-read-date nil t target))
         (path (concat org-journal-dir target ".org")))
    (unless (file-exists-p path)
       (org-journal-new-entry t time))
    path))

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

(push '("J" "Zettel Journal" plain #'zettelkasten-journal-capture
        "ev%?"
        :immediate-finish t
        :jump-to-captured t)
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
  (let ((zettel-title (or title
                          (read-string "Title: "))))
    (setq zettel-capture-filename
          (zettelkasten--title-to-fname zettel-title))
    (org-capture nil "Z")
    (end-of-line)
    (insert zettel-title)
    (setq zettel-capture-filename nil)
    (if type
        (zettelkasten-set-type type)
      (zettelkasten-set-type))
    (zettelkasten-set-tag)
    (save-buffer)))

;;; Open from Zettel
(org-link-set-parameters "zk" :follow #'org-zettelkasten-open)

(defun org-zettelkasten-open (path)
  "Open file at PATH."
  (let* ((path (nreverse (split-string path "::")))
         (target (car path)))
    (find-file (caar (zettelkasten-db-query
                      [:select filename
                       :from nodes
                       :where (= zkid $s1)]
                      target)))
    (goto-char (point-min))
    (search-forward target nil t)
    (ignore-error (org-back-to-heading))))

(defun zettelkasten-flat-predicates ()
  "Return predicates as uniquified, flat list."
  (delete-dups (-flatten zettelkasten-predicates)))

(defun zettelkasten--set-keyword (keyword value)
  "Set KEYWORD to VALUE."
  (save-excursion
    (zettelkasten--ensure-keyword keyword)
    (delete-region (point) (line-end-position))
    (insert " " value)))

;;;###autoload
(defun zettelkasten-set-type (&optional type)
  "Set TYPE for zettel."
  (interactive)
  (zettelkasten--set-keyword
   "RDF_TYPE"
   (or type
       (completing-read "Type: " (-flatten zettelkasten-classes)))))

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

(defun zettelkasten-edit-custom-id ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (element (org-element-parse-buffer))
         (id-data (zettelkasten-get-property-or-keyword-upwards
                   filename element "CUSTOM_ID"))
         (title (zettelkasten--get-file-title))
         (title-proc (s-replace-all '((" " . "-")
                                      ("," . "")
                                      ("." . ""))
                                    title))
         (id-old (caar id-data))
         (id-entity (cadr id-data))
         (id-new (read-string "Custom ID: " (or id-old
                                                title-proc)))
         (id-old-object (-flatten (zettelkasten-db-query
                                   [:select :distinct [n:filename]
                                    :from nodes n
                                    :inner-join edges e
                                    :on (= n:zkid e:subject)
                                    :where (= object $s1)]
                                   id-old))))
    (save-excursion
      ;; update id
      (if (eq id-entity 'file)
          (zettelkasten--set-keyword "CUSTOM_ID" id-new)
        (org-set-property "CUSTOM_ID" id-new))

      ;; update subjects, this file
      (goto-char (point-min))
      (while (search-forward (concat ":" id-old "::") nil t)
        (replace-match (concat ":" id-new "::")))

      ;; update objects, this and other files
      (when id-old-object
        (y-or-n-p (format "Zk: There are %s links using the old ID. Update?"
                          (length id-old-object)))
        (dolist (zettel id-old-object)
          (find-file zettel)
          (goto-char (point-min))
          (while (search-forward (concat ":" id-old " ") nil t)
            (replace-match (concat ":" id-new "")))
          (goto-char (point-min))
          (while (search-forward (concat ":" id-old "]") nil t)
            (replace-match (concat ":" id-new "]")))
          (save-buffer)
          (unless (equal filename zettel)
            (kill-buffer)))))))


;;;###autoload
(defun zettelkasten-set-tag (&optional tag)
  "Set TAG for zettel."
  (interactive)
  (let* ((title (zettelkasten--get-file-title))
         (title-proc (s-replace-all '((" " . "-")
                                      ("," . "")
                                      ("." . ""))
                                    title))
         (tag-old
          (zettelkasten--get-keyword "TAG"))
         (tag-sel (or tag
                        (read-string "Tag: " (or tag-old
                                                 title-proc)))))
    (when (and tag-sel (stringp tag-sel) (string-match "\\S-" tag-sel))
      (zettelkasten--set-keyword "TAG" tag-sel))))

;;;###autoload
(defun zettelkasten-set-type-and-tag ()
  "Set type and label for zettel."
  (interactive)
  (zettelkasten-set-type)
  (unless (or (s-contains? "/txt/" (buffer-file-name))
              (s-contains? "/jr/" (buffer-file-name)))
    (zettelkasten-set-tag)))

(defun zettelkasten--get-predicates-by-domain (subject-types)
  "Get all predicates which domains match type in SUBJECT-TYPES."
  (-flatten
   (zettelkasten-db-query
    [:select [name]
     :from predicates
     :where (in domain $v1)]
    (vconcat subject-types))))

(defun zettelkasten--get-ranges-of-predicates (predicates)
  (-flatten
   (zettelkasten-db-query
    [:select [range]
     :from predicates
     :where (in name $v1)]
    (vconcat predicates))))

(defun zettelkasten--generalize-types (types)
  (delete-dups
   (mapcan
    (lambda (type)
      (zettelkasten--tree-parents-rec
       type
       zettelkasten-classes))
    types)))

(defun zettelkasten--format-info (format entries)
  (mapcar
   (lambda (entry)
     (let ((title (car entry))
           (filename (cadr entry))
           (zkid (caddr entry)))
       

       ))
   entries))


(defun zettelkasten--format-zettel-info (entries)
  "Format ENTRIES. Expects list of lists where (title filename zkid)."
  (mapcar
   (lambda (entry)
     (let ((title (car entry))
           (filename (cadr entry))
           (zkid (caddr entry)))
       (list
        (format "%s [%s]"
                (s-pad-right 120 " " (s-truncate 120 title))
                (s-truncate 40 (file-name-base filename)))
        zkid
        title)))
   entries))

(defun zettelkasten--select-node (&optional type-constraints)
  (save-excursion
    (let ((object nil))
      (ivy-read "Link Zettel: "
                (zettelkasten--format-zettel-info
                 (if type-constraints
                     (zettelkasten-db-query
                      [:select :distinct [n:title n:filename n:zkid]
                               :from nodes n
                               :inner-join edges e
                               :on (= n:zkid e:subject)
                               :where (= predicate "rdf:type")
                               :and (in object $v1)]
                      (vconcat type-constraints))
                   (zettelkasten-db-query [:select [title filename zkid]
                                           :from nodes])))
                :action
                (lambda (selection)
                  (if (listp selection)
                      (setq object (list (cadr selection) (caddr selection)))
                    (if (s-starts-with? "val:" selection)
                        (setq object (list (s-chop-prefix "val:" selection)
                                           (s-chop-prefix "val:" selection)))
                      (other-window 1)
                      (zettelkasten-new-zettel selection) ;;todo
                      (user-error "Error, because: %s" "new zettel!")))))
      object)))

(defun zettelkasten--build-link-triple (filename element)
  "Build triple for link using FILENAME and ELEMENT."
  (save-excursion
    (let* ((subject (caar (zettelkasten-get-property-or-keyword-upwards
                          filename element "CUSTOM_ID")))
           (subject-types (car (zettelkasten-get-property-or-keyword-upwards
                            filename element "RDF_TYPE")))
           (types-generalized (zettelkasten--generalize-types
                                subject-types))
           (predicate-by-domain (zettelkasten--get-predicates-by-domain
                                 types-generalized))
           (predicate (completing-read "Predicate: " predicate-by-domain))
           (predicate-range (zettelkasten--get-ranges-of-predicates
                             (list predicate)))
           (object-types (-flatten (zettelkasten--tree-children-rec
                                    (car predicate-range) zettelkasten-classes)))
           (object-id-title (zettelkasten--select-node object-types)))
      (list subject predicate object-id-title))))

;;;###autoload
(defun zettelkasten-insert-link (&optional subject predicate object)
  (interactive)
  (let* ((heading-p (or (s-starts-with? ":" (thing-at-point 'line t))))
         (triple (unless (and subject predicate object) (zettelkasten--build-link-triple
                                                         (buffer-file-name) (org-element-parse-buffer))))
         (subject (or subject (car triple)))
         (predicate (or predicate (cadr triple)))
         (object (or object (caddr triple)))
         (object-id (car object))
         (object-title (cadr object)))
    (if heading-p
        (zettelkasten--add-to-property "TURTLE"
                                       (concat predicate "::" object-id))
      (insert (format "[[zk:%s::%s::%s][%s]]"
                      subject predicate object-id object-title)))))


;;;###autoload
(defun zettelkasten-insert-link-loop ()
  "Insert Links as a org-list."
  (interactive)
  (insert "- ")
  (zettelkasten-insert-link)
  (newline)
  (zettelkasten-insert-link-loop))


(defun zettelkasten-heading-set-relation-to-context (&optional predicate target)
  "Set relation of heading to parent resource. Use PREDICATE if provided."
  (let* ((filename (buffer-file-name))
         (element (org-element-parse-buffer))
         (pred (or predicate
                   (completing-read
                    "Predicate: "
                    (zettelkasten-flat-predicates)
                    nil nil "dct:isPartOf")))
         (targets (append
                   (list (list (zettelkasten--get-file-title element)
                               (zettelkasten--get-file-id filename element)))
                   (org-element-map element 'headline
                     (lambda (headline)
                       (let ((h-zkid (org-element-property :CUSTOM_ID headline)))
                         (when h-zkid
                           (list (org-element-property :raw-value headline) 
                                 h-zkid)))))))
         (target (or target (cadr (assoc (completing-read "Target:" targets) targets))))
         (turtle (format "%s::%s" pred target)))
    (zettelkasten--add-to-property "TURTLE" turtle)))

;;;###autoload
(defun zettelkasten-heading-to-node (&optional rdftype prov-id predicate)
  "Create node form heading by adding RDFTYPE, PROV-ID and set relation to parent ressource with PREDICATE if provided."
  (interactive)
  (zettelkasten-set-type-headline rdftype)
  (zettelkasten-id-get-create prov-id)
  (zettelkasten-heading-set-relation-to-context predicate)
  (org-set-property "GENERATED_AT_TIME"
                    (concat (format-time-string "%Y-%m-%dT%H:%M:%S+")
                            (job/current-timezone-offset-hours))))

(defun zettelkasten-id-get-create (&optional prov-id ret)
  "Select and set ID for heading. Use PROV-ID if provided."
  (let ((zk-id (or prov-id (org-entry-get nil "CUSTOM_ID"))))
    (unless (or prov-id (and zk-id (stringp zk-id) (string-match "\\S-" zk-id)))
      (let* ((org-id-method 'ts)
             (org-id-ts-format "%Y-%m-%dT%H%M%S.%1N")
             (myid
              (or prov-id
                  (completing-read
                   "ID: "
                   (append
                    (list (org-id-new))
                    (zettelkasten--get-ids (buffer-file-name) (org-element-parse-buffer))))))
             (editid (read-string "Edit ID: " myid)))
        (setq zk-id editid)))
    (if ret
        zk-id
      (org-set-property "CUSTOM_ID" zk-id))))

;; Dirs and Queries

(defun zettelkasten--ensure-keyword (keyword)
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
  (zettelkasten--ensure-keyword "DESCRIPTOR")
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

(defun zettelkasten--get-all-descriptor-candidates ()
  (delete-dups
   (append
    (-flatten
     (zettelkasten-db-query
      [:select :distinct [object]
       :from edges
       :where (in predicate $v1)]
      (zettelkasten-predicate-hierachy
       zettelkasten-subject-predicate)))
    (-flatten
     (zettelkasten-db-query
      [:select :distinct [tag]
       :from tags])))))


;;;###autoload
(defun zettelkasten-zettel-add-descriptor (&optional descriptor)
  "Add DESCRIPTOR to zettel."
  (interactive)
  (let ((desc
         (or descriptor
             (completing-read
              "Descriptor: "
              (append
               '("#break#")
               (zettelkasten--get-all-descriptor-candidates))))))
    (unless (equal desc "#break#")
      (zettelkasten--add-to-keyword "DESCRIPTOR" desc))
    (unless (or descriptor (equal desc "#Break#"))
      (zettelkasten-zettel-add-descriptor))))

;;;###autoload
(defun zettelkasten-headline-add-descriptor ()
  "Add descriptor to current headline."
  (interactive)
  (let* ((add
          (completing-read
           "Descriptor [Headline]: "
           (zettelkasten--get-all-descriptor-candidates))))
    (zettelkasten--add-to-property "DESCRIPTOR" add))
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
(defun zettelkasten-open-zettel (&optional all-nodes)
  "Select and open zettel. ALL-NODES includes headings."
  (interactive)
  (ivy-read
   (format "Zettel: ")
   (if all-nodes
       (zettelkasten-db-title-filename-nodes)
     (zettelkasten-db--title-filename))
   :action
   (lambda (selection)
     (if (listp selection)
         (progn
           (find-file (cadr selection))
           (goto-char (point-min))
           (when all-nodes
             (search-forward (caddr selection) nil t)
             (org-back-to-heading)))
       (zettelkasten-new-zettel selection)))))

;;;###autoload
(defun zettelkasten-open-zettel-descriptor ()
  "Select descriptor and open resource that match."
  (interactive)
  (ivy-read
   "Zettel: " (zettelkasten-db-title-filename-nodes
               (zettelkasten-db--nodes-with-descriptor))
   :action
   (lambda (selection)
     (find-file
      (cadr selection)))))

(defun zettelkasten--tree-children-rec (key tree)
  "Accepts string (KEY) and nested list (TREE)
to return all children recursively."
  (when (consp tree)
    (cl-destructuring-bind (x . y) tree
      (if (equal x key)
          tree ;; fct of comparing
        (or (zettelkasten--tree-children-rec key x)
            (zettelkasten--tree-children-rec key y))))))

(defun zettelkasten--tree-parents-rec (key tree &optional path-in)
  "Accepts string (KEY) and nested list (TREE) to return all parents.
Uses PATH-IN internally to return path."
  (let ((path (or path-in nil)))
    (when (consp tree)
      (cl-destructuring-bind (x . y) tree
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
            (zettelkasten-db-query [:select :distinct [title zkid]
                                    :from nodes
                                    :where (in zkid $v1)]
                                   objects-edges))
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

(defun zettelkasten-get-property-or-keyword-upwards (filename element property &optional skip)
  "For ELEMENT of FILENAME get PROPERTY of next parent resource. SKIP one level if non-nil. Start at point."
  (when skip
    (outline-up-heading 1))
  (let ((return-value))
    (while (not return-value)
      (let ((prop-value (cdr (assoc property (org-entry-properties)))))
        (condition-case nil
            (if prop-value
                (setq return-value (list (split-string prop-value) 'heading))
              (outline-up-heading 1))
          (error
           (if (equal property "CUSTOM_ID")
               ;; corner case for ids and labels
               (setq return-value
                     (list
                      (split-string
                       (zettelkasten--get-file-id filename element))
                      'file))
             (setq return-value (list (split-string
                                       (zettelkasten--get-keyword
                                        property
                                        element))
                                      'file)))))))
    return-value))

(defun zettelkasten--get-backlinks (filename)
  "Files linking to node at point in FILENAME. Returns list of links."
  (let* ((zkid (caar (zettelkasten-get-property-or-keyword-upwards
                      filename
                      (org-element-parse-buffer)
                      "CUSTOM_ID")))
         (tags (-flatten (zettelkasten-db-query
                 [:select :distinct [tag]
                  :from tags
                  :where (= zkid $s1)]
                 zkid)))
         (backlinks
          (zettelkasten-db-query [:select [e:predicate n:title n:filename n:zkid]
                                  :from v_edges_union e
                                  :inner-join nodes n
                                  :on (= e:subject n:zkid)
                                  :where (in e:object $v1)]
                                 (vconcat (append `(,zkid) tags))))
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
                    (nth 3 link)))
            backlinks)))

;;;###autoload
(defun zettelkasten-open-backlink ()
  "Select and open backlink to current node."
  (interactive)
  (let* ((zettel (zettelkasten--get-backlinks (buffer-file-name)))
         (selected (assoc (completing-read "Zettel: " zettel) zettel)))
    (org-zettelkasten-open (cadr selected))))

;;;###autoload
(defun zettelkasten-update-org-agenda-files ()
  "Update `org-agenda' files and exclude files that are handled by `org-journal'."
  (interactive)
  (when zettelkasten-org-agenda-integration
    (let ((not-zettelkasten-agenda-files
           (seq-filter
            (lambda (filename)
              (not (s-starts-with? zettelkasten-zettel-directory filename)))
            (org-agenda-files)))
          (zettelkasten-agenda-files
           (-flatten (zettelkasten-db-query
                      [:select [filename]
                       :from files
                       :where (= todo 't)]))))
      (setq org-agenda-files (append not-zettelkasten-agenda-files
                                     zettelkasten-agenda-files)))))

;;;###autoload
(defun zettelkasten-rename-file ()
  "Rename file and update db accordingly."
  (interactive)
  (let* ((old (buffer-file-name))
         (new (read-string "New name: " old)))
    (zettelkasten-db-query [:delete-from files
                            :where (= filename $s1)]
                           old)
    (zettelkasten-db-query [:delete-from nodes
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
      (zettelkasten-db-query [:delete-from files
                              :where (= filename $s1)]
                             filename)
      (kill-current-buffer)
      (delete-file filename))))

(defun zettelkasten--get-keyword (keyword &optional element)
  "Get value of KEYWORD using ELEMENT."
  (org-element-map
      (or element
          (org-element-parse-buffer 'greater-element))
      'keyword
    (lambda (kw)
      (if (string= (org-element-property :key kw) keyword)
          (org-element-property :value kw)))
    :first-match t))

(defun zettelkasten--add-to-list (lst value)
  "Add VALUE to LST and sort it."
  (let* ((new (push value lst))
         (sorted (sort (delete-dups new) #'string<)))
    sorted))

(defun zettelkasten--add-to-keyword (keyword value)
  "Add VALUE to file KEYWORD."
  (let* ((old (split-string (or (zettelkasten--get-keyword keyword) "")))
         (new (zettelkasten--add-to-list old value))
         (str (mapconcat 'identity new " ")))
    (zettelkasten--set-keyword keyword str)))

(defun zettelkasten--add-to-property (property value)
  "Add VALUE to heading PROPERTY."
  (let* ((old (split-string (or (org-entry-get nil property) "")))
         (new (zettelkasten--add-to-list old value))
         (str (mapconcat 'identity new " ")))
    (org-set-property property str)))


(provide 'zettelkasten)
;;; zettelkasten.el ends here
