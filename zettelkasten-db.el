;;; zettelkasten-db.el --- Functions to maintain a zettel-archive -*- lexical-binding: t -*-

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
;; Library provides the database api.
;;
;; Zettelkasten
;;
;;; Code:
(require 'emacsql)
(require 'emacsql-sqlite)


(defcustom zettelkasten-db-file "~/.emacs.d/var/zettelkasten/zkdb.db"
  "Location of the zettelkasten database."
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-db-update-method 'when-idle
  "Method to update the zettelkasten database.
Options: `immediate' and `when-idle'."
  :group 'zettelkasten)

(defcustom zettelkasten-db-idle-seconds 2
  "Number of seconds to wait until database update is triggered."
  :type 'integer
  :group 'zettelkasten)

(defvar zettelkasten-db-dirty nil
  "Zettel that are to be updated. List of filenames.")

(defvar zettelkasten-db--connection (make-hash-table :test #'equal)
  "Database connection to Zettelkasten database.")

(defun zettelkasten-db--get-connection ()
  "Return the database connection, if any."
  (gethash (file-truename zettelkasten-zettel-directory)
           zettelkasten-db--connection))

(defconst zettelkasten-db--schemata
  '((capture [(id integer :primary-key)
              feed date priority])
    (files [(filename :unique :primary-key)
            (title :not-null)
            todo
            (modtime :not-null)
            (hash :non-null)])
    (id [(id integer :primary-key)
         (filename :not-null)
         (fileid :not-null)
         (zkid :unique :not-null)])
    (link [(id integer :primary-key)
           (filename :not-null)
           (linkid :not-null)])
    (index [(id integer :primary-key)
            filename entry title])
    (descriptor [(id integer :primary-key)
                 filename descriptor])
    (collection [(id integer :primary-key)
                 filename collection])
    (meta [(filename :unique :primary-key)
           (hash :non-null)])
    (nodes [(id integer :primary-key)
            (filename :not-null)
            (zkid :not-null :unique)
            (type :not-null)
            rdftype label title todo])
    (edges [(id integer :primary-key)
            filename subject predicate object])))


;; (zettelkasten-db-query [:create-table files
;;                                       ([(filename :unique :primary-key)
;;                                         (title :not-null)
;;                                         (id :not-null)
;;                                         (modtime :not-null)])])

;; files
;; filename, title, id, modification-time

(defun zettelkasten-db--initialize (db)
  (emacsql-with-transaction db
    (dolist (schema zettelkasten-db--schemata)
      (emacsql db [:create-table-if-not-exists $i1 $S2]
               (car schema) (cadr schema)))))

(defun zettelkasten-db ()
  "Entrypoint to zettelkasten database."
  (unless (and (zettelkasten-db--get-connection)
               (emacsql-live-p (zettelkasten-db--get-connection)))
    (make-directory (file-name-directory zettelkasten-db-file) t)
    (let* ((db-exists (file-exists-p zettelkasten-db-file))
           (conn (emacsql-sqlite zettelkasten-db-file)))
      (set-process-query-on-exit-flag (emacsql-process conn) nil)
      (puthash (file-truename zettelkasten-zettel-directory)
               conn
               zettelkasten-db--connection)
      (unless db-exists
        (zettelkasten-db--initialize conn))))
  (zettelkasten-db--get-connection))

(defun zettelkasten-db-query (sql &rest args)
  (apply #'emacsql (zettelkasten-db) sql args))

;;;###autoload
(defun zettelkasten-db-reset-cache ()
  (interactive)
  (ignore-errors
    (zettelkasten-db-query [:drop-table files])
    (message "Dropped table files")
    (zettelkasten-db-query [:drop-table id])
    (message "Dropped table id")
    (zettelkasten-db-query [:drop-table link])
    (message "Dropped table link")
    (zettelkasten-db-query [:drop-table index])
    (message "Dropped table index")
    (zettelkasten-db-query [:drop-table descriptor])
    (message "Dropped table descriptor")
    (zettelkasten-db-query [:drop-table collection])
    (message "Dropped table collection")
    (zettelkasten-db-query [:drop-table meta])
    (message "Dropped table meta")
    (zettelkasten-db-query [:drop-table nodes])
    (message "Dropped table nodes")
    (zettelkasten-db-query [:drop-table edges])
    (message "Dropped table edges"))
  (zettelkasten-db--initialize (emacsql-sqlite zettelkasten-db-file)))

;;; Extracting data
(defun zettelkasten--extract-title (&optional filename element)
  (zettelkasten-extract-value "TITLE" (when element)))

(defun zettelkasten--extract-id (filename element)
  (let* ((fileid (list (zettelkasten--filename-to-id filename)))
         (customids
          (org-element-map element 'node-property
            (lambda (property)
              (when (string= (org-element-property :key property) "CUSTOM_ID")
                (org-element-property :value property)))))
         (label (list (if (equal (zettelkasten-extract-value "ZK_LABEL" element) "t")
                          (zettelkasten--extract-title element)
                        (zettelkasten-extract-value "ZK_LABEL" element))))
         (ids (remove nil (append fileid customids label))))
    ;; when there is no label 'nil' is added to the list, which causes problems
    (mapcar (lambda (id)
              (list (car fileid) id))
            ids)))

(defun zettelkasten--extract-todo (element)
  (when (org-element-map element 'headline
          (lambda (headline)
            (org-element-property :todo-type headline)))
    t))


(defun zettelkasten--extract-links (element)
  (delete-dups (org-element-map element 'link
                 (lambda (link)
                   (when (or (string= (org-element-property :type link) "zk")
                             (string= (org-element-property :type link) "autocite"))
                     (car (nreverse
                       (split-string
                        (org-element-property :path link) "::"))))))))

(defun zettelkasten--extract-index (element)
  (ignore-errors
    (split-string
     (zettelkasten-extract-value "INDEX" element) "\"\s\"" t "\"")))

(defun zettelkasten--process-subjects (subjects)
  (delete-dups
   (-flatten
    (mapcar
     (lambda (subject)
       (let ((subject-list nil))
         (cond ((s-contains? "<>" subject)
                (let ((split (split-string subject "<>")))
                  (push subject subject-list)
                  (push
                   (format "%s>%s" (car split) (cadr split)) subject-list)
                  (push
                   (format "%s>%s" (cadr split) (car split)) subject-list)
                  (push (car split) subject-list)
                  (push (cadr split) subject-list)))
               ((s-contains? zettelkasten-descriptor-chain-sep subject)
                (let* ((split
                        (split-string
                         subject zettelkasten-descriptor-chain-sep))
                       (sub (car split)))
                  (push sub subject-list)
                  (pop split)
                  (dolist (subject split)
                    (setq sub
                          (concat
                           sub zettelkasten-descriptor-chain-sep subject))
                    (push sub subject-list))))
               (t (push subject subject-list)))
         subject-list))
     subjects))))

(defun zettelkasten-db--extract-types-file (&optional fname el)
  "Extracts rdftypes and return list of vectors"
  (let* ((filename (or fname (buffer-file-name)))
        (element (or el (org-element-parse-buffer)))
        (types (zettelkasten-extract-value "RDF_TYPE" element)))
    (when types
      (let ((types-split (split-string types)))
        (mapcar
         (lambda (type)
           (vector nil
                   filename
                   (zettelkasten--filename-to-id filename)
                   "rdf:type"
                   type))
         types-split)))))

(defun zettelkasten-db--extract-types-headings (&optional fname el)
  "Extracts rdftypes from headings and retuns list of vectors."
  (let* ((filename (or fname (buffer-file-name)))
         (element (or el (org-element-parse-buffer))))
    (-flatten
     (org-element-map element 'headline
       (lambda (headline)
         (when (org-element-property :CUSTOM_ID headline)
           (let ((customid (org-element-property :CUSTOM_ID headline))
                 (types (org-element-property :RDF_TYPE headline)))
             (when types
               (mapcar (lambda (type)
                         (vector nil filename customid "rdf:type" type))
                       (split-string types))))))))))


(defun zettelkasten-db--extract-subjects-file (filename element)
  "Extracts subjects, processes them and return list of vectors"
  (let ((subjects
         (zettelkasten-extract-value "DESCRIPTOR" element)))
    (when subjects
      (let ((subjects-proc (zettelkasten--process-subjects
                            (split-string subjects))))
        (mapcar
         (lambda (subject)
           (vector nil
                   filename
                   (zettelkasten--filename-to-id filename)
                   "skos:subject"
                   subject))
         subjects-proc)))))

(defun zettelkasten-db--extract-subjects-headings (filename element)
  "Extracts headline-subjects, processes them and return list of vectors"
  (-flatten
   (org-element-map element 'headline
     (lambda (headline)
       (when (org-element-property :CUSTOM_ID headline)
         (let ((customid (org-element-property :CUSTOM_ID headline))
               (subjects (org-element-property :DESCRIPTOR headline)))
           (when subjects
             (mapcar (lambda (subject)
                       (vector nil filename customid "skos:subject" subject))
                     (zettelkasten--process-subjects
                      (split-string subjects))))))))))



(defun zettelkasten--extract-descriptor (element)
  (ignore-errors
    (let* ((descriptor-keyword
            (split-string (zettelkasten-extract-value "DESCRIPTOR" element)))
           (descriptor-headings
            (-flatten
             (org-element-map element 'node-property
               (lambda (property)
                 (when (string= (org-element-property :key property) "DESCRIPTOR")
                   (split-string (org-element-property :value property)))))))
           (descriptor-conc
            (append descriptor-keyword descriptor-headings))
           (descriptor-list nil))
      (dolist (descriptor descriptor-conc)
        (cond ((s-contains? "<>" descriptor)
               (let* ((chain-split (split-string
                                    (s-replace "#" "" descriptor) "<>")))
                 (push (format "#%s" (car chain-split)) descriptor-list)
                 (push (format "#%s" (cadr chain-split)) descriptor-list)
                 (push (format "#%s<>%s"
                               (car chain-split) (cadr chain-split))
                       descriptor-list)
                 (push (format "#%s>%s"
                               (car chain-split) (cadr chain-split))
                       descriptor-list)
                 (push (format "#%s>%s"
                               (cadr chain-split) (car chain-split))
                       descriptor-list)))
              ((s-contains? zettelkasten-descriptor-chain-sep descriptor)
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
              (t (push descriptor descriptor-list))))
      descriptor-list)))

(defun zettelkasten--extract-collection (element)
  (ignore-errors
    (let* ((collection-keyword
            (split-string (zettelkasten-extract-value "COLLECTION" element)))
           (collection-headings
            (-flatten
             (org-element-map element 'node-property
               (lambda (property)
                 (when (string= (org-element-property :key property) "COLLECTION")
                   (split-string (org-element-property :value property)))))))
           (collection-conc
            (append collection-keyword collection-headings))
           (collection-list nil))
      (dolist (descriptor collection-conc)
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


;;; Update database
(defun zettelkasten-db--update-files (filename element)
  (let ((title (zettelkasten--extract-title filename element))
        (todo (zettelkasten--extract-todo element))
        (modtime (format-time-string "%Y-%m-%d %H:%M:%S%3N"))
        (hash (secure-hash 'sha1 (current-buffer))))
    (zettelkasten-db-query [:delete-from files
                                         :where (= filename $s1)]
                           filename)
    (zettelkasten-db-query [:insert :into files
                                    :values $v1]
                           (vector filename title todo modtime hash))))

(defun zettelkasten-db--update-id (filename element)
  (let* ((ids (zettelkasten--extract-id filename element))
         (ids-vec (mapcar
                   (lambda (id)
                     (vector nil filename (car id) (cadr id)))
                   ids)))
    (zettelkasten-db-query [:delete-from id
                            :where (= filename $s1)]
                           filename)
    (zettelkasten-db-query [:insert :into id
                            :values $v1]
                           ids-vec)))

(defun zettelkasten-db--update-link (filename element)
  (let ((links (zettelkasten--extract-links element)))
    (when links
      (let ((link-vec (mapcar
                       (lambda (link)
                         (vector nil filename link))
                       links)))
        (zettelkasten-db-query [:delete-from link
                                :where (= filename $s1)]
                               filename)
        (zettelkasten-db-query [:insert :into link
                                :values $v1]
                               link-vec)))))

(defun zettelkasten-db--update-index (filename element)
  (let ((index (zettelkasten--extract-index element)))
    (when index
      (let ((idx-vec (mapcar
                      (lambda (idx)
                        (let* ((i (split-string idx "::"))
                               (entry (car i))
                               (title (if (equal (cadr i ) "t")
                                          (zettelkasten--extract-title)
                                        (cadr i))))
                          (vector nil filename entry title)))
                      index)))
        (zettelkasten-db-query [:delete-from index
                                             :where (= filename $s1)]
                               filename)
        (zettelkasten-db-query [:insert :into index
                                        :values $v1]
                               idx-vec)))))

(defun zettelkasten-db--update-descriptor (filename element)
  (let ((descriptor (zettelkasten--extract-descriptor element)))
    (when descriptor
      (let ((desc-vec (mapcar
                      (lambda (desc)
                        (vector nil filename desc))
                      descriptor)))
        (zettelkasten-db-query [:delete-from descriptor
                                             :where (= filename $s1)]
                               filename)
        (zettelkasten-db-query [:insert :into descriptor
                                        :values $v1]
                               desc-vec)))))

(defun zettelkasten-db--update-collection (filename element)
  (let ((collection (zettelkasten--extract-collection element)))
    (when collection
      (let ((col-vec (mapcar
                      (lambda (col)
                        (vector nil filename col))
                      collection)))
        (zettelkasten-db-query [:delete-from collection
                                             :where (= filename $s1)]
                               filename)
        (zettelkasten-db-query [:insert :into collection
                                        :values $v1]
                               col-vec)))))

(defun zettelkasten-db--update-meta (filename)
  (let ((hash (secure-hash 'sha1 (current-buffer))))
    (zettelkasten-db-query [:delete-from meta
                            :where (= filename $s1)]
                           filename)
    (zettelkasten-db-query [:insert :into meta
                            :values $v1]
                           (vector filename hash))))

(defun zettelkasten-db--update-nodes (filename element)
  "Format [id filename zkid type rdftype label title]"
  (let* ((f-zkid (zettelkasten--filename-to-id filename))
         (f-rdftype (zettelkasten-extract-value "RDF_TYPE" element))
         (f-label (zettelkasten-extract-value "ZK_LABEL" element))
         (f-title (zettelkasten--extract-title filename element))
         (f-todo (zettelkasten-extract-todo-state element))
         (vfile
          (list
           (vector nil filename f-zkid "file" f-rdftype f-label f-title f-todo)))
         (vhead (org-element-map element 'headline
                  (lambda (x)
                    (let ((h-zkid (org-element-property :CUSTOM_ID x)))
                      (when h-zkid
                        (vector nil
                                filename
                                h-zkid
                                "heading"
                                (org-element-property :RDF_TYPE x)
                                (org-element-property :ZK_LABEL x)
                                (org-element-property :raw-value x)
                                nil))))))
         (vcomp (append vfile vhead)))
    (zettelkasten-db-query [:delete-from nodes
                                         :where (= filename $s1)]
                           filename)
    (zettelkasten-db-query [:insert :into nodes
                                    :values $v1]
                           vcomp)
    vcomp))

(defun zettelkasten-db--update-edges (&optional fname el)
  (let* ((filename (or fname (buffer-file-name)))
         (element (or el (org-element-parse-buffer)))
         (ftype (zettelkasten-db--extract-types-file filename element))
         (htype (zettelkasten-db--extract-types-headings filename element))
         (fsubjects (zettelkasten-db--extract-subjects-file filename element))
         (hsubjects (zettelkasten-db--extract-subjects-headings filename element))
         (turtle
          (-flatten
           (org-element-map element 'headline
             (lambda (headline)
               (when (org-element-property :CUSTOM_ID headline)
                 (let ((customid (org-element-property :CUSTOM_ID headline))
                       (turtle
                        (ignore-errors
                          (split-string
                           (org-element-property :TURTLE headline)))))
                   (append
                    (mapcar (lambda (pair)
                              (let ((split (split-string pair "::")))
                                (vector
                                 nil filename customid
                                 (car split) (cadr split))))
                            turtle)
                    (when (and (s-contains? "/zettel/jr/" filename)
                               (member
                                (org-element-property :RDF_TYPE headline)
                                (-flatten
                                 (assoc "prov:Activity" zettelkasten-classes))))
                      (list (vector nil
                                    filename customid
                                    "time:intervalDuring"
                                    (file-name-base filename)))))))))))
         (orglinks
          (org-element-map element 'link
            (lambda (link)
              (when (or (string= (org-element-property :type link) "zk")
                        (string= (org-element-property :type link) "autocite"))
                (let ((linksplit (split-string
                                  (org-element-property :path link) "::"))
                      (zettelid (zettelkasten--filename-to-id filename)))
                  ;; not all links have subject, predicate, object
                  (cond ((= 1 (length linksplit))
                         (vector nil
                                 filename
                                 zettelid
                                 nil
                                 (car linksplit)))
                        ((= 2 (length linksplit))
                         (vector nil
                                 filename
                                 zettelid
                                 (car linksplit)
                                 (cadr linksplit)))
                        ((= 3 (length linksplit))
                         (vector nil
                                 filename
                                 (car linksplit)
                                 (cadr linksplit)
                                 (caddr linksplit)))))))))
         (links (append ftype htype fsubjects hsubjects turtle orglinks)))
    (zettelkasten-db-query [:delete-from edges
                            :where (= filename $s1)]
                           filename)
    (when links
      (zettelkasten-db-query [:insert :into edges
                              :values $v1]
                             links))
    links))

;; TODO: improve support for option filename
(defun zettelkasten-db-update-zettel (&optional filename hash)
  (let* ((fname (file-truename (or filename (buffer-file-name))))
         (curr-hash (or hash (secure-hash 'sha1 (current-buffer))))
         (db-hash (caar (zettelkasten-db-query [:select hash :from meta
                                                :where (= filename $s1)]
                                               fname))))
    (unless (string= curr-hash db-hash)
      (let ((element (org-element-parse-buffer)))
        ;; (zettelkasten-db--update-files fname element)
        ;; (zettelkasten-db--update-id fname element)
        ;; (zettelkasten-db--update-link fname element)
        ;; (zettelkasten-db--update-index fname element)
        ;; (zettelkasten-db--update-descriptor fname element)
        (zettelkasten-db--update-collection fname element)
        (zettelkasten-db--update-meta fname)
        (zettelkasten-db--update-nodes fname element)
        (zettelkasten-db--update-edges fname element)))))

(defun zettelkasten-db--mark-dirty ()
  (add-to-list 'zettelkasten-db-dirty (buffer-file-name)))

(defun zettelkasten-db--update-on-timer ()
  (let ((len (length zettelkasten-db-dirty)))
    (when zettelkasten-db-dirty
      (dolist (filename zettelkasten-db-dirty)
        (with-temp-buffer
          (condition-case nil
              (progn
                (insert-file-contents filename)
                (org-mode)              ;; necessary for parsing of todo-state
                (zettelkasten-db-update-zettel filename))
            (error (message (format "Zettelkasten: File '%s' missing" filename))))
          (pop zettelkasten-db-dirty)))
      (message "Zettelkasten: Updated %s zettel." len))))

(defun zettelkasten-db-update ()
  "Update database"
  (pcase zettelkasten-db-update-method
    ('immediate
     (zettelkasten-db-update-zettel))
    ('when-idle
     (zettelkasten-db--mark-dirty))
    (_
     (user-error "Invalid `zettelkasten-db-update-method'"))))

(defun zettelkasten-zettel-p (&optional filename)
  (s-starts-with?
   zettelkasten-zettel-directory
   (or filename (buffer-file-name))))

(add-hook 'after-save-hook (lambda ()
                             (when (zettelkasten-zettel-p)
                               (zettelkasten-db-update))))

(when (eq zettelkasten-db-update-method 'when-idle)
  (run-with-idle-timer
   zettelkasten-db-idle-seconds t #'zettelkasten-db--update-on-timer))


(defun zettelkasten-db-process ()
  (interactive)
  (let* ((files (zettelkasten--get-all-files)))
    (setq zettelkasten-db-dirty (append zettelkasten-db-dirty files))))

;;; Helper
(defun zettelkasten-extract-todo-state (el)
  (when (org-element-map el 'headline
          (lambda (headline)
            (org-element-property :todo-type headline)))
    t))

(defun zettelkasten-db--title-filename (&optional filenames)
  "Returns list of lists: title, filename, zkid only for files."
  (if filenames
      (zettelkasten-db-query [:select [title filename zkid]
                                      :from nodes
                                      :where (in filename $v1)
                                      :and (= type "file")]
                             (vconcat filenames))
    (zettelkasten-db-query [:select [title filename zkid]
                                    :from nodes
                                    :where (= type "file")])))

(defun zettelkasten-db-title-filename-nodes ()
  "Returns list of lists: title [file-name-base], filename, zkid for all nodes."
  (mapcar
   (lambda (node)
     (list
      (format "%s [%s]" (car node) (file-name-base (cadr node)))
      (cadr node)
      (caddr node)))
   (zettelkasten-db-query
    [:select [title filename zkid]
             :from nodes])))


(defun zettelkasten-db--values-descriptor (&optional files)
  "Returns list of disctinct values for subject."
  (if files
      (-flatten
       (zettelkasten-db-query [:select :distinct [object]
                                       :from edges
                                       :where (in filename $v1)
                                       :and (= predicate "skos:subject")]
                              (vconcat files)))
    (-flatten
     (zettelkasten-db-query [:select :distinct [object]
                                     :from edges
                                     :where (= predicate "skos:subject")
                                     :or (= predicate "skos:primarySubject")]))))

(defun zettelkasten-db--files-matching-descriptor (&optional input-files)
  "Select descriptor and return matching zettel."
  (let* ((subject
          (completing-read
           (format "Subject [%s]: " (when input-files (safe-length input-files)))
           (if input-files
               (append (zettelkasten-db--values-descriptor input-files)
                       '("Break"))
             (zettelkasten-db--values-descriptor))))
         (output-files
          (if input-files
              (-flatten (zettelkasten-db-query
                         [:select :distinct [filename]
                          :from edges
                          :where (in filename $v1)
                          :and (= predicate "skos:subject")
                          :and (= object $s2)
                          ]
                         (vconcat input-files) subject))
            (-flatten
             (zettelkasten-db-query [:select :distinct [filename]
                                     :from edges
                                     :where (= object $s1)
                                     :and (= predicate "skos:subject")]
                                    subject)))))
    (if (string= subject "Break")
        input-files
      (if (<= (safe-length output-files) zettelkasten-descriptor-cycle-threshold)
          output-files
        (zettelkasten-db--files-matching-descriptor output-files)))))



;; (length (zettelkasten-db-query [:select [title id]
;;                                 :from files]))

(provide 'zettelkasten-db)
;;; zettelkasten.el ends here
