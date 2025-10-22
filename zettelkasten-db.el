;;; zettelkasten-db.el --- Functions to maintain a zettel-archive -*- lexical-binding: t -*-

;; Copyright (C) 2017 Jan Ole Bangen.

;; Author: Jan Ole Bangen <jobangen@gmail.com>
;; URL:
;; Package-Version:
;; Version: 0.5.0
;; Package-Requires: emacsql emacsql-sqlite
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
(require 'dash)

(defcustom zettelkasten-db-file "~/.emacs.d/var/zettelkasten/zkdb.db"
  "Location of the zettelkasten database."
  :group 'zettelkasten
  :type 'string)

(defcustom zettelkasten-db-update-method 'when-idle
  "Method to update the zettelkasten database.
Options: `immediately' `immediately-async' and `when-idle'."
  :group 'zettelkasten)

(defcustom zettelkasten-db-idle-seconds 2
  "Number of seconds to wait until database update is triggered."
  :type 'integer
  :group 'zettelkasten)

(defcustom zettelkasten-db-predicate-data '()
  "Predicates, List of vectors with format: [nil name domain range invers]"
  :type 'list
  :group 'zettelkasten)

(defcustom zettelkasten-db-emacsql-lib 'emacsql-sqlite
  "Emacsql library to use. Options: emacsql-sqlite, emacsql-sqlite3 "
  :type 'symbol
  :group 'zettelkasten)

(when (eq zettelkasten-db-update-method 'immediately-async)
  (require 'async))

(require zettelkasten-db-emacsql-lib)

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

    (files
     ( [(id integer :primary-key)
        (filename :unique)
        (hash :not-null)
        todo]))

    (nodes
     ([(id integer :primary-key)
       (filename :not-null)
       (zkid :not-null :unique)
       (type :not-null)
       title]
      (:foreign-key [filename] :references files [filename] :on-delete :cascade)))

    (tags
     ([(id integer :primary-key)
       (zkid :not-null)
       (tag :not-null)
       ]
      (:foreign-key [zkid] :references nodes [zkid] :on-delete :cascade)))

    (predicates
     ([(id integer :primary-key)
       (name :non-null :unique)
       domain range
       (inverse :unique)]))

    (edges
     ([(id integer :primary-key)
       (subject :not-null)
       predicate
       (object :not-null)]
      (:foreign-key [subject] :references nodes [zkid] :on-delete :cascade)))

    (edges_inferred
     ([(id integer :primary-key)
       (inferred_from)
       (subject)
       (predicate)
       (object)]
      (:foreign-key [inferred_from] :references edges [id] :on-delete :cascade)
      (:foreign-key [object] :references nodes [zkid] :on-delete :cascade)
      ))
    ))

(defconst zettelkasten-db--trigger-edges
  "CREATE TRIGGER IF NOT EXISTS
infer_edges_after_insert_edges
AFTER INSERT ON edges
WHEN
(SELECT inverse from predicates where name = NEW.predicate) IS NOT NULL
BEGIN
INSERT INTO edges_inferred (inferred_from, subject, predicate, object)
VALUES (NEW.id,
NEW.object,
(SELECT inverse FROM predicates WHERE name = NEW.predicate),
NEW.subject);
END")

(defconst zettelkasten-db--view-edges-union
  "CREATE VIEW IF NOT EXISTS
v_edges_union
AS
SELECT subject, predicate, object FROM edges
UNION ALL
SELECT subject, predicate, object FROM edges_inferred")

(defun zettelkasten-db--initialize (db)
  "Initialize DB with schema."
  (emacsql-with-transaction db
    (dolist (schema zettelkasten-db--schemata)
      (emacsql db [:create-table-if-not-exists $i1 $S2]
               (car schema) (cadr schema))
      (message "Zk: created table: %s" (car schema)))
    (unless (eq zettelkasten-db-emacsql-lib 'emacsql-sqlite3)
      (emacsql db zettelkasten-db--trigger-edges)
      (message "Zk: created trigger: infer_edges_after_insert_edges"))
    (emacsql db zettelkasten-db--view-edges-union)
    (message "Zk: created view: v_edges_union")))

(defun zettelkasten-db ()
  "Entrypoint to zettelkasten database."
  (unless (and (zettelkasten-db--get-connection)
               (emacsql-live-p (zettelkasten-db--get-connection)))
    (make-directory (file-name-directory zettelkasten-db-file) t)
    (let* ((db-exists (file-exists-p zettelkasten-db-file))
           (conn (funcall zettelkasten-db-emacsql-lib zettelkasten-db-file)))
      ;; (set-process-query-on-exit-flag (emacsql-process conn) nil)
      (puthash (file-truename zettelkasten-zettel-directory)
               conn
               zettelkasten-db--connection)
      (unless db-exists
        (zettelkasten-db--initialize conn))))
  (zettelkasten-db--get-connection))

(defun zettelkasten-db-query (sql &rest args)
  "SQL and ARGS are passed to `zettelkasten-db'."
  (apply #'emacsql (zettelkasten-db) sql args))

;;;###autoload
(defun zettelkasten-db-reset-cache ()
  "Reset cache by dropping tables"
  (interactive)
  (let ((db (funcall zettelkasten-db-emacsql-lib zettelkasten-db-file)))
    (dolist (schema (reverse zettelkasten-db--schemata))
      (message "Zk: dropping table: %s" (car schema))
      (emacsql db [:drop-table-if-exists $i1] (car schema)))
    (message "Zk: dropping trigger: infer_edges_after_insert_edges")
    (zettelkasten-db-query
     "DROP TRIGGER IF EXISTS infer_edges_after_insert_edges")
    (message "Zk: dropping view: v_edges_union")
    (zettelkasten-db-query
     "DROP VIEW IF EXISTS v_edges_union")
    (zettelkasten-db--initialize db)
    (zettelkasten-db-update-predicates)
    (when (y-or-n-p "Zk: Process zettel?")
      (zettelkasten-db-process))))

;;;###autoload
(defun zettelkasten-db-update-predicates ()
  "Update predicate info."
  (interactive)
  (zettelkasten-db-query [:delete-from predicates])
  (message "Inserting predicates")
  (zettelkasten-db-query [:insert :into predicates :values $v1]
                         zettelkasten-db-predicate-data))

(defun zettelkasten--process-chain (subjects)
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
                  ;; add complete descriptor chain
                  (push sub subject-list)
                  (pop split)
                  (dolist (subject split)
                    (setq sub
                          (concat
                           sub zettelkasten-descriptor-chain-sep subject))
                    (unless (s-ends-with? ">" sub)
                      (push sub subject-list)))))
               (t (push subject subject-list)))
         subject-list))
     subjects))))

(defun zettelkasten--triples-for-objects (subject predicate objects)
  "Create triple using SUBJECT, PREDICATE for each item in OBJECTS and return list of vectors."
  (mapcar
   (lambda (object)
     (vector nil subject predicate object))
   objects))

(defun zettelkasten--triples-for-keyword (filename element keyword predicate &optional objectfn)
  "Create list of triples for KEYWORD using FILENAME, ELEMENT and PREDICATE. Optionally process value of KEYWORD first with OBJECTFN."
  (let ((objects (zettelkasten--get-keyword keyword element)))
    (when objects
      (zettelkasten--triples-for-objects
       (zettelkasten--get-file-id filename element)
       predicate
       (if objectfn
           (funcall objectfn (split-string objects))
         (split-string objects))))))

(defun zettelkasten--triples-for-property (element property predicate &optional objectfn)
  "Create list of triples for PROPERTY using ELEMENT and PREDICATE. Optionally process value of PROPERTY first with OBJECTFN."
  (-flatten
   (org-element-map element 'headline
     (lambda (headline)
       (when (org-element-property :CUSTOM_ID headline)
         (let ((customid (org-element-property :CUSTOM_ID headline))
               (objects (org-element-property property headline)))
           (when objects
             (zettelkasten--triples-for-objects
              customid
              predicate
              (if objectfn
                  (funcall objectfn (split-string objects))
                (split-string objects))))))))))

;;; Extracting data
(defun zettelkasten--get-file-todo-state (filename el)
  (when (or (member
             'todo
             (org-element-map el 'headline
               (lambda (headline)
                 (org-element-property :todo-type headline))))
            (and (s-starts-with? org-journal-dir filename)
                 (time-less-p (current-time)
                              (date-to-time (concat
                                             (file-name-base filename)
                                             "T24:00:00")))))
    t))

(defun zettelkasten--get-file-id (filename element)
  "Get id for FILENAME using ELEMENT."
  (or (zettelkasten--get-keyword "CUSTOM_ID" (when element))
      (zettelkasten--filename-to-id filename)))

(defun zettelkasten--get-ids (filename element)
  (append
   (list (zettelkasten--get-file-id filename element))
   (org-element-map element 'headline
     (lambda (headline)
       (org-element-property :CUSTOM_ID headline)))))

(defun zettelkasten--get-file-title (&optional element)
  "Get title of zettel using ELEMENT if provided."
  (zettelkasten--get-keyword "TITLE" (when element)))

(defun zettelkasten--get-file-node (filename element)
  (let ((f-zkid (zettelkasten--get-file-id filename element))
        (f-title (zettelkasten--get-file-title element)))
    (list (vector nil filename f-zkid 'file f-title))))

(defun zettelkasten--get-tags (filename element)
  (let* ((file-zkid (zettelkasten--get-file-id filename element))
         (file-tags (zettelkasten--get-keyword "TAG" element))
         (zkid-tag-pairs
          (append
           (when file-tags
             (mapcar
              (lambda (tag)
                (vector nil file-zkid tag))

              (split-string file-tags)))
           (apply #'append (org-element-map element 'headline
                             (lambda (headline)
                               (let ((headline-zkid (org-element-property :CUSTOM_ID headline)))
                                 (when headline-zkid
                                   (let ((headline-tags (org-element-property :TAG headline)))
                                     (when headline-tags
                                       (mapcar
                                        (lambda (tag)
                                          (vector nil headline-zkid tag))
                                        (split-string headline-tags))))))))))))

    zkid-tag-pairs))


(defun zettelkasten--get-headings-nodes (filename element)
  (org-element-map element 'headline
    (lambda (x)
      (let ((h-zkid (org-element-property :CUSTOM_ID x)))
        (when h-zkid
          (vector nil
                  filename
                  h-zkid
                  'heading
                  (org-element-property :raw-value x) ;; title
                  ))))))

(defun zettelkasten--get-key-prop-edges (filename element)
  (let ((edges nil))
    (dolist (mapping zettelkasten-edges-mappings)
      (let* ((org (car mapping))
             (org-symbol (intern (concat ":" org)))
             (predicate (cadr mapping))
             (fun (caddr mapping)))
        (setq edges
              (append
               edges
               (zettelkasten--triples-for-keyword filename element org predicate fun)
               (zettelkasten--triples-for-property element org-symbol predicate fun)))))
    edges))

(defun zettelkasten--get-headings-turtle (element)
  (-flatten
   (org-element-map element 'headline
     (lambda (headline)
       (when (org-element-property :CUSTOM_ID headline)
         (let ((customid (org-element-property :CUSTOM_ID headline))
               (turtle (org-element-property :TURTLE headline)))
           (when turtle
             (mapcar
              (lambda (pred-obj)
                (let ((split (split-string pred-obj "::")))
                  (vector nil customid (car split) (cadr split))))
                      (split-string turtle)))))))))

(defun zettelkasten--get-orglinks (filename element)
  (org-element-map element 'link
    (lambda (link)
      (when (or (string= (org-element-property :type link) "zk")
                (string= (org-element-property :type link) "autocite"))
        (let ((linksplit (split-string
                          (org-element-property :path link) "::"))
              (zettelid (zettelkasten--get-file-id filename element)))
          ;; not all links have subject, predicate, object
          (cond ((= 1 (length linksplit))
            (vector nil zettelid nil (car linksplit)))
           ((= 2 (length linksplit))
            (vector nil zettelid (car linksplit) (cadr linksplit)))
           ((= 3 (length linksplit))
            (vector nil (car linksplit) (cadr linksplit) (caddr linksplit)))
           ))))))

;;; Update database
(defun zettelkasten-db--update-files (filename element &optional debug)
  ""
  (let* ((hash (secure-hash 'sha1 (current-buffer)))
         (todo (zettelkasten--get-file-todo-state filename element))
         (file (vector nil filename hash todo)))
     (zettelkasten-db-query [:delete-from files
                             :where (= filename $s1)]
                            filename)
    (if (not debug)
        (zettelkasten-db-query [:insert :into files
                                :values $v1]
                               file)
      (message (format "Inserting file: %s" file))
      (zettelkasten-db-query [:insert :into files
                              :values $v1]
                             file))))

(defun zettelkasten-db--update-nodes (filename element &optional debug)
  "Format [id FILENAME zkid type title] using ELEMENT."
  (let* ((file (zettelkasten--get-file-node filename element))
         (headings (zettelkasten--get-headings-nodes filename element))
         (nodes (append file headings)))
    (when (eq zettelkasten-db-emacsql-lib 'emacsql-sqlite3)
      (zettelkasten-db-query [:delete-from nodes
                              :where (= filename $s1)]
                             filename))
    (if (not debug)
        (zettelkasten-db-query [:insert :into nodes
                                :values $v1]
                               nodes)
      (dolist (node nodes)
        (message (format "Inserting node: %s" node))
        (zettelkasten-db-query [:insert :into nodes
                                :values $v1]
                               node)))))

(defun zettelkasten-db--update-tags (filename element &optional debug)
  (let* ((zkid-tag-rows (zettelkasten--get-tags filename element)))
    (when debug
      (message "Updating tags")
      (message (format "pairs %s" zkid-tag-rows)))

    (when zkid-tag-rows
      (zettelkasten-db-query [:insert :into tags
                              :values $v1]
                             zkid-tag-rows))))


(defun zettelkasten-db--update-edges (&optional fname el debug)
  "Update table edges for FNAME using EL."
  (let* ((filename (or fname (buffer-file-name)))
         (element (or el (org-element-parse-buffer)))
         (ids (zettelkasten--get-ids filename element))
         (key-prop (zettelkasten--get-key-prop-edges filename element))
         (turtle (zettelkasten--get-headings-turtle element))
         (orglinks (zettelkasten--get-orglinks filename element))
         (edges (append key-prop turtle orglinks)))

    (when (eq zettelkasten-db-emacsql-lib 'emacsql-sqlite3)
      (zettelkasten-db-query [:delete-from edges
                              :where (in subject $v1)]
                             (vconcat ids)))
    (when edges
      (if (not debug)
          (zettelkasten-db-query [:insert :into edges
                                  :values $v1]
                                 edges)
        (dolist (edge edges)
          (message (format "Inserting edge: %s" edge))
          (zettelkasten-db-query [:insert :into edges
                                  :values $v1]
                                 edge))))))

;; TODO: improve support for option filename
(defun zettelkasten-db-update-zettel (&optional filename hash)
  (let* ((fname (file-truename (or filename (buffer-file-name))))
         (curr-hash (or hash (secure-hash 'sha1 (current-buffer))))
         (db-hash (caar (zettelkasten-db-query [:select hash :from files
                                                :where (= filename $s1)]
                                               fname))))
    (unless (string= curr-hash db-hash)
      (message "Zettelkasten: Updating... %s" (car (last (split-string fname "/"))))
      (let ((element (org-element-parse-buffer)))
        (zettelkasten-db--update-files fname element)
        (zettelkasten-db--update-nodes fname element)
        (zettelkasten-db--update-tags fname element)
        (zettelkasten-db--update-edges fname element))))
  (when zettelkasten-org-agenda-integration
    (zettelkasten-update-org-agenda-files))
  t)

(defun zettelkasten-db-update-zettel-async (filename hash)
  (async-start
   `(lambda ()
      (add-to-list 'load-path "~/.emacs.d/straight/build/s")
      (add-to-list 'load-path "~/.emacs.d/straight/build/dash")
      (add-to-list 'load-path "~/.emacs.d/straight/build/org")
      (add-to-list 'load-path "~/.emacs.d/straight/build/org-journal")
      (add-to-list 'load-path "~/.emacs.d/straight/build/emacsql")
      (add-to-list 'load-path "~/.emacs.d/straight/build/emacsql-sqlite")
      (add-to-list 'load-path "~/.emacs.d/straight/build/zettelkasten")
      (require 's)
      (require 'dash)
      (require 'org-capture)
      (require 'org-element)
      (require 'org-journal)
      (require 'emacsql)
      (require 'emacsql-sqlite-builtin)
      (require 'zettelkasten)
      ,(zettelkasten-db-update-zettel filename hash)
      (file-name-base ,filename))
   (lambda (result)
     (message "[zk] async update finished: '%s'" result))))

(defun zettelkasten-db--mark-dirty ()
  (unless (string-match-p "_archive$" (buffer-file-name))
    (add-to-list 'zettelkasten-db-dirty (buffer-file-name))))

(defun zettelkasten-db--update-on-timer ()
  (let ((len (length zettelkasten-db-dirty)))
    (when zettelkasten-db-dirty
      (dolist (filename zettelkasten-db-dirty)
        (with-temp-buffer
          (condition-case err
              (progn
                (insert-file-contents filename)
                (org-mode) ;; necessary for todo-state parsing
                (zettelkasten-db-update-zettel filename))
            (error (message (format "zk debug: Updating '%s', error: %s" filename (error-message-string err)))))
          (pop zettelkasten-db-dirty)))
      (message "Zettelkasten: Updated %s zettel." len))))

(defun zettelkasten-db-update (filename hash)
  "Update database"
  (pcase zettelkasten-db-update-method
    ('immediately
     (zettelkasten-db-update-zettel))
    ('when-idle
     (zettelkasten-db--mark-dirty))
    ('immediately-async
     (zettelkasten-db-update-zettel-async filename hash))
    (_
     (user-error "Invalid `zettelkasten-db-update-method'"))))

(defun zettelkasten-zettel-p (&optional filename)
  (s-starts-with?
   zettelkasten-zettel-directory
   (or filename (buffer-file-name))))

(add-hook 'after-save-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              (when (zettelkasten-zettel-p filename)
                (zettelkasten-db-update
                 filename
                 (secure-hash 'sha1 (current-buffer)))))))

(when (eq zettelkasten-db-update-method 'when-idle)
  (run-with-idle-timer
   zettelkasten-db-idle-seconds t #'zettelkasten-db--update-on-timer))

(defun zettelkasten-db-process ()
  (interactive)
  (let ((files (zettelkasten--get-all-files)))
    (setq zettelkasten-db-dirty (append zettelkasten-db-dirty files)))
  (unless (eq zettelkasten-db-update-method 'when-idle)
    (zettelkasten-db--update-on-timer)))

;;; Helper
(defun zettelkasten-db--title-filename (&optional filenames)
  "Returns list of lists: title, filename, zkid only for files."
  (if filenames
      (zettelkasten-db-query [:select [title filename zkid]
                              :from nodes
                              :where (in filename $v1)
                              :and (= type 'file)]
                             (vconcat filenames))
    (zettelkasten-db-query [:select [title filename zkid]
                            :from nodes
                            :where (= type 'file)])))

(defun zettelkasten-db-title-filename-nodes (&optional nodes)
  "Returns list of lists: title [file-name-base], filename, zkid for all nodes."
  (mapcar
   (lambda (node)
     (list
      (format "%s [%s]" (car node) (file-name-base (cadr node)))
      (cadr node)
      (caddr node)))
   (if nodes
       (zettelkasten-db-query
        [:select [title filename zkid]
         :from nodes
         :where (in zkid $v1)]
        (vconcat nodes))
     (zettelkasten-db-query
      [:select [title filename zkid]
       :from nodes]))))

(defun zettelkasten-db--values-descriptor (&optional nodes)
  "Returns list of disctinct values for subject."
  (if nodes
      (-flatten
       (zettelkasten-db-query [:select :distinct [object]
                               :from edges
                               :where (in subject $v1)
                               :and (in predicate $v2)]
                              (vconcat nodes)
                              (zettelkasten-predicate-hierachy
                               zettelkasten-subject-predicate)))
    (-flatten
     (zettelkasten-db-query [:select :distinct [object]
                             :from edges
                             :where (in predicate $v1)]
                            (zettelkasten-predicate-hierachy
                             zettelkasten-subject-predicate)))))

(defun zettelkasten-db--nodes-with-descriptor (&optional input-nodes)
  "Select descriptor and return matching zettel."
  (let* ((descriptor
          (completing-read
           (format "Descriptor [%s]: "
                   (when input-nodes (safe-length input-nodes)))
           (if input-nodes
               (append (zettelkasten-db--values-descriptor input-nodes)
                       '("#Break#"))
             (zettelkasten-db--values-descriptor))))
         (output-nodes
          (if input-nodes
              (-flatten (zettelkasten-db-query
                         [:select :distinct [subject]
                          :from edges
                          :where (in subject $v1)
                          :and (in predicate $v2)
                          :and (= object $s3)]
                         (vconcat input-nodes)
                         (zettelkasten-predicate-hierachy
                          zettelkasten-subject-predicate)
                         descriptor))
            (-flatten
             (zettelkasten-db-query [:select :distinct [subject]
                                     :from edges
                                     :where (= object $s1)
                                     :and (in predicate $v2)]
                                    descriptor
                                    (zettelkasten-predicate-hierachy
                                     zettelkasten-subject-predicate)
                                    )))))
    (if (string= descriptor "#Break#")
        input-nodes
      (if (<= (safe-length output-nodes) zettelkasten-descriptor-cycle-threshold)
          output-nodes
        (zettelkasten-db--nodes-with-descriptor output-nodes)))))

(provide 'zettelkasten-db)
;;; zettelkasten.el ends here
