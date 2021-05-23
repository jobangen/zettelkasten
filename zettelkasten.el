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
;; (require 'zettelkasten-cache)
(require 'zettelkasten-visualization)
(require 'zettelkasten-db)

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

* Meta

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

(defvar zettelkasten-capture-state nil)

;;;###autoload
(defun zettelkasten-set-context-filter ()
  (interactive)
  (let* ((completions zettelkasten-context-filter-list)
         (filter
          (assoc (completing-read "Filter: " completions) completions)))
    (setq zettelkasten-context-filter filter)))


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
     ("{" . "") ("}" . "")
     ("ß" . "ss") ("ä" . "ae")
     ("ü" . "ue") ("ö" . "oe")
     ("é" . "e") ("ó" . "o"))
   (s-downcase title)))

(defun zettelkasten--get-all-files ()
  (directory-files-recursively zettelkasten-zettel-directory "org$"))


(defun zettelkasten-elfeed-get-feed-title ()
  "Get feed title from elfeed buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward "Feed: ")
    (replace-regexp-in-string
     "\n\\'" ""
     (s-replace "Feed: " "" (thing-at-point 'line t)))))

(defun zettelkasten-elfeed-get-title ()
  "Get feed title from elfeed buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward "Title: ")
    (replace-regexp-in-string
     "\n\\'" ""
     (s-replace "Title: " "" (thing-at-point 'line t)))))

;; (defvar zettelkasten-capture-db
;;   (db-make
;;    `(db-hash
;;      :filename ,(format "/home/job/.emacs.d/var/zettelkasten/zettelkasten-capture-db"))))

(defun zettelkasten-elfeed-clean-feedtitle (feedtitle)
  (s-replace-all
   '((": Table of Contents" . "")
     ("tandf: " .  "")
     ("SAGE Publications: " . "")
     ("SAGE Publications Inc: " . "")
     ("SAGE Publications Ltd: " . "")
     ("SAGE Publications Ltd STM: " . "")
     ("SAGE Publications India: " . "")
     ("Wiley: " . ""))
   feedtitle))


;;;###autoload
(defun zettelkasten-elfeed-report ()
  (interactive)
  (let* ((feeds (-flatten
                 (zettelkasten-db-query [:select :distinct [feed] :from capture])))
         (data
          (mapcar
           (lambda (feed)
             `(,(caar (zettelkasten-db-query [:select (funcall count feed)
                                             :from capture :where (= feed $s1)]
                                             feed))
               ,(caar (zettelkasten-db-query [:select (funcall count feed)
                                              :from capture
                                              :where (= feed $s1)
                                              :and (= priority "A")]
                                             feed))
               ,(caar (zettelkasten-db-query [:select (funcall count feed)
                                              :from capture
                                              :where (= feed $s1)
                                              :and (= priority "B")]
                                             feed))
               ,(caar (zettelkasten-db-query [:select (funcall count feed)
                                              :from capture
                                              :where (= feed $s1)
                                              :and (= priority "C")]
                                             feed))
               ,(caar (zettelkasten-db-query [:select (funcall count feed)
                                              :from capture
                                              :where (= feed $s1)
                                              :and (= priority "D")]
                                             feed))
               ,(caar (zettelkasten-db-query [:select (funcall count feed)
                                              :from capture
                                              :where (= feed $s1)
                                              :and (= priority "E")]
                                             feed))
               ,(zettelkasten-elfeed-clean-feedtitle feed)))
           feeds))
         (sorted (--sort (cond ((not (= (nth 1 it) (nth 1 other)))
                                (> (nth 1 it) (nth 1 other)))
                               ((not (= (nth 2 it) (nth 2 other)))
                                (> (nth 2 it) (nth 2 other)))
                               ((not (= (nth 3 it) (nth 3 other)))
                                (> (nth 3 it) (nth 3 other)))
                               ((not (= (nth 4 it) (nth 4 other)))
                                (> (nth 4 it) (nth 4 other)))
                               ((not (= (nth 5 it) (nth 5 other)))
                                (> (nth 5 it) (nth 5 other)))
                               (t (< (nth 0 it) (nth 0 other))))
                         data)))
    (switch-to-buffer-other-window "*Info*")
    (erase-buffer)
    (insert "  # | A B C D E | Title\n")
    (insert "-------------------------------------------\n")
    (dolist (entry sorted)
      (insert (format "%s | %s %s %s %s %s | %s\n"
                      (s-pad-left 3 " " (number-to-string (car entry)))
                      (nth 1 entry)
                      (nth 2 entry)
                      (nth 3 entry)
                      (nth 4 entry)
                      (nth 5 entry)
                      (nth 6 entry) ;; title
                      )))
    (goto-char (point-min))))

;;;###autoload
(defun zettelkasten-capture-elfeed ()
  (interactive)
  (let ((title (zettelkasten-elfeed-get-title))
        (feed (zettelkasten-elfeed-get-feed-title))
        (priority (completing-read "Priority: " '("A" "B" "C" "D" "E"))))
    (zettelkasten-db-query [:insert :into capture
                                    :values ([nil $s1 $s2 $s3])]
                           feed (format-time-string "%Y-%m-%d") priority)
    (mark-whole-buffer)
    (org-capture nil "z")
    (org-edit-headline (s-truncate 51 title))
    (org-priority (cond ((equal priority "A")
                         ?A)
                        ((equal priority "B")
                         ?B)
                        ((equal priority "C")
                         ?C)
                        ((equal priority "D")
                         ?D)
                        ((equal priority "E")
                         ?E)))
    (org-set-property "RDF_TYPE" "bibo:Article")
    (org-capture-finalize)
    (elfeed-show-next)))

;;;###autoload
(defun zettelkasten-elfeed-skip ()
  (interactive)
  (let ((feed (zettelkasten-elfeed-get-feed-title)))
    (zettelkasten-db-query [:insert :into capture
                                        :values ([nil $s1 $s2 $s3])]
             feed (format-time-string "%Y-%m-%d") "Z"))
  (elfeed-show-next))


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
    (zettelkasten-set-type-and-label)
    (save-buffer)))


;;; Open from Zettel
(org-link-set-parameters "zk" :follow #'org-zettelkasten-open)

(defun org-zettelkasten-open (path)
  (let* ((path (nreverse (split-string path "::")))
         (target (car path)))
    (when zettelkasten-capture-state
      (kill-current-buffer))
    (find-file (caar (zettelkasten-db-query
                      [:select filename
                       :from nodes
                       :where (= zkid $s1)
                       :or (= label $s1)]
                      target)))
    (goto-char (point-min))
    (search-forward target nil t)
    (ignore-error (org-back-to-heading)))
  (when zettelkasten-capture-state
    (zettelkasten-capture-mode)))

;;; TODO: add support for exporting links to headlines
(defun zettelkasten-org-zk-export (path desc format)
  "Format zk links for export."
  (let* ((filename
          (caar (zettelkasten-db-query [:select filename :from id
                                                :where (= zkid $s1)]
                                       path)))
         (html-name
          (s-replace-all '(("/home/job/Dropbox/db/zk/zettel/" . "")
                           (".org" . ".html"))
                         (plist-get entry :file))))
    (when (eq format 'html)
      (format "<a href=\"./%s\">%s</a>"
              html-name
              desc))))

(org-link-set-parameters "zk" :export 'zettelkasten-org-zk-export)


;;; https://emacs.stackexchange.com/questions/21713/how-to-get-property-values-from-org-file-headers
;;; refactor!
(defun org-global-props (&optional property buffer)
  "Get the alists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

(defun zettelkasten-flat-predicates ()
    ())


(setq zettelkasten-predicates
      '(nil "rdf:type"
            "prov:generatedAtTime" ;; entity by activity at time
            "prov:used"
            "prov:wasInformedBy"     ;;activity by activity
            "prov:wasGeneratedBy"    ;;entity by activity
            "prov:wasAssociatedWith" ;; activity with agent
            "prov:specializationOf"  ;;entity -- entity
            "prov:hadPrimarySource"
            "prov:wasRevisionOf"
            "prov:atLocation" ;; ... at Location
            "time:intervalStartedBy" "time:intervalStarts"
            "time:intervalFinishedBy" "time:intervalFinishes"
            "time:after" "time:before"
            "time:intervalMetBy" "time:intervalMeets"
            "time:intervalContains" "time:intervalDuring"
            "time:hours"
            "skos:member" "skos:memberOf"
            "skos:subject" "skos:isSubjectOf"
            "skos:primarySubject" "skos:isPrimarySubjectOf"
            "skos:broaderTransitive" "skos:broader"
            "skos:narrowerTransitive" "skos:narrower" "skos:related"
            "skos:mappingRelation"
            "skos:closeMatch"
            "skos:exactMatch"
            "skos:broadMatch"
            "skos:narrowMatch"
            "skos:relatedMatch"
            "skos:inScheme"
            "skos:definition"
            "dct:issued"
            "dct:creator" "dct:date"
            "dct:hasPart" "dct:isPartOf"
            "dct:language"
            "dct:references" "dct:referencedBy"
            "prov:memberOf"
            "bibo:cites" "bibo:citedBy"
            "bibo:reviewOf"
            "foaf:member" "foaf:memberOf"
            "zktb:introductionBy"
            "zktb:wasAuthoredBy"
            "zkt:wasAnnotatedBy"
            "zktb:wasEditedBy"
            "zktb:wasDedicatedTo"
            "zkt:wasCoinedBy"
            "zkt:wasHeldBy"
            "zkt:wasOrganizedBy"
            "zktb:wasTranslatedBy"
            "zkt:hadParticipant"        ;activity -- person
            "zkt:wasLedBy"              ;activity by agent
            "zkt:hasType"
            "zkt:isTypeOf"
            "zkt:startedAtLocation"
            "zkt:endedAtLocation"
            "zktm:atBodilyLocation"
            "zktm:sensation"
            "zktm:sensationQualifiedBy"
            "zktm:dosage"
            "zktm:wasPerformedOn"
            "zkt:applicant"
            "zkt:employer"
            "zkt:sentBy"
            "zkt:sentTo"))

(setq zettelkasten-classes
      '("owl:Class"
        ;;
        ("prov:Activity"
         ("zkt:Project"
          "zkt:Seminar" "zkt:Sitzung"
          "zkt:MusicEvent"
          "zkt:Appointment"
          "zkt:Employment"
          "zkt:ApplicationProcedure"
          "zkt:Übungen"
          "zkt:Vortrag"
          ("zktm:MedicalActivity"
           ("zktm:Vaccination"
            "zktm:Examination"
            "zktm:Treatment"
            "zktm:SelfPerception"))
          ("zkt:SpatialMovement"
           ("zkt:Wandern"
            "zkt:Laufen"))))
        ;;
        ("prov:Entity"
         ("skos:Concept prov:Entity")
         "prov:Collection"
         ("prov:Plan"
          ("zkt:Rezept"
           ("zktm:Vaccine"))
          ("dct:BibliographicResource"
           ("zktb:Article"
            "zktb:Book" "zktb:InBook"
            "zktb:Collection" "zktb:Lexikon"
            "zktb:InCollection"
            "zktb:Journal"
            "zktb:ClassicalText"))
          "zkt:DocumentPart"
          "zkt:Draft"
          "zkt:SlideShow"
          "zkt:Excerpt"
          "zkt:Quote"
          "zkt:Note"))
        ;;
        ("prov:Agent"
         ("prov:Person foaf:Person"
          "prov:Organization foaf:Organization"))
        ;;
        ("prov:Location"
         ("prov:Location geo:Point"
          ("zkt:TrainStation")))
        "time:ProperInterval"
        "dct:Collection"
        "dct:LinguisticSystem" "dct:Software" "dct:PhysicalResource" "dct:Location"
        "skos:ConceptScheme"
        "skos:Concept"
        "skos:Collection"
        "foaf:Group"))


;;;###autoload
(defun zettelkasten-set-type (&optional type)
  (interactive)
  (let ((type-sel (or type (completing-read "Type: "
                                            (-flatten zettelkasten-classes)))))
    (zettelkasten-zettel-ensure-keyword "RDF_TYPE")
    (delete-region (point) (line-end-position))
    (insert (format " %s" type-sel))))

;;;###autoload
(defun zettelkasten-set-type-headline (&optional type)
  (interactive)
  (let ((type-sel (or type (completing-read "Type: "
                                            (-flatten zettelkasten-classes)))))
    (org-set-property "RDF_TYPE" type-sel)))


;;;###autoload
(defun zettelkasten-set-label (&optional label)
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
      (zettelkasten-zettel-ensure-keyword "ZK_LABEL")
      (delete-region (point) (line-end-position))
      (insert (format " %s" label-sel)))))

;;;###autoload
(defun zettelkasten-set-type-and-label ()
  (interactive)
  (zettelkasten-set-type)
  (unless (or (s-contains? "/txt/" (buffer-file-name))
              (s-contains? "/jr/" (buffer-file-name)))
    (zettelkasten-set-label)))


;;;###autoload
(defun zettelkasten-insert-link-at-point (&optional link-target)
  (interactive)
  (let* ((zk-id (org-entry-get nil "CUSTOM_ID"))
         (zettel-target link-target)
         (predicate (completing-read "Predicate: " zettelkasten-predicates))
         (turtle (s-starts-with? ":TURTLE" (thing-at-point 'line t)))
         (zettel
          (save-excursion
            (unless link-target
              (ivy-read "Link Zettel: "
                        (zettelkasten-db-query [:select [title zkid]
                                                :from nodes])
                        :action
                        (lambda (selection)
                          (if (listp selection)
                              (setq zettel-target (cadr selection))
                            (if (s-starts-with? "val:" selection)
                                (setq zettel-target selection)
                              (other-window 1)
                              (zettelkasten-new-zettel selection)
                              (setq zettel-target (buffer-file-name)))))))))
         (zettel-id (if (s-starts-with? "val:" zettel-target)
                        (s-chop-prefix "val:" zettel-target)
                      (car
                       (remove nil
                               (car (zettelkasten-db-query [:select [label zkid]
                                                            :from nodes
                                                            :where (= zkid $s1)]
                                                           zettel-target))))))
         (zettel-title
          (if (s-starts-with? "val:" zettel-target)
              (s-chop-prefix "val:" zettel-target)
            (read-string "Title: "
                         (car (zettelkasten-db-query [:select title
                                                      :from nodes
                                                      :where (= zkid $s1)]
                                                     zettel-target))))))
    (if predicate
        (if (and zk-id (stringp zk-id) (string-match "\\S-" zk-id))
            (if (equal turtle t)
                (insert (format "%s::%s" predicate zettel-id))
              (insert (format "[[zk:%s::%s::%s][%s]]"
                              zk-id predicate zettel-id zettel-title)))
          (insert (format "[[zk:%s::%s][%s]]"
                          predicate zettel-id zettel-title)))
      (insert (format "[[zk:%s][%s]]" zettel-id zettel-title)))))


(defun zettelkasten-heading-set-relation-to-context ()
  (let* ((predicate (completing-read "Predicate: " zettelkasten-predicates
                                     nil nil "dct:isPartOf"))
         (target (completing-read "Target:"
                                  (zettelkasten-db-query [:select zkid
                                                          :from nodes
                                                          :where (= filename $s1)]
                                                         (buffer-file-name))))
         (turtle (format "%s::%s" predicate target)))
    (org-set-property "TURTLE" turtle)))

;;;###autoload
(defun zettelkasten-heading-to-node ()
  (interactive)
  (zettelkasten-set-type-headline)
  (zettelkasten-id-get-create)
  (zettelkasten-heading-set-relation-to-context))

(defun zettelkasten-id-get-create ()
  (let ((zk-id (org-entry-get nil "CUSTOM_ID")))
    (unless (and zk-id (stringp zk-id) (string-match "\\S-" zk-id))
      (let* ((myid
              (completing-read
               "ID: " `(,(zettelkasten-extract-value "ZK_LABEL")
                        ,(concat "zk-" (s-chop-suffix ".org" (buffer-name)))
                        ,(s-chop-suffix ".org" (buffer-name))
                        ,(concat "zk-" (s-left 15 (s-replace "T" "-" (org-id-new)))))))
             (editid (read-string "Edit ID: " myid)))
        (setq zk-id editid))
      (org-set-property "CUSTOM_ID" zk-id))
    zk-id))

;;;###autoload
(defun zettelkasten-insert-link-heading-at-point (&optional custom-id)
  (interactive)
  (if custom-id
      (insert (format "[[zk:%s][%s]]"
                      custom-id
                      (completing-read "Description: ")))
    (setq zettelkasten-capture-state 'link-heading)
    (zettelkasten-capture)))

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

(defun zettelkasten-zettel-ensure-keyword (keyword)
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
  (interactive "*")
  (text-mode)
  (zettelkasten-zettel-ensure-keyword "DESCRIPTOR")
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
    (zettelkasten-zettel-ensure-keyword "COLLECTION")
    (insert (concat " "
                    (or collection
                        (completing-read
                         "Collection: "
                         (-flatten (zettelkasten-db-query
                                    [:select :distinct [collection]
                                             :from collection ])))))))
  (unless collection
    (zettelkasten-zettel-add-collection)))

;;;###autoload
(defun zettelkasten-zettel-add-descriptor (&optional descriptor)
  (interactive)
  (let ((desc
         (or descriptor
             (completing-read
              "Descriptor: "
              (-flatten
               (zettelkasten-db-query [:select :distinct [object]
                                       :from edges
                                       :where (= predicate "skos:subject")]))))))
    (save-excursion
      (zettelkasten-zettel-ensure-keyword "DESCRIPTOR")
      (insert (concat " " desc))
      (zettelkasten-sort-tags)))
  (unless descriptor
    (zettelkasten-zettel-add-descriptor)))

;;;###autoload
(defun zettelkasten-zettel-add-index (&optional index)
  (interactive)
  (let* ((idx (or index
                  (completing-read
                   "Zettel: "
                   (zettelkasten-db-query [:select :distinct title :from index :where (= entry "t")]))))
         (zet-title (zettelkasten--extract-title))
         (new-title (read-string "Title: " zet-title))
         (ins-title (if (equal zet-title new-title)
                        t
                      new-title)))
    (zettelkasten-zettel-ensure-keyword "INDEX")
    (end-of-line)
    (insert (format " \"%s::%s\"" idx ins-title))))


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
(defun zettelkasten-headline-add-collection ()
  "Add descriptor to current headline."
  (interactive)
  (let* ((current
          (split-string
           (or (org-entry-get nil "COLLECTION") "")))
         (add
          (ivy-read "Collection [Headline]: "
                    (-flatten (zettelkasten-db-query
                               [:select :distinct [collection]
                                        :from collection]))))
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

(defun zettelkasten--get-collection-zettel ()
  (let ((collection
         (completing-read
          "Collection: " (-flatten
                          (zettelkasten-db-query
                           [:select :distinct [collection]
                                    :from collection])))))
    (zettelkasten-db-query
     [:select [title files:filename collection]
              :from collection
              :left-outer-join files
              :on (= collection:filename files:filename)
              :where (= collection $s1)]
     collection)))



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
(defun zettelkasten-open-zettel (&optional nodes)
  ;; TODO: filter
  (interactive)
  (ivy-read
   (format "Zettel [%s]: " (car zettelkasten-context-filter))
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
(defun zettelkasten-open-zettel-random ()
  (interactive)
  (let* ((zettel
          (zettelkasten-db--title-filename))
         (rand-element
          (random (safe-length zettel))))
    (find-file (cadr (nth rand-element zettel)))))

;;;###autoload
(defun zettelkasten-open-zettel-collection ()
  (interactive)
  ;; (let ((collection
  ;;        (completing-read
  ;;         "Collection: " (-flatten
  ;;                         (zettelkasten-db-query
  ;;                          [:select :distinct [collection]
  ;;                                   :from collection]))))))

  (ivy-read
   (format "Zettel [%s]: " (car zettelkasten-context-filter))
   (zettelkasten--get-collection-zettel)
   :preselect "Inbox"
   :action
   (lambda (selection)
     (find-file
      (cadr selection)))))


(defun zettelkasten--get-cons-title-fname (plist)
  (mapcar (lambda (arg)
            (cons
             (plist-get arg :title)
             (plist-get arg :file)))
          plist))

(defun zettelkasten--filename-to-id (filename)
  "Extract id from FILENAME. Return string."
  (let* ((fname-chop
          (s-chop-prefix
           zettelkasten-zettel-directory
           (s-chop-suffix ".org" filename))))
    (cond ((s-prefix? "txt" fname-chop)
           (s-chop-prefix "txt/" fname-chop))
          ((s-prefix? "jr" fname-chop)
           (s-chop-prefix "jr/" fname-chop))
          ((s-prefix? "rdf" fname-chop)
           (s-chop-prefix "rdf/" fname-chop))
          ((s-prefix? "eph" fname-chop)
           (s-left 15 (s-chop-prefix "eph/" fname-chop)))
          (t (s-left 15 fname-chop)))))

;;;###autoload
(defun zettelkasten-open-zettel-descriptor ()
  (interactive)
  (ivy-read
   "Zettel: " (zettelkasten-db--title-filename
               (zettelkasten-db--files-matching-descriptor))
   :action
   (lambda (selection)
     (find-file
      (cadr selection)))))

(defun zettelkasten-db--values-predicate ()
  (-flatten
   (zettelkasten-db-query [:select :distinct [predicate]
                           :from edges])))


(defun zettelkasten-db--nodes-semantic (&optional input-nodes)
  (let* ((predicate (completing-read "Predicate: "
                                     (zettelkasten-db--values-predicate)))
         (objects-edges (vconcat
                         (-flatten
                          (zettelkasten-db-query [:select :distinct [object]
                                                  :from edges
                                                  :where (= predicate $s1)]
                                                 predicate))))
         (objects
          (zettelkasten-db-query [:select :distinct [title zkid label]
                                  :from nodes
                                  :where (in zkid $v1)
                                  :or (in label $v2)]
                                 objects-edges objects-edges))
         (object (cdr (assoc (completing-read "Object: " objects) objects))))
    (-flatten (zettelkasten-db-query [:select :distinct [subject]
                                      :from edges
                                      :where (= predicate $s1)
                                      :and (in object $v2)]
                                     predicate (vconcat object)))))


;;;###autoload
(defun zettelkasten-open-semantic ()
  (interactive)
  (let* ((completions (mapcar
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
         (selection (assoc (completing-read "Node: " completions) completions)))
    (find-file (cadr selection))
    (search-forward (caddr selection) nil t)))



(defun zettelkasten-open-zettel-todo ()
  (interactive)
  (let* ((completions (zettelkasten-db-query
                       [:select [title filename]
                                :from files
                                :where (= todo 't)]))
         (selection (assoc (completing-read "Zettel: " completions) completions)))
    (find-file (cadr selection))))


(defun zettelkasten--get-backlinks (filename)
  "Files linking to nodes of FILENAME. Return list links."
  (let* ((current-ids ;; all ids of this file
          (-flatten (zettelkasten-db-query [:select [zkid label]
                                                    :from nodes
                                                    :where (= filename $s1)]
                                           filename)))
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
              (list (concat "<- "
                            (s-pad-right (+ 2 len) " " (car link))
                            (cadr link))
                    (caddr link)))

            backlinks)))

  
;;;###autoload
  (defun zettelkasten-open-backlink ()
    (interactive)
    (let* ((zettel (zettelkasten--get-backlinks (buffer-file-name)))
           (selected (assoc (completing-read "Zettel: " zettel) zettel)))
      (find-file (cadr selected))))

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
  (save-excursion
    (org-back-to-heading)
    (org-todo "TODO")
    (org-set-property "CATEGORY" "zkt")
    (org-set-tags '("zkt" "followup"))))

;;;###autoload
(defun zettelkasten-headline-reset ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-set-tags '())
    (org-todo "")
    (ignore-errors
      (org-priority ?\ ))))

;;;###autoload
(defun zettelkasten-zettel-open-external ()
  (interactive)
  (let ((element (org-element-parse-buffer))
        (zkid (zettelkasten--filename-to-id (buffer-file-name))))
    (cond ((s-starts-with? "zktb:" (zettelkasten-extract-value "RDF_TYPE" element))
           (org-link-open-from-string
            (concat
             "file:"
             zettelkasten-bibliography-file
             "::"
             (file-name-base (buffer-file-name)))))
          ((member
            "prov:Location"
            (split-string (zettelkasten-extract-value "RDF_TYPE" element)))
           (let ((latitude
                  (caar (zettelkasten-db-query [:select object
                                           :from edges
                                           :where (= subject $s1)
                                           :and (= predicate "geo:lat")]
                                          zkid)))
                 (longitude
                  (caar (zettelkasten-db-query [:select object
                                           :from edges
                                           :where (= subject $s1)
                                           :and (= predicate "geo:long")]
                                          zkid))))
             (browse-url (format
                          "https://www.google.com/maps/search/?api=1&query=%s,%s"
                          latitude longitude)))))))


(defhydra hydra-zettelkasten (:color blue)
  "Zettelkasten"
  ("C-ä" zettelkasten-open-zettel "Open Zettel" :column "Open")
  ("Ä" (zettelkasten-open-zettel t) "Open Nodes")
  ("R" zettelkasten-open-zettel-random "Open random")
  ("ä" zettelkasten-open-zettel-collection "Open collection")
  ("d" zettelkasten-open-zettel-descriptor "Open descriptor")
  ("jw" zettelkasten-journal-weekly-file "Weekly file")

  ("C-r" zettelkasten-inbox-process (format "Process inbox [%s]" 5) :color red :column "Inbox")
  ("b" zettelkasten-inbox-bury "Bury" :color red)
  ("t" zettelkasten-inbox-trash "Trash" :color red)

  ("p" zettelkasten-capture-push "Push Link" :column "Zettelkasten")
  ("P" (zettelkasten-capture-push t) "Push Heading")
  ("f" zettelkasten-set-context-filter "Set filter")
  ("D" zettelkasten-replace-descriptor "Replace Desc.")
  ("I" zettelkasten-info "Info")

  ("c" zettelkasten-zettel-add-collection "Add collection" :column "Zettel")
  ("#" zettelkasten-zettel-add-descriptor "Add descriptor")
  ("x" zettelkasten-zettel-add-index "Add Index")
  ("l" zettelkasten-insert-link-at-point "Link")
  ("L" zettelkasten-insert-link-heading-at-point "Link heading")
  ("i" zettelkasten-zettel-info "Info")
  ("v" zettelkasten-vis-buffer "Visualize")
  ("ü" zettelkasten-set-type-and-label "Set label and type")
  ("o" zettelkasten-zettel-open-external "Open external")

  ("hc" zettelkasten-headline-add-collection "Add collection" :column "Heading")
  ("C-#" zettelkasten-headline-add-descriptor "Add descriptor")
  ("r" zettelkasten-capture-refile "Refile")
  ("+" zettelkasten-heading-to-node "Node")
  ("hf" zettelkasten-headline-set-followup "Set followup")
  ("hr" zettelkasten-headline-reset "Reset")

  ("sl" zetteldeft-avy-link-search "search link" :column "Search")
  ("sf" zetteldeft-search-current-id "seach current file")
  ("sd" zetteldeft-deft-new-search "search deft ")
  ("st" zetteldeft-avy-tag-search "search tag")
  ("n" org-noter "noter" :column "Other")
  ("u" zettelkasten-update-org-agenda-files "Update agenda")
  ("q" nil "Quit"))


;; (defun zettelkasten-rfloc (file headline)
;;   (let ((pos
;;          (save-excursion
;;            (save-selected-window
;;              (if (string= file (buffer-file-name))
;;                  (find-file file)
;;                (find-file-other-window file))
;;              (org-find-exact-headline-in-buffer headline)))))
;;     (org-refile nil nil (list headline file nil pos))))

;; (defun zettelkasten-refile-base (&optional arg)
;;   (let* ((linked
;;           (zettelkasten-cache-entry-ids
;;            (plist-get (zettelkasten-cache-entry-filename) :links)))
;;          (target nil) ;; Better solution?
;;          (zettel-other-buffer
;;           (save-selected-window
;;             (zettelkasten-db-query
;;              [:select [title]
;;               :from files
;;               :where (= filename $s1)]
;;              (buffer-file-name))))
;;          (target-set
;;           (if (or (equal arg '(4))
;;                   (and (not linked)
;;                        (s-starts-with?
;;                         zettelkasten-zettel-directory
;;                         (buffer-file-name)) ;; del if wrapper
;;                        (not (equal (buffer-file-name) zettelkasten-inbox-file))))
;;               (setq target (buffer-file-name))
;;             (ivy-read "Refile to: "
;;                       (zettelkasten--get-cons-title-fname
;;                        (if (or (not (s-starts-with?
;;                                      zettelkasten-zettel-directory
;;                                      (buffer-file-name)))
;;                                (equal (buffer-name)
;;                                       "zettelkasten-inbox.org"))
;;                            (zettelkasten-db--title-filename)
;;                            linked))
;;                       :preselect zettel-other-buffer
;;                       :action
;;                       (lambda (selection)
;;                         (setq target (cdr selection))))))
;;          (headline
;;           (save-excursion
;;             (save-selected-window
;;               (if (string= target (buffer-file-name))
;;                   (find-file target)
;;                 (find-file-other-window target))
;;               (ivy-read "Headline: "
;;                         (counsel-outline-candidates))))))
;;     (zettelkasten-rfloc target headline)
;;     (cons target headline)))

;;;###autoload
;; (defun zettelkasten-refile (&optional arg)
;;   (interactive)
;;   (let ((initial-zettel (buffer-name))
;;         (refile-data
;;          (zettelkasten-refile-base arg)))
;;     (unless (string= (car refile-data) (buffer-file-name))
;;       (other-window 1))
;;     (org-refile '(16))
;;     (if (string= (cdr refile-data) "Refile")
;;         (zettelkasten-refile)
;;       (zettelkasten-headline-reset))
;;     (when (string= initial-zettel "zettelkasten-inbox.org")
;;       (zettelkasten-inbox-process))))

;;;###autoload
(defun zettelkasten-inbox-process ()
  (interactive)
  (ignore-errors
    (windmove-left))
  (find-file zettelkasten-inbox-file)
  (widen)
  (goto-char (point-min))
  (search-forward "* Refile")
  (org-sort-entries nil ?p)
  (org-next-visible-heading 1)
  (org-narrow-to-subtree)
  (org-show-all))

;;;###autoload
(defun zettelkasten-inbox-bury ()
  (interactive)
  (widen)
  (zettelkasten-rfloc zettelkasten-inbox-file "Refile")
  (zettelkasten-inbox-process))

;;;###autoload
(defun zettelkasten-inbox-trash ()
  (interactive)
  (widen)
  (org-todo "CANCELLED")
  (zettelkasten-rfloc zettelkasten-inbox-file "Trash")
  (zettelkasten-inbox-process))

;; TODO: subitems, 
;;;###autoload
(defun zettelkasten-create-index-topic ()
  (interactive)
  (let* ((zettel-1
          (zettelkasten-db-query [:select [filename title] :from index
                                          :where (= entry "t")]))
         (zettel-2
          (zettelkasten-db-query [:select [filename entry title] :from index
                                          :where (not (= entry "t"))])))
    (switch-to-buffer-other-window "*zettelkasten-index*")
    (erase-buffer)
    (insert (format "#+TITLE: Zettelkasten index\n\n"))
    (dolist (row zettel-1)
      (let ((fname (car row))
            (title (cadr row)))
        (insert (format "- [[file:%s][%s]]\n" fname title))))
    (dolist (row zettel-2)
      (let ((fname (car row))
            (index (cadr row))
            (title (caddr row)))
        (goto-char (point-min))
        (while (search-forward (format "[%s]]" index) nil t)
          (end-of-line)
          (newline)
          (insert (format "  - [[file:%s][%s]]" fname title)))))
    (org-mode)
    (previous-line)
    (ignore-errors
      (org-sort-list t ?a))))


;;;###autoload
(defun zettelkasten-capture-push (&optional heading)
  (interactive)
  (if (not heading)
      (setq zettelkasten-capture-state 'push)
    (setq zettelkasten-capture-state 'push-heading))
  (zettelkasten-capture))

(defun zettelkasten-link-zettel-other-window (&optional heading)
  (let* ((filename-ow
          (save-excursion
            (save-selected-window
              (other-window 1)
              (buffer-file-name))))
         (id (if (not heading)
                 (zettelkasten--filename-to-id filename-ow)
               (save-excursion
                 (save-selected-window
                   (other-window 1)
                   (zettelkasten-id-get-create)))))
         (title (if (not heading)
                    (zettelkasten-db-query
                     [:select [title]
                      :from files
                      :where (= filename $s1)]
                     other)
                  (save-excursion
                    (save-selected-window
                      (other-window 1)
                      (org-back-to-heading t)
                      (when (looking-at org-complex-heading-regexp)
                        (match-string-no-properties 4)))))))
    (insert (format "** [[zk:%s][%s]]" id title))))


;;;###autoload
(defun zettelkasten-capture-refile ()
  (interactive)
  (setq zettelkasten-capture-state 'refile)
  (zettelkasten-capture))

(defun zettelkasten-refile-subtree-other-window ()
  (let ((current-rfloc
         (list
          (org-display-outline-path t t nil t)
          (buffer-file-name (buffer-base-buffer))
          nil
          (org-with-wide-buffer
           (org-back-to-heading t)
           (point-marker)))))
    (other-window 1)
    (zettelkasten-headline-reset)
    (org-refile nil nil current-rfloc)
    (other-window 1)))

;;;###autoload
(defun zettelkasten-capture-finalize ()
  (interactive)
  (cond ((equal zettelkasten-capture-state 'push)
         (progn
           (setq zettelkasten-capture-state t)
           (zettelkasten-link-zettel-other-window)))
        ((equal zettelkasten-capture-state 'push-heading)
         (progn
           (setq zettelkasten-capture-state t)
           (zettelkasten-link-zettel-other-window t)))
        ((equal zettelkasten-capture-state 'refile)
         (progn
           (zettelkasten-refile-subtree-other-window)
           (setq zettelkasten-capture-state t)))
        ((equal zettelkasten-capture-state 'link-heading)
         (org-with-wide-buffer
          (org-back-to-heading t)
          (when (looking-at org-complex-heading-regexp)
            (let* ((predicate
                     (completing-read "Predicate: " zettelkasten-predicates))
                   (turtle
                    (save-selected-window
                      (other-window 1)
                      (s-starts-with? ":TURTLE" (thing-at-point 'line t))))
                   (heading (match-string-no-properties 4))
                   (heading-edit (read-string "Heading: " heading))
                   (custom-id (zettelkasten-id-get-create))
                   )
              (save-buffer)
              (zettelkasten-capture-kill)
              (other-window 1)
              (let ((zk-id (org-entry-get nil "CUSTOM_ID")))
                (if turtle
                    (insert (format "%s::%s"
                                    predicate
                                    custom-id))
                  (insert (format "[[zk:%s::%s::%s][%s]]"
                                  zk-id
                                  predicate
                                  custom-id
                                  heading-edit))))))))
        (zettelkasten-capture-state
         (progn
           (save-buffer)
           (zettelkasten-capture-kill)
           (other-window 1)
           (when (equal (buffer-file-name) zettelkasten-inbox-file)
             (zettelkasten-inbox-process)
             (zettelkasten-zettel-info))))
        (t (zettelkasten-capture-kill))))

;;;###autoload
(defun zettelkasten-capture-kill ()
  (interactive)
  (setq zettelkasten-capture-state nil)
  (kill-current-buffer))



(defvar zettelkasten-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'zettelkasten-capture-finalize)
    (define-key map "\C-c\C-k" #'zettelkasten-capture-kill)
    map)
  "Keymap for `zettelkasten-capture-mode', a minor mode.")

(defvar zettelkasten-capture-mode-hook nil
  "Hook for the `zettelkasten-capture-mode' minor mode.")

(define-minor-mode zettelkasten-capture-mode
  "Minor mode for special key bindings in a zettelkasten capture buffer.
Turning on this mode runs the normal hook `zettelkasten-capture-mode-hook'."
  nil " capture" zettelkasten-capture-mode-map
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<zettelkasten-capture-mode-map>Capture buffer, finish \
`\\[zettelkasten-capture-finalize]', abort `\\[zettelkasten-capture-kill]'.")))

;;;###autoload
(defun zettelkasten-capture ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  (zettelkasten-open-zettel)
  (goto-char (point-min))
  (end-of-line)
  (search-forward "*")
  (zettelkasten-capture-mode))



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

(defun zettelkasten-each-file ()
  (interactive)
  (let* ((files (-flatten
                 (zettelkasten-db-query
                  [:select filename
                   :from nodes
                   :where (= rdftype "time:DateTimeDescription")]))))
    (dolist (zettel files)
      (find-file zettel)
      (goto-char (point-min))
      (while (search-forward "time:DateTimeDescription" nil t)
        (replace-match "time:DateTimeInterval"))
      (save-buffer)
      (kill-current-buffer))))



(defun zettelkasten-each-file-2 ()
  (interactive)
  (let* ((path "/home/job/Dropbox/db/zk/zettel/jr/")
         (files
          (directory-files path nil ".org$")))
    (dolist (zettel files)
      (let ((fullfname
             (concat path "/" zettel)))
        (find-file fullfname)
        (if (not (search-forward "RDF_TYPE:" nil t))
            (progn (zettelkasten-zettel-ensure-keyword "RDF_TYPE")
                   (goto-char (point-min))
                   (search-forward "RDF_TYPE:" nil t)
                   (end-of-line)
                   (insert " time:ProperInterval")
                   (save-buffer)))
        (kill-current-buffer)))))

(defun zettelkasten-each-file-3 ()
  (interactive)
  (let ((files (zettelkasten--get-all-files)))
    (dolist (zettel files)
      (goto-char (point-min))
      (ignore-errors
        (find-file zettel)
        (search-forward "DESCRIPTOR:")
        (narrow-to-region (point) (line-end-position))
        (while (search-forward " #" nil t)
          (replace-match " "))
        (widen)
        (save-buffer)
        (kill-buffer)
        (message zettel)))))





;;;###autoload
(defun zettelkasten-link-archive ()
  (interactive)
  (let* ((arch-path "/home/job/archive/date-description/")
         (file (read-file-name
                "File: " arch-path))
         (file-proc (car (split-string (s-replace arch-path "" file) " -- ")))
         (description (read-string "Description: "
                                   (last (split-string file-proc "/")))))
    (insert (format "[[arch:%s][%s]]" file-proc description))))

;;;###autoload
(defun zettelkasten-zettel-rename ()
  (interactive)
  (let* ((old (buffer-file-name))
         (new (read-string "New name: " old)))
    (zettelkasten-db-query [:delete-from files
                            :where (= filename $s1)]
                           old)
    (zettelkasten-db-query [:delete-from nodes
                            :where (= filename $s1)]
                           old)
    (zettelkasten-db-query [:delete-from edges
                            :where (= filename $s1)]
                           old)
    (zettelkasten-db-query [:delete-from id
                            :where (= filename $s1)]
                           old)
    (zettelkasten-db-query [:delete-from link
                            :where (= filename $s1)]
                           old)
    (zettelkasten-db-query [:delete-from index
                            :where (= filename $s1)]
                           old)
    (zettelkasten-db-query [:delete-from descriptor
                            :where (= filename $s1)]
                           old)
    (zettelkasten-db-query [:delete-from collection
                            :where (= filename $s1)]
                           old)
    (rename-file old new)
    (kill-buffer (current-buffer))
    (find-file new)))



(defun zettelkasten-extract-value (keyword &optional element)
  (org-element-map
      (or element (org-element-parse-buffer 'greater-element))
      'keyword
    (lambda (kw)
      (if (string= (org-element-property :key kw) keyword)
          (org-element-property :value kw)))
    :first-match t))

;;;###autoload
(defun zettelkasten-journal-weekly-file ()
  (interactive)
  (let* ((filename (format-time-string "%Y-w%W.org"))
         (filepath (concat org-journal-dir filename)))
    (if (file-exists-p filepath)
        (find-file filepath)
      (switch-to-buffer-other-window filename)
      (insert
       (format-time-string "#+TITLE: jr: %Y-W%W:\n#+RDF_TYPE: time:ProperInterval\n#+COLLECTION: journal\n#+DESCRIPTOR: #%Y #%A #W%W @journal\n\n* %Y-W%W"))
      (write-file filepath))))

;;;###autoload
(defun zettelkasten-yearly-file (&optional year)
  (interactive)
  (let* ((yearp (or (when year (number-to-string year)) (read-string "Year: ")))
         (filepath (concat org-journal-dir yearp ".org")))
    (zettelkasten-db-query [:delete-from nodes
                            :where (= filename $s1)]
                           filepath)
    (zettelkasten-db-query [:delete-from edges
                            :where (= filename $s1)]
                           filepath)
    (when (file-exists-p filepath)
      (delete-file filepath))
    (switch-to-buffer filepath)
    (insert (format "#+TITLE: %s
#+RDF_TYPE: time:DateTimeInterval

* [[zk:%s::time:hasDateTimeDescription::dtd-%s][DTD]]
* DateTimeDescription
:PROPERTIES:
:RDF_TYPE: time:DateTimeDescription
:CUSTOM_ID: dtd-%s
:END:
- [[zk:dtd-%s::time:year::%s][%s]]
"
                    yearp yearp yearp yearp yearp yearp yearp))
    (write-file filepath)
    (kill-current-buffer)))


;;;###autoload
(defun zettelkasten-create-year-files ()
  (interactive)
  (let ((years (number-sequence 1900 2021 1)))
    (dolist (year years)
      (zettelkasten-yearly-file year))))

;;;###autoload
(defun zettelkasten-move-to-todays-tasks ()
  (interactive)
  ;; old file
  (goto-char (point-min))
  (search-forward "Today's Tasks" nil t)
  ;; today's file
  (org-journal-new-entry '4)
  (unless (search-forward "Today's Tasks" nil t)
    (progn
      (goto-char (point-max))
      (insert "** Today's Tasks\n")
      (insert (format-time-string "<%Y-%m-%d>\n"))))
  (job/org-add-tags-today)
  (org-todo "TODO")
  (org-narrow-to-subtree)
  (goto-char (point-max))
  ;; old file
  (other-window 1)
  (org-narrow-to-subtree)
  (goto-char (point-min))
  (while (search-forward "- [ ] " nil t)
    (let ((substring (thing-at-point 'line)))
      (other-window 1)
      (insert substring)
      (other-window 1)
      (backward-char 4)
      (insert ">")))
  (widen)
  (other-window 1)
  (widen))




;; (zettelkasten-db-query
;;            [:select :distinct [collection]
;;                     :from collection
;;                     :where (in collection ["index" "content"])])

;; (zettelkasten-db-query
;;  [:select [title files:filename collection]
;;           :from collection
;;           :left-outer-join files
;;           :on (= collection:filename files:filename)
;;           :where (= collection $s1)
;;           ] "content")


;; (zettelkasten-db-query [:select [e1:object e2:object e3:object]
;;                         :from edges e
;;                         :left-join edges e1 :on (= e:subject e1:subject)
;;                         :left-join edges e2 :on (= e:subject e2:subject)
;;                         :left-join edges e3 :on (= e:subject e3:subject)
;;                         :where (= e:predicate "rdf:type")
;;                         :and (= e:object "zkt:Wandern")
;;                         :and (= e1:predicate "time:intervalDuring")
;;                         :and (= e2:predicate "time:hours")
;;                         :and (= e3:predicate "zkt:distanceKM")
;;                         :order-by e1:object
;;                         ])


(provide 'zettelkasten)
;;; zettelkasten.el ends here
