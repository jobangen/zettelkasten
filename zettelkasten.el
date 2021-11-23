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
  "Threshold for remaining Zettel to break the selection cycle"
  :group 'zettelkasten
  :type 'integer)

(defcustom zettelkasten-org-agenda-integration nil
  "If non-nil, add zettel with todos to `org-agenda-files'"
  :type 'boolean)

(defvar zettelkasten-capture-state nil)

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
                      (time-string (format-time-string "%Y-%m-%d-%H%M")))
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
  (directory-files-recursively zettelkasten-zettel-directory "org$"))

;;;###autoload
(defun zettelkasten-new-zettel (&optional title type)
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
    (if type
        (zettelkasten-set-type type)
      (zettelkasten-set-type))
    (save-buffer)))

;;; Open from Zettel
(org-link-set-parameters "zk" :follow #'org-zettelkasten-open)

(defun org-zettelkasten-open (path)
  (let* ((path (nreverse (split-string path "::")))
         (target (car path))
         (target-label (concat "%" target " %")))
    (when zettelkasten-capture-state
      (kill-current-buffer))
    (find-file (caar (zettelkasten-db-query
                      [:select filename
                       :from nodes
                       :where (= zkid $s1)
                       :or (= label $s2)
                       :or (like label $s3)] ;; multiple labels?
                      target target target-label)))
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



(defun zettelkasten-flat-predicates ()
  (delete-dups (-flatten zettelkasten-predicates)))


(setq zettelkasten-predicates
      '(nil ("rdf:type")
            (("prov:wasInfluencedBy"
              ("zkt:symbolizes")        ;word-concept
              ("zkt:wasSymbolizedBy")
              ("zkt:refersTo")          ;concept-thing
              ("zkt:wasReferedToBy")    ;concept-thing
              ("zkt:standsFor")         ;word-thing
              ("prov:wasAttributedTo"   ;; entity to agent
               ("zktb:wasAuthoredBy")
               ("zktb:wasEditedBy")
               ("zktb:introductionBy")
               ("zktb:wasTranslatedBy")
               ("zkt:wasAnnotatedBy")
               ("zkt:wasCoinedBy"))

              ("prov:wasAssociatedWith" ;; activity with agent
               ("zkt:hadParticipant"    ;; event -- agent
                ("zkt:wasPerformedBy"   ;; part of, active
                 ("zkt:wasLedBy"))
                ("zkt:wasPerformedWith" ;; part of, passive?
                 ("zktm:wasPerformedOn")))
               ("zkt:hadNonParticipant"
                ("zkt:wasOrganizedBy")
                ("zkt:wasDirectedAt"))
               ("zkt:hadActiveAssociate"
                ("zkt:wasPerformedBy" ;; part of, active
                 ("zkt:wasLedBy"))
                ("zkt:wasOrganizedBy"))
               ("zkt:hadPassiveAssociate"
                ("zkt:wasPerformedWith" ;; part of, passive?
                 ("zktm:wasPerformedOn"))
                ("zkt:wasDirectedAt"))
               ("zkt:hadResponsibleParty") ;; activity -- agent
               ("zkt:applicant")
               ("zkt:employer")
               ("zkt:employee")
               ("zkt:sentBy")
               ("zkt:sentTo"))

              ("prov:wasDerivedFrom" ;;entity -- entity
               ("prov:hadPrimarySource")
               ("prov:wasQuotedFrom")
               ("prov:wasRevisionOf"))
              ("prov:wasGeneratedBy") ;;entity by activity
              ("prov:wasInvalidatedBy")
              ("prov:used"
               ("zkt:perceptionOf"))
              ("prov:actedOnBehalfOf")
              ("prov:wasInformedBy") ;;activity by activity
              ("prov:wasStartedBy")
              ("prov:wasEndedBy"))
             ("prov:alternateOf"
              ("prov:specializationOf" ;;entity -- entity
               ))
             ("prov:hadMember")
             ("prov:memberOf")
             ("prov:atLocation"
              ("zkt:startedAtLocation")
              ("zkt:endedAtLocation")) ;; ... at Location
             ("prov:generatedAtTime")  ;; entity at instant
             ("prov:qualifiedInfluence"
              ("prov:qualifiedUsage"
               ("zkt:qualifiedPerception")))
             ("skos:semanticRelation"
              ("skos:related"
               ("skos:relatedMatch"))
              ("skos:broaderTransitive"
               ("skos:broader"
                ("skos:broadMatch"))
               ("zkt:isTypeOf")) ;;
              ("skos:narrowerTransitive"
               ("skos:narrower"
                ("skos:narrowMatch"))
               ("zkt:hasType"))
              ("skos:mappingRelation"
               ("skos:closeMatch"
                ("skos:exactMatch")
                ("skos:relatedMatch")
                ("skos:broadMatch")
                ("skos:narrowMatch")))))

            ;; SKOS
            (
             ("skos:subject"
              ("skos:primarySubject"))
             ("skos:isSubjectOf"
              ("skos:isPrimarySubjectOf"))
             ("skos:member")
             ("skos:memberOf")
             ("skos:inScheme")
             ("skos:definition"))
            ("time:intervalStartedBy" "time:intervalStarts"
             "time:intervalFinishedBy" "time:intervalFinishes"
             "time:after" "time:before"
             "time:intervalMetBy" "time:intervalMeets"
             "time:intervalContains" "time:intervalDuring"
             "time:minutes"
             "time:hours"
             "time:days"
             "time:hasDateTimeDescription")
            (("dct:issued")
             ("dct:date")
             ("dct:hasPart")
             ("dct:isPartOf")
             ("dct:language"))
            "zkt:addressee" ;; addressee of entity, letter etc
            ;; descriptive meta data
            ("zkt:distanceKM")
            ("zkt:wasEvidencedBy")      ;; Concept -- ...
            ("zkt:result")
            ("foaf:member") ("foaf:memberOf")


            ("zktb:wasDedicatedTo")

            ("zktm:atBodilyLocation")
            ("zkt:hadQualia")
            ("zkt:dosage")
            ("prov:entity")))

(setq zettelkasten-predicate-domain-range
      '(("zkt:symbolizes" ("zkt:LinguisticForm" ("skos:Concept")))
        ("zkt:wasSymbolizedBy" ("skos:Concept" ("zkt:LinguisticForm")))
        ("zkt:refersTo" ("skos:Concept" ("owl:Class")))
        ("zkt:wasReferedToBy" ("owl:Class" ("skos:Concept")))
        ("zkt:standsFor" ("zkt:LinguisticForm" ("owl:Class")))
        ("prov:wasAttributedTo" ("prov:Entity" ("prov:Agent")))
        ("zktb:wasAuthoredBy" ("prov:Entity" ("prov:Agent")))
        ("zktb:wasEditedBy" ("prov:Entity" ("prov:Agent")))
        ("zkt:wasCoinedBy" ("prov:Entity" ("prov:Agent")))
        ;;
        ("prov:wasAssociatedWith" ("prov:Activity" ("prov:Agent")))
        ;; 
        ("zkt:hadParticipant" ("zkt:Event" ("prov:Agent")))
        ("zkt:hadNonParticipant" ("zkt:Event" ("prov:Agent")))
        ("zkt:hadActiveAssociate" ("zkt:Event" ("prov:Agent")))
        ("zkt:hadPassiveAssociate" ("zkt:Event" ("prov:Agent")))
        ;; 
        ("zkt:wasPerformedBy" ("zkt:Event" ("prov:Agent")))
        ("zkt:wasPerformedWith" ("zkt:Event" ("prov:Agent")))
        ("zkt:wasOrganizedBy" ("zkt:Event" ("prov:Agent")))
        ("zkt:wasDirectedAt" ("zkt:Event" ("prov:Agent")))
        ;; 
        ("zkt:wasLedBy" ("zkt:Event" ("prov:Agent")))
        ("zkt:wasPerformedOn" ("zkt:Event" ("prov:Agent")))
        ;;
        ("prov:wasGeneratedBy" ("prov:Entity" ("prov:Activity")))
        ;;
        ("prov:used" ("prov:Activity" ("prov:Entity")))
        ;;
        ("prov:wasInformedBy" ("prov:Activity" ("prov:Activity")))
        ;;
        ("prov:wasDerivedFrom" ("prov:Entity" ("prov:Entity")))
        ("prov:wasRevisionOf" ("prov:Entity" ("prov:Entity")))
        ("prov:hadPrimarySource" ("prov:Entity" ("prov:Entity")))
        ;;
        ("prov:atLocation" ("owl:Class" ("prov:Location")))

        ("prov:memberOf" ("prov:Entity" ("prov:Collection")))
        ;;
        ("skos:broaderTransitive" ("skos:Concept" ("skos:Concept")))
        ("skos:broader" ("skos:Concept" ("skos:Concept")))
        ("skos:narrowerTransitive" ("skos:Concept" ("skos:Concept")))
        ("skos:narrower" ("skos:Concept" ("skos:Concept")))
        ("skos:narrowMatch" ("skos:Concept" ("skos:Concept")))
        ("skos:broadMatch" ("skos:Concept" ("skos:Concept")))
        ("skos:related" ("skos:Concept" ("skos:Concept")))
        ("skos:subject" ("owl:Class" ("owl:Class")))
        ("skos:primarySubject" ("owl:Class" ("owl:Class")))
        ("skos:isSubjectOf" ("owl:Class" ("owl:Class")))
        ("skos:isPrimarySubjectOf" ("owl:Class" ("owl:Class")))
        ;; 
        ("dct:issued" ("prov:Entity" ("time:DateTimeInterval")))
        ("dct:date" ("owl:Class" ("time:DateTimeInterval")))
        ("dct:language" ("owl:Class" ("dct:LinguisticSystem")))
        ("dct:isPartOf" ("owl:Class" ("owl:Class")))
        ("dct:hasPart" ("owl:Class" ("owl:Class")))
        ("zkt:result" ("owl:Class" ("owl:Class")))
        ("zkt:dosage" ("zkt:Event" ("value")))
        ;;
        ("foaf:memberOf" ("foaf:Person" ("foaf:Group")))
        ;;
        ("time:hasDateTimeDescription" ("owl:Class" ("time:DateTimeDescription")))
        ("time:minutes" ("prov:Activity" ("value")))
        ("time:hours" ("prov:Activity" ("value")))
        ("time:days" ("prov:Activity" ("value")))
        ;;
        ("zkt:distanceKM" ("zkt:Event" ("value")))
        ))

(setq zettelkasten-classes
      '("owl:Class"
        ;; Event, Procedure, Relationship
        ("prov:Activity"
         ("zkt:Event"
          ("zkt:Sitzung")
          ("zkt:Seminar")
          ("zkt:Experience"))
         ("zkt:Procedure"
          ("zkt:ApplicationProcedure")
          ("zkt:Project"
           ("zkt:PhD")))
         ("zkt:Relationship"
          ("zkt:ContractualRelationship"
           ("zkt:Employment")))

         (("zktm:HealthcareActivity"
           ("zktm:Treatment"
            ("zktm:Vaccination"))
           ("zktm:Diagnostics"))
          ("zkt:SpatialMovement")))
        ;;
        ("prov:Entity"
         ("zkt:LinguisticForm")
         ("skos:Concept")
         ("prov:Collection")
         ("prov:Plan"
          ("dct:LinguisticSystem")
          ("skos:Concept prov:Plan")
          ("zkt:Rezept")
          ("zktm:Vaccine"))
         ("zkt:RealObject"
          ("dct:Software")
          ("dct:BibliographicResource"
           ("zktb:ProperBibliographicResource"
            ("zktb:Article"
             ("zktb:Review"))
            ("zktb:Book")
            ("zktb:InBook")
            ("zktb:Collection")
            ("zktb:Lexikon")
            ("zktb:InCollection")
            ("zktb:Journal")
            ("zktb:Issue")
            ("zktb:Thesis")
            ("zktb:ClassicalText")
            ("zktb:Report"))
           ("zkt:BibliographicEphemera"
            ("zkt:DocumentPart"         ;; paragraph etc
             ("zkt:FormalDocumentPart") ;; with headline? Section, Chapter
             ("zkt:Quote") ;; cites other text, is part of document
             )
            ("zkt:Draft")
            ("zkt:SlideShow")
            ("zkt:Excerpt")
            ("zkt:Letter")
            ("zkt:Contract")
            ("zkt:Mitschrift")
            ("zkt:Note")))))
        ;;
        ("prov:Agent"
         ("foaf:Agent"
          ("foaf:Person")
          ("foaf:Organization"))
         ("prov:Person"
          ("foaf:Person")
          ("prov:Person foaf:Person"))
         ("prov:SoftwareAgent")
         ("prov:Organization"
          ("foaf:Group")
          ("foaf:Organization")))
        ;;
        ("prov:InstantaneousEvent"
         ("zkt:Waypoint"))
        ;;
        ("prov:Location"
         ("zkt:RealObject prov:Location geo:Point"
          ("zkt:TrainStation")
          ("zkt:Airport")))
        ("time:ProperInterval")
        ("time:DateTimeInterval")
        ("time:DateTimeDescription")
        ("dct:Collection")
        ("dct:PhysicalResource")
        ("skos:ConceptScheme")
        ("skos:Collection")
        ("prov:Influence"
         ("zkt:SemanticRelation")
         ("prov:EntityInfluence"
          ("prov:Usage")
          ("zkt:Perception")))
        "value"))


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
  (let ((type-sel (or (when (stringp type)
                        type)
                      (completing-read "Type: "
                                       (or type
                                           (-flatten zettelkasten-classes))))))
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
(defun zettelkasten-insert-link-at-point (&optional link-predicate link-target)
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
                               (zettelkasten-tree-get-path
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
                                    (zettelkasten-tree-assoc
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

(defun zettelkasten-insert-link-loop ()
  (interactive)
  (insert "- ")
  (zettelkasten-insert-link-at-point)
  (newline)
  (zettelkasten-insert-link-loop))



(defun zettelkasten-heading-set-relation-to-context (&optional pred)
  (let* ((predicate (or pred
                     (completing-read "Predicate: "
                                      (zettelkasten-flat-predicates)
                                      nil nil "dct:isPartOf")))
         (target (completing-read "Target:"
                                  (zettelkasten-db-query [:select zkid
                                                          :from nodes
                                                          :where (= filename $s1)]
                                                         (buffer-file-name))))
         (turtle (format "%s::%s" predicate target)))
    (org-set-property "TURTLE" turtle)))

;;;###autoload
(defun zettelkasten-heading-to-node (&optional rdftype prov-id predicate)
  (interactive)
  (zettelkasten-set-type-headline rdftype)
  (zettelkasten-id-get-create prov-id)
  (zettelkasten-heading-set-relation-to-context predicate))

(defun zettelkasten-heading-to-docpart ()
  (interactive)
  (zettelkasten-set-type-headline '("zkt:DocumentPart"
                                    "zkt:FormalDocumentPart"))
  (zettelkasten-id-get-create (concat (file-name-base) "--"))
  (zettelkasten-heading-set-relation-to-context "dct:isPartOf"))


(defun zettelkasten-id-get-create (&optional prov-id)
  (let ((zk-id (org-entry-get nil "CUSTOM_ID")))
    (unless (and zk-id (stringp zk-id) (string-match "\\S-" zk-id))
      (let* ((myid
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
        (zettelkasten-zettel-ensure-keyword "DESCRIPTOR")
        (insert (concat " " desc))
        (zettelkasten-sort-tags)))
    (unless (or descriptor (equal desc "#Break#"))
      (zettelkasten-zettel-add-descriptor))))

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
;; https://stackoverflow.com/questions/11912027/emacs-lisp-search-anything-in-a-nested-list

(defun zettelkasten-tree-assoc (key tree)
  (when (consp tree)
    (destructuring-bind (x . y) tree
      (if (equal x key)
          tree ;; fct of comparing
        (or (zettelkasten-tree-assoc key x)
            (zettelkasten-tree-assoc key y))))))

(defun zettelkasten-tree-get-path (key tree &optional path-in)
  (let ((path (or path-in nil)))
    (when (consp tree)
      (destructuring-bind (x . y) tree
        (if (equal x key)
            path
          (if (listp x)
              (push (car x) path)
            (push x path))
          (or (progn
                (zettelkasten-tree-get-path key x path))
              (progn
                (when path-in (pop path))
                (zettelkasten-tree-get-path key y path))))))))

(defun zettelkasten-predicate-hierachy (predicate)
  "Returns vector of predicate hierarchy."
  (vconcat
   (-flatten
    (zettelkasten-tree-assoc predicate
                             zettelkasten-predicates))))


(defun zettelkasten-db--nodes-semantic (&optional input-nodes)
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
                                          '("##Break"))
                                       (zettelkasten-flat-predicates)))))
    (unless (equal predicate "##Break")
      (let* ;; lookup predicate hierarchy
          ((predicates (zettelkasten-predicate-hierachy predicate))
           ;; get object label or id with selected predicate
           (objects-edges (vconcat
                           (-flatten
                            (if input-nodes
                                (zettelkasten-db-query [:select :distinct [object]
                                                        :from edges
                                                        :where (in subject $v1)
                                                        :and (in predicate $v2)]
                                                       input-nodes predicates )
                              (zettelkasten-db-query [:select :distinct [object]
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
    (if (or (equal predicate "##Break")
            (< (length output-nodes) 11))
        output-nodes
      (zettelkasten-db--nodes-semantic (vconcat output-nodes)))))

;;;###autoload
(defun zettelkasten-open-semantic ()
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

(defun zettelkasten-get-property-or-filetag-upwards (filename element property)
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
  (let* ((current-ids ;; all ids of this file
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




(defhydra hydra-zettelkasten (:color blue)
  "Zettelkasten"
  ("C-ä" zettelkasten-open-zettel "Open Zettel" :column "Open")
  ("Ä" (zettelkasten-open-zettel t) "Open Nodes")
  ("R" zettelkasten-open-zettel-random "Open random")
  ("ä" zettelkasten-open-zettel-collection "Open collection")
  ("d" zettelkasten-open-zettel-descriptor "Open descriptor")
  ("s" zettelkasten-open-semantic "Open semantic")
  ("jw" zettelkasten-journal-weekly-file "Weekly file")

  ("C-r" zettelkasten-inbox-process (format "Process inbox [%s]" 5) :color red :column "Inbox")
  ("b" zettelkasten-inbox-bury "Bury" :color red)
  ("t" zettelkasten-inbox-trash "Trash" :color red)

  ("l" zettelkasten-insert-link-at-point "Link" :column "Edit")
  ("L" zettelkasten-insert-link-loop "Link loop")

  ("p" zettelkasten-capture-push "Push Link" :column "Zettelkasten")
  ("P" (zettelkasten-capture-push t) "Push Heading")
  ("D" zettelkasten-replace-descriptor "Replace Desc.")
  ("I" zettelkasten-info "Info")

  ("c" zettelkasten-zettel-add-collection "Add collection" :column "Zettel")
  ("#" zettelkasten-zettel-add-descriptor "Add descriptor")
  ("x" zettelkasten-zettel-add-index "Add Index")
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
  ("hz" zettelkasten-node-to-zettel "Zettel")

  ("n" org-noter "noter" :column "Other")
  ("u" zettelkasten-update-org-agenda-files "Update agenda")
  ("q" nil "Quit"))

;;;###autoload
(defun zettelkasten-rename-file ()
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
  (org-element-map
      (or element (org-element-parse-buffer 'greater-element))
      'keyword
    (lambda (kw)
      (if (string= (org-element-property :key kw) keyword)
          (org-element-property :value kw)))
    :first-match t))




(provide 'zettelkasten)
;;; zettelkasten.el ends here
