;;; zettelkasten-cache.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Zettelkasten-cache
;;
;;; Code:
(require 'org-element)
(require 'org-el-cache)
(require 's)

(defun zettelkasten-extract-value (keyword &optional element)
  (org-element-map
      (or element (org-element-parse-buffer 'greater-element))
      'keyword
    (lambda (kw)
      (if (string= (org-element-property :key kw) keyword)
          (org-element-property :value kw)))
    :first-match t))

(defun zettelkasten-extract-title (&optional filename element)
  (zettelkasten-extract-value "TITLE" (when element)))

(defun zettelkasten-extract-id (filename)
  (let ((fname-chop
         (s-chop-prefix
          zettelkasten-zettel-directory
          (s-chop-suffix ".org" filename))))
    (cond ((s-prefix? "txt" fname-chop)
           (s-chop-prefix "txt/" fname-chop))
          ((s-prefix? "jr" fname-chop)
           (s-chop-prefix "jr/" fname-chop))
          ((s-prefix? "eph" fname-chop)
           (s-left 15 (s-chop-prefix "eph/" fname-chop)))
          (t (s-left 15 fname-chop)))))

(defun zettelkasten-extract-collections (filename element)
  (ignore-errors
    (let* ((collection-string
            (zettelkasten-extract-value "COLLECTION" element))
           (collection-split
            (split-string collection-string))
           (collection-headings
            (-flatten
             (org-element-map element 'node-property
               (lambda (property)
                 (when (string= (org-element-property :key property) "COLLECTION")
                   (split-string (org-element-property :value property)))))))
           (collection-conc
            (append collection-split collection-headings))
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

(defun zettelkasten-extract-descriptors (filename element)
  (ignore-errors
    (let* ((descriptor-string
            (zettelkasten-extract-value "DESCRIPTOR" element))
           (descriptor-split
            (split-string descriptor-string))
           (descriptor-headings
            (-flatten
             (org-element-map element 'node-property
               (lambda (property)
                 (when (string= (org-element-property :key property) "DESCRIPTOR")
                   (split-string (org-element-property :value property)))))))
           (descriptor-conc
            (append descriptor-split descriptor-headings))
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

(defun zettelkasten-extract-index (filename element)
  (ignore-errors
    (split-string (zettelkasten-extract-value "INDEX" element) "\"\s\"" t "\"")))


(defun zettelkasten-extract-link-ids (filename el)
  (org-element-map el 'link
    (lambda (link)
      (when (or (string= (org-element-property :type link) "zk")
                (string= (org-element-property :type link) "autocite"))
        (org-element-property :path link)))))

(defun zettelkasten-extract-custom-ids (filename el)
  (org-element-map el 'node-property
    (lambda (property)
      (when (string= (org-element-property :key property) "CUSTOM_ID")
        (org-element-property :value property)))))



(def-org-el-cache
  zettelkasten-cache
  (list zettelkasten-zettel-directory)
  (lambda (filename element)
    (list
     :file filename
     :title (zettelkasten-extract-title filename element)
     :id (zettelkasten-extract-id filename)
     ;; :collections (zettelkasten-extract-collections filename element)
     :descriptors (zettelkasten-extract-descriptors filename element)
     ;; :index (zettelkasten-extract-index filename element)
     :links (zettelkasten-extract-link-ids filename element)
     :custom-ids (zettelkasten-extract-custom-ids filename element)
     :todo (zettelkasten-extract-todo-state filename element))))



;; Update / Initialize the cache
;; (add-hook 'after-save-hook (lambda ()
;;                              (when (zettelkasten-zettel-p)
;;                                (org-el-cache-update zettelkasten-cache))))

;;;###autoload
(defun zettelkasten-force-update-cache ()
  (interactive)
  (org-el-cache-force-update zettelkasten-cache))

(defun zettelkasten-cache-entry-filename (&optional filename)
  (org-el-cache-get zettelkasten-cache (or filename (buffer-file-name))))

(defun zettelkasten-cache-entry-ids (ids)
  (org-el-cache-select
   zettelkasten-cache
   (lambda (filename entry)
     (member (plist-get entry :id) ids))))

(defun zettelkasten-cache-entries-where-member (match key)
  (org-el-cache-select
   zettelkasten-cache
   (lambda (filename entry)
     (member match (plist-get entry key)))))

(defun zettelkasten-cache-values-collection ()
  (let ((collections nil))
    (org-el-cache-each
     zettelkasten-cache
     (lambda (filename data)
       (setq collections
             (cons
              (plist-get data :collections)
              collections))))
    (delete-dups (-flatten collections))))

(defun zettelkasten-cache-values-descriptor (&optional entries)
  (delete-dups
   (-flatten
    (if entries
        (mapcar
         (lambda (arg)
           (plist-get arg :descriptors))
         entries)
      (org-el-cache-map
       zettelkasten-cache
       (lambda (filename entry)
         (plist-get entry :descriptors)))))))


;;;###autoload
(defun zettelkasten-cache-check-id-unique ()
  (interactive)
  (org-el-cache-each
   zettelkasten-cache
   (lambda (filename1 entry1)
     (let ((id1 (plist-get entry1 :id)))
       (org-el-cache-each
        zettelkasten-cache
        (lambda (filename2 entry2)
          (let ((id2 (plist-get entry2 :id)))

            (unless (string= filename1 filename2)
              (when (string= id1 id2)
                (message (format "%s = %s" filename1 filename2)))))))))))

;;;###autoload
(defun zettelkasten-chache-check-links ()
  (interactive)
  (let ((ids
         (org-el-cache-map
          zettelkasten-cache
          (lambda (filename entry)
            (plist-get entry :id)))))
    (switch-to-buffer "*zettelkasten-info*")
    (erase-buffer)
    (org-mode)
    (org-el-cache-each
     zettelkasten-cache
     (lambda (filename entry)
       (dolist (link (plist-get entry :links))
         (unless (member link ids)
           (insert (format "[[%s]]: %s\n" filename link))))))))



(provide 'zettelkasten-cache)
;;; zettelkasten-cache.el ends here
