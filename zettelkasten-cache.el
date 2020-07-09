;;; zettelkasten-cache.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Zettelkasten-cache
;;
;;; Code:
(require 'org-element)
(require 'org-el-cache)
(require 's)

(defun zettelkasten-extract-value (element keyword)
  (org-element-map element 'keyword
    (lambda (kw)
      (if (string= (org-element-property :key kw) keyword)
          (org-element-property :value kw)))
    :first-match t))

(defun zettelkasten-extract-title (filename element)
  (zettelkasten-extract-value element "TITLE"))

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

(defun zettelkasten-extract-collections (filename element)
  (ignore-errors
    (let* ((collection-string
            (zettelkasten-extract-value element "COLLECTION"))
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

(defun zettelkasten-extract-descriptors (filename element)
  (ignore-errors
    (let* ((descriptor-string
            (zettelkasten-extract-value element "DESCRIPTOR"))
           (descriptor-split
            (split-string descriptor-string))
           (descriptor-headings
            (-flatten (org-element-map element 'node-property
               (lambda (property)
                 (when (string= (org-element-property :key property) "DESCRIPTOR")
                   (split-string (org-element-property :value property)))))))
           (descriptor-conc
            (append descriptor-split descriptor-headings))
           (descriptor-list nil))
      (dolist (descriptor descriptor-conc)
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

(defun zettelkasten-extract-link-ids (filename el)
  (org-element-map el 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "zk")
        (org-element-property :path link)))))

(def-org-el-cache
  zettelkasten-cache
  (list zettelkasten-zettel-directory)
  (lambda (filename element)
    (list
     :file filename
     :title (zettelkasten-extract-title filename element)
     :id (zettelkasten-extract-id filename)
     :collections (zettelkasten-extract-collections filename element)
     :descriptors (zettelkasten-extract-descriptors filename element)
     :links (zettelkasten-extract-link-ids filename element))))

;; Update / Initialize the cache
(add-hook 'after-save-hook (lambda () (org-el-cache-update zettelkasten-cache)))

;;;###autoload
(defun zettelkasten-force-update-cache ()
  (interactive)
  (org-el-cache-force-update zettelkasten-cache))

(defun zettelkasten-cache-entry-id (id)
  (car (org-el-cache-select
        zettelkasten-cache
        (lambda (filename entry)
          (string= (plist-get entry :id) id)))))
(defun zettelkasten-cache-entry-filename (&optional filename)
  (org-el-cache-get zettelkasten-cache (or filename (buffer-file-name))))


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


(provide 'zettelkasten-cache)
;;; zettelkasten-cache.el ends here
