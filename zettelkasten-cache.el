;;; zettelkasten-cache.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Zettelkasten-cache
;;
;;; Code:
(require 'org-element)
(require 'org-el-cache)


(defun zettelkasten-extract-value (element keyword)
  (org-element-map element 'keyword
    (lambda (kw)
      (if (string= (org-element-property :key kw) keyword)
          (org-element-property :value kw)))
    :first-match t))

(defun zettelkasten-extract-title (element)
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

(defun zettelkasten-extract-collections (element)
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

(defun zettelkasten-extract-descriptors (element)
  (ignore-errors
    (let* ((descriptor-string
            (zettelkasten-extract-value element "DESCRIPTOR"))
           (descriptor-split
            (split-string descriptor-string))
           (descriptor-list nil))
      (dolist (descriptor descriptor-split)
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
     :title (zettelkasten-extract-title element)
     :id (zettelkasten-extract-id filename)
     :collections (zettelkasten-extract-collections element)
     :descriptors (zettelkasten-extract-descriptors filename element)
     :links (zettelkasten-extract-link-ids filename element))))

;; Update / Initialize the cache
(add-hook 'after-save-hook (lambda () (org-el-cache-update zettelkasten-cache)))

(provide 'zettelkasten-cache)
;;; zettelkasten-cache.el ends here
