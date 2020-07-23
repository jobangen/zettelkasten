;;; zettelkasten-descriptor.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Zettelkasten-descriptor
;;
;;; Code:
(require 'cl-lib)
(require 'zettelkasten-cache)
(require 'dash)

(defun zettelkasten--clean-descriptors (filename)
  (remove
   "@content"
   (remove
    "@index"
    (plist-get (zettelkasten-cache-entry-filename filename) :descriptors))))

(defun zettelkasten-jaccard-index (list1 list2)
  "Calculate jaccard index of two uniquified lists."
  (let* ((list1-unique (delete-dups list1))
         (list1-len (length list1-unique))
         (list2-unique (delete-dups list2))
         (list2-len (length list2-unique))
         (intersec (length (-intersection list1-unique list2-unique))))
    (/ (float intersec)
       (- (+ list1-len list2-len) intersec))))

(defun zettelkasten--calc-similarities (filename)
  (let* ((desc
          (zettelkasten--clean-descriptors filename))
         (similarities nil))
    (org-el-cache-each
     zettelkasten-cache
     (lambda (filename2 entry)
       (unless (string= filename filename2)
         (let* ((desc2
                 (zettelkasten--clean-descriptors filename2))
                (sim (round (* 100 (zettelkasten-jaccard-index desc desc2)))))
           ;; (push (cons filename2 sim) similarities)
           (when (> sim 0)
             (push (cons filename2 sim) similarities))))))
    (seq-take (sort similarities (lambda (x y) (> (cdr x) (cdr y)))) 25)))

;;;###autoload
(defun zettelkasten-zettel-info ()
  (interactive)
  (let* ((zettel-id (plist-get (zettelkasten-cache-entry-filename) :id))
         (zettel-backlinks
          (org-el-cache-select
           zettelkasten-cache
           (lambda (filename entry)
             (member zettel-id (plist-get entry :links)))))
         (zettel-title
          (plist-get (zettelkasten-cache-entry-filename) :title))
         (zettel-sim
          (zettelkasten--calc-similarities (buffer-file-name))))
    (switch-to-buffer-other-window "*zettelkasten-info*")
    (erase-buffer)
    (org-mode)
    (insert (format "#+TITLE: %s\n\n" zettel-title))
    (when zettel-backlinks
      (dolist (entry zettel-backlinks)
        (insert (format "- [[file:%s][%s]]\n"
                        (plist-get entry :file)
                        (plist-get entry :title))))
      (insert "\n"))
    (insert "| JI | Title |\n|--|--|\n")
    (dolist (sim zettel-sim)
      (insert (format "| %s | [[file:%s][%s]] |\n"
                      (cdr sim)
                      (car sim)
                      (s-truncate
                       76
                       (plist-get
                        (zettelkasten-cache-entry-filename (car sim)) :title)))))
    (insert "|--|--|")
    (previous-logical-line)
    (org-table-align)
    (other-window 1)))

;;;###autoload
(defun zettelkasten-replace-descriptor ()
  (interactive)
  (let* ((desc-old
          (completing-read
           "Descriptor: "
           (zettelkasten-cache-values-descriptor)))
         (desc-new (read-string "Descriptor: " desc-old))
         (zettel (zettelkasten-cache-entries-where-member desc-old :descriptors)))
    (dolist (entry zettel)
      (find-file (plist-get entry :file))
      (while (search-forward desc-old nil t)
        (replace-match desc-new))
      (zettelkasten-sort-tags)
      (save-buffer)
      (kill-buffer))))


(provide 'zettelkasten-descriptor)
;;; zettelkasten-descriptor.el ends here
