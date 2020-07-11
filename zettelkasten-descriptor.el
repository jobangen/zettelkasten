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
(defun zettelkasten-zettel-similarities ()
  (interactive)
  (let* ((zettel-title
          (plist-get (zettelkasten-cache-entry-filename) :title))
         (zettel-sim
          (zettelkasten--calc-similarities (buffer-file-name))))
    (switch-to-buffer-other-window "*zettelkasten-similarities*")
    (erase-buffer)
    (org-mode)
    (insert (format "#+TITLE: Similarities: %s\n\n" zettel-title))
    (insert "| JI | Title |\n|--|--|\n")
    (dolist (sim zettel-sim)
      (insert (format "| %s | [[file:%s][%s]] |\n"
                      (cdr sim)
                      (car sim)
                      (s-truncate
                       75
                       (plist-get
                        (zettelkasten-cache-entry-filename (car sim)) :title)))))
    (insert "|--|--|")
    (goto-char (point-min))
    (next-logical-line 2)
    (org-table-align)
    (other-window 1)))

(provide 'zettelkasten-descriptor)
;;; zettelkasten-descriptor.el ends here
