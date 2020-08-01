;;; zettelkasten-visualization.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; zettelkasten-visualization
;;
;;; Code:

(defun my-list-intersect-p (list1 list2)
  "Return non-nil if any elements of LIST1 appear in LIST2.
Comparison is done with `equal'."
  (while (and list1 (not (member (car list1) list2)))
    (pop list1))
  list1)

(defun zettelkasten-vis-linked-nodes ()
  (interactive)
  (let (links (-flatten
               (org-el-cache-map
                zettelkasten-cache
                (lambda (filename entry)
                  (plist-get entry :links)))))
    (org-el-cache-select
     zettelkasten-cache
     (lambda (filename entry)
       (or (member (plist-get entry :id) links) (plist-get entry :links))))))

(defun zettelkasten-vis-buffer-nodes (buffer depth)
  (let ((ids
         (list (plist-get
                (zettelkasten-cache-entry-filename buffer)
                :id))))
    (dotimes (i depth)
      (let ((linked
             (mapcar
              (lambda (arg)
                (plist-get arg :links))
              (zettelkasten-cache-entry-ids ids)))
            (linking
             (mapcar
              (lambda (arg)
                (plist-get arg :id))
              (org-el-cache-select
               zettelkasten-cache
               (lambda (filename entry)
                 (my-list-intersect-p ids (plist-get entry :links)))))))
        (setq ids (-flatten (append ids linked linking)))))
    (zettelkasten-cache-entry-ids (delete-dups ids))))

(defun zettelkasten-vis-add-nodes (entries)
  (dolist (entry entries)
    (let ((id (plist-get entry :id))
          (title (s-replace "&" "\\&" (plist-get entry :title)))
          (file (plist-get entry :file)))
      (insert (format "G.add_node(\"%s\")\n" id))
      (unless (member "journal" (plist-get entry :collections))
        (insert
         (format "labeldict.update({'%s': \"\\href{%s}{%s}\"})\n"
                 id file (s-truncate 20 title)))))))

(defun zettelkasten-vis-colorize-nodes (entries)
  (dolist (entry entries)
    (cond ((member "txt" (plist-get entry :collections))
           (insert
            (format "color_map.append('yellow')\n")))
          ((member "index" (plist-get entry :collections))
           (insert
            (format "color_map.append('blue')\n")))
          ((member "content" (plist-get entry :collections))
           (insert
            (format "color_map.append('red')\n")))
          ((member "proj" (plist-get entry :collections))
           (insert
            (format "color_map.append('pink')\n")))
          ((member "journal" (plist-get entry :collections))
           (insert
            (format "color_map.append('grey')\n")))
          (t (insert
              (format "color_map.append('black')\n"))))))

(defun zettelkasten-vis-add-edges (entries)
  (let ((nodes-list (mapcar (lambda (arg)
                              (plist-get arg :id))
                            entries)))
    (dolist (entry entries)
      (let ((id (plist-get entry :id)))
        (dolist (link (plist-get entry :links))
          (when (member link nodes-list)
            (insert (format "G.add_edge(\"%s\", \"%s\")\n"
                            id link))))))))

(defun zettelkasten-vis-buffer ()
  (interactive)
  (let* ((pyfile "/home/job/Dropbox/db/zk/zettel/tmp/vis.py")
         (outfile "/home/job/Dropbox/db/zk/zettel/img/vis.pdf")
         (root-entry (zettelkasten-cache-entry-filename (buffer-file-name)))
         (root-fname (plist-get root-entry :file))
         (root-title (s-replace "&" "\\&" (plist-get root-entry :title)))
         (depth (read-string "Depth: " "3"))
         (nodes (zettelkasten-vis-buffer-nodes (buffer-file-name)
                                               (string-to-number depth))))
    (with-temp-file pyfile
      (progn
        (insert "import networkx as nx\n")
        (insert "import matplotlib\n")
        (insert "matplotlib.use('pgf')\n")
        (insert "import matplotlib.pyplot as plt\n")
        (insert "G = nx.Graph()\n")
        (insert "plt.figure(num=None, figsize=(16, 17), dpi=120)\n")
        (insert "color_map = []\n")
        (insert "labeldict = {}\n")
        (insert "plt.rc('text', usetex=True)\n")
        (insert "plt.rc('font', family='serif')\n")
        (insert "matplotlib.rcParams['pgf.preamble']=[r'\\usepackage[colorlinks=true, urlcolor=black]{hyperref}', ]\n")
        (insert (format "plt.title(r\"\\href{%s}{%s}\")\n" root-fname root-title))
        (zettelkasten-vis-add-nodes nodes)
        (zettelkasten-vis-colorize-nodes nodes)
        (zettelkasten-vis-add-edges nodes)
        ;; (insert "pos = nx.spring_layout(G)\n")
        (insert "pos = nx.kamada_kawai_layout(G)\n")
        ;; (insert "pos = nx.fruchterman_reingold_layout(G)\n")
        (insert "nx.draw_networkx(G, pos, labels=labeldict, with_labels = True, font_size = 12, font_color = 'black', node_color=color_map, edge_color= 'grey', node_size = 100)\n")
        ;; (insert "plt.axis('off')\n")
        (insert
         (format "plt.savefig(\"%s\", bbox_inches=\"tight\");\n" outfile))))
    (shell-command (format "python3 %s" pyfile))
    (other-window 1)
    (find-file outfile)))





(defun zettelkasten-info ()
  (interactive)
  (let* ((nodes (length (org-el-cache-map
                         zettelkasten-cache
                         (lambda (filename entry)
                           (plist-get entry :id)))))
         (nodes-txt
          (length (zettelkasten-cache-entries-where-member "txt" :collections)))
         (nodes-index
          (length (zettelkasten-cache-entries-where-member "index" :collections)))
         (nodes-content
          (length (zettelkasten-cache-entries-where-member "content" :collections)))
         (nodes-proj
          (length (zettelkasten-cache-entries-where-member "proj" :collections)))
         (nodes-journal
          (length (zettelkasten-cache-entries-where-member "journal" :collections)))
         (edges (length
                 (-flatten (org-el-cache-map
                            zettelkasten-cache
                            (lambda (filename entry)
                              (plist-get entry :links)))))))
    (switch-to-buffer-other-window "*zettelkasten-info*")
    (erase-buffer)
    (org-mode)
    (insert (format "#+TITLE: Zettelkasten info\n\n"))
    (insert (format "- Nodes: %s\n" nodes))
    (insert (format "  - index: %s\n" nodes-index))
    (insert (format "  - content: %s\n" nodes-content))
    (insert (format "  - proj: %s\n" nodes-proj))
    (insert (format "  - txt: %s\n" nodes-txt))
    (insert (format "  - journal: %s\n" nodes-journal))
    (insert (format "- Edges: %s\n" edges))))


(provide 'zettelkasten-visualization)
;;; zettelkasten-descriptor.el ends here
