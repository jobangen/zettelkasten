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

;; obsolete
(defun zettelkasten-vis-buffer-nodes-2 (buffer depth)
  (let ((nodes nil)
        (sources (list buffer)))
    (dotimes (i depth)
      (let ((targets
             (delete-dups
              (-flatten
               (zettelkasten-db-query [:select [id:filename link:filename]
                                       :from link
                                       :inner-join id
                                       :on (= link:linkid id:zkid)
                                       :where (in link:filename $v1)
                                       :or (in id:filename $v2)]
                                      (vconcat sources) (vconcat sources))))))
        (setq nodes (append nodes sources targets))
        (setq sources targets)))
    (delete-dups nodes)))

(defun zettelkasten-vis-buffer-nodes (filename depth)
  (let ((nodes (-flatten (zettelkasten-db-query [:select [zkid label]
                                                 :from nodes
                                                 :where (= filename $s1)]
                                                filename))))
    (dotimes (i depth)
      (let ((nodes2
             (delete-dups
              (-flatten
               (zettelkasten-db-query
                [:select [subject object]
                 :from edges
                 :where (not (in predicate ["dct:language"
                                            "time:intervalDuring"
                                            "rdf:type"]))
                 :and (in subject $v1)
                 :or (in object $v2)]
                (vconcat nodes) (vconcat nodes))))))
        (setq nodes nodes2)))
    (-flatten (zettelkasten-db-query [:select [zkid label]
                                      :from nodes
                                      :where (in zkid $v1)
                                      :or (in label $v2)]
                                     (vconcat nodes) (vconcat nodes)))))

(defun zettelkasten-vis-add-nodes (ids)
  (let ((zettel-sel
         (zettelkasten-db-query [:select [filename zkid title rdftype]
                                         :from nodes
                                         :where (in zkid $v1)
                                         :or (in label $v2)]
                                (vconcat ids) (vconcat ids))))
    (dolist (zettel zettel-sel)
      (let ((filename (car zettel))
            (zkid (cadr zettel))
            (title (s-replace-all '(("&" . "\\&")
                                    ("{" . "\\{") ("}" . "\\}")
                                    ("\\" . ""))
                                  (caddr zettel)))
            (type (cadddr zettel)))
        (insert (format "G.add_node(\"%s\")\n" zkid))
        (insert
         (format "labeldict.update({'%s': \"\\href{%s}{%s}\"})\n"
                 zkid filename (s-truncate 30 title)))
        (cond ((member type (-flatten
                             (assoc "prov:Activity" zettelkasten-classes)))
               (insert (format "color_map.append('orange')\n")))
              ((or (s-starts-with? "zktb:" type)
                   (string= "zkt:Mitschrift" type))
               (insert (format "color_map.append('blue')\n")))
              ((string= "zkt:Seminarplan" type)
               (insert (format "color_map.append('violet')\n")))
              ((string= "zkt:Sitzungsplan" type)
               (insert (format "color_map.append('violet')\n")))
              ((s-starts-with? "skos:Concept" type)
               (insert (format "color_map.append('green')\n")))
              ((string= "time:DateTimeInterval" type)
               (insert (format "color_map.append('yellow')\n")))
              ((string= "time:DateTimeDescription" type)
               (insert (format "color_map.append('grey')\n")))
              ((or (string= "zkt:Project" type)
                   (string= "zkt:Forschungswerkstatt" type)
                   (string= "zkt:Kolloquium" type)
                   (string= "zkt:Seminar" type))
               (insert (format "color_map.append('red')\n")))
              ((or (string= "prov:Person" type)
                   (string= "prov:Person foaf:Person" type))
               (insert (format "color_map.append('grey')\n")))
              (t (insert
                  (format "color_map.append('black')\n"))))))))

;; obsolete
(defun zettelkasten-vis-colorize-nodes (ids)
  (let ((types (zettelkasten-db-query [:select [rdftype]
                                       :from nodes
                                       :where (in zkid $v1)]
                                      (vconcat ids))))
    (dolist (type types)
      (cond ((string= "sem:Event" (car type))
             (insert (format "color_map.append('blue')\n")))
            ((string= "time:ProperInterval" (car type))
             (insert (format "color_map.append('yellow')\n")))
            ((string= "foaf:Project" (car type))
             (insert (format "color_map.append('green')\n")))
            (t (insert
                (format "color_map.append('black')\n")))))))

(defun zettelkasten-vis-add-edges (ids)
  (let* ((vids (vconcat ids))
         (edges
          (zettelkasten-db-query [:select [e:subject n:zkid]
                                  :from edges e
                                  :inner-join nodes n
                                  :on-case
                                  :when (= e:object n:zkid) :then-1
                                  :when (= e:object n:label) :then-1
                                  :else-0 :end-=-1
                                  :where (in e:subject $v1)
                                  :and (in e:object $v2)]
                                 vids vids)))
    (dolist (edge edges)
      (let ((source (car edge))
            (target (cadr edge)))
        (insert (format "G.add_edge(\"%s\", \"%s\")\n"
                        source target))))))

(defun zettelkasten-vis-buffer ()
  (interactive)
  (let* ((pyfile "/home/job/Dropbox/db/zk/zettel/tmp/vis.py")
         (outfile "/home/job/Dropbox/db/zk/zettel/img/vis.pdf")
         (root (buffer-file-name))
         (root-title (s-replace-all '(("&" . "\\&")
                                  ("{" . "\\{") ("}" . "\\}")
                                  ("\\" . ""))
                                (zettelkasten--extract-title)))
         (depth (read-string "Depth: " "3"))
         (nodes (zettelkasten-vis-buffer-nodes root (string-to-number depth))))
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
        (insert (format "plt.title(r\"\\href{%s}{%s}\", fontsize = 24)\n"
                        root root-title))
        (zettelkasten-vis-add-nodes nodes)
        ;; (zettelkasten-vis-colorize-nodes nodes)
        (zettelkasten-vis-add-edges nodes)
        (insert "pos = nx.kamada_kawai_layout(G)\n")
        (insert "nx.draw_networkx(G, pos, labels=labeldict, with_labels = True, font_size = 11, font_color = 'black', edge_color= 'grey', node_size = 100, node_color=color_map)\n")
        (insert "plt.axis('off')\n")
        (insert
         (format "plt.savefig(\"%s\", bbox_inches=\"tight\");\n" outfile))))
    (shell-command (format "python3 %s" pyfile))
    (other-window 1)
    (find-file outfile)))

(defun zettelkasten-info ()
  (interactive)
  (let* ((nodes (caar (zettelkasten-db-query [:select (funcall count id)
                                              :from nodes])))
         ;; (nodes-txt
         ;;  (caar (zettelkasten-db-query [:select (funcall count :distinct filename)
         ;;                                :from collection
         ;;                                :where (= collection "txt")])))
         ;; (nodes-index
         ;;  (caar (zettelkasten-db-query [:select (funcall count :distinct filename)
         ;;                                :from collection
         ;;                                :where (= collection "index")])))
         ;; (nodes-content
         ;;  (caar (zettelkasten-db-query [:select (funcall count :distinct filename)
         ;;                                :from collection
         ;;                                :where (= collection "content")])))
         (nodes-proj
           (caar (zettelkasten-db-query [:select (funcall count :distinct id)
                                         :from nodes
                                         :where (= rdftype "zkt:Project")])))
         (nodes-journal
          (caar (zettelkasten-db-query [:select (funcall count :distinct id)
                                        :from nodes
                                        :where (= rdfType "time:ProperInterval")])))
         (edges
          (caar (zettelkasten-db-query [:select (funcall count id)
                                        :from edges]))))
    (switch-to-buffer-other-window "*zettelkasten-info*")
    (erase-buffer)
    (org-mode)
    (insert (format "#+TITLE: Zettelkasten info\n\n"))
    (insert (format "- Nodes: %s\n" nodes))
    ;; (insert (format "  - index: %s\n" nodes-index))
    ;; (insert (format "  - content: %s\n" nodes-content))
    (insert (format "  - proj: %s\n" nodes-proj))
    ;; (insert (format "  - txt: %s\n" nodes-txt))
    (insert (format "  - journal: %s\n" nodes-journal))
    (insert (format "- Edges: %s\n" edges))))


(provide 'zettelkasten-visualization)
;;; zettelkasten-descriptor.el ends here
