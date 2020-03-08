;; Altered version of this function from org-ref and org-ref-pubmed
;; Attempt to fix the problem of multiple "PT" records and "No conversion..." error

;; The point of parsing the MEDLINE text is so we can make bibtex entries. We
;; only support Journal articles for now.

(defun pubmed-pmid-to-bibtex (pmid)
  "Convert a PMID to a bibtex entry."
  (let* ((data (pubmed-parse-medline pmid))
         (type (downcase (cdr (rassoc "Journal Article" data))))
         (title (cdr (assoc "TI" data)))
         (authors (mapconcat 'cdr
                             (-filter (lambda (x)
                                        (string= (car x) "FAU"))
                                      data)
                             " and "))
         (abstract (cdr (assoc "AB" data)))
         (volume (cdr (assoc "VI" data)))
         (issue (cdr (assoc "IP" data)))
         (journal (cdr (assoc "JT" data)))
         (year (cdr (assoc "DP" data)))
         (pages (cdr (assoc "PG" data)))
         (aid (cdr (assoc "AID" data))))

    (cond
     ((string= type "journal article")
      (concat "@article{,
 author = {" authors "},
 title = {" title "},
 abstract = {" abstract "},
 journal = {" journal "},
 volume = {" volume "},
 number = {" issue "},
 year = {" (car (split-string year)) "},
 pages = {" pages "},
 doi = {" (replace-regexp-in-string " \\[doi\\]" "" (or aid "")) "},
}"))
     (t
      (message "No conversion for type: %s" type)))))

 
