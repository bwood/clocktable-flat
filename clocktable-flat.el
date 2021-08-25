(defcustom org-clock-clocktable-flat-columns
  '("Date" "Project" "Hours" "Notes")
  "Columns for the org-clocktable-write-flat clocktable formatter."
  :group 'org-clocktable
  :type '(repeat string))


(defun org-clocktable-write-flat (ipos tables params)
  "Write out a flat clock table at position IPOS in the current
buffer. Useful to render a table which can be exported to CSV and
then imported into accounting/invoiving tools.

TABLES is a list of tables with clocking data as produced by
`org-clock-get-table-data'.

PARAMS is the parameter property list obtained from the dynamic block
definition.  Several parameters are used beyond those used by the
clocktable dynamic block:
    :customer A string to be inserted into the 'Customer Name' column
    :price A number to be inserted into the 'Item Price' column as a rate
    :number A string to be inserter into the Invoice Number column
    :columns A list which overrides the default CSV columns from
             `org-clock-clocktable-flat-columns'

Several other columns are calculated automatically:
    Item Name: The path to the parent headline
    Item Desc: The headline
    Quantity: The clocktime.  Useful with the `;t' formatter to get a
              decimal billable time.
"
   (let ((properties (plist-get params :properties))
	(ts (plist-get params :tstart))
	(te (plist-get params :tend))
	(columns (or (plist-get params :columns)
                    org-clock-clocktable-flat-columns)))

;    (insert "\n")
;    (dolist (column columns)   ; for our purposes we don't need the headers
;      (insert column ","))
;    (insert "\n")
    (dolist (table tables)
      (let ((rows (nth 2 table)) ;rows 2nd element of table list
           (parents (list ""))) ; parents = empty list
       (dotimes (row-idx (length rows))
         (let ((row (nth row-idx rows))) ; row is the last row
           (if (listp row) ; Return t if OBJECT is a list, that is, a cons cell or nil.
               (progn      ; Eval BODY forms sequentially and return value of last one.
                 (dotimes (parent-idx
                           (- (nth 0 (nth (- row-idx 1) rows)) (nth 0 row)))
                   (setcdr (last parents 2) nil)) ;Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.
                 (if (and (< row-idx (- (length rows) 1))
                          (< (nth 0 row) (nth 0 (nth (+ row-idx 1) rows))))
                     (nconc parents (list (cadr row))) ;Concatenate any number of lists by altering them.
;                   (insert ",")
                   (dolist (column columns)
                     (cond
                      ((equal column "Date")
		       (when (string-match "\\([[:digit:]]+\\)-\\([[:digit:]]+\\)-\\([[:digit:]]+\\)" ts)
			 (insert (match-string 2 ts) "/" (match-string 3 ts) "/" (match-string 1 ts))))
		      ((equal column "Project") 
		       (insert
                       ; from org-clock.el.gz: org-clocktable-write-default
			(if properties
			    (concat
			     (mapconcat
			      (lambda (p) (or (cdr (assoc p (nth 4 row))) ""))
		              properties "") "") "")))  ;properties columns, maybe
                      ((equal column "Notes") 
		       (insert "\"" (nth 1 row) "\""))
; Would be nice to get this working
;                       (dotimes (parent-idx (length parents))
;                         (insert (nth parent-idx parents))
;                         (if (not (or (= parent-idx 0)
;                                      (= parent-idx (- (length parents) 1))))
;                            (insert "/"))))                       
                      ;((equal column "Item Desc") (insert (cadr row))) ;this is (car (cdr cons-cell)) or (nth 1 cons-cell)
                      ((equal column "Hours")
                       (insert (format "%0.2f" (/ (nth 3 row) 60.0))))
;                      ((equal column "Item Price")
 ;                      (insert (format "%s" price)))
)
                     (insert ",")) ;should skip the comma at the end of the line
                   (insert "\n"))))))))))
;    (insert "#+TBLFM: " (plist-get params :formula))
;    (org-ctrl-c-ctrl-c)
;    (goto-char ipos)
;    (skip-chars-forward "^|")))
;
; The cons function is used to construct lists, and the car and cdr functions are used to take them apart.
; The car of a list is, quite simply, the first item in the list. Thus the car of the list (rose violet daisy buttercup) is rose.
; The cdr of a list is the rest of the list, that is, the cdr function returns the part of the list that follows the first item. Thus, while the car of the list '(rose violet daisy buttercup) is rose, the rest of the list, the value returned by the cdr function, is (violet daisy buttercup).
