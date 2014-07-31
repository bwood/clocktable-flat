(defcustom org-clock-clocktable-flat-columns
  '("Invoice Date" "Invoice Number" "Customer Name"
    "Item Name" "Item Desc" "Quantity" "Item Price")
  "Columns for the org-clocktable-write-flat clocktable formatter.
Default value matches the ZOHO CSV import format."
  :group 'org-clocktable
  :type '(repeat string))


(defun org-clocktable-write-flat (ipos tables params)
  "Write out a flat clock table at position IPOS in the current
buffer.  Useful to render a table which can be exported to CSV and
then imported into accounting/invoiving tools such as Zoho Invoicing.
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
  (let ((date (format-time-string "%Y-%m-%d"))
       (customer (plist-get params :customer))
       (number (plist-get params :number))
       (price (plist-get params :price))
       (columns (or (plist-get params :columns)
                    org-clock-clocktable-flat-columns)))
    (insert "\n|")
    (dolist (column columns)
      (insert column "|"))
    (insert "\n|-\n")
    (dolist (table tables)
      (let ((rows (nth 2 table))
           (parents (list "")))
       (dotimes (row-idx (length rows))
         (let ((row (nth row-idx rows)))
           (if (listp row)
               (progn
                 (dotimes (parent-idx
                           (- (nth 0 (nth (- row-idx 1) rows)) (nth 0 row)))
                   (setcdr (last parents 2) nil))
                 (if (and (< row-idx (- (length rows) 1))
                          (< (nth 0 row) (nth 0 (nth (+ row-idx 1) rows))))
                     (nconc parents (list (cadr row)))
                   (insert "|")
                   (dolist (column columns)
                     (cond
                      ((equal column "Invoice Date") (insert date))
                      ((equal column "Invoice Number") (insert number))
                      ((equal column "Customer Name") (insert customer))
                      ((equal column "Item Name")
                       (dotimes (parent-idx (length parents))
                         (insert (nth parent-idx parents))
                         (if (not (or (= parent-idx 0)
                                      (= parent-idx (- (length parents) 1))))
                             (insert "/"))))
                      ((equal column "Item Desc") (insert (cadr row)))
                      ((equal column "Quantity")
                       (insert (org-minutes-to-hh:mm-string (nth 3 row))))
                      ((equal column "Item Price")
                       (insert (format "%s" price))))
                     (insert "|"))
                   (insert "\n"))))))))
    (insert "#+TBLFM: " (plist-get params :formula))
    (org-ctrl-c-ctrl-c)
    (goto-char ipos)
    (skip-chars-forward "^|")))
