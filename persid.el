;;; persid.el --- Persistent Identifier Library -*- lexical-binding: t -*-

;; Copyright (C) 2023 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/emacs-persid
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: text, bib, references

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Persistent Identifier Library
;; =============================
;;
;; The Persistent Identifier Library allows to manipulate persistent
;; identifiers that are used to locate scholar resources online.  The
;; library knows about the following formats:
;;
;; - isbn: International Standard Book Number (https://isbn.org)
;; - issn: International Standard Serial Number (https://www.issn.org)
;; - doi: Digital Object identifier (https://www.doi.org)
;; - pmid: PubMed (https://pubmed.ncbi.nlm.nih.gov)
;; - pmcid: PubMed Central (https://www.ncbi.nlm.nih.gov/pmc)
;; - arxiv: Cornell University (https://arxiv.org)
;;
;; Given an identifier in one of the known formats, the libray can
;; query information about the resources and format it as a bibtex
;; entry.  To do so, the library primarily use the crossref.org API
;; (doi,pmid, pcmi), openalex.org API (issn), arxiv API (arxiv)
;; and OpenLibrary Book API (isbn).  Unfortunately, OpenLibrary is
;; far from being complete and a lot of ISBN records are missing.
;;
;; The main function is the interactive `persid-bibtex-from` function
;; that accept a single identifier and return the corresponding
;; bibtex.  To do that, the format of the identifier is first
;; identified and normalized (if needed), then  the correspoding bibtex
;; is queried online:
;;
;; - doi: the crossref API is used
;; - pmid : the pubmed API is used to convert the pmid to doi
;;          and then the crossref is used.
;; - pmcid : the pubmed API is used to convert the pmcid to doi
;;           and then the crossref is used.
;; - arxiv: the arxiv API is used
;; - issn (info only): the openalex API is used
;; - isbn: the OpenLibrary Book API is used
;;
;; See also:
;;   Information Studies: APIs for scholarly resources
;;   https://guides.lib.berkeley.edu/information-studies/apis
;;
;;   Free and Paid APIs to access books information via ISBN
;;   https://www.vinzius.com/post/free-and-paid-api-isbn/
;;
;; Example usage:
;; ==============
;;
;; (insert (persid-bibtex-from "arxiv:2008.06030"))
;;
;; @Article{2020arXiv200806030R,
;;        author = {{Rougier}, Nicolas P.},
;;         title = "{On the design of text editors}",
;;       journal = {arXiv e-prints},
;;      keywords = {Computer Science - Human-Computer Interaction},
;;          year = 2020,
;;         month = aug,
;;           eid = {arXiv:2008.06030},
;;         pages = {arXiv:2008.06030},
;; archivePrefix = {arXiv},
;;        eprint = {2008.06030},
;;  primaryClass = {cs.HC},
;;        adsurl = {https://ui.adsabs.harvard.edu/abs/2020arXiv200806030R},
;;       adsnote = {Provided by the SAO/NASA Astrophysics Data System}
;; }
;;
;;; NEWS:
;;
;; Version alpha
;; - First implementation
;;
;;; Code:
(require 'bibtex)
(require 'json)
(require 'time-date)
(require 'xml)

;; Declare external variables for the linter:
(defvar savehist-additional-variables)

(defgroup persid nil
  "Persistent Identifier Library."
  :group 'tex
  :prefix "persid-")

(defcustom persid-mail-address user-mail-address
  "Email used to access \"polite pools\".
Useful on various domains that provide faster access.  Set to nil if
you prefer anonymous access."
  :group 'persid
  :type 'string)

(defcustom persid-isbn-generate-citekey nil
  "If non-nil, automatically generate a citekey when getting BibTeX from ISBN.

The value `prompt' means that the citekey will be presented to the user in the
minibuffer area when generated, allowing for edits.

The value `user' means to respect the value of `bibtex-autokey-edit-before-use'
set by the user.

The creation of the citekey is handled by the built-in `bibtex-mode' via the
`bibtex-clean-entry' callable, and should respect user's configuration of the
package, see `bibtex-generate-autokey'."
  :group 'persid
  :type '(choice (symbol :tag "Generate citekey automatically" t)
                 (symbol :tag "Prompt user after generating citekey" 'prompt)
                 (symbol :tag "Respect user's configuration" 'user)
                 (const :tag "Don't generate citekey" nil)))

(defconst persid-formats '(isbn issn doi pmid pmcid arxiv)
  "List of known identifier formats.")

(defvar persid-history nil
  "List of identifiers.")

(eval-after-load "savehist"
  '(add-to-list 'savehist-additional-variables 'persid-history))

(defconst persid-isbn-10-regex
  (concat "\\("                   ;; Group 1 (optional)
          "ISBN:?\\|isbn:?"       ;; Prefix
          " *\\)?"
          "\\("                   ;; Group 2 (mandatory)
          "[0-9]\\{1,5\\}"        ;; Registration group
          "[- ]?"                 ;; Optional separator
          "[0-9]\\{1,7\\}"        ;; Registrant
          "[- ]?"                 ;; Optional separator
          "[0-9]\\{1,7\\}"        ;; Publication
          "[- ]?"                 ;; Optional separator
          "[0-9]"                 ;; Check digit
          "\\)")                  ;;
  "Regular expression to match isbn-10 format.

Note: this regex will also match non isbn-10 numbers because
proper matching would require look-ahead regex that Emacs is
missing.  You can however perform post-validation by checking that
the total number of digits is exactly 10.

See also:
https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/ch04s13.html")

(defconst persid-isbn-13-regex
  (concat "\\("                   ;; Group 1 (optional)
          "ISBN:?\\|isbn:?"       ;; Prefix
          " *\\)?"
          "\\("                   ;; Group 2 (mandatory)
          "97[89]"                ;; EAN prefix (978 or 979)
          "[- ]?"                 ;; Optional separator
          "[0-9]\\{1,5\\}"        ;; Registration group
          "[- ]?"                 ;; Optional separator
          "[0-9]\\{1,7\\}"        ;; Registrant
          "[- ]?"                 ;; Optional separator
          "[0-9]\\{1,7\\}"        ;; Publication
          "[- ]?"                 ;; Optional separator
          "[0-9]"                 ;; Check digit
          "\\)")                  ;;
  "Regular expression to match isbn-13 format.

Note: this regex will also match non isbn-13 numbers because
proper matching would require look-ahead regex that Emacs is
missing.  You can however perform post-validation by checking that
the total number of digits is exactly 13.

See also:
https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/ch04s13.html")

(defconst persid-issn-regex
  (concat "\\("               ;; Group 1 (optional)
          "ISSN:?\\|issn:?"   ;; Prefix
          " *\\)?"
          "\\("               ;; Group 2 (mandatory)
          "[0-9]\\{4\\}"      ;; Title number (part 1)
          "-"                 ;; Separator
          "[0-9]\\{3\\}"      ;; Title number (part 2)
          "[0-9xX]"           ;; Check digit
          "\\)")              ;;
  "Regular expression to match issn format.")

(defconst persid-doi-regex
  (concat "\\("                ;; Group 1 (optional)
          "DOI:?\\|doi:?\\|"   ;; Prefix
          "https://doi.org/"   ;;
          " *\\)?"             ;;
          "\\("                ;; Group 2 (mandatory)
          "10\\."              ;; doi namespace
          "[0-9]\\{4,9\\}"     ;; Registrant
          "/"                  ;; Separator
          "[-+._;()/:A-Z0-9]+" ;; Object ID
          "\\)")               ;;
  "Regular expression to match doi format.

Note: This will only match 99% of known doi.  In order to catch
all of them, it would require a lot of complex regex that might
not be worth the effort.  See
https://www.crossref.org/blog/dois-and-matching-regular-expressions")

(defconst persid-pmid-regex
  (concat "\\("               ;; Group 1 (optional)
          "PMID:?\\|pmid:?"   ;; Prefix
          " *\\)?"            ;;
          "\\("               ;; Group 2 (mandatory)
          "[1-9]"             ;;
          "[0-9]\\{0,7\\}"    ;;
          "\\)")              ;;
"Regular expression to match pmid format.")

(defconst persid-pmcid-regex
  (concat "\\("               ;; Group 1 (optional)
          "PCMID:?\\|pcmid:?" ;; Prefix
          " *\\)?"
          "\\("               ;; Group 2 (mandatory)
          "PMC"               ;; Identifier
          "[1-9]"             ;;
          "[0-9]\\{0,7\\}"    ;;
          "\\)")              ;;
"Regular expression to match pmcid format.")

(defconst persid-arxiv-regex
  (concat "\\("             ;; Group 1 (optional)
          "ar[Xx]iv:?"      ;; Prefix
          " *\\)?"
          "\\("             ;; Group 2 (mandatory)
          "[0-9]\\{2\\}"    ;; Year
          "[0-9]\\{2\\}"    ;; Month
          "\\."             ;; Separator
          "[0-9]\\{4,5\\}"  ;; Number
          "\\(v[0-9]+\\)?"  ;; Version
          "\\)")
  "Regular expression to match the new arxiv format.

Note: This will only match arxiv ids registered after 01/04/2007
See https://arxiv.org/help/arxiv_identifier_for_services")

;; Alternatves:
;; http://openlibrary.org/api/books?bibkeys=ISBN:%s&format=json&jscmd=data
;; http://www.librarything.com/services/rest/1.1/?method=librarything.ck.getwork&isbn=%s
;; http://classify.oclc.org/classify2/Classify?isbn=%s&summary=true
;; https://www.googleapis.com/books/v1/volumes?q=%s+isbn&maxResults=1
(defconst persid-isbn-query-url
  "http://openlibrary.org/api/books?bibkeys=ISBN:%s&format=json&jscmd=data"
  "URL to query for an isbn book (json).")


(defconst persid-issn-query-url
  "https://api.openalex.org/venues/issn:%s"
  "URL to query for an issn venue (json).")

(defconst persid-doi-query-url
  "https://api.crossref.org/works/%s/transform/application/x-bibtex"
  "URL to query for a doi work (bibtex).")

(defconst persid-pmid-query-url
  "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=%s&format=json"
  "URL to query for a pmid conversion (json).")

(defconst persid-pmcid-query-url
  "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=%s&format=json"
  "URL to query for a pmcid conversion (json).")

(defconst persid-arxiv-query-url
  "https://ui.adsabs.harvard.edu/abs/arXiv:%s/exportcitation"
  "URL to query for an arxiv article (bibtex).")

(defun persid-isbn-10-check (identifier)
  "Check whether IDENTIFIER is a valid ISBN-10.
Returns a normalized identifier."

  (when (string-match persid-isbn-10-regex identifier)
    (let* ((group1 (match-string 1 identifier))
           (group2 (match-string 2 identifier))
           (normalized (replace-regexp-in-string "[- ]" "" group2)))
      (if (and (= 10 (length normalized))
               (eq (+ (length group1) (length group2)) (length identifier)))
          normalized))))

(defun persid-isbn-13-check (identifier)
  "Check whether IDENTIFIER is a valid ISBN-13.
Returns a normalized identifier."

  (when (string-match persid-isbn-13-regex identifier)
    (let* ((group1 (match-string 1 identifier))
           (group2 (match-string 2 identifier))
           (normalized (replace-regexp-in-string "[- ]" "" group2)))
      (when (and (= 13 (length normalized))
                 (eq (+ (length group1) (length group2)) (length identifier)))
          normalized))))

(defun persid-isbn-check (identifier)
  "Check whether IDENTIFIER is a valid ISBN-10 or ISBN-13.
Returns a normalized identifier."

  (or (persid-isbn-10-check identifier)
      (persid-isbn-13-check identifier)))

(defun persid-issn-check (identifier)
  "Check whether IDENTIFIER is a valid ISSN.
Returns a normalized identifier."

  (when (string-match persid-issn-regex identifier)
    (let* ((group1 (match-string 1 identifier))
           (group2 (match-string 2 identifier)))
      (when (eq (+ (length group1) (length group2)) (length identifier))
        group2))))

(defun persid-doi-check (identifier)
  "Check whether IDENTIFIER is a valid DOI.
Returns a normalized identifier."

  (when (string-match persid-doi-regex identifier)
    (let* ((group1 (match-string 1 identifier))
           (group2 (match-string 2 identifier)))
      (when (eq (+ (length group1) (length group2)) (length identifier))
        group2))))

(defun persid-pmid-check (identifier)
  "Check whether IDENTIFIER is a valid PMID.
Returns a normalized identifier."

  (when (string-match persid-pmid-regex identifier)
    (let* ((group1 (match-string 1 identifier))
           (group2 (match-string 2 identifier)))
      (when (eq (+ (length group1) (length group2)) (length identifier))
        group2))))

(defun persid-pmcid-check (identifier)
  "Check whether IDENTIFIER is a valid PMCID.
Returns a normalized identifier."

  (when (string-match persid-pmcid-regex identifier)
    (let* ((group1 (match-string 1 identifier))
           (group2 (match-string 2 identifier)))
      (when (eq (+ (length group1) (length group2)) (length identifier))
        group2))))

(defun persid-arxiv-check (identifier)
  "Check whether IDENTIFIER is a valid ARXIV.
Returns a normalized identifier."

  (when (string-match persid-arxiv-regex identifier)
    (let* ((group1 (match-string 1 identifier))
           (group2 (match-string 2 identifier)))
      (when (eq (+ (length group1) (length group2)) (length identifier))
        group2))))

(defun persid-identify (identifier)
  "Try to identify the format(s) of IDENTIFIER among known formats."

  (let ((formats '()))
    (dolist (id-format persid-formats)
      (let ((funcheck (intern (format "persid-%s-check" id-format))))
        (when (funcall funcheck identifier)
          (push id-format formats))))
    formats))

(defun persid-bibtex-from-doi (identifier)
  "Retrieve bibtex information from a DOI IDENTIFIER."

  (when-let ((doi (persid-doi-check identifier)))
    (let* ((url (format persid-doi-query-url doi))
           (url (if persid-mail-address
                    (format "%s?mailto=%s" url persid-mail-address)
                    url)))
      (with-temp-buffer
        (url-insert-file-contents url)
        (buffer-string)))))

(defun persid-bibtex-from-pmid (identifier)
  "Retrieve bibtex information from a PMID IDENTIFIER."

  (when-let* ((pmid (persid-pmid-check identifier))
              (doi (persid--pmc/pmid-to-doi pmid))
              (doi (persid-doi-check doi)))
    (persid-bibtex-from-doi doi)))

(defun persid-bibtex-from-pmcid (identifier)
  "Retrieve bibtex information from a PMCID IDENTIFIER."

  (when-let* ((pmcid (persid-pmcid-check identifier))
              (doi (persid--pmc/pmcid-to-doi pmcid))
              (doi (persid-doi-check doi)))
    (persid-bibtex-from-doi doi)))

(defun persid-info-from-issn (identifier)
  "Retrieve information from a ISSN IDENTIFIER."

  (when-let ((issn (persid-issn-check identifier)))
    (persid--openalex/venue persid-issn-query-url issn)))

(defun persid--info-from-openlibrary (openlibrary-url)
  "Retrieve necessary information to create a BibTeX entry from OPENLIBRARY-URL.

Specifically, retrieve the title, authors, year of publication, publishers,
ISBN (either 13 or 10), and URL of the corresponding bibliographic work on the
OpenLibrary website, from the response given by a query to the Book API from
OpenLibrary via OPENLIBRARY-URL, and return the results as an alist.

See more: https://openlibrary.org/dev/docs/api/books"

  (with-temp-buffer
    (url-insert-file-contents openlibrary-url)
    (let-alist (cdar (json-read))
      (list (cons 'title .title)
            (cons 'author (string-join (mapcar #'cdadr .authors) " and "))
            (cons 'year (format-time-string
                         "%Y"
                         (encode-time
                          (decoded-time-set-defaults
                           (parse-time-string .publish_date)))))
            (cons 'publisher (string-join (mapcar #'cdar .publishers) " and "))
            (cons 'isbn (car (append (or .identifiers.isbn_13
                                         .identifiers.isbn_10) nil)))
            (cons 'url .url)))))

(defun persid-bibtex-from-isbn (identifier)
  "Retrieve bibtex information from an ISBN IDENTIFIER."

  (when-let ((isbn (persid-isbn-check identifier)))
    (let* ((url (format persid-isbn-query-url isbn))
           (info (persid--info-from-openlibrary url))
           (bibtex (let-alist info
                     (format "@book{NO_KEY,
title     = {%s},
author    = {%s},
publisher = {%s},
year      = {%s},
isbn      = {%s},
url       = {%s},
}" .title .author .publisher .year .isbn .url))))
      (with-temp-buffer
        (insert bibtex)
        (when persid-isbn-generate-citekey
          (bibtex-mode)
          (unless (eq persid-isbn-generate-citekey 'user)
            (setq-local bibtex-autokey-edit-before-use
                        (eq persid-isbn-generate-citekey 'prompt)))
          (bibtex-clean-entry t))
        (buffer-string)))))

(defun persid--decode-entities (html)
  "Decode HTML entities.
See https://emacs.stackexchange.com/questions/3138"

  (with-temp-buffer
    (save-excursion (insert html))
    (xml-parse-string)))

(defun persid-bibtex-from-arxiv (identifier)
  "Retrieve bibtex information from an ARXIV IDENTIFIER."

  (when-let* ((arxiv (persid-arxiv-check identifier))
              (url (format persid-arxiv-query-url arxiv)))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let ((text (delete-and-extract-region (point-min) (point-max))))
        (insert (url-unhex-string text)))
      (goto-char (point-min))
      (when-let* ((beg (save-excursion (search-forward "@ARTICLE")))
                  (end (save-excursion (search-forward "</textarea>"))))
        (persid--decode-entities
         (concat "@Article"
                 (buffer-substring beg (- end (length "</textarea>")))))))))

(defun persid-bibtex-from (identifier)
  "Retrieve bibtex information from given IDENTIFIER."

  (let ((formats (persid-identify identifier))
        (bibtex))
    (catch 'found
      (dolist (format formats)
        (let ((bibfun (intern (format "persid-bibtex-from-%s" format))))
          (when (functionp bibfun)
            (setq bibtex (funcall bibfun identifier))
            (throw 'found nil)))))
    bibtex))

;;;###autoload
(defun persid-insert-bibtex (identifier)
  "Insert bibtex information from given IDENTIFIER."

  (interactive
   (list (read-from-minibuffer "Identifier: " nil nil nil 'persid-history)))
  (insert (persid-bibtex-from identifier)))

(defun persid--openalex/normalize-name (name)
  "Normalize NAME.
Convert from \"firstname(s) surname\" to \"surname, firstname(s)\"."

  (let ((parts (split-string name " ")))
    (concat (car (last parts)) ", "
            (mapconcat #'identity (butlast parts) " "))))

(defun persid--openalex/venue (url identifier &optional email)
  "Return the name of venue identified by IDENTIFIER using openalex.
If an EMAIL is provided, it is appended to the query such as to
access the (faster) polite-pool.  URL should be a valid query
URL."

  (let* ((email (or email persid-mail-address))
         (url (format url identifier))
         (url (if email
                  (format "%s?mailto=%s" url email)
                url))
         (info (with-temp-buffer (url-insert-file-contents url)
                                 (json-parse-buffer :object-type 'plist)))
         (title (plist-get info :display_name))
         (publisher (plist-get info :publisher)))
    (format "%s (%s)" title publisher)))

(defun persid--pmc/pmid-to-doi (identifier &optional email)
  "Convert a PMID IDENTIFIER into a DOI identifier.
If an EMAIL is provided, it is appended to the query such as to
access the (faster) polite-pool."

  ;; See https://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
  (let* ((email (or email persid-mail-address))
         (url (concat (format persid-pmid-query-url identifier)
                      (when email (concat "&mail=" email)))))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let ((json (json-parse-buffer :object-type 'plist)))
        (plist-get (elt (plist-get json :records) 0) :doi)))))

(defun persid--pmc/pmcid-to-doi (identifier &optional email)
  "Convert a PMID IDENTIFIER into a DOI identifier.
If an EMAIL is provided, it is appended to the query such as to
access the (faster) polite-pool."

  (persid--pmc/pmid-to-doi identifier email))


(provide 'persid)
;;; persid.el ends here
