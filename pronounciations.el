(emacspeak-pronounce-set-dictionary 'text-mode
 '((" -[0-9]+\\.?[0-9]+%?" re-search-forward . #[257 "\300\301\302\"P\207" [" minus " substring 2] 5 "

(fn NUMBER)"])))
(emacspeak-pronounce-set-dictionary 'comint-mode
 '(("[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}" re-search-forward . emacspeak-pronounce-uuid) ("[0-9a-f]\\{40\\}" re-search-forward . emacspeak-pronounce-sha-checksum)))
(emacspeak-pronounce-set-dictionary 'conf-space-mode
 '(("[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}" re-search-forward . emacspeak-pronounce-uuid)))
(emacspeak-pronounce-set-dictionary 'conf-unix-mode
 '(("[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}" re-search-forward . emacspeak-pronounce-uuid)))
(emacspeak-pronounce-set-dictionary 'conf-mode
 '(("[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}" re-search-forward . emacspeak-pronounce-uuid)))
(emacspeak-pronounce-set-dictionary 'xml-mode
 '(("http://search.yahoo.com/mrss/" . "media") ("http://purl.org/dc/elements/1.1/" . "DC") ("http://www.w3.org/1999/xhtml" . " xhtml ") ("http://www.w3.org/2001/XMLSchema" . " XSD ") ("http://www.w3.org/2001/XMLSchema-instance" . " XSI ") ("http://www.w3.org/2001/vxml" . " vxml ") ("http://www.w3.org/2001/xml-events" . " XEvents ") ("http://www.w3.org/2002/xforms" . " XForms ") ("http://www.w3.org/1999/XSL/Transform" . " XSLT ") ("http://www.w3.org/2003/XInclude" . "XInclude") ("http://www.w3.org/2002/06/xhtml2" . " xhtml2 ") ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" . "RDF Syntax") ("http://www.w3.org/2005/Atom" . " atom ")))
(emacspeak-pronounce-set-dictionary 'magit-mode
 '(("[0-9a-f]\\{40\\}" re-search-forward . emacspeak-pronounce-sha-checksum)))
(emacspeak-pronounce-set-dictionary 'eww-mode
 '(("[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\(\\.[0-9]\\{3\\}\\)?\\([zZ]\\|\\([+-][0-9]\\{2\\}:[0-9]\\{2\\}\\)\\)" re-search-forward . emacspeak-speak-decode-rfc-3339-datetime) ("http://search.yahoo.com/mrss/" . "media") ("http://purl.org/dc/elements/1.1/" . "DC") ("http://www.w3.org/1999/xhtml" . " xhtml ") ("http://www.w3.org/2001/XMLSchema" . " XSD ") ("http://www.w3.org/2001/XMLSchema-instance" . " XSI ") ("http://www.w3.org/2001/vxml" . " vxml ") ("http://www.w3.org/2001/xml-events" . " XEvents ") ("http://www.w3.org/2002/xforms" . " XForms ") ("http://www.w3.org/1999/XSL/Transform" . " XSLT ") ("http://www.w3.org/2003/XInclude" . "XInclude") ("http://www.w3.org/2002/06/xhtml2" . " xhtml2 ") ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" . "RDF Syntax") ("http://www.w3.org/2005/Atom" . " atom ")))
(emacspeak-pronounce-set-dictionary 'latex-mode
				    '(("^" . "caret")))

(defun emacspeak-pronounce-latex-integral (str)
  (let* ((prefix (concat " integral from "))
	(range (substring str 4))
	(first-bracket (s-prefix? "{" range))
	(last-bracket (s-suffix? "}" range)))
    (concat prefix
	    (if first-bracket (substring range 1 4)
	      (substring range 0 1))
	    " to "
	    (if last-bracket (substring range -4 -1)
	      (substring range -1))
	    " ")))

(emacspeak-pronounce-set-dictionary 'org-mode
				    '(				      ("{\\delta}" . " Dirac delta ")

				      ("\\int_\\([a-zA-Z0-9]\\|\\({[a-zA-Z0-9]_[a-zA-Z0-9]}\\)\\)\\^\\([a-zA-Z0-9]\\|\\({[a-zA-Z0-9]_[a-zA-Z0-9]}\\)\\)" re-search-forward . emacspeak-pronounce-latex-integral)
				      ("[a-z]([a-zA-Z])" re-search-forward . (lambda(str) (concat (substring str 0 1) " of " (substring str 2 -1) " ")))
				      ("\\int " . " integral ")
				      ("_{i=1}^n" . " from i=1 to n ")
				      ("{\\\\bf\s[a-zA-Z]}" re-search-forward . (lambda (str) (concat " " (substring str 5 6) " ")))
				      ("[A-Z]/[A-Z]" re-search-forward . (lambda (str) (concat " " (replace-regexp-in-string "/" " over " str) " ")))
				      ("{[0-9]+}" re-search-forward . (lambda (str) (concat " " (substring str 1 -1) " ")))
				      ("{-x}" . " negative x ")
				      ("\\frac{1}" . " 1over ")
				      				      ("\\frac{2}" . " 1over ")
				      ("\\xi" . " ksee ")
				      ("\\frac{1}{2}" . " 1half ")
				      ("^3" . " cubed ")
				      ("{-1/2}" . " negative 1half ")
				      ("-1/2" . " negative 1half ")
				      ("{1/2}" . " 1half ")
				      ("1/2" . " 1half ")
				      ("{-1}" . " negative 1 ")
				      ("\\ln" . " natural log ")
				      ("\left" . " left")
				      ("\\tau" . " tou ")("}{" . " over ")("\\frac". " fraction ")("\\;" . " dot ")("\\geq" . " greater than or equal to ") ("\\leq" . " less than or equal to ") ("\\ge" . " greater than ") ("\\le" . " less than ") ("&=" . " equal to ")("\\cdot" . " dot ") ("\\sqrt" . " square root")("\\gamma" . " gamma") ("^2" . " squared ") ("{AB}" . "{A B}") ("_a" . " sub ay ") ("\\mod" . " modulo ") ("$" . " ") ("_" . " sub ") ("^" . " to the power of ") ("{". " start ") ("}" . " end ") ("\\" . " ")))
