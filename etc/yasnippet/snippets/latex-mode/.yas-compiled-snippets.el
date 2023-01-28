;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
		     '(("verb" "\\begin{verbatim}\n$0\n\\end{verbatim}\n" "\\begin{verbatim} ... \\end{verbatim}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/verb.yasnippet" nil nil)
		       (";." "\\\\vdots " "vdots" 'auto
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/vdots" nil nil)
		       ("use" "\\usepackage[$2]{$1}$0" "\\usepackage" nil
			("misc")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/use.yasnippet" nil nil)
		       ("url" "\\url{${1:$$(yas/choose-value '(\"http\" \"ftp\"))}://${2:address}}$0" "\\url" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/url.yasnippet" nil nil)
		       ("tt" "{\\tt $1}$0" "{\\tt ...}" nil
			("font")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/tt.yasnippet" nil nil)
		       ("td" "`(backward-delete-char 1)`^{$1}" "To the power"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/to_the_power" nil nil)
		       ("->" "\\to" "to"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/to" nil nil)
		       ("xx" "\\\\times" "times"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/times" nil nil)
		       ("tikzplot" "\\begin{figure}[$1]\n	\\centering\n	\\begin{tikzpicture}\n		\\begin{axis}[\n			xmin= ${2:-10}, xmax= ${3:10},\n			ymin= ${4:-10}, ymax = ${5:10},\n			axis lines = middle,\n		]\n			\\addplot[domain=$2:$3, samples=${6:100}]{$7};\n		\\end{axis}\n	\\end{tikzpicture}\n	\\caption{$8}\n	\\label{${9:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n\\end{figure}\n" "Tikz Plot" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/tikz_plot" nil nil)
		       ("thus" "\\\\therefore " "therefore"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/therefore" nil nil)
		       ("tan" "\\\\tan" "tan"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/tan" nil nil)
		       ("table" "\\begin{table}[htbp]\n  \\centering\n  \\begin{tabular}{${3:format}}\n    $0\n  \\end{tabular}\n  \\caption{${1:caption}}\n  \\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n\\end{table}\n" "\\begin{table} ... \\end{table}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/table.yasnippet" nil nil)
		       ("sup" "\\\\sup_{$1 \\\\in $2} $0" "supremum"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/supremum" nil nil)
		       ("sum" "\\sum_{$1}^{$2}$0" "\\sum_{n}^{}" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/sum.yasnippet" nil nil)
		       ("sub*" "\\subsection*{${1:name}}\n$0" "\\subsection*" nil
			("sections")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/substar.yasnippet" nil nil)
		       ("cc" "\\subset" "subset"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/subset" nil nil)
		       ("subfloat" "\\begin{figure}[ht]\n  \\centering\n  \\subfloat[${6:caption}]{\\includegraphics[$3]{figures/${1:path.png}}}${5:~}\n  \\subfloat[${7:caption}]{\\includegraphics[$4]{figures/${2:path.png}}}\n  \\caption{\\label{fig:${8:label}} $0}\n\\end{figure}\n" "subfloat" nil nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/subfloat" nil nil)
		       ("subfig" "\\subfigure[${1:caption}]{\n  \\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n  $0\n}\n" "\\subfigure" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/subfig.yasnippet" nil nil)
		       ("sub" "\\subsection{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\subsection" nil
			("sections")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/sub.yasnippet" nil nil)
		       ("star" "\\\\star" "star"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/star" nil nil)
		       ("stackrel" "\\\\stackrel{$1}{$0}" "stackrel"
			(and
			 (texmathp)
			 'auto)
			nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/stackrel" nil nil)
		       ("ssub*" "\\subsubsection*{${1:name}}\n$0" "\\subsubsection*" nil
			("sections")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/ssubstar.yasnippet" nil nil)
		       ("ssub" "\\subsubsection{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\subsubsection" nil
			("sections")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/ssub.yasnippet" nil nil)
		       ("sin" "\\\\sin" "sin"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/sin" nil nil)
		       ("~~" "\\\\sim" "similar"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/similar" nil nil)
		       ("set" "\\\\left\\\\{ $1 \\\\right\\\\}$0" "set"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/set" nil nil)
		       ("sec*" "\\section*{${1:name}}\n$0" "\\section*" nil
			("sections")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/secstar.yasnippet" nil nil)
		       ("sec" "\\section{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\section" nil
			("sections")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/sec.yasnippet" nil nil)
		       ("sec" "\\\\sec" "sec"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/sec" nil nil)
		       ("sc" "{\\scshape $1}$0" "{\\sc ...}" nil
			("font")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/sc.yasnippet" nil nil)
		       ("rvec" "\\\\begin{pmatrix} ${1:x}_{${2:1}} & ${3:\\\\dots} & $1_{${4:n}} \\\\end{pmatrix}" "row vector" 'auto
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/row_vector" nil nil)
		       ("ref" "`(unless yas/modified-p (consult-reftex-insert-reference nil 'dont-insert))`" "\\ref" nil
			("references")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/ref.yasnippet" nil nil)
		       ("qq" "\\\\quad " "quad"
			(and
			 (texmathp)
			 'auto)
			nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/quad" nil nil)
		       ("prod" "\\prod_{$1}^{$2}$0\n" "\\prod_{n}^{}" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/prod.yasnippet" nil nil)
		       ("perp" "\\\\perp" "perp"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/perp" nil nil)
		       ("part" "\\\\frac{\\\\partial $1}{\\\\partial ${2:x}} $0" "partial derivative"
			(texmathp)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/partial_derivative" nil nil)
		       ("par" "\\paragraph{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\paragraph" nil
			("sections")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/par.yasnippet" nil nil)
		       ("ox" "\\otimes" "otimes"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/otimes" nil nil)
		       ("o+" "\\oplus" "oplus"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/oplus" nil nil)
		       ("nEE" "\\\\nexists" "notexists"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/notexists" nil nil)
		       ("notin" "\\\\not\\\\in" "not in"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/not_in" nil nil)
		       ("neq" "\\\\ne" "not equal"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/not_equal" nil nil)
		       ("norm" "\\|$1\\|$0" "norm"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/norm" nil nil)
		       (";;" "\\\\\\\\\n`(save-excursion (previous-line)(make-string (current-indentation) ?\\s))`$0" "newline"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/newline" nil nil)
		       ("minipage" "\\begin{minipage}[${1:htbp}]{${2:1.0}${3:\\linewidth}}\n  $0\n\\end{minipage}" "\\begin{minipage}[position][width] ... \\end{minipage}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/minipage.yasnippet" nil nil)
		       ("matrix" "\\begin{${1:$$(yas/choose-value '(\"pmatrix\" \"bmatrix\" \"Bmatrix\" \"vmatrix\" \"Vmatrix\" \"smallmatrix\"))}} $0 \\end{$1}" "\\begin{matrix} ... \\end{}" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/matrix.yasnippet" nil nil)
		       ("mtemplate" "\\documentclass{article}\n\n\\usepackage{amsmath}\n\\usepackage{amssymb}\n\\usepackage{mathtools}\n\\usepackage{amsfonts}\n\\usepackage[utf8]{inputenc}\n\\usepackage{parskip}\n\\usepackage{sectsty}\n\n\\author{`user-full-name`\\vspace{-2ex}}\n\\title{\\vspace{-3.0cm}${1:Title$(capitalize yas-text)}\\vspace{-2ex}}\n\n\\sectionfont{\\fontsize{12}{15}\\underline\\selectfont}\n\n\\newcommand{\\R}{\\mathbb{R}}\n\\newcommand{\\Z}{𝚭}\n\\newcommand{\\Q}{\\mathbb{Q}}\n\n\\begin{document}\n\\maketitle\n\n$0\n\n\\end{document}" "Math template" nil
			("skeleton")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/math_template" nil nil)
		       ("log" "\\\\log" "log"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/log" nil nil)
		       ("ln" "\\\\ln" "ln"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/ln" nil nil)
		       ("lst" "\\begin{lstlisting}[float,label=lst:${1:label},caption=nextHopInfo: ${2:caption}]\n$0\n\\end{lstlisting}" "listing" nil nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/listing" nil nil)
		       ("limsup" "\\\\limsup_{${1:n} \\\\to ${2:\\\\infty}} $0" "limsup"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/limsup" nil nil)
		       ("lim" "\\\\lim_{${1:n} \\\\to ${2:\\\\infty}} $0" "limit" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/limit" nil nil)
		       ("limit" "\\lim_{$1}$0\n" "\\lim_{n}" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/lim.yasnippet" nil nil)
		       ("letter" "\\documentclass{letter}\n\\signature{${1:Foo Bar}}\n\\address{${2:Address line 1 \\\\\\\\ \nAddress line 2 \\\\\\\\\nAddress line 3}}\n\\begin{document}\n \n\\begin{letter}\n{${3:Recipient's address}}\n\n\\opening{Dear ${4:Sir}:}\n\n$0\n \n\\closing{Yours Sincerely,}\n \n\\end{letter}\n \n\\end{document}\n\n" "\\documentclass{letter} ..." nil
			("skeleton")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/letter.yasnippet" nil nil)
		       ("<<" "\\\\ll" "lesser lesser"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/lesser_lesser" nil nil)
		       ("leq" "\\\\le$0" "less or equal"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/less_or_equal" nil nil)
		       ("<->" "\\\\leftrightarrow\n" "leftrightarrow"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/leftrightarrow" nil nil)
		       ("lab" "\\label{${1:label$(unless yas/modified-p (reftex-label nil 'dont-insert))}}$0\n" "\\label" nil
			("references")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/label.yasnippet" nil nil)
		       ("item" "\\begin{itemize}\n\\item $0\n\\end{itemize}\n" "\\begin{itemize} ... \\end{itemize}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/item.yasnippet" nil nil)
		       ("itd" "\\item[${1:label}] $0" "\\item[] (description)" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/itd.yasnippet" nil nil)
		       ("it" "\\item $0" "\\item" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/it.yasnippet" nil nil)
		       ("inv" "`(delete-backward-char 1)`^{-1}" "inverse"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/inverse" nil nil)
		       ("integ" "\\\\${1:$$(yas/choose-value '(\"int\" \"oint\" \"iint\" \"iiint\" \"iiiint\" \"idotsint\"))}$0" "Indefinite integral (all)"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/int.yasnippet" nil nil)
		       ("mk" "`(if (eq major-mode 'org-mode) \"\\\\(\" \"\\\\( \")`$0`(if (eq major-mode 'org-mode) \" \\\\)\" \" \\\\)\")`" "Inline Math"
			(and
			 (not
			  (texmathp))
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/inline_math" nil nil)
		       ("infin" "\\\\inf_{$1 \\\\in $2} $0" "infimum"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/infimum" nil nil)
		       ("ig" "\\includegraphics${1:[$2]}{$0}" "includegraphics" nil nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/includegraphics" nil nil)
		       ("inn" "\\\\in" "in"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/in" nil nil)
		       ("=>" "\\implies$0" "implies"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/implies" nil nil)
		       ("<=" "\\\\impliedby $0" "implied by"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/implied_by" nil nil)
		       ("iff" "\\iff" "if and only if"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/if_and_only_if" nil nil)
		       ("href" "\\href{${1:url}}{${2:text}}$0" "\\href{url}{text}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/href.yasnippet" nil nil)
		       ("hat" "`(condition-case nil (progn (backward-sexp) (kill-sexp) (delete-char 1)) (error (setq argument 'nil)))`\\\\hat{`(if argument (current-kill 0))`}$0" "hat"
			(and
			 (texmathp)
			 'auto)
			("math")
			((argument 't))
			"/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/hat" nil nil)
		       ("geq" "\\\\ge" "greater or equal"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/greater_or_equal" nil nil)
		       (">>" "\\\\gg" "greater greater"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/greater_greater" nil nil)
		       ("graphics" "\\includegraphics[width=${1:\\linewidth}]{${2:file}}" "\\includegraphics" nil nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/graphics.yasnippet" nil nil)
		       ("newgls" "\\newglossaryentry{$1}{name={$1},\n  description={$2.}}\n" "\\newglossaryentry{...}{...}" nil
			("misc")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/gls.yasnippet" nil nil)
		       ("frame" "\\begin{frame}{${1:Frame Title$(capitalize yas-text)}}\n$0\n\\end{frame}\n" "\\begin{frame} ... \\end{frame}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/frame.yasnippet" nil nil)
		       ("/" "`(condition-case nil (save-excursion (backward-sexp) (kill-sexp) (delete-char 1)) (error (setq numerator 'nil)))`\\\\frac{`(if numerator (yank))`}{$1}$0" "Fraction slash"
			(texmathp)
			("math")
			((numerator 't))
			"/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/fraction_slash" nil nil)
		       ("//" "`(condition-case nil (save-excursion (backward-sexp) (kill-sexp) (delete-char 1)) (error (setq numerator 'nil)))`\\\\frac{`(if numerator (current-kill 0))`}{$1}$0" "Fraction auto"
			(and
			 (texmathp)
			 'auto)
			("math")
			((numerator 't))
			"/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/fraction_auto" nil nil)
		       ("frac" "\\frac{${1:numerator}}{${2:denominator}}$0" "\\frac{numerator}{denominator}" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/frac.yasnippet" nil nil)
		       ("VV" "\\\\forall$0" "forall"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/forall" nil nil)
		       ("floor" "\\\\left\\\\lfloor $1 \\\\right\\\\rfloor $0" "floor function"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/floor_function" nil nil)
		       ("fig" "\\begin{figure}[htbp]\n  \\centering\n  $0\n  \\caption{${1:caption}}\n  \\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n\\end{figure}\n" "\\begin{figure} ... \\end{figure}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/fig.yasnippet" nil nil)
		       ("exp" "\\\\exp" "exp"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/exp" nil nil)
		       ("EE" "\\\\exists" "Exists"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/exists" nil nil)
		       ("eqv" "\\\\equiv$0" "equiv"
			(and
			 (texmathp)
			 'auto)
			nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/equiv" nil nil)
		       ("eqs" "\\begin{${1:$$(yas/choose-value '(\"align\" \"align*\" \"multline\" \"gather\" \"subequations\"))}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0\n\\end{$1}\n" "\\begin{align} ... \\end{align}" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/eqs.yasnippet" nil nil)
		       ("eq" "\\begin{equation}\n\\label{${1:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0\n\\end{equation}\n" "\\begin{equation} ... \\end{equation}" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/eq.yasnippet" nil nil)
		       ("enum" "\\begin{enumerate}\n\\item $0\n\\end{enumerate}\n" "\\begin{enumerate} ... \\end{enumerate}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/enum.yasnippet" nil nil)
		       ("em" "{\\em $1}$0" "{\\em ...}" nil
			("font")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/em.yasnippet" nil nil)
		       ("..." "\\\\dots$0" "dots"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/dots" nil nil)
		       ("doc" "\\documentclass[$2]{${1:$$(yas/choose-value '(\"article\" \"report\" \"book\" \"letter\"))}}\n\n\\begin{document}\n$0\n\\end{document}\n" "\\documentclass" nil nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/doc.yasnippet" nil nil)
		       ("dm" "\\[ `(save-excursion (previous-line)(make-string (current-indentation) ?\\s))`$0 \\]\n" "Display Math"
			(and
			 (not
			  (texmathp))
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/display_math" nil nil)
		       ("diag3" "\\\\begin{bmatrix}\n${1:\\\\ddots}  &  & \\\\\\\\\n & ${2:\\\\ddots}  & \\\\\\\\\n &  & ${3:\\\\ddots}\n \\\\end{bmatrix}" "Diagonal bmatrix" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/diagonal_bmatrix" nil nil)
		       ("desc" "\\begin{description}\n\\item[${1:label}] $0\n\\end{description}\n" "\\begin{description} ... \\end{description}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/desc.yasnippet" nil nil)
		       ("dint" "\\\\int_{${1:-\\\\infty}}^{${2:\\\\infty}}$0" "definite integral"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/definite_integral" nil nil)
		       ("ddot" "`(condition-case nil (progn (backward-sexp) (kill-sexp) (delete-char 1)) (error (setq argument 'nil)))`\\\\ddot{`(if argument (current-kill 0))`}$0" "ddot"
			(and
			 (texmathp)
			 'auto)
			nil
			((argument 't))
			"/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/ddots" nil nil)
		       ("ddt" "\\\\frac{\\\\mathrm{d} $1}{\\\\mathrm{d} ${2:t}}$0" "d-by-dt"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/d-by-dt" nil nil)
		       ("csc" "\\\\csc" "csc"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/csc" nil nil)
		       ("cot" "\\\\cot" "cot"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/cot" nil nil)
		       ("cos" "\\\\cos" "cos"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/cos" nil nil)
		       ("coprod" "\\coprod_{$1}^{$2}$0\n" "\\coprod_{n}^{}" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/coprod.yasnippet" nil nil)
		       ("conj" "`(delete-backward-char 1)`^{\\\\star}$0" "complex conjugate"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/complex_conjugate" nil nil)
		       ("cols" "\\begin{columns}\n  \\begin{column}{.${1:5}\\textwidth}\n  $0\n  \\end{column}\n\n  \\begin{column}{.${2:5}\\textwidth}\n\n  \\end{column}\n\\end{columns}" "columns" nil nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/columns" nil nil)
		       ("cvec" "\\\\begin{pmatrix} ${1:x}_{${2:1}}\\\\\\\\ ${3:\\\\vdots}\\\\\\\\ $1_{${4:n}} \\\\end{pmatrix}" "column vector" 'auto
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/column_vector" nil nil)
		       (",," "& $0" "column-separator"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/column-separator" nil nil)
		       ("code" "\\begin{lstlisting}${1:[language=${2:Matlab}]}\n$0\n\\end{lstlisting}" "code" nil nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/code" nil nil)
		       ("cite" "`(unless yas-modified-p (call-interactively 'citar-insert-citation))`" "\\cite" nil
			("references")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/cite.yasnippet" nil nil)
		       ("cha*" "\\chapter*{${1:name}}\n$0" "\\chapter*" nil
			("sections")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/chastar.yasnippet" nil nil)
		       ("cha" "\\chapter{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\chapter" nil
			("sections")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/cha.yasnippet" nil nil)
		       ("ceil" "\\\\left\\\\lceil $1 \\\\right\\\\rceil $0" "ceiling function"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/ceiling_function" nil nil)
		       ("case" "\\\\begin{cases}\n  $1\n\\\\end{cases}$0" "cases"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/cases" nil nil)
		       ("case" "\\begin{cases}\n$0 \\\\\\\\\n\\end{cases}\n" "\\begin{cases} ... \\end{cases}" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/case.yasnippet" nil nil)
		       ("bf" "{\\bf $1}$0" "{\\bf ... }" nil
			("font")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/bold.yasnippet" nil nil)
		       ("mat3" "\\\\begin{bmatrix}\n${1:A} & ${2:$1} & ${3:$1} \\\\\\\\\n${4:$1} & ${5:$1} & ${6:$1} \\\\\\\\\n${7:$1} & ${8:$1} & ${9:$1} \n\\\\end{bmatrix}" "bmatrix (3 x 3)" nil nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/bmatrix_(3_x_3)" nil nil)
		       ("mat2" "\\\\begin{bmatrix}\n${1:A} & ${2:$1} \\\\\\\\\n${3:$1} & ${4:$1}\n \\\\end{bmatrix}" "bmatrix (2 x 2)" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/bmatrix_(2_x_2)" nil nil)
		       ("block" "\\begin{${1:$$(yas/choose-value '(\"block\" \"exampleblock\" \"alertblock\"))}}{${2:Block Title}}\n\n\\end{$1}\n" "\\begin{*block} ... \\end{*block}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/block.yasnippet" nil nil)
		       ("binom" "\\binom{${1:n}}{${2:k}}" "\\binom{n}{k}" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/binom.yasnippet" nil nil)
		       ("bigop" "\\\\big${1:$$(yas/choose-value '(\"oplus\" \"otimes\" \"odot\" \"cup\" \"cap\" \"uplus\" \"sqcup\" \"vee\" \"wedge\"))}_{$2}^{$3}$0\n" "\\bigop_{n}^{}" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/bigop.yasnippet" nil nil)
		       ("big" "\\\\${1:$$(yas/choose-value '(\"big\" \"Big\" \"bigg\" \"Bigg\"))}l( $0  \\\\$1r)" "\\bigl( ... \\bigr)" nil
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/big.yasnippet" nil nil)
		       ("bib" "\\bibliographystyle{plain}\n\\bibliography{$1}$0" "\\bibliography" nil
			("misc")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/bib.yasnippet" nil nil)
		       ("begin" "\\begin{${1:$$(yas/choose-value (mapcar 'car (LaTeX-environment-list)))}}\n$0\n\\end{$1}" "\\begin{environment} ... \\end{environment}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/begin.yasnippet" nil nil)
		       ("beamer" "\\documentclass[xcolor=dvipsnames]{beamer}\n\n\\usepackage{graphicx,subfigure,url}\n\n% example themes\n\\usetheme{Frankfurt}\n\\usecolortheme{seahorse}\n\\usecolortheme{rose}\n\n% put page numbers\n% \\setbeamertemplate{footline}[frame number]{}\n% remove navigation symbols\n% \\setbeamertemplate{navigation symbols}{}\n\n\\title{${1:Presentation Title}}\n\\author{${2:Author Name}}\n\n\\begin{document}\n	\n\\frame[plain]{\\titlepage}\n	\n\\begin{frame}[plain]{Outline}\n	\\tableofcontents\n\\end{frame}\n	\n\\section{${3:Example Section}}\n\\begin{frame}{${4:Frame Title}}\n\n\\end{frame}\n\n\\end{document}\n" "\\documentclass{beamer} ..." nil
			("skeleton")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/beamer.yasnippet" nil nil)
		       ("basict" "\\documentclass[11pt]{article}\n\n\\usepackage{graphicx,amsmath,amssymb,subfigure,url,xspace}\n\n\\title{${1:title}}\n\\author{${2:Author Name}}\n\n\\begin{document}\n\\maketitle\n\n$0\n\n\\end{document}\n" "\\documentclass{article} ..." nil
			("skeleton")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/basict" nil nil)
		       ("template" "\\input{`my-preamble-file`}\n% \\usepackage{hyperref}\n% \\hypersetup{\n%     colorlinks,\n%     citecolor=cyan,\n%     filecolor=black,\n%     linkcolor=blue,\n%     urlcolor=black}\n\n\\author{`user-full-name`\\vspace{-2ex}}\n\\title{\\vspace{-3.0cm}${1:Title$(capitalize yas-text)}\\vspace{-2ex}}\n${2:\\date{${3:\\today}}}\n\n\\begin{document}\n\n\\begingroup\n\\let\\center\\flushleft\n\\let\\endcenter\\endflushleft\n\\maketitle\n\\endgroup\n\n% \\tableofcontents\n\n$0\n\\end{document}" "Basic template" nil
			("skeleton")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/basic_template" nil nil)
		       ("bar" "`(condition-case nil (progn (backward-sexp) (kill-sexp) (delete-char 1)) (error (setq argument 'nil)))`\\\\bar{`(if argument (current-kill 0))`}$0" "bar"
			(and
			 (texmathp)
			 'auto)
			("math")
			((argument 't))
			"/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/bar" nil nil)
		       ("article" "\\documentclass[11pt]{article}\n\n\\usepackage{graphicx,amsmath,amssymb,subfigure,url,xspace}\n\\newcommand{\\eg}{e.g.,\\xspace}\n\\newcommand{\\bigeg}{E.g.,\\xspace}\n\\newcommand{\\etal}{\\textit{et~al.\\xspace}}\n\\newcommand{\\etc}{etc.\\@\\xspace}\n\\newcommand{\\ie}{i.e.,\\xspace}\n\\newcommand{\\bigie}{I.e.,\\xspace}\n\n\\title{${1:title}}\n\\author{${2:Author Name}}\n\n\\begin{document}\n\\maketitle\n\n\n\\bibliographystyle{${3:plain}}\n\\bibliography{${4:literature.bib}}\n\n\\end{document}\n" "\\documentclass{article} ..." nil
			("skeleton")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/article.yasnippet" nil nil)
		       ("arr" "\\begin{array}{$1}\n  $0\n\\end{array}\n" "\\begin{array} ... \\end{array}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/array.yasnippet" nil nil)
		       ("arctan" "\\\\arctan" "arctan"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/arctan" nil nil)
		       ("arcsin" "\\\\arcsin" "arcsin"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/arcsin" nil nil)
		       ("arcsec" "\\\\arcsec" "arcsec"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/arcsec" nil nil)
		       ("arccsc" "\\\\arccsc" "arccsc"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/arccsc" nil nil)
		       ("arccot" "\\\\arccot" "arccot"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/arccot" nil nil)
		       ("arccos" "\\\\arccos" "arccos"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/arccos" nil nil)
		       ("align*" "\\begin{align*}\n  $0\n\\end{align*}" "\\begin{align*} ... \\end{align*}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/alignstar.yasnippet" nil nil)
		       ("align" "\\begin{align}\n  $0\n\\end{align}" "\\begin{align} ... \\end{align}" nil
			("environments")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/align.yasnippet" nil nil)
		       ("adj" "`(delete-backward-char 1)`^{\\\\dagger}$0" "adjoint"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/adjoint" nil nil)
		       ("abs" "\\begin{abstract}\n$0\n\\end{abstract}" "\\abstract" nil
			("sections")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/abstract.yasnippet" nil nil)
		       ("cb" "`(delete-backward-char 1)`^3$0" "^3"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/^3" nil nil)
		       ("sr" "`(delete-backward-char 1)`^2$0" "^2"
			(and
			 (texmathp)
			 'auto)
			("math")
			nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/^2" nil nil)
		       ("vec2" "\\\\begin{bmatrix} ${1:x}_{${2:1}} \\\\\\\\ ${3:$1}_{${4:2}} \\\\end{bmatrix}" "2-vector" nil nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/2-vector" nil nil)
		       ("inf" "\\infty" "infinity" nil nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/latex-mode/+new-snippet+" nil nil)))


;;; Do not edit! File generated at Sun Jan 22 13:17:00 2023
