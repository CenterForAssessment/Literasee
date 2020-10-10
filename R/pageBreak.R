
###  Place page breaks for HTML and LaTeX

`pageBreak` <- function() {
	###  NOTE:  ALL 'cat()' text must be flush left in R script!
	cat("
<!-- HTML_Start -->
<!-- pageBreak -->
<div class='breakboth' ></div>
<!-- LaTeX_Start pageBreak
\\pagebreak
LaTeX_End -->\n"
	)
}
