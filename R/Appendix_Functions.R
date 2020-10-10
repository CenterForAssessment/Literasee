#####
###  startAppendix - reset counters and page/table/figure naming conventions
#####

`startAppendix` <- function(appendix.number=1L, use.alpha=TRUE, set.counters=TRUE, table.name=NULL, figure.name=NULL) {
  if (set.counters)  setCounters()
  options(table_counter=FALSE)

  if (is.null(table.name) & use.alpha) { # Default
    options(table_counter_str = paste0("**Table ", LETTERS[appendix.number], "%s:**")) # "**Table A%s:**"
  } else {
    if (!is.null(table.name)) options(table_counter_str = table.name)
  }

  if (is.null(figure.name) & use.alpha) { # Default
    options(fig_caption_no_sprintf = paste0("**Figure ", LETTERS[appendix.number], "%d:** %s")) # "**Figure A%d:** %s"
  } else {
    if (!is.null(figure.name)) options(fig_caption_no_sprintf = figure.name)
  }

  ###  NOTE:  ALL 'cat()' text must be flush left in R script!

  cat("
<!-- HTML_Start -->
<!-- startAppendix -->
<div id=\"appendices\" class=\"section level1 appendix-reset\">

<!-- LaTeX_Start startAppendix
\\renewcommand{\\thesection}{", LETTERS[appendix.number], "}
\\titleformat{\\section}{\\normalfont\\Large\\bfseries}{}{0pt}{}
\\pagenumbering{arabic}
\\renewcommand*{\\thepage}{\\thesection\\arabic{page}}
LaTeX_End -->\n", sep=""
	)
}

###   LaTeX has to be all on one line... Some explanation of the above:
###   https://tex.stackexchange.com/a/136541
# \titleformat{\section}
#   {\normalfont\Large\bfseries}   % The style of the section title
#   {}                             % a prefix
#   {0pt}                          % How much space exists between the prefix and the title
#   {}                             % How the section is represented.  'Appendix \thesection:\quad' would give 'Appendix A:  My Title'


#####
###  endAppendix - restart counters for session if re-rendering
#####

`endAppendix` <- function() {
  cat("
<!-- HTML_Start -->
<!-- endAppendix -->
</div>

<!-- LaTeX_Start endAppendix
LaTeX_End -->\n")
  options(table_counter_str = NULL)
  options(fig_caption_no_sprintf = NULL)
}
