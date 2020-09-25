tblNum <- function(advance.counter=0) {return(getOption("table_number")+advance.counter)}

tblNumIncrement <- function(advance.counter=1) {options("table_number" = getOption("table_number")+advance.counter); return(getOption("table_number"))}

tblCap <- function(caption.text=NULL, advance.counter=1) {
  if (as.integer(advance.counter) != 0L) tblNumIncrement(advance.counter=advance.counter)
  if (is.null(caption.text))  return(NULL)
  if (!is.null(getOption("table_counter_str"))) {
    tmp.caption <- paste(getOption("table_counter_str"), caption.text)
    tmp.caption <- gsub("[%]s", getOption("table_number"), tmp.caption)
    return(tmp.caption)
  }
  return(paste0("**Table ", getOption("table_number"), ":** ", caption.text))
}

eqnNumNext <- function() {options("equation_counter" = getOption("equation_counter")+1); return(getOption("equation_counter"))}

eqnNum <- function(advance.counter=0, eqn.name="t1", em.space=150) {
  pos <- 1
  if (!is.null(eqn.name)) assign(eqn.name, getOption('equation_counter')+1, envir=as.environment(pos)) else getOption('equation_counter')+1
  return(cat('\\hspace{', em.space, 'em} \\text{(', getOption('equation_counter')+advance.counter, ')}', sep=""))
}

setCounters <- function(tbl.counter=0, fig.counter=0, eqn.counter=0) {
  options("table_number" = tbl.counter)
  options("fig_caption_no" = fig.counter)
  options("equation_counter" = eqn.counter)
}

startAppendix <- function(appendix.number=1L, use.alpha=TRUE, set.counters=TRUE, table.name=NULL, figure.name=NULL) {
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
<!-- LaTeX_Start
\\renewcommand{\\thesection}{", LETTERS[appendix.number], "}
\\pagenumbering{arabic}
\\renewcommand*{\\thepage}{\\thesection\\arabic{page}}
LaTeX_End -->\n", sep=""
	)
}

endAppendix <- function() {
  options(table_counter_str = NULL)
  options(fig_caption_no_sprintf = NULL)
}

pageBreak <- function() {
	###  NOTE:  ALL 'cat()' text must be flush left in R script!
	cat("
<!-- HTML_Start -->
<div class='breakboth' ></div>
<!-- LaTeX_Start
\\pagebreak
LaTeX_End -->\n"
	)
}

trimWhiteSpace <- function(line) gsub("(^ +)|( +$)", "", line)

read.markdown.table <- function(file, stringsAsFactors = FALSE, strip.white = TRUE, ...){
    if (length(file) > 1) {
        lines <- file
    } else if (grepl('\n', file)) {
        con <- textConnection(file)
        lines <- readLines(con)
        close(con)
    } else {
        lines <- readLines(file)
    }
    lines <- lines[!grepl('^[[:blank:]+-=:_|]*$', lines)]
    lines <- gsub('(^\\s*?\\|)|(\\|\\s*?$)', '', lines)
    read.delim(text = paste(lines, collapse = '\n'), sep = '|',
               stringsAsFactors = stringsAsFactors, strip.white = strip.white, ...)
}
