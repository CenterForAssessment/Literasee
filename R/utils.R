tblNum <- function(advance.counter=0) {return(getOption("table_number")+advance.counter)}

tblNumIncrement <- function(advance.counter=1) {options("table_number" = getOption("table_number")+advance.counter); return(getOption("table_number"))}

tblCap <- function(caption.text=NULL, advance.counter=1) {
  if (as.integer(advance.counter) != 0L) tblNumIncrement(advance.counter=advance.counter)
  if (is.null(caption.text))  return(NULL)
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
