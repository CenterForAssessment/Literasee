tblNum <- function(advance.counter=0) {
  if (!is.null(getOption("table_num_str"))) {
    return(sprintf(getOption("table_num_str"), getOption("table_number")+advance.counter))
  }
  return(getOption("table_number")+advance.counter)
}

figNum <- function(advance.counter=0) {
  if (!is.null(getOption("fig_num_str"))) {
    return(sprintf(getOption("fig_num_str"), getOption("fig_caption_no")+advance.counter))
  }
  return(getOption("fig_caption_no")+advance.counter)
}

tblNumIncrement <- function(advance.counter=1) {options("table_number" = getOption("table_number")+advance.counter); return(getOption("table_number"))}
figNumIncrement <- function(advance.counter=1) {options("fig_caption_no" = getOption("fig_caption_no")+advance.counter); return(getOption("fig_caption_no"))}

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

trimWhiteSpace <- function(line) gsub("(^ +)|( +$)", "", line)

"%w/o%" <- function(x,y) x[!x %in% y]

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


#####
###   Functions borrowed from `rmarkdown` package to avoid use of :::
#####

#  https://github.com/rstudio/rmarkdown/blob/1707d0b5189e3f12caf16a6cd53391f5be2b2f08/R/util.R#L90
read_utf8 <- function(file) {
  if (inherits(file, 'connection')) con <- file else {
    con <- base::file(file, encoding = 'UTF-8'); on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
}

# https://github.com/rstudio/rmarkdown/blob/97013fa09961246aa96dd59c1f48b7845a167607/R/pandoc.R#L664
# get the path to the pandoc-citeproc binary
pandoc_citeproc <- function() {
  bin <- "pandoc-citeproc"
  p <- file.path(rmarkdown::find_pandoc()$dir, bin)
  if (xfun::is_windows()) p <- xfun::with_ext(p, "exe")
  if (file.exists(p)) p else bin
}

getFileNameExtension <- function (filename, dot=TRUE) {
  # remove a path
  splitted <- strsplit(x=filename, split='/')[[1]]
  # or use .Platform$file.sep in stead of '/'
  filename <- splitted [length(splitted)]
  ext <- ''
  splitted <- strsplit(x=filename, split='\\.')[[1]]
  l <-length (splitted)
  if (l > 1 && sum(splitted[1:(l-1)] != ''))  ext <-splitted [l]
  # the extention must be the suffix of a non-empty name
  if (dot) ext <- paste0(".", ext)
  ext
}
