###   Render the differences between two files as an HTML document
diffFile <- function(
  current_file,
  reference_file = "HEAD",
  diff_algorithm="minimal", # ('--patience', '--minimal', '--histogram', or '--myers'('myers' is git default))
  clean_empyt_lines=FALSE,
  keep_diff = FALSE,
  view = TRUE,
  return_results=TRUE) {

  ##  Check arguments
  if (!grepl("[--]", diff_algorithm)) diff_algorithm <- paste0("--", diff_algorithm)
  if (diff_algorithm == "--") diff_algorithm <- NULL
  current_file_name <- current_file

  ##  test that we can find the git executable
  tryCatch(system2("git", "--version", stderr = TRUE, stdout = TRUE), error = function(e)
    stop("Cannot find the git executable: is it on your system path?"))

  ##  are we in a git repository?
  in_git_repo <- tryCatch({
    suppressWarnings(res <- system2("git", "status", stderr = TRUE, stdout = TRUE))
    status <- attr(res, "status")
    if (is.null(status) || status == 0L) TRUE else FALSE
  }, error = function(e) FALSE)

  ##  construct the git diff call
  ##  we always expect current_file to be an actual file
  if (!file.exists(current_file)) {
    stop("current_file does not exist: ", current_file)
  }
  ##  if we are comparing two actual files, we need to include --no-index in the command-line args
  index_str <- NULL
  if (!nzchar(reference_file)) {
    ##  empty string, which we will treat as the current version of the file in the HEAD
    ##  leave index_str as NULL
  } else {
    if (file.exists(reference_file)) {
        index_str <- "--no-index"
    }
  }
  if (is.null(index_str)) {
    ##  we think that the reference_file argument is a git reference
    ##  are we in a git repo?
    if (!in_git_repo) stop("The 'reference_file' parameter appears to be a git reference, but your working directory does not seem to be a git repository")
  }

  if (clean_empyt_lines) {
    ##  Remove extra blank lines (allow only one "" line)
    current.text <- read_utf8(current_file)
    tmp.empty <- which(current.text=="")
    tmp.dup.empty <- tmp.empty[which(diff(tmp.empty)==1)+1]
    current.text <- current.text[-tmp.dup.empty]
    current_file <- tempfile("tmp_current_file", fileext = ".txt")
    con <- file(current_file, "wt")
    writeLines(current.text, con)
    close(con)

    if (!is.null(index_str)) {
      reference.text <- read_utf8(reference_file)
      tmp.empty <- which(reference.text=="")
      tmp.dup.empty <- tmp.empty[which(diff(tmp.empty)==1)+1]
      reference.text <- reference.text[-tmp.dup.empty]
      reference_file <- tempfile("tmp_reference_file", fileext = ".txt")
      con <- file(reference_file, "wt")
      writeLines(reference.text, con)
      close(con)
    }
  }
  
  ###  Run 'git diff' on reference_file/current_file
  ##   rather than suppressWarnings, should capture warnings and then just screen out the one we want to ignore
  args <- c("diff", "-U3000", "--word-diff", diff_algorithm, index_str, reference_file, current_file) # , "--color-moved=dimmed-zebra"
  # if (debug) cat("args: ", args, "\n")
  suppressWarnings(diffout <- system2("git", args, stdout = TRUE))
  status <- attr(diffout, "status")
  ##   -U3000 specifies how much context is included around each individual change
  ##   --minimal :: 'Spend extra time to make sure the smallest possible diff is produced.'
  ##   we want the whole document here so it needs to be large (could count lines in file?)

  ## check what we got
  if ((is.null(status) || identical(status, 0L)) && length(diffout) < 1) {
    ## no output but status was OK, so there are no differences between the files
    message("Files are identical")
    return(NULL)
  }
  ## status of 1 means that it ran successfully but there were differences
  ## it seems that status can also be NULL here on a successful run
  ## status 2 means there was an error
  if (!is.null(status) && !identical(status, 1L)) {
    stop("error generating file differences (1)")
  }

  if (length(diffout) < 6) {  ##  no output
    stop("error generating file differences (2)")
  }

  ## strip the headers, which are the first 5 lines
  diffout <- diffout[seq(from = 6, to = length(diffout), by = 1)]

  ## escape HTML content now, so that in the final document it won't be interpreted as actual HTML
  ## do this before adding our HTML markup on changes, otherwise those wouldn't be interpreted as HTML either
  diffout <- htmltools::htmlEscape(diffout)

  ## AVI Added:  Find moved code/text chunks and markup
  # tmp.diffout <- diffout # diffout <- tmp.diffout # debugging
  ins.indx <- grep("[{][+]", diffout)
  del.indx <- grep("[[][-]", diffout)
  dup.indx <- intersect(ins.indx, del.indx)

  if (length(dup.indx) > 0) {
    if (!is.null(index_str)) {
      if (!exists("reference.text")) reference.text <- read_utf8(reference_file)
      moved.added <- vector(mode="character", length=length(diffout))
      moved.deleted <- vector(mode="character", length=length(diffout))
      for (d in dup.indx) { # Moved chunks (reformat for readability)
        tmp.added <- tmp.deleted <- trimWhiteSpace(diffout[d])
        extracted.add <- unique(do.call(rbind, stringr::str_match_all(tmp.added, "[\\[][-]\\s*(.*?)\\s*[-][\\]]")))
        extracted.del <- unique(do.call(rbind, stringr::str_match_all(tmp.deleted, "[{][+]\\s*(.*?)\\s*[+][}]")))
        if (nrow(extracted.add)==0 | nrow(extracted.del)==0) next
        if (!any(nchar(extracted.add)>100) | !any(nchar(extracted.del)>100)) next # Only looking for big diffs
        for (i in seq(nrow(extracted.add))) tmp.added <- gsub(extracted.add[i,1], "", tmp.added, fixed=TRUE)
        for (i in seq(nrow(extracted.del))) tmp.deleted <- gsub(extracted.del[i,1], "", tmp.deleted, fixed=TRUE)
        tmp.added <- gsub("[{][+]|[+][}]", " ", tmp.added) # Add a space instead of "" to avoid running words together inadvertently
        tmp.deleted <- gsub("[[][-]|[-][]]", " ", tmp.deleted) # added space not caught (yet) by 'git diff'
        if (any(agrepl(tmp.deleted, reference.text))) {
          to.file <- tempfile("tmp_orig", fileext = ".txt")
          td.file <- tempfile("tmp_deleted", fileext = ".txt")
          con1 <- file(to.file, "wt")
          con2 <- file(td.file, "wt")
          cat(trimWhiteSpace(reference.text[agrep(tmp.deleted, reference.text)[1]]), file = con1)
          cat(tmp.deleted, file = con2)
          close(con1)
          close(con2)
          args <- c("diff","--word-diff", diff_algorithm, index_str, to.file, td.file)
          new.deleted.diff <- suppressWarnings(system2("git", args, stdout = TRUE)[-(1:5)])
          unlink(c(to.file, td.file))
        } else new.deleted.diff <- tmp.deleted
        if (any(agrepl(tmp.added, reference.text))) {
          to.file <- tempfile("tmp_orig", fileext = ".txt")
          ta.file <- tempfile("tmp_added", fileext = ".txt")
          con1 <- file(to.file, "wt")
          con2 <- file(ta.file, "wt")
          cat(trimWhiteSpace(reference.text[agrep(tmp.added, reference.text)[1]]), file = con1)
          cat(tmp.added, file = con2)
          close(con1)
          close(con2)
          # new.added.diff <- system("git diff --word-diff --no-index tmp_orig.txt tmp_added.txt"), intern=TRUE)[-(1:5)]
          args <- c("diff","--word-diff", diff_algorithm, index_str, to.file, ta.file) # , "--color-moved=dimmed-zebra"
          new.added.diff <- suppressWarnings(system2("git", args, stdout = TRUE)[-(1:5)]) # , normalizePath(to.file), normalizePath(ta.file)
          unlink(c(to.file, ta.file))
        } else new.added.diff <- tmp.added
        moved.added[d] <- ifelse(length(new.added.diff)>0, new.added.diff, "")
        moved.deleted[d] <- ifelse(length(new.deleted.diff)>0, new.deleted.diff, "")
      }
      moved.added.indx <- moved.deleted.indx <- as.numeric(NA)
      for (m in which(moved.added!="")) {if(any(agrepl(moved.added[m], moved.deleted))) moved.added.indx <- c(moved.added.indx, m)}
      for (m in which(moved.deleted!="")) {if(any(agrepl(moved.deleted[m], moved.added))) moved.deleted.indx <- c(moved.deleted.indx, m)}
      for (m in (intersect(moved.added.indx, moved.deleted.indx) %w/o% NA)) {
        if(any(agrepl(moved.added[m], moved.deleted))){
          diffout[m] <- paste("<ins class=\"movedin\">",  moved.added[m], "</ins>",
                              "<del class=\"movedout\">", moved.deleted[m], "</del>", sep="\n")
        }
      }
    } else {
      message("Whole moved chunks cannot use special marked up when reference_file is a git commit tag or 'HEAD'")
    }
  }

  # stop("stopAgain")

  ###  Find novel chunks and markup
  ins.indx <- grep("[{][+]", diffout) %w/o% grep("<del|<ins", diffout)
  del.indx <- grep("[[][-]", diffout) %w/o% grep("<del|<ins", diffout)
  ins.text <- sapply(diffout[ins.indx], trimWhiteSpace, USE.NAMES=FALSE)
  del.text <- sapply(diffout[del.indx], trimWhiteSpace, USE.NAMES=FALSE)

  trimInsChunk <- function(line) gsub("(^[{][+]+)|([+][}]+$)", "", line)
  trimDelChunk <- function(line) gsub("(^[[][-]+)|([-][]]+$)", "", line)
  ##  Find chunks that are entirely new insertions (begin with '{+' and end with '+}' - no addition or subtraction within)
  whole.ins.chunks <- sapply(ins.text, function(f) (!grepl("[{][+]|[+][}]|[[][-]|[-][]]", trimInsChunk(f)) & nchar(f)>200), USE.NAMES=FALSE)
  if (any(whole.ins.chunks)) {
    whole.ins.text <- sapply(ins.text[whole.ins.chunks], trimInsChunk, USE.NAMES=FALSE)
    for (tmp.ins.txt in whole.ins.text) {
      novel.indx <- agrep(tmp.ins.txt, ins.text)
      diffout[ins.indx[novel.indx]] <- gsub("[+][}]", "</ins>", gsub("[{][+]", "<ins class=\"allnew\">", ins.text[agrep(tmp.ins.txt, ins.text)]))
      ins.indx <- ins.indx[-novel.indx]
      ins.text <- ins.text[-novel.indx]
    }
  }

  ##  Find chunks that are entirely deleted (begin with '[-' and end with '-]' and no addition or subtraction within)
  whole.del.chunks <- sapply(del.text, function(f) (!grepl("[{][+]|[+][}]|[[][-]|[-][]]", trimDelChunk(f)) & nchar(f)>200), USE.NAMES=FALSE)
  if (any(whole.del.chunks)) {
    whole.del.text <- sapply(del.text[whole.del.chunks], trimDelChunk, USE.NAMES=FALSE)
    for (tmp.del.txt in whole.del.text) {
      rmv.indx <- agrep(tmp.del.txt, del.text)
      diffout[rmv.indx] <- gsub("[+][}]", "</del>", gsub("[{][+]", "<del class=\"allrm\">", del.text[agrep(tmp.del.txt, del.text)]))
      del.indx <- del.indx[-rmv.indx]
      del.text <- del.text[-rmv.indx]
    }
  }

  ## mark up the insert/deletes as HTML markup
  ins.indx <- grep("[{][+]", diffout)
  del.indx <- grep("[[][-]", diffout)

  for (ins in ins.indx) {
    tmp.added <- diffout[ins]
    extracted.ins <- unique(do.call(rbind, stringr::str_match_all(tmp.added, "[{][+]\\s*(.*?)\\s*[+][}]")))
    if (nrow(extracted.ins)==0) next
    for (i in seq(nrow(extracted.ins))) {
      tmp.added <- gsub(extracted.ins[i,1], paste0("<ins class=\"ins\">", extracted.ins[i,2], "</ins>"), tmp.added, fixed=TRUE)
    }
    diffout[ins] <- tmp.added
  }
  for (del in del.indx) {
    tmp.deleted <- diffout[del]
    extracted.del <- unique(do.call(rbind, stringr::str_match_all(tmp.deleted, "[\\[][-]\\s*(.*?)\\s*[-][\\]]")))
    if (nrow(extracted.del)==0) next
    for (i in seq(nrow(extracted.del))) {
      tmp.deleted <- gsub(extracted.del[i,1], paste0("<del class=\"del\">", extracted.del[i,2], "</del>"), tmp.deleted, fixed=TRUE)
    }
    diffout[del] <- tmp.deleted
  }

  ##  Might still need to catch stragglers?
  # diffout <- gsub("[-", "<del class=\"del\">", diffout, fixed = TRUE)
  # diffout <- gsub("-]", "</del>", diffout, fixed = TRUE)
  # diffout <- gsub("{+ ", "<ins class=\"ins\">", diffout, fixed = TRUE)
  # diffout <- gsub("+}", "</ins>", diffout, fixed = TRUE)

  intfile <- tempfile()
  con <- file(intfile, "wt")
  ## build markdown template
  diffs_html_file <- tempfile(fileext = ".html")
  con2 <- file(diffs_html_file, "wt")
  cat("<pre id = \"diffcontent\">\n", file = con2)
  cat(diffout, sep = "\n", file = con2, append = TRUE)
  cat("</pre>\n", file = con2, append = TRUE)
  close(con2)
  cat(c("---", "title: '&nbsp;'", "output:", "  html_document:", "    includes:",
        paste0("      before_body: ", diffs_html_file), "---"), sep = "\n", file = con)

  css <- c(".movedout { background-color: #0d0d0d; color: #ffd0d7; }",
    ".movedin{ background-color: #0d0d0d; color: #4dffa7; }",
    ".allrm{ background-color: #b30000; color: #ffff00; }",
    ".allnew{ background-color: lime; color: #6e7697; }",
    ".del { background-color: SandyBrown; color: black; }",
    ".ins{ background-color: PaleGreen; color: black; }",
    ## styles to make <pre> tags line-wrap
    "#diffcontent { white-space: pre-wrap;       /* css-3 */",
    "white-space: -moz-pre-wrap;  /* Mozilla, since 1999 */",
    "white-space: -pre-wrap;      /* Opera 4-6 */",
    "white-space: -o-pre-wrap;    /* Opera 7 */",
    "word-wrap: break-word;       /* Internet Explorer 5.5+ */",
    "word-break: break-word; }    /* AVI Added */")
  cat(c("\n<style>", css, "</style>"), sep = "\n", file = con, append = TRUE)

  close(con)
  out <- list(raw = rmarkdown::render(intfile, output_format = "html_document", quiet = TRUE))
  unlink(intfile)

  if (keep_diff) {
    if (tail(strsplit(getwd(), "[/]|[\\]")[[1]], 1) == "Draft") tmp.dir <- "." else tmp.dir <- "Draft"
    current_diff_name <- sub(getFileNameExtension(current_file_name), "-DIFF.html", tail(strsplit(current_file_name, "[/]|[\\]")[[1]], 1))
    file.copy(out$raw, file.path(tmp.dir, current_diff_name), overwrite=TRUE)
    out$raw <- normalizePath(file.path(tmp.dir, current_diff_name))
  }
  if (view) {
    browseURL(out$raw)
  }
  if (return_results) {
    return(out)
  }
}
