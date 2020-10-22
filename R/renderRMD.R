renderRMD <- function (
  input,
  report_params
) {

  ###   rmd_input is same as rmd_input in renderMultiDocument, or has full YAML
  rmd_input <- sub("-Knitted.Rmd", ".Rmd", sub(file.path("Markdown", "raw_knitted", ""), "", input))

  ### Find pandoc - preference goes to Rstudio version (for now)
  my.pandoc <- rmarkdown::pandoc_exec()

  if(nchar(my.pandoc)==0) stop(
    "The program 'pandoc' was not found. Check 'Sys.getenv(\"RSTUDIO_PANDOC\")'.  If necessary,
      please install pandoc directly or a version of Rstudio (>=v0.98.932) that also contains it.")

  ###  Create the Draft folder if doesn't already exist
  if (!dir.exists(file.path("Draft"))) { # , "assets", "raw_knitted"
    dir.create(file.path("Draft"), showWarnings=FALSE) # recursive=TRUE,
  }

  ##
  ###   Post-process .Rmd file to make tables and figure placement human readable
  ##

  file <- file(input)
  rmd.text <- read_utf8(file)
  close(file)

  ###   Check SGP_Report validity of markdown text
  if (any(grepl("<!-- HTML_Start", rmd.text))) {
    if (length(grep("<!-- HTML_Start", rmd.text)) != length(grep("<!-- LaTeX_Start", rmd.text))){
      stop("There must be equal number of '<!-- HTML_Start' and '<!-- LaTeX_Start' elements in the .Rmd file.")
    }
  }

  ### Remove detritus (e.g. report set-up) between YAML and SGPreport body
  if (any(grepl("<!--SGPreport-->", rmd.text))) {
    # Valid YAML could end in "---" or "..."  - test for both.
    rmd.yaml <- rmd.text[grep("---", rmd.text)[1]:ifelse(length(grep("---", rmd.text))>=2, grep("---", rmd.text)[2], grep("[.][.][.]", rmd.text)[1])]
    start.index <- grep("<!--SGPreport-->", rmd.text)
  	rmd.text <- c(rmd.yaml, rmd.text[start.index:length(rmd.text)])
  }

  ###   Post-process HTML/LaTeX code

  ###   Clean up LaTeX tables
  for(j in grep("\\label[{]my[}]", rmd.text)) {
    rmd.text[j] <- gsub("[{]my[}]", paste("{table", strsplit(strsplit(rmd.text[j], "[*][*]Table ")[[1]][2], ":[*][*]")[[1]][1], "}", sep=""), rmd.text[j])
  }
  for(j in grep("\\caption[{][*][*]Table", rmd.text)) {
    rmd.text[j] <- gsub(":[*][*]", ":}", gsub("\\caption[{][*][*]Table", "\\caption*{\\\\textbf{Table", rmd.text[j]))
  }
  ## ztable doesn't have \caption
  for(j in grep("[*][*]Table", rmd.text)) {
    rmd.text[j] <- gsub("\\\\end", "}\\\\end", gsub(":[*][*]", ":}", gsub("[*][*]Table", "\\\\caption*{\\\\textbf{Table", rmd.text[j])))
  }

  ###   Scrub HTML code
  # html.index <- grep("<!-- HTML_Start", rmd.text)
  # latex.index<- grep("<!-- LaTeX_Start", rmd.text)
  # # combined.index <- c(rbind(html.index, (latex.index-1)))
  # eval.index <- gsub(", [:]", ":", paste0(c(rbind(paste0("-(", html.index), paste0(":", (latex.index-1), ")"))), collapse="", sep=", "))
  # eval.index <- paste0("c(", substr(eval.index, 1, nchar(eval.index)-2), ")")
  #
  # rmd.text <- rmd.text[eval(parse(text=eval.index))] # tmp.rmd.text <- rmd.text # for debugging tmp.rmd.text -> rmd.text

  table.count<-0L
  fig.count <- 0L
  apdx.count<- 0L

  while(any(grepl("<!-- LaTeX_Start", rmd.text))) {
    html.index <- grep("<!-- HTML_Start", rmd.text)[1]
    latex.start<- grep("<!-- LaTeX_Start", rmd.text)[1]
    latex.stop <- grep("LaTeX_End -->", rmd.text)[1]
    if (latex.stop != length(rmd.text)) {  #  Needed if input ends at 'LaTeX_End -->'
      final.text.chunk <- rmd.text[(latex.stop+1):length(rmd.text)]
    } else final.text.chunk <- ""

    if (grepl("dualTable", rmd.text[latex.start])) {  #  Extract source code for placement of tables from dualTable
      table.count <- table.count+1L
      if (table.count==1L) {
        dir.create(file.path("Draft", "assets", "src", "tables"), recursive=TRUE, showWarnings=FALSE)
      }
      table.file <- file.path("assets", "src", "tables", paste0("table_", table.count, ".txt"))
      cat(rmd.text[html.index:latex.stop], file=file.path("Draft", table.file), append=FALSE, sep="\n")

      # Convert tables to markdown to make readable
      cap.index <- grep("\\\\caption[*][{]\\\\textbf[{]", rmd.text[latex.start:latex.stop])
      if (length(cap.index) > 0) {
        tmp.cap <- rmd.text[latex.start:latex.stop][cap.index]
        tmp.cap <- gsub("[{]|[}]", "", sub("\\\\label.*", "", sub("\\\\caption[*][{]\\\\textbf[{]", "", tmp.cap)))
        rmd.text[latex.start:latex.stop][cap.index] <- ""
      } else tmp.cap <- "TABLE CAPTION MISSING"

      if (grepl("%", rmd.text[latex.start+1])) latex.start <- latex.start+1
      cat(rmd.text[(latex.start+1):(latex.stop-1)], file=file.path(tempdir(), "tmp.tex"), append=FALSE)
      tmp.table.md <- system(paste(my.pandoc, "-s -r latex", file.path(tempdir(), "tmp.tex"), "-t markdown -o -"), intern = TRUE)

      rmd.text <- c(rmd.text[1:(html.index-1)], paste("<!-- ", tmp.cap), tmp.table.md, "-->", "",
                   "```{r, cache=FALSE, results='asis', echo=FALSE}", paste0("    cat(readLines('", table.file, "'), sep = '\\n')"), "```",
                   final.text.chunk)
      next
    }
    if (grepl("placeFigure", rmd.text[latex.start])) {  #  Extract source code for placement of figures from placeFigure
      fig.count <- fig.count+1L
      if (fig.count==1L) {
        dir.create(file.path("Draft", "assets", "src", "figures"), recursive=TRUE, showWarnings=FALSE)
      }
      fig.file <- file.path("assets", "src", "figures", paste0("Figure_", fig.count, ".txt"))
      cat(rmd.text[html.index:latex.stop], file=file.path("Draft", fig.file), append=FALSE, sep="\n")

      tmp.cap <- rmd.text[latex.start:latex.stop][grep("\\\\caption[*]", rmd.text[latex.start:latex.stop])]
      tmp.cap <- gsub("[{]|[}]", "", sub("\\\\caption[*][{][{][{]\\\\bf[{]", "", tmp.cap))

      rmd.text <- c(rmd.text[1:(html.index-1)], paste("<!-- ", tmp.cap, " -->"), "",
                   "```{r, cache=FALSE, results='asis', echo=FALSE}", paste0("    cat(readLines('", fig.file, "'), sep = '\\n')"), "```",
                   final.text.chunk)
      next
    }
    if (grepl("pageBreak", rmd.text[latex.start])) {  #  Re-insert pageBreak
      rmd.text <- c(rmd.text[1:(html.index-1)],
                   "```{r, cache=FALSE, results='asis', echo=FALSE}", "    pageBreak()", "```",
                   final.text.chunk)
      next
    }
    if (grepl("startAppendix", rmd.text[latex.start])) {  #  Re-insert startAppendix
      apdx.count <- apdx.count+1L
      rmd.text <- c(rmd.text[1:(html.index-1)],
                   "```{r, cache=FALSE, results='asis', echo=FALSE}", paste0("    startAppendix(appendix.number=", apdx.count, ")"), "```",
                   final.text.chunk)
    }
    if (grepl("endAppendix", rmd.text[latex.start])) {  #  Re-insert endAppendix
      rmd.text <- c(rmd.text[1:(html.index-1)],
                   "```{r, cache=FALSE, results='asis', echo=FALSE}", "    endAppendix()", "```",
                   final.text.chunk)
    }
  }
  # if (apdx.count > 0L) rmd.text <- c(rmd.text, "```{r, cache=FALSE, results='asis', echo=FALSE}", "    endAppendix()", "```\n")

  ###  Re-check validity of rmd.text
  if (any(grepl("<!-- HTML_Start", rmd.text))) {
    if (length(grep("<!-- HTML_Start", rmd.text)) != length(grep("<!-- LaTeX_Start", rmd.text))){
      stop("There must be equal number of '<!-- HTML_Start' and '<!-- LaTeX_Start' elements in the .Rmd file.")
    }
  }

  ##  Get rid of random latex(...) comments
  for(j in grep("%latex", rmd.text)) rmd.text[j] <- ""

  ##  Remove extra blank lines (allow two "" lines)
  rmd.empty <- which(rmd.text=="")
  rmd.dup.empty <- rmd.empty[which(diff(rmd.empty)==1)+1]
  rmd.dup.dup.empty <- rmd.dup.empty[which(diff(rmd.dup.empty)==1)+1]
  rmd.text2 <- rmd.text[-rmd.dup.dup.empty]

  ###   Write out tailored/renerable .Rmd file to disk.  Save multiple versions
  draft.file <- file.path("Draft", sub(".Rmd", "-Compiled_Draft.Rmd", rmd_input, ignore.case = TRUE))
  existing.drafts <- sub(".Rmd", "", list.files(file.path("Draft"), pattern=".Rmd", ignore.case=TRUE))
  existing.drafts <- agrep(tail(strsplit(draft.file, "[/]|[\\]")[[1]], 1), existing.drafts, value=TRUE)

  if (length(existing.drafts) > 0) {
    last.version <- sub("v", "", max(sapply(strsplit(existing.drafts, "_"), tail, 1)))
    if (last.version == "Draft") {
      last.version <- 1L
      file.rename(draft.file, sub("-Compiled_Draft", "-Compiled_Draft_v1", draft.file))
    }
    next.version <- sum(as.numeric(last.version), 1, na.rm=TRUE)
    draft.file <- file.path("Draft", sub(".Rmd", paste0("-Compiled_Draft_v", next.version, ".Rmd"), rmd_input, ignore.case = TRUE))
  }

  writeLines(rmd.text, draft.file)
  message(paste("\n\t RMD draft rendered from intermediate file: ", input, "\n"))
}### End renderRMD
