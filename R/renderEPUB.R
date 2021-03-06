renderEPUB <- function(
  input,
  cover_img = NULL,
  add_cover_title = FALSE,
  number_sections = TRUE,
  convert_header_levels = c(5,6),
  literasee_template = "sgp_report",
  epub_template = NULL,
  epub_css = "default",
  bibliography = "default",
  csl = "default",
  pandoc_args = NULL) {

  ###   Determine .Rmd/.md inputs

  #  rmd_input is same as rmd_input in renderMultiDocument, or has full YAML
  rmd_input <- sub("-Knitted.Rmd", ".Rmd", sub(file.path("Markdown", "raw_knitted", ""), "", input))

  #  Use HTML rendered .md file if it exists
  input.md <- file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE))
  if (!file.exists(input.md)) {
    input.md <- input
  }

  ##
  ###   Initial checks of alternative css and/or pandoc template
  ##

  ##  CSS check from Gmisc--docx_document - credit to Max Gordon/Gforge https://github.com/gforge
  if (epub_css != "default") {
    if (!all(sapply(epub_css, file.exists))) {
      alt_epub_css <- list.files(pattern = ".css$")
      if (length(alt_epub_css) > 0) {
        alt_epub_css <- paste0("\n You do have alternative file name(s) in current directory that you may intend to use.",
                               " You may want to have a YAML section that looks something like:",
                               "\n---", "\noutput:", "\n  Literasee::multi_document:",
                               "\n    epub_css: \"", paste(alt_epub_css, collapse = "\", \""),
                               "\"", "\n---")
      } else {
        alt_epub_css <- ""
      }
      stop("One or more of the css-file(s) that you've specified can't be identified.",
           "The file(s) '", paste(epub_css[!sapply(epub_css, file.exists)],
                collapse = "', '"), "'", " can't be found in the file path provided.")
    }
  } else epub_css <- system.file("rmarkdown", "templates", literasee_template, "resources", "epub.css" , package = "Literasee")

  if (epub_template == "default") {
    epub_template <- NULL
  } else {
    if (!file.exists(epub_template)) {
      stop("The epub_template file that you've specified can't be found in the file path provided.")
    } else epub_template <- paste("--template ", epub_template)
  } # EPUB default is pandoc template for now otherwise :: else epub_template <- system.file("rmarkdown", "templates", "sgp_report", "resources", "epub_report.html" , package = "Literasee")

  ##  Check csl file
  if (!is.null(csl)) {
    if (csl != "default") {
      if (!file.exists(csl)) {
        stop("The csl file that you've specified can't be found in the file path provided.")
      } else csl <- paste("--csl", csl)
    } else csl <- paste("--csl", system.file("rmarkdown", "content", "bibliography", "apa-5th-edition.csl" , package = "Literasee"))
  }

  ### Check defaults
  epub_number_sections <- ifelse(number_sections, "--number-sections", NULL)

  ###  pandoc args
  if(!is.null(pandoc_args)){
    if(any(grepl("--highlight-style", pandoc_args))) {
      highlight <- pandoc_args[grepl("--highlight-style", pandoc_args)]
      pandoc_args <- pandoc_args[!grepl("--highlight-style", pandoc_args)]
    } else {
      highlight <- "--highlight-style pygments"
    }
  } else {
    highlight <- "--highlight-style pygments"
  }  ##  End 'Initial checks'

  ##
  ###   EPUB post-processing of rendered/knitted markdown
  ##

  if (!dir.exists(file.path("EPUB", "markdown"))) {
    dir.create(file.path("EPUB", "markdown"), recursive=TRUE, showWarnings=FALSE)
  }
  # setwd("EPUB")

  ###  Get YAML from .Rmd file (as a list)
  file <- file(rmd_input) # original rmd_input file
  rmd.yaml <- rmarkdown::yaml_front_matter(file)
  close(file)

  tmp.yaml <- "---"
  # cat("---\n", file="epub.yaml")
  if ("title" %in% names(rmd.yaml)) {
    if(is.character(rmd.yaml$title)) tmp.yaml <- c(tmp.yaml, paste("title:", rmd.yaml$title))
  }
  if ("subtitle" %in% names(rmd.yaml)) {
    if(is.character(rmd.yaml$subtitle)) tmp.yaml <- c(tmp.yaml, paste("subtitle:", rmd.yaml$subtitle))
  }
  if ("author" %in% names(rmd.yaml)) {
    if(is.character(rmd.yaml$author)) {
      tmp.yaml <- c(tmp.yaml, paste("author:", rmd.yaml$author))
    } else {
      if (length(rmd.yaml$author)>1) {
        tmp.auth <- paste("author: [", rmd.yaml$author[[1]][["name"]], ", ", sep="")
        #       cat("author: [", file="epub.yaml", append=TRUE)
        for (name in 2:length(rmd.yaml$author)) {
          tmp.auth <- paste(tmp.auth, rmd.yaml$author[[name]][["name"]], sep="")
          #         cat(rmd.yaml$author[[name]][["name"]], file="epub.yaml", append=TRUE)
          if (name != length(rmd.yaml$author)) tmp.auth <- paste(tmp.auth, ", ", sep="")
          #           cat(", ", file="epub.yaml", append=TRUE)
        }
        tmp.auth <- paste(tmp.auth, "]", sep="")
        tmp.yaml <- c(tmp.yaml, tmp.auth)
        #         cat("]\n", file="epub.yaml", append=TRUE)
      } else tmp.yaml <- c(tmp.yaml, paste("author: ", rmd.yaml$author[[1]][["name"]]))
      #       cat(paste("author: ", rmd.yaml$author[[1]][["name"]], "\n"), file="epub.yaml", append=TRUE)
    }
  }

  if ("date" %in% names(rmd.yaml)) {
    tmp.yaml <- c(tmp.yaml, paste("date: ", rmd.yaml$date))
  }

  tmp.yaml <- c(tmp.yaml, "language: en-US", "rights:  Creative Commons Non-Commercial Share Alike 3.0", "---")

  ###  Get .md file rendered from .rmd for html output
  file <- file(input.md) # file.path("..", input.md)
  md.text <- read_utf8(file)
  close(file)

  ###   Check SGP_Report validity of markdown text
  if (any(grepl("<!-- HTML_Start", md.text))) {
    if (length(grep("<!-- HTML_Start", md.text)) != length(grep("<!-- LaTeX_Start", md.text))){
      stop("There must be equal number of '<!-- HTML_Start' and '<!-- LaTeX_Start' elements in the .Rmd file.")
    }
  }

  ##  Scrub LaTeX code
  while(any(grepl("<!-- LaTeX_Start", md.text))) {
    latex.start<-grep("<!-- LaTeX_Start", md.text)[1]
    latex.end <- grep("LaTeX_End -->", md.text)[1]
    md.text <- md.text[-(latex.start:latex.end)]
  }

  if (any(grepl("<!--SGPreport-->", md.text))) {
  	start.index <- grep("<!--SGPreport-->", md.text)
  	md.text <- md.text[start.index:length(md.text)]
  }

  for (header.level in rev(convert_header_levels)) {
    header <- paste(paste(rep("#", header.level), collapse=""), "")
    index <- grep(header, md.text)

    for (i in index) {
      if(grepl("![[]", md.text[i+1])) { # insure next line is a markdown figure '![...'
        # Turn MD header into a HTML div
        md.text[i] <- paste("<div class='caption'>", gsub(header, "", md.text[i]), '</div>')
      }
      if(grepl("![[]", md.text[i+2])) { # insure next line is a markdown figure '![...'
        md.text[i] <- paste("<div class='caption' style='page-break-before:always;'>", gsub(header, "", md.text[i]), '</div>')
      }
    }
  }

  md.text <- gsub("[(]img", file.path("(.","img"), md.text) #odd - need regexpr in find, but not replace...
  md.text <- gsub("style=''", "style=';'", md.text)
  md.text <- gsub("FALSE>", ">", md.text)

  input.epub <- file.path("markdown", gsub(".Rmd|.md", "-epub.md", rmd_input, ignore.case=TRUE))
  if ("abstract" %in% names(rmd.yaml)) {
    abstract <- paste("<div class='lead' id='document_lead'><p style='text-align:center;'>", rmd.yaml$abstract, "</p></div>")
    writeLines(c(tmp.yaml, abstract, md.text), input.epub)
  } else writeLines(c(tmp.yaml, md.text), input.epub)

  # setwd("..")

  if (!is.null(cover_img)) {
    if (add_cover_title) {
      dir.create(file.path("EPUB", "cover_img"), recursive=TRUE, showWarnings=FALSE)
      coverTitle(cover_img, title=rmd.yaml$title, subtitle=rmd.yaml$subtitle, out_dir=file.path("EPUB", "cover_img"))
      tmp_cover <- paste("--epub-cover-image=",
                         file.path("EPUB", "cover_img", paste(gsub(" ", "_", rmd.yaml$title), ".jpeg", sep="")), sep="")
    } else {
      tmp_cover <- paste("--epub-cover-image=", cover_img, sep="")
    }
  } else tmp_cover <- NULL

  ###
  ### system() call to pandoc
  ###

  ### Find pandoc - preference goes to Rstudio version (for now)
  my.pandoc <- rmarkdown::pandoc_exec()
  my.pandoc_citeproc <- pandoc_citeproc()

  if(nchar(my.pandoc)==0) stop(
      "The program 'pandoc' was not found. Check 'Sys.getenv(\"RSTUDIO_PANDOC\")'.  If necessary,
      please install pandoc directly or a version of Rstudio (>=v0.98.932) that also contains it.")

  ### Bibliography
  if(!is.null(bibliography)) {
    if (bibliography == "default") {
      biblio<-paste("--filter", my.pandoc_citeproc, "--bibliography",
                    system.file("rmarkdown", "content", "bibliography", "Literasee.bib" , package = "Literasee"))
    } else {
      if(file.exists(bibliography)) {
        biblio <- paste("--filter", my.pandoc_citeproc, "--bibliography", bibliography)
      } else stop("YAML 'bibliography:' file not found.")
    }
  }

  syst.call <- paste(my.pandoc, "-S -o", file.path("EPUB", gsub(".Rmd|.md", ".epub", rmd_input, ignore.case=TRUE)), file.path("EPUB", input.epub),
                     tmp_cover, "--epub-stylesheet ", epub_css, epub_template, epub_number_sections, highlight, biblio, csl, pandoc_args)
  message(paste("\n\t Rendering EPUB with system call to pandoc:\n\n", syst.call, "\n\n\tIntermediate file used: ", input.md, "\n"))
  system(syst.call)
}  # End 'renderEPUB' function
