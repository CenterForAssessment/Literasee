renderHTML <- function (
  input,
  number_sections = TRUE,  # Now changed to false.  Any of these needed if passed in through renderMultiDocument???
  number_section_depth=3,
  toc = TRUE,
  toc_depth = 2,
  self_contained = TRUE,
  dev="png",
  literasee_template = "sgp_report",
  html_template = "default",
  html_css = "default",
  bibliography = "default",
  csl = "default",
  md_extensions=NULL,
  pandoc_args = NULL,
  report_params = NULL,
  output_dir=file.path(".", "HTML"),
  ...) {

  ##
  ### Initial checks of alternative css and/or pandoc template
  ##

	##  CSS check from Gmisc::docx_document - credit to Max Gordon/Gforge https://github.com/gforge
	if (html_css != "default") {
		if (!all(sapply(html_css, file.exists))) {
			alt_html_css <- list.files(pattern = ".css$")
			if (length(alt_html_css) > 0) {
				alt_html_css <- paste0("\n You do have alternative file name(s) in current directory that you may intend to use.",
															 " You may want to have a YAML section that looks something like:",
															 "\n---", "\noutput:", "\n  Literasee::multi_document:",
															 "\n    html_css: \"", paste(alt_html_css, collapse = "\", \""),
															 "\"", "\n---")
			} else {
				alt_html_css <- ""
			}
			stop("One or more of the css-file(s) that you've specified can't be identified.",
					 "The file(s) '", paste(html_css[!sapply(html_css, file.exists)],
					 											 collapse = "', '"), "'", " can't be found in the file path provided.")
		}
	} else {
    html_css <- system.file("rmarkdown", "templates", literasee_template, "resources", "html_report.css" , package = "Literasee")
    number_sections <- FALSE # Could use numbering in html_report_head_level_*.css based on number_section_depth
    md_extensions <- c(md_extensions, "-native_divs")
    if (!self_contained) {
      if (!dir.exists(file.path(output_dir, "assets", "css"))) {
        dir.create(file.path(output_dir, "assets", "css"), showWarnings = FALSE, recursive = TRUE)
      }
      file.copy(html_css, file.path(output_dir, "assets", "css"))
      html_css <- file.path(output_dir, "assets", "css", tail(strsplit(html_css, "[/]|[\\]")[[1]], 1))
    }
  }
	if (length(html_css)>1 & any(html_css != "default")) {
		html_css <- c(html_css, system.file("rmarkdown", "templates", literasee_template, "resources", "html_report.css" , package = "Literasee"))
	}

	html_css <- unique(html_css)

  ### Check pandoc template

	if (html_template != "default") {
		if (html_template == "simple") html_template <- system.file("rmarkdown", "templates", literasee_template, "resources", "html_report_simple.html" , package = "Literasee")
		if (!file.exists(html_template)) {
			stop("The html_template file that you've specified can't be found in the file path provided.")
		}
	} else html_template <- system.file("rmarkdown", "templates", literasee_template, "resources", "html_report.html" , package = "Literasee")

	### Bibliography

	if (!is.null(bibliography)) {
		my.pandoc_citeproc <- pandoc_citeproc()
		if (bibliography == "default") {
			pandoc_args <-c(pandoc_args, "--filter", my.pandoc_citeproc, "--bibliography",
											system.file("rmarkdown", "content", "bibliography", "Literasee.bib" , package = "Literasee"))
			bibliography <- NULL
		} else {
			if(file.exists(bibliography)) {
				pandoc_args <-c(pandoc_args, "--filter", my.pandoc_citeproc, "--bibliography", bibliography)
				bibliography <- NULL
			} else stop("'bibliography' file not found.")
		}
	}

	##  Check csl file
	if (!is.null(csl)) {
		if (csl != "default") {
			if (!file.exists(csl)) {
				stop("The csl file that you've specified can't be found in the file path provided.")
			} else {
				pandoc_args <- c(pandoc_args, "--csl", csl) # Use pandoc_args here since docx_document passes that to html_document
				csl <- NULL
			}
		} else {
			pandoc_args <- c(pandoc_args, "--csl", system.file("rmarkdown", "content", "bibliography", "apa-5th-edition.csl" , package = "Literasee"))
			csl <- NULL
		}
	}  ##  END 'Initial checks'

	##
 	###  Render HTML (and master .md file)
	##

	message("\n\t Rendering HTML with call to render(..., multi_document):\n")

	render(input,
  			 output_format = multi_document(..., # passed args to rmarkdown::html_document
  			 				number_sections=number_sections, number_section_depth=number_section_depth, toc=toc, toc_depth=toc_depth, self_contained=self_contained,
  			 				dev=dev, literasee_template=literasee_template, html_template=html_template, css=html_css, bibliography=bibliography,
  			 				csl=csl, md_extensions=md_extensions, pandoc_args=pandoc_args), params = report_params, output_dir=output_dir)

	###   Move "master" .md file to HTML/markdown directory
	if (grepl("HTML", output_dir)) {
		dir.create(file.path(output_dir, "markdown"), showWarnings=FALSE)
		if (file.copy(file.path(output_dir, gsub(".Rmd", ".md", input, ignore.case=TRUE)), file.path(output_dir, "markdown"), overwrite=TRUE)) {
		    file.remove(file.path(output_dir, gsub(".Rmd", ".md", input, ignore.case=TRUE)))
    }
	}

  ###   Scrub LaTeX code from HTML
  ##  Get .html text rendered from multi_document above
  html.file <- file.path(output_dir, gsub(".Rmd", ".html", input, ignore.case=TRUE))
  file <- file(html.file)
  html.text <- read_utf8(file)
  close(file)

  latex.start<-grep("<!-- LaTeX_Start", html.text)
  latex.end <- grep("LaTeX_End -->", html.text)
  eval.index <- gsub(", [:]", ":", paste0(c(rbind(paste0("-(", latex.start), paste0(":", latex.end, ")"))), collapse="", sep=", "))
  eval.index <- paste0("c(", substr(eval.index, 1, nchar(eval.index)-2), ")")

  html.text <- html.text[eval(parse(text=eval.index))] # tmp.html.text <- html.text # html.text <- tmp.html.text

  ##  Remove extra blank lines (allow two "" lines)
  html.empty <- which(html.text=="")
  html.dup.empty <- html.empty[which(diff(html.empty)==1)+1]
  html.dup.dup.empty <- html.dup.empty[which(diff(html.dup.empty)==1)+1]
  html.text <- html.text[-html.dup.dup.empty]

  if (!self_contained) {
    render.dir <- gsub(".Rmd", "_files", input, ignore.case=TRUE)
    asset.dirs <- list.dirs(file.path("HTML", render.dir), full.names = FALSE)
    nest.test <- sapply(seq(asset.dirs), function(f) grepl(asset.dirs[f], asset.dirs[-f], fixed=TRUE))
    if(any(nest.test)) asset.dirs <- asset.dirs[-which(nest.test, arr.ind = TRUE)[,2]]

    for (a in asset.dirs) {
      tmp.path <- file.path("HTML", render.dir, a)
      files.to.find <- sub(file.path("HTML", ""), "", list.files(tmp.path, full.names = TRUE))
      for (b in files.to.find) {
        find.index <- grep(b, html.text, fixed=TRUE)
        if (length(find.index) == 0) {
          b <- sub(render.dir, file.path(render.dir, ""), b)
          find.index <- grep(b, html.text, fixed=TRUE)
        }
        if (length(find.index) == 0) next # if still not found go to next directory
        if (grepl(file.path("HTML", "assets"), b)) {
          tmp.new.dir <- paste(file.path(head(strsplit(sub(paste0(".*", file.path("HTML", "assets", "")), "", b), "[/]|[\\]")[[1]], -1), ""), collapse="")
          if (!dir.exists(file.path("HTML", "assets", tmp.new.dir))) {
            dir.create(file.path("HTML", "assets", tmp.new.dir), showWarnings = FALSE, recursive = TRUE)
          }
          new.b <- file.path("HTML", "assets", tmp.new.dir, tail(strsplit(b, "[/]|[\\]")[[1]], 1))
          if (file.copy(file.path("HTML", b), new.b)) file.remove(file.path("HTML", b))
          html.text[find.index] <- gsub(b, sub(file.path("HTML", ""), "", new.b), html.text[find.index], fixed=TRUE)
        }
        if (any(grepl("[.]csl$|[.]css$|[.]js$|[.]R$", list.files(tmp.path)))) {
          if (!dir.exists(file.path("HTML", "assets", "src"))) {
            dir.create(file.path("HTML", "assets", "src"), showWarnings = FALSE, recursive = TRUE)
          }
          if (file.copy(file.path("HTML", b), file.path("HTML", "assets", "src"), recursive=TRUE)) file.remove(file.path("HTML", b))
          html.text[find.index] <- gsub(sub(file.path("HTML", ""), "", tmp.path), file.path("assets", "src"), html.text[find.index], fixed=TRUE)
        }
        if (grepl(file.path("SGP_Report", "assets"), b)) {
          tmp.new.dir <- paste(file.path(head(strsplit(sub(paste0(".*", file.path("SGP_Report", "assets", "")), "", b), "[/]|[\\]")[[1]], -1), ""), collapse="")
          if (!dir.exists(file.path("HTML", "assets", tmp.new.dir))) {
            dir.create(file.path("HTML", "assets", tmp.new.dir), showWarnings = FALSE, recursive = TRUE)
          }
          new.b <- file.path("HTML", "assets", tmp.new.dir, tail(strsplit(b, "[/]|[\\]")[[1]], 1))
          if (file.copy(file.path("HTML", b), new.b)) file.remove(file.path("HTML", b))
          html.text[find.index] <- gsub(b, sub(file.path("HTML", ""), "", new.b), html.text[find.index], fixed=TRUE)
        }
        if (grepl("Goodness_of_Fit", b)) {
          gof.path <- strsplit(b, "[/]|[\\]")[[1]] # get 2 directories back '../../Goodness_of_Fit'
          gof.path <- paste(file.path(gof.path[(which(gof.path=="Goodness_of_Fit")-2):which(gof.path=="Goodness_of_Fit")], ""), collapse="")
          tmp.new.dir <- file.path(gof.path, head(strsplit(sub(paste0(".*", gof.path), "", b), "[/]|[\\]")[[1]], -1))
          if (!dir.exists(file.path("HTML", "assets", "img", tmp.new.dir))) {
            dir.create(file.path("HTML", "assets", "img", tmp.new.dir), showWarnings = FALSE, recursive = TRUE)
          }
          new.b <- file.path("HTML", "assets", "img", tmp.new.dir, tail(strsplit(b, "[/]|[\\]")[[1]], 1))
          if (file.copy(file.path("HTML", b), new.b)) file.remove(file.path("HTML", b))
          html.text[find.index] <- gsub(b, sub(file.path("HTML", ""), "", new.b), html.text[find.index], fixed=TRUE)
        }
      }
    }
    if(any(grepl(render.dir, html.text))) {
      leftover.text.links <- grep(render.dir, html.text)
      for (lotl in leftover.text.links) {
        results <- regmatches(gsub('\"', "'", html.text[lotl]), regexec(paste0(render.dir, "\\s*(.*?)\\s*'"), gsub('\"', "'", html.text[lotl])))[[1]]
        if (length(results) > 0) {
          old.file <- gsub("(^'+)|('+$)", "", results[grep(render.dir, results)])
        } else next
        tmp.new.dir <- paste(file.path(head(strsplit(sub(file.path(render.dir, ""), "", old.file), "[/]|[\\]")[[1]], -1), ""), collapse="")
        if (!dir.exists(file.path("HTML", "assets", "misc", tmp.new.dir))) {
          dir.create(file.path("HTML", "assets", "misc", tmp.new.dir), showWarnings = FALSE, recursive = TRUE)
        }
        new.file <- file.path("HTML", "assets", "misc", sub(file.path(render.dir, ""), "", old.file))
        if (file.copy(file.path("HTML", old.file), new.file)) file.remove(file.path("HTML", old.file))
        html.text[lotl] <- gsub(old.file, sub(file.path("HTML", ""), "", new.file), html.text[lotl], fixed=TRUE)
      }
    }
    ##  Find remaining render.dir dependencies and move to assets/misc
    ##  https://stackoverflow.com/a/59783181
    if (length(leftover.asset.files <- list.files(file.path("HTML", render.dir), full.names = FALSE, recursive=TRUE)) > 0) {
      lapply(list.files(file.path("HTML", render.dir), include.dirs=TRUE, full.names=TRUE), function(x) {
        fi <- file.info(x)
        if (fi$isdir) {
            f <- list.files(x, all.files=TRUE, recursive=TRUE, full.names=TRUE)
            sz <- sum(file.info(f)$size)
            if (sz==0L) unlink(x, TRUE) # print(x) # as precaution, print to make sure before using unlink(x, TRUE)
        }
      })
    } else unlink(file.path("HTML", render.dir), recursive = TRUE)
  }

  ##  Save cleaned HTML code
  writeLines(html.text, html.file)

  return(NULL)
}### End renderMultiDocument
