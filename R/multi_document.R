multi_document <- function (
  ...,
  number_sections = TRUE,
  number_section_depth=3,
  toc = TRUE,
  toc_depth = 2,
  self_contained = TRUE,
  dev = "png",
  literasee_template = "sgp_report",
  html_template = "default",
  css = "default",
  bibliography = "default",
  csl = "default",
  md_extensions=NULL,
  pandoc_args = NULL) {

	scrubHeaders <- function(output_str, header_levels=header_levels) {
		for (header.level in header_levels) {
			index <- grep(paste0("</h", header.level, ">"), output_str)
			for (i in index) output_str[i] <- paste("<h", header.level, ">", gsub("<span.*|.*span>", "", output_str[i]), sep="")
		}
		return(output_str)
	}

	### Initial checks of alternative css and/or pandoc template

  ##  CSS check from Gmisc--docx_document - credit to Max Gordon/Gforge https://github.com/gforge
  if (any(css != "default")) {
    if (!all(sapply(css, file.exists))) {
      alt_css <- list.files(pattern = ".css$")
      if (length(alt_css) > 0) {
        alt_css <- paste0("\n You do have alternative file name(s) in current directory that you may intend to use.",
                          " You may want to have a YAML section that looks something like:",
                          "\n---", "\noutput:", "\n  Literasee::multi_document:",
                          "\n    css: \"", paste(alt_css, collapse = "\", \""),
                          "\"", "\n---")
      } else {
        alt_css <- ""
      }
      stop("One or more of the css-file(s) that you've specified can't be identified.",
           "The file(s) '", paste(css[!sapply(css, file.exists)],
                                   collapse = "', '"), "'", " can't be found from the directory '",
           getwd(), "'", " - i.e. the directory where you have your .Rmd-file", alt_css)
    }
  } else {
    css <- system.file("rmarkdown", "templates", literasee_template, "resources", "html_report.css" , package = "Literasee")
    if (!self_contained) {
      if (!dir.exists(file.path("HTML", "assets", "css"))) {
        dir.create(file.path("HTML", "assets", "css"), showWarnings = FALSE, recursive = TRUE)
      }
      file.copy(css, file.path("HTML", "assets", "css"))
      css <- file.path("HTML", "assets", "css", tail(strsplit(css, "[/]|[\\]")[[1]], 1))
    }
  }
  if (length(css)>1 & any(css != "default")) {
    css <- c(css, system.file("rmarkdown", "templates", literasee_template, "resources", "html_report.css" , package = "Literasee"))
  }
  css <- unique(css)

  ##  Check csl file
  if (!is.null(csl)) {
    if (csl != "default") {
      if (!file.exists(csl)) {
        stop("The csl file that you've specified can't be found in the file path provided.")
      } else {
      	pandoc_args <- c(pandoc_args, "--csl", csl)
      	csl <- NULL
      }
    } else {
    	pandoc_args <- c(pandoc_args, "--csl", system.file("rmarkdown", "content", "bibliography", "apa-5th-edition.csl" , package = "Literasee"))
    	csl <- NULL
    }
  }

  if (html_template != "default") {
    if (!file.exists(html_template)) {
      stop("The html_template file that you've specified can't be found in the directory '",
           getwd(), "'", " - i.e. the directory where you have your .Rmd-file")
    }
  } else html_template <- system.file("rmarkdown", "templates", literasee_template, "resources", "html_report.html" , package = "Literasee")

  ### Bibliography

  if (!is.null(bibliography)) {
  	my.pandoc_citeproc <- pandoc_citeproc()
    if (bibliography == "default") {
      pandoc_args <-c(pandoc_args, "--filter", my.pandoc_citeproc, "--bibliography",
                      system.file("rmarkdown", "content", "bibliography", "Literasee.bib" , package = "Literasee"))
      rm(bibliography)
    } else {
      if(file.exists(bibliography)) {
        pandoc_args <-c(pandoc_args, "--filter", my.pandoc_citeproc, "--bibliography", bibliography)
        rm(bibliography)
      } else stop("'bibliography' file not found.")
    }
  }
  output_ret_val <- html_document(toc=toc, toc_depth=toc_depth, number_sections=number_sections, section_divs = number_sections,
  											dev=dev, self_contained=self_contained, theme=NULL, mathjax=NULL, template=html_template, css=css,
                        keep_md=TRUE, copy_resources=!self_contained, md_extensions = md_extensions, pandoc_args=pandoc_args, ...)
  # if (!self_contained) output_ret_val$pandoc$args <- grep("--standalone", output_ret_val$pandoc$args, invert=TRUE, value=TRUE)
  output_ret_val$post_processor_old <- output_ret_val$post_processor
  output_ret_val$post_processor <- post_processor <- function(
      metadata, input_file, output_file, clean, verbose, old_post_processor = output_ret_val$post_processor_old) {
    output_file <- old_post_processor(metadata = metadata, input_file = input_file,
                    output_file = output_file, clean = clean, verbose = verbose)
    output_str <- readLines(output_file, warn = FALSE, encoding = "UTF-8")
    output_str <- scrubHeaders(output_str, header_levels=(which(1:6 > number_section_depth)))
    writeLines(output_str, output_file, useBytes = TRUE)
    return(output_file)
  }
  return(output_ret_val)
}### End multi_document
