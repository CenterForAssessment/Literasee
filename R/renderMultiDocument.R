renderMultiDocument <- function (
  rmd_input,
  report_format = c("HTML", "PDF"), # Can also produce "RMD", "EPUB" and "DOCX"
  make_draft_final=FALSE,
  cleanup_aux_files = TRUE,
  number_sections = TRUE,
  number_section_depth=3,
  convert_header_levels=c(5,6),
  toc = TRUE,
  toc_depth = 2,
  self_contained = TRUE,
  docx_self_contained = FALSE,
  dev="png",
  literasee_template = "sgp_report",
  html_template = "default",
  epub_template = "default", # Currently uses pandoc epub template.
  pdf_template = "default",
  html_css = "default",
  epub_css = "default",
  docx_css = "default",
  cover_img = NULL,
  add_cover_title = FALSE,
  bibliography = "default",
  csl = "default",
  md_extensions=NULL,
  pandoc_args = NULL, # "--webtex" for LaTeX math in both HTML and PDF
  report_params = NULL,
  ...) {

  ##
  ###   Draft functionality
  ##

  ###  Check for make_draft_final and 'RMD' report_format requested at same time.  Should be done seperately.
  if (make_draft_final & "RMD" %in% toupper(report_format)) {
    stop("Both draft rendering and making draft final (report_format = c('RMD', ...), make_draft_final=TRUE) can not/should not be run simultaneously.")
  }

  ###  Make changes and copy files required when make_draft_final = TRUE
  if (make_draft_final) {
    tmp.messages <- "\t#####   Argument make_draft_final = TRUE provided.  The Following changes have been made:   #####\n\n"
    #  Force working directory to top level 'SGP_Report'
    if (tail(strsplit(getwd(), "[/]|[\\]")[[1]], 1) == "Draft") {
      tmp.messages <- c(tmp.messages, "\t\tWorking directory changed from 'Draft' to main dir (e.g. 'SGP_Report')\n")
      setwd("..")
    }
    #  Make sure rmd_input is set as the .Rmd draft doc
    if (grepl("[/]Draft[/]|Draft[/]", rmd_input)) {
      tmp.messages <- c(tmp.messages, "\t\t'Draft/' removed from rmd_input argument (e.g. 'Draft/My_Report-Compiled_Draft.Rmd' -> 'My_Report-Compiled_Draft.Rmd')\n")
      rmd_input <- tail(strsplit(rmd_input, "[/]|[\\]")[[1]], 1)
    }
    if (!file.exists(file.path("Draft", rmd_input))) {
      message(tmp.messages)
      stop(paste("File", rmd_input, "does not exist in the 'Draft' directory."))
    }
    #  Copy draft .Rmd to main directory (renaming from '*-Compiled_Draft.Rmd' to '*-Final_Draft.Rmd')
    new.rmd.name <- gsub("-Compiled_Draft.Rmd", "-Final_Draft.Rmd", rmd_input)
    tmp.messages <- c(tmp.messages, paste0("\t\t'Draft/", rmd_input,"' copied to main directory and renamed '", new.rmd.name, "')\n"))
    if (file.exists(new.rmd.name)) tmp.messages <- c(tmp.messages, paste("\t\t\tPrevious version of", new.rmd.name, "has been overwritten.\n"))
    if (!file.copy(file.path("Draft", rmd_input), file.path(new.rmd.name), overwrite = TRUE)) {
      message(tmp.messages)
      stop(paste("File", new.rmd.name, "could not be copied/overwritten."))
    } else file.remove(file.path("Draft", rmd_input))
    rmd_input <- new.rmd.name
    #  Copy Draft/Assets files
    if (file.exists(draft.assets <- file.path("Draft", "assets"))) {
      tmp.messages <- c(tmp.messages, "\t\t'Draft/assets/* subdirectories and files moved (overwritten) to main directory /assets/*.\n")
      if (!dir.exists("assets")) {
        dir.create("assets", showWarnings=FALSE)
      }
      common.dirs <- intersect(list.dirs("assets", full.names=FALSE), list.dirs(draft.assets, full.names=FALSE))
      common.dirs <- common.dirs[common.dirs != ""]
      if (length(common.dirs) > 0) {
        tmp.messages <- c(tmp.messages, paste0("\t\t\t", file.path(tail(strsplit(getwd(), "[/]|[\\]")[[1]], 1), "assets", common.dirs),
                          "/ subdirectory already exists and files have been overwritten.\n"))
      }
      if (!all(dir.copy.tf <- file.copy(list.dirs(draft.assets, recursive = FALSE), file.path("assets"), recursive = TRUE))) {
        message(tmp.messages)
        stop(paste("Directory/files in", list.dirs(draft.assets, recursive = FALSE)[!dir.copy.tf], "could not be copied/overwritten."))
      } else unlink(file.path("Draft"), recursive = TRUE)
      tmp.messages <- c(tmp.messages, "\t\t'Draft/* directory and subdirectories removed.\n")
    }
    ###   Rename original from '*.Rmd' to '*-Parent.Rmd')
    ###   Not necessary?  Maybe re-add later...
    # orig.rmd.name <- gsub("-Final_Draft.Rmd", ".Rmd", rmd_input)
    # parent.rmd.name <- gsub(".Rmd", "-Parent.Rmd", orig.rmd.name)
    # if (file.exists(orig.rmd.name)) tmp.messages <- c(tmp.messages, paste0("\t\t\tOriginal ('parent') file '", orig.rmd.name, "' has been renamed '",parent.rmd.name, "'.\n"))
    # if (!file.rename(orig.rmd.name, parent.rmd.name)) {
    #   message(tmp.messages)
    #   stop(paste0("File '", orig.rmd.name, "' could not be renamed."))
    # }
    message(tmp.messages) # Re-issue warnings at end of function since knitr/render have so much console output
  }

  ##
  ###   Pre-render .Rmd input if 'HTML' not in report_format
  ##

  base_input <- rmd_input
  if (!"HTML" %in% toupper(report_format)) { # (!"HTML" %in% toupper(report_format))|("RMD" %in% toupper(report_format))
    ###  Create the RMD folder Structure if doesn't already exist
    if (!dir.exists(file.path("Markdown", "raw_knitted"))) {
      dir.create(file.path("Markdown", "raw_knitted"), recursive=TRUE, showWarnings=FALSE)
    }

    ###  Get params from YAML if not supplied (works 'magically' in knit - not sent in as argument...)
    if (is.null(report_params)) {
      file <- file(file.path(".", rmd_input))
      params <- knitr::knit_params(read_utf8(file))
      close(file)
      if (length(params)==0) params <- NULL
    } else params <- report_params

    ###  Use knitr::knit to render pre-processed .Rmd file with all text and analyses, tables, figures etc. computed
    base_input <- file.path("Markdown", "raw_knitted", sub("[.]Rmd", "-Knitted.Rmd", rmd_input, ignore.case=TRUE))
    knitr::knit(rmd_input, output=base_input)
  }

  ##
  ###   Polished document functionality
  ##

  ###  Check for HTML routine request, or at least required master .md output
  # if (any(c("EPUB", "PDF", "DOCX") %in% toupper(report_format)) & !"HTML" %in% toupper(report_format) & !file.exists(file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)))) {
  #   if (!file.exists(file.path("HTML", gsub(".Rmd", ".html", rmd_input, ignore.case=TRUE)))) {
  #       message("\n\tThe file ", file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)), " was not found, but is required for
  #         report_format ", paste(report_format, collapse=", "), ". The 'HTML' step has been added to 'report_format' and run.
  #         NOTE: Document ", file.path("HTML", gsub(".Rmd", ".html", rmd_input, ignore.case=TRUE)), " will be also created.\n")
  #       report_format <- c("HTML", report_format)
  #   } else stop("\n\tThe file ", file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)), " was not found, but is required for
  #       report_format ", paste(report_format, collapse=", "), ". Add 'HTML' the 'report_format' argument and re-run.
  #       NOTE: Document ", file.path("HTML", gsub(".Rmd", ".html", rmd_input, ignore.case=TRUE)), " will be overwritten when re-run.\n")
  # }


  ###  Render HTML (and master .md file)
  if ("HTML" %in% toupper(report_format)) {
  	renderHTML(input=rmd_input, number_sections, number_section_depth, toc, toc_depth,
  						 self_contained, dev, literasee_template, html_template, html_css, bibliography, csl, md_extensions, pandoc_args, report_params=report_params)
  }

  ###  Render 'Draft' to .Rmd with post processing
  if ("RMD" %in% toupper(report_format)) {
    if ("HTML" %in% toupper(report_format)) {
      #  'renderRMD' not set up to use 'HTML' rendered .md file, but use it if rendering at same time.
      pre_rendered_input <- file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE))
    } else pre_rendered_input <- base_input

    renderRMD(input=pre_rendered_input, report_params)
  }

  ###  Render EPUB/PDF/DOCX using HTML master .md file (if available - if not, -Knitted.Rmd)
  if ("EPUB" %in% toupper(report_format)) {
    renderEPUB(input=base_input, cover_img, add_cover_title, number_sections, convert_header_levels,
               literasee_template, epub_template, epub_css, bibliography, csl, pandoc_args)
  }

  if ("PDF" %in% toupper(report_format)) {
    renderPDF(input=base_input, keep_tex=!cleanup_aux_files, number_sections, convert_header_levels,
    					literasee_template, pdf_template, bibliography, csl, pandoc_args)
  }

  if ("DOCX" %in% toupper(report_format)) {
    renderDOCX(input=base_input, self_contained=docx_self_contained,
    					 number_sections, number_section_depth, docx_css, bibliography, csl, pandoc_args)
    message("\n\n\tDOCX rendering is complete.  The output is the file \n\n\t",
            file.path("DOCX", gsub(".Rmd", "-docx.html", rmd_input, ignore.case=TRUE)), "\n\t
        In order to create a .docx file from it, you must serve the .html file
        and then copy and paste from the web page to a blank Word document.
        see http://gforge.se/2013/12/fast-track-publishing-using-knitr-part-i/
        for more detail.

        One can now serve local web pages/sites using the R package 'servr'.
        For example, issue the command 'servr::httd(dir = \".\",port=4444)'
        at the prompt (assuming you have 'servr' installed) and then go to\n",
            paste("\n\thttp://localhost:4444/DOCX/", gsub(".Rmd", "-docx.html", rmd_input, ignore.case=TRUE), sep=""),
            "\n\n\tin a web browser. Chrome seems to have the best results, at least in Mac OSX
        (Although Safari preserves math images created by --webtex in pandoc).
        This should preserve all images, formatting, etc. when you copy and paste in Word.

        NOTE: You may need to change the port number in the call to httd(...).
        NOTE: Setting docx_self_contained = TRUE creates self contained documents that
              some browsers will retain images in the copy/paste to Word.")
  }

  if (cleanup_aux_files) {
    unlink("Markdown", recursive = TRUE)
    for(tmp.dir in gsub("RMD", "Draft", toupper(report_format))) {
      unlink(file.path(tmp.dir, "markdown"), recursive = TRUE)
    }
  }
  if (make_draft_final) message(tmp.messages) # Re-issue warnings at end of function since knitr/render have so much console output
}### End renderMultiDocument
