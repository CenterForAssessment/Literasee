renderMultiDocument <- function (
  rmd_input,
  report_format = c("HTML", "PDF"), # Can also produce "EPUB", "DOCX"
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
  pandoc_args = NULL, # "--webtex" for LaTeX math in both HTML and PDF
  ...) {

	###  Checks of alternative css and/or pandoc template done in externalized functions

  ###  Check for HTML routine request, or at least required master .md output

  if (any(c("EPUB", "PDF", "DOCX") %in% toupper(report_format)) & !"HTML" %in% toupper(report_format) & !file.exists(file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)))) {
    if (!file.exists(file.path("HTML", gsub(".Rmd", ".html", rmd_input, ignore.case=TRUE)))) {
        message("\n\tThe file ", file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)), " was not found, but is required for
          report_format ", paste(report_format, collapse=", "), ". The 'HTML' step will be added to 'report_format' and run.
          NOTE: Document ", file.path("HTML", gsub(".Rmd", ".html", rmd_input, ignore.case=TRUE)), " will be also created.\n")
        report_format <- c("HTML", report_format)
    } else stop("\n\tThe file ", file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)), " was not found, but is required for
        report_format ", paste(report_format, collapse=", "), ". Add 'HTML' the 'report_format' argument and re-run.
        NOTE: Document ", file.path("HTML", gsub(".Rmd", ".html", rmd_input, ignore.case=TRUE)), " will be overwritten when re-run.\n")
  }

  ###
  ###  Render HTML (and master .md file)
  ###

  if ("HTML" %in% toupper(report_format)) {
  	renderHTML(input=rmd_input, number_sections, number_section_depth, toc, toc_depth,
  						 self_contained, dev, literasee_template, html_template, html_css, bibliography, csl, pandoc_args)
  }

  if ("EPUB" %in% toupper(report_format)) {
    renderEPUB(input=rmd_input, cover_img, add_cover_title, number_sections, convert_header_levels,
               literasee_template, epub_template, epub_css, bibliography, csl, pandoc_args)
  }

  if ("PDF" %in% toupper(report_format)) {
    renderPDF(input=rmd_input, keep_tex=cleanup_aux_files, number_sections, convert_header_levels,
    					literasee_template, pdf_template, bibliography, csl, pandoc_args)
  }

  if ("DOCX" %in% toupper(report_format)) {
    renderDOCX(input=rmd_input, self_contained=docx_self_contained,
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
    for(tmp.dir in report_format) {
      unlink(file.path(tmp.dir, "markdown"), recursive = TRUE)
    }
  }
}### End renderMultiDocument
