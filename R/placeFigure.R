placeFigure <- function(
	files,
	rows=1,
	columns=1,
	figure.id=NULL,
	caption=NULL,
	caption.position="top", # "bottom"
	pdf.width=NULL,
	html.width=NULL,
	page.break=FALSE) {

	###  Some checks

	# if (length(files) != as.numeric(rows)*as.numeric(columns)) stop("Number of image file(s) does not match the specified rows/columns configuration.")

	if (is.null(pdf.width)) pdf.width <- 1/columns
	if (is.null(html.width)) html.width <- round((650 - 10*columns)/columns, 0)

	if (tolower(caption.position)=="top") caption.top <- TRUE else caption.top <- FALSE
	###  Universal variables
	if (is.null(caption)) {
		html.caption <- getOption("fig_caption_no_sprintf")
	}	else	html.caption <- gsub("</p>\n", "", gsub("<p>", "", markdown::markdownToHTML(text=Gmisc::figCapNo(caption), fragment.only=TRUE)))
	###  HTML image placement  NOTE:  ALL 'cat()' text must be flush left in R script!

	cat("
<!-- HTML_Start -->\n")

	if (!is.null(caption) & caption.top) {
		cat("
<div class='caption'>", html.caption, "</div>")
	}

	cat(paste0("
<div class='content-node image'", ifelse(!is.null(figure.id), paste0(" id = '", figure.id,"'"), ""), ">"))
	cat("
<div class='image-content'>")

	###  Might need to look into how h6 captions set up to put multiple images in a matrix.

	for (img in files) {
		cat(paste0("
<img src='",img, "' style='width: ", html.width, "px;'/>"))
	}

	if (!is.null(caption) & !caption.top) {
		cat("
<div class='caption'>", html.caption, "</div>")
	}

	cat("
</div>
</div>")

	if (page.break) {
		cat("
<div class='breakboth' ></div>\n")
	} else cat("\n")

	###  LaTeX image placement  NOTE:  ALL 'cat()' text must be flush left in R script!

	tex.caption <- gsub("</p>\n", "", gsub("<p>", "", markdown::markdownToHTML(text=paste0("{", sprintf(getOption("fig_caption_no_sprintf", ""), Gmisc::figCapNoLast(), ""), "} ", caption), fragment.only=TRUE)))
	# 	tex.caption <- paste0(gsub("[*][*]", paste0("\\\\caption*{\\\\label{fig:", figure.id, "} ", "{\\\\bf{"), gsub(":[*][*]", ":}}",
	# 		sprintf(getOption("fig_caption_no_sprintf"), Gmisc::figCapNoLast(), ""))), caption, "}")
	# } else tex.caption <- paste0(gsub("[*][*]", paste0("\\\\caption*{{\\\\bf{"), gsub(":[*][*]", ":}}",
	# 		sprintf(getOption("fig_caption_no_sprintf"), Gmisc::figCapNoLast(), ""))), caption, "}")
	tex.caption <- gsub("<strong>", "{\\\\bf{", gsub("</strong>", "}}", tex.caption))
	tex.caption <- gsub("<em>", "{\\\\textit{", gsub("</em>", "}}", tex.caption))
	tex.caption <- gsub("<sup>th</sup>", "$^{th}$", tex.caption)
	tex.caption <- gsub("<sup>st</sup>", "$^{st}$", tex.caption)
	tex.caption <- gsub("<sup>rd</sup>", "$^{rd}$", tex.caption)
	tex.caption <- gsub("<br></br>", "\\\\", tex.caption)
	if (is.null(getOption("fig_caption_no_sprintf"))) cap.type <- "caption" else cap.type <- "caption*"
	if (!is.null(figure.id)){
		tex.caption <- paste0("\\", cap.type, "{\\label{fig:", figure.id, "} ", tex.caption, "}")
	} else tex.caption <- paste0("\\", cap.type, "{", tex.caption, "}")

	cat("
<!-- LaTeX_Start
\\begin{figure}[H]\n")

	if (caption.top) {
		cat(tex.caption)
	}

	for (img in files) {
		cat(paste0("
  \\begin{subfigure}[b]{", pdf.width, "\\textwidth}"))
		cat(paste0("
    \\includegraphics[width=\\textwidth]{", img, "}
  \\end{subfigure}"))
	}

	if (!caption.top) {
		cat(tex.caption)
	}

	cat("
\\end{figure}\n")

	if (page.break) {
		cat("
\\pagebreak\n")
	}

	cat("
LaTeX_End -->\n")

} ###  END placeFigure
