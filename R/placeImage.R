`placeImage` <- function(
	files,
	rows=1,
	columns=1,
	figure.id=NULL,
	figure.style=NULL, # "'text-align: center; margin-top: 1em;'"
	caption=NULL,
	caption.position="top", # "bottom"
	html.width="100%",
	click.zoom=FALSE,
	page.break=FALSE) {

	###  Some checks
	if (is.null(html.width)) html.width <- round((650 - 10*columns)/columns, 0)
	if (tolower(caption.position)=="top") caption.top <- TRUE else caption.top <- FALSE

	###  Universal variables
	if (is.null(caption)) {
		html.caption <- getOption("fig_caption_no_sprintf")
	}	else	html.caption <- gsub("</p>\n", "", gsub("<p>", "", markdown::markdownToHTML(text=Gmisc::figCapNo(caption), fragment.only=TRUE)))

	###  HTML image placement  NOTE:  ALL 'cat()' text must be flush left in R script!
	# tmp.fig.text <- c("<!-- HTML_Start --><!-- placeFigure -->")
	cat("
<!-- HTML_Start -->\n<!-- placeFigure -->\n")

	cat(paste0("
<div class='figure'", ifelse(!is.null(figure.style), paste0(" style= '", figure.style,"'"), ""), "><span id=fig:", ifelse(!is.null(figure.id), figure.id, paste0("figure", getCounter("figure"))), "></span>"))
	# tmp.fig.text <- c(tmp.fig.text, paste0("<div class='figure'", ifelse(!is.null(figure.style), paste0(" style= '", figure.style,"'"), ""), "><span id=fig:", ifelse(!is.null(figure.id), figure.id, paste0("FIG_", getCounter("figure"))), "></span>"))

	if (!is.null(caption) & caption.top) {
		cat("
<p class='caption'>", html.caption, "</p>")
		# tmp.fig.text <- c(tmp.fig.text, paste0("<p class='caption'>", html.caption, "</p>"))
	}

	if (click.zoom) {
		##  create a grid with zoomable images
		html_files <- files
		suppressWarnings(length(html_files) <- prod(dim(matrix(html_files, ncol = columns))))
		img.mtx <- matrix(html_files, rows, columns, byrow=TRUE)

		cat("
<div class='click-zoom'>
<div class='img-row'>")

		for (k in seq(columns)) {
			cat("
<div class='img-column'>")
			for (img in img.mtx[,k][!is.na(img.mtx[,k])]) {
				cat("
<label>
<input type='checkbox'>", paste0("
<img src='",img, "' style='width: ", html.width, "' data-prefix='Figure'>"), "
</label>")
			}
			cat("
</div><!-- END img-column-->")
		}
		cat("
</div><!-- END img-row -->\n</div><!-- END click-zoom -->\n")
	} else {
		cat(paste0("<img src='", files, "' alt='", paste0("Figure ", getCounter("figure")), "' width= '", html.width, "' data-prefix='Figure'/>"))
		# tmp.fig.text <- c(tmp.fig.text, paste0("<img src='", files, "' alt='", paste0("Figure ", getCounter("figure")), "' width= '", html.width, "' data-prefix='Figure'/>"))
	}

	if (!is.null(caption) & !caption.top) {
		cat("
<div class='caption'>", html.caption, "</div>")
		# tmp.fig.text <- c(tmp.fig.text, paste0("<p class='caption'>", html.caption, "</p>"))
	}

	cat("
</div><!-- END div.figure -->")
	# tmp.fig.text <- c(tmp.fig.text, "</div><!-- END div.figure -->")

	if (page.break) {
		cat("
<div class='breakboth' ></div>\n")
	} else cat("\n")

	# cat(paste0(tmp.fig.text, collapse=""))
} ###  END placeFigure
