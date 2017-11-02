`reportSGP` <- function(
	sgp_object,
	year,
	state=NULL,
	output_directory="Documents/reports",
	output_file_name=NULL,
	content="sgp_report",
	references=TRUE,
	cover_page="SGP_REPORT_COVER_PAGE.tex"
) {

	### Set variables to NULL to prevent R CMD check warnings

	YEAR <- NULL


	### Utility functions

	strtail <- function (s, n = 1) {
		if (n < 0) substring(s, 1 - n) else substring(s, nchar(s) - n + 1)
	}


	### Create state (if NULL) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		state <- SGP::getStateAbbreviation(tmp.name, "reportSGP")
	}


	### Create output_file

	if (is.null(output_file_name)) {
		output_file_name <- paste0(paste(getStateAbbreviation(state, type="LONG"), "SGP_Report", strtail(year, 4), sep="_"), ".Rmd")
	}
	output_file <- file.path(output_directory, output_file_name)



	###  Begin new .Rmd output_file
	if (!dir.exists(dirname(output_directory))) dir.create(dirname(output_directory), showWarnings = FALSE, recursive = TRUE)
	content_bones <- system.file("rmarkdown", "content", content, package = "Literasee")
	cat(readLines(file.path(content_bones, "skeleton.yml")), sep = "\n", file=output_file)

	######
	###			Identify key characteristics of SGP Object - load required packages, etc.
	######

	###  Check for EOCT grades
	eoct.tf <- any(grepl("EOCT", sgp_object@Data[YEAR==year]$GRADE))
	cat(readLines(file.path(content_bones, "SGP_OBJECT_TESTS.Rmd")), sep = "\n", file=output_file, append = TRUE)

	###  Check for SIMEX SGPs

	###  Check for BASELINE SGPs

	###  Check for SGProjections


	######
	###			Add "Bones" to the skeleton .Rmd
	######

	###  Introduction
	cat(readLines(file.path(content_bones, "1_INTRODUCTION.Rmd")), sep = "\n", file=output_file, append = TRUE)

	###  Data
	if (eoct.tf) {
		cat(readLines(file.path(content_bones, "2.2_DATA.Rmd")), sep = "\n", file=output_file, append = TRUE)
	} else cat(readLines(file.path(content_bones, "2.1_DATA.Rmd")), sep = "\n", file=output_file, append = TRUE)

	###  Analytics
	cat(readLines(file.path(content_bones, "3_ANALYTICS.Rmd")), sep = "\n", file=output_file, append = TRUE)

	###  References
	if (references) cat("\n\n#  References \n", file=output_file, append = TRUE)

	### Cover Page

	if (identical(cover_page, "SGP_REPORT_COVER_PAGE.tex")) {
		cat(readLines(file.path(content_bones, cover_page)), sep = "\n", file=file.path(output_directory, cover_page), append = TRUE)
	} else {
		cat(readLines(cover_page), sep = "\n", file=file.path(output_directory, "SGP_REPORT_COVER_PAGE.tex"), append = TRUE)
	}


	###  Scrub-a-dub
	sgp.rmd <- readLines(output_file)
	sgp.rmd <- gsub("year", shQuote(year), sgp.rmd)
	sgp.rmd <- gsub("sgp_object", as.character(as.list(match.call())[["sgp_object"]]), sgp.rmd)
	cat(sgp.rmd, sep = "\n", file=output_file)

} ### END reportSGP
