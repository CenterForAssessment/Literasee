`reportSGP` <- function(
	state,
	sgp_object,
	report_params,
	output_knit=TRUE
) {

	### Set variables to NULL to prevent R CMD check warnings

	cover_page <- references <- sgp_year <- output_directory <- YEAR <- NULL # Extras here to avoid R CMD check errors temporarily with dev params

###  Params to Build
	# output_directory="Documents/reports"
	# output_file_name=NULL
	# content="sgp_report"
	# license="default"
	# references=TRUE
	# cover_page="default"
	# sgp.year # "2018_2019"
	# sgp.type # SGP, SGP_SIMEX_RANKED, baselines, etc.
	# year # "2019"
	# analytics.org.name/.abv
	# non.state/.org/.org.abv/.type # 'Partnership for Assessment of Readiness for College and Careers', 'P...A...R...C...C... consortium', 'PARCC', type - 'consortium', 'district'
	# state.org/.org/.org.abv/.type # also duplicate with non.state/.abv (just leave non.* null for most cases) state.type defaults to 'state'
	# sgp.model.name/.abv/.url # [Georgia Student Growth Model (GSGM)](http://www.gadoe.org/Curriculum-Instruction-and-Assessment/Assessment/Pages/Georgia-Student-Growth-Model.aspx)
	# appendix/.title/.description # 'Appendix A', 'Goodness of Fit Plots', 'Appendix A displays Goodness of Fit plots for each analysis conducted for Spring 2019.'
	# eoct.tf
	# sgp.cohort.size
	#
	# valid.data.rule.general
	# valid.data.rule.eog
	# valid.data.rule.eoct
	# data.prep.text
	#
	# good.fit.example.grade
	# misfit.example.grade
	# good.fit.example.subject
	# misfit.example.subject
	# good.fit.example.norm.group
	# misfit.example.norm.group
	# good.fit.example.description
	# misfit.example.description

### Utility functions
	"%w/o%" <- function(x,y) x[!x %in% y]

	strtail <- function (s, n = 1) {
		if (n < 0) substring(s, 1 - n) else substring(s, nchar(s) - n + 1)
	}


	### Create state (if NULL) from sgp_object (if possible)

	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		state <- state.abv <- SGP::getStateAbbreviation(tmp.name, "reportSGP")
	}
	state.name <- SGP::getStateAbbreviation(state.abv, type="state")


	### Create output_file

	if (is.null(output_file_name)) {
		output_file_name <- paste0(paste(SGP::getStateAbbreviation(state, type="LONG"), "SGP_Technical_Report", strtail(sgp_year, 4), sep="_"), ".Rmd")
	}
	output_file <- file.path(output_directory, output_file_name)

	if (!dir.exists(dirname(output_directory))) dir.create(dirname(output_directory), showWarnings = FALSE, recursive = TRUE)
	content_bones <- system.file("rmarkdown", "content", content, package = "Literasee")

	cat(readLines(file.path(content_bones, "skeleton.yml")), sep = "\n", file=output_file)


	######
	###			Identify key characteristics of SGP Object - load required packages, etc.
	######

	###  Check for EOCT grades
	eoct.tf <- any(grepl("EOCT", sgp_object@Data[YEAR==sgp_year]$GRADE))
	cat(readLines(file.path(content_bones, "SGP_OBJECT_TESTS.Rmd")), sep = "\n", file=output_file, append = TRUE)

	###  Check for SIMEX SGPs

	###  Check for BASELINE SGPs

	###  Check for SGProjections


	######
	###			Add "Bones" to the skeleton .Rmd
	######

	###  Introduction
	cat(c("\n\n", readLines(file.path(content_bones, "1_INTRODUCTION.Rmd"))), sep = "\n", file=output_file, append = TRUE)

	###  Data
	if (eoct.tf) {
		cat(c("\n\n", readLines(file.path(content_bones, "2_DATA_EOCT.Rmd"))), sep = "\n", file=output_file, append = TRUE)
	} else cat(c("\n\n", readLines(file.path(content_bones, "2_DATA.Rmd"))), sep = "\n", file=output_file, append = TRUE)

	###  Analytics
	cat(c("\n\n", readLines(file.path(content_bones, "3.1_ANALYTICS.Rmd"))), sep = "\n", file=output_file, append = TRUE)

	if (eoct.tf) {
		cat(c("\n\n", readLines(file.path(content_bones, "3.2_ANALYTICS_EOCT.Rmd"))), sep = "\n", file=output_file, append = TRUE)
	} else {
		cat(c("\n\n", readLines(file.path(content_bones, "3.2_ANALYTICS.Rmd"))), sep = "\n", file=output_file, append = TRUE)
	}
	cat(c("\n\n", readLines(file.path(content_bones, "3.3_ANALYTICS.Rmd"))), sep = "\n", file=output_file, append = TRUE)


	###  Goodness of Fit
	cat(c("\n\n", readLines(file.path(content_bones, "4.1_GoFit.Rmd"))), sep = "\n", file=output_file, append = TRUE)

	# Section for examples of good/bad fit (e.g. Georgia)?  Put in manually?

	if (eoct.tf) {
		cat(c("\n\n", readLines(file.path(content_bones, "4.2_GoFit_EOCT.Rmd"))), sep = "\n", file=output_file, append = TRUE)
	} else cat(c("\n\n", readLines(file.path(content_bones, "4.2_GoFit.Rmd"))), sep = "\n", file=output_file, append = TRUE)

	###  References
	if (references) cat("\n\n#  References \n", file=output_file, append = TRUE)

	### Cover Page

	if (identical(cover_page, "default")) {
		file.copy(file.path(content_bones, "SGP_REPORT_COVER_PAGE.tex"), file.path(output_directory, "SGP_REPORT_COVER_PAGE.tex"), overwrite = TRUE)
		file.copy(file.path(content_bones, "images", "CFA_Logo.png"), file.path(output_directory, "images", "CFA_Logo.png"), overwrite = TRUE)
	} else {
		if (!is.null(cover_page)) {
			if (file.exists(cover_page)) {
				file.copy(cover_page, file.path(output_directory, "SGP_REPORT_COVER_PAGE.tex"), overwrite = TRUE)
			} else stop("Cover Page file specified does not exist.")
		}
	}


	if (identical(license, "default")) {
		file.copy(file.path(content_bones, "LICENSE.tex"), file.path(output_directory, "LICENSE.tex"), overwrite = TRUE)
		file.copy(file.path(content_bones, "images", "doclicense-CC-by-sa.pdf"), file.path(output_directory, "images", "doclicense-CC-by-sa.pdf"), overwrite = TRUE)
	} else {
		if (!is.null(license)) {
			if (file.exists(license)) {
				file.copy(license, file.path(output_directory, "LICENSE.tex"), overwrite = TRUE)
			} else stop("License file specified does not exist.")
		}
	}


	###  Scrub-a-dub
	sgp.rmd <- readLines(output_file)
	sgp.rmd <- gsub("sgp_year", shQuote(sgp_year), sgp.rmd)
	sgp.rmd <- gsub("XXX-SGP_STATE-XXX", SGP::getStateAbbreviation(state, type="Long"), sgp.rmd)
	if (!output_knit) sgp.rmd <- gsub("sgp_object", as.character(as.list(match.call())[["sgp_object"]]), sgp.rmd)
	if (is.null(cover_page) & is.null(license)) sgp.rmd <- sgp.rmd[-grep("includes:", sgp.rmd)]
	if (is.null(cover_page)) sgp.rmd <- sgp.rmd[-grep("in_header: \"SGP_REPORT_COVER_PAGE.tex\"", sgp.rmd)]
	if (is.null(license)) sgp.rmd <- sgp.rmd[-grep("before_body: \"LICENSE.tex\"", sgp.rmd)]

	cat(sgp.rmd, sep = "\n", file=output_file)

	if (output_knit) knitr::knit(input=output_file, output=output_file)

} ### END reportSGP
