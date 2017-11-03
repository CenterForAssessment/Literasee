`reportSGP` <- function(
	sgp_object,
	sgp.year,
	output_file="docs/SGP_Report.Rmd",
	content="tech_report",
	GoFit.Examples=NULL, # file paths for examples of good fit/misfit
	references=TRUE,
) {

	### Set variables to NULL to prevent R CMD check warnings
	YEAR <- NULL

	###  Begin new .Rmd output_file
	if (!dir.exists(dirname(output_file))) dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
	content_bones <- system.file("rmarkdown", "content", content, package = "Literasee")
	cat(readLines(file.path(content_bones, "skeleton.yml")), sep = "\n", file=output_file)

	######
	###			Identify key characteristics of SGP Object - load required packages, etc.
	######

	###  Check for EOCT grades
	eoct.tf <- any(grepl("EOCT", sgp_object@Data[YEAR==sgp.year]$GRADE))
	cat(readLines(file.path(content_bones, "SGP_Object_Tests.Rmd")), sep = "\n", file=output_file, append = TRUE)

	###  Check for SIMEX SGPs

	###  Check for BASELINE SGPs

	###  Check for SGProjections


	######
	###			Add "Bones" to the skeleton .Rmd
	######

	###  Introduction
	cat(readLines(file.path(content_bones, "1_INTRO.Rmd")), sep = "\n", file=output_file, append = TRUE)

	###  Data
	if (eoct.tf) {
		cat(readLines(file.path(content_bones, "2.2_DATA.Rmd")), sep = "\n", file=output_file, append = TRUE)
	} else cat(readLines(file.path(content_bones, "2.1_DATA.Rmd")), sep = "\n", file=output_file, append = TRUE)

	###  Analytics
	cat(readLines(file.path(content_bones, "3_ANALYTICS.Rmd")), sep = "\n", file=output_file, append = TRUE)

	###  Goodness of Fit
	cat(readLines(file.path(content_bones, "4_GoFIT.Rmd")), sep = "\n", file=output_file, append = TRUE)

	if (!is.null(GoFit.Examples)) {

	cat("
	As an example, Figure `r getCounter('figure')+1` shows exemplary model fit.
	Figure `r getCounter('figure')+2` demonstrates minor model misfit.",
	file=output_file, append = TRUE)

	cat("
	```{r, results='asis', echo=FALSE, G8_Marginal_Distributions}
		placeFigure(
			files = './img/Math_G8_Marginal_Distributions-2_Prior.pdf',
			caption = 'Comparison of the Uniformity of Distributions for 8<sup>th</sup> Grade Mathematics Estimates.')
	```
	")

	}
	
	###  References
	if (references) cat("\n\n#  References \n", file=output_file, append = TRUE)


	###  Scrub-a-dub
	sgp.rmd <- readLines(output_file)
	sgp.rmd <- gsub("sgp.year", shQuote(sgp.year), sgp.rmd)
	sgp.rmd <- gsub("sgp_object", as.character(as.list(match.call())[["sgp_object"]]), sgp.rmd)
	cat(sgp.rmd, sep = "\n", file=output_file)

} ### END reportSGP
