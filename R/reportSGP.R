`reportSGP` <- function(
	sgp_object,
	sgp.year,
	output_file="docs/SGP_Report.Rmd",
	content="tech_report",
	references=TRUE
) {

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

	###  References
	if (references) cat("\n\n#  References \n", file=output_file, append = TRUE)


	###  Scrub-a-dub
	sgp.rmd <- readLines(output_file)
	sgp.rmd <- gsub("sgp.year", shQuote(sgp.year), sgp.rmd)
	sgp.rmd <- gsub("sgp_object", as.character(as.list(match.call())[["sgp_object"]]), sgp.rmd)
	cat(sgp.rmd, sep = "\n", file=output_file)

} ### END reportSGP
