#' Create a new report using the NCIEA RMarkdown template
#'
#' @param file_name The name of the file for your RMarkdown document. Defaults to "report" and creates a file called report.Rmd
#' @param type_of_report The type of report you're creating ("General", "Simple Report", or "Cleaning")
#'
#' @return
#' @export
#'
#' @examples
new_nciea_report <- function(file_name = "new_report",
                            type_of_report = "nciea_report") {

  if (is.null(type_of_report) | toupper(type_of_report) %in% c("NCIEA_REPORT", "GENERAL")) {
    type_of_report = "nciea_report"
  } else {
    stop(paste(shQuote(toupper(type_of_report)), "not supported.  Please select", shQuote("NCIEA_REPORT"), "or", shQuote("GENERAL")))
  }

  rmarkdown::draft(file = stringr::str_glue("{file_name}.Rmd"),
                   template = type_of_report,
                   package = "Literasee",
                   create_dir = FALSE,
                   edit = FALSE)
}
