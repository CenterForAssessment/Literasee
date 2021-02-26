gofPrint <- function(
  sgp_object,
  years=NULL,
  content_areas=NULL,
  grades=NULL,
  norm_group=NULL,
  sgp_types=NULL,
  output_format=c("PDF", "PNG", "DECILE_TABLES")) {

  CONTENT_AREA <- YEAR <- SGP_TYPE <- NULL

  if (is.null(years) & is.null(content_areas) & is.null(sgp_types)) {
    tmp.gof <- names(sgp_object@SGP[["Goodness_of_Fit"]])
  } else {
    all.gof <- sapply(names(sgp_object@SGP[["Goodness_of_Fit"]]), strsplit, "[.]", USE.NAMES=FALSE)
    all.gof <- lapply(lapply(all.gof, matrix, nrow=1), data.table)
    all.gof <- rbindlist(all.gof, fill=TRUE)
    if (ncol(all.gof) < 3) all.gof[, "V3" := as.character(NA)]
    setnames(all.gof, c("V1", "V2", "V3"), c("CONTENT_AREA", "YEAR", "SGP_TYPE"))

    if (is.null(years)) years <- unique(all.gof$YEAR)
    if (is.null(content_areas)) content_areas <- unique(all.gof[YEAR %in% years, CONTENT_AREA])
    if (is.null(sgp_types)) sgp_types <- unique(all.gof[YEAR %in% years & CONTENT_AREA %in% content_areas, SGP_TYPE])

    tmp.gof <- all.gof[CONTENT_AREA %in% content_areas & YEAR %in% years & SGP_TYPE %in% sgp_types]
    tmp.gof <- unlist(lapply(seq(nrow(tmp.gof)), function(f) paste(c(tmp.gof[f,])[c(!is.na(tmp.gof[f,]))], collapse=".")))
  }

  for (i in tmp.gof) {
    if (length(sgp_object@SGP[["Goodness_of_Fit"]][[i]]) > 0L) {
      if ("DECILE_TABLES" %in% output_format) dec.tbl <- "/Decile_Tables" else dec.tbl <- NULL
      dir.create(paste0("Goodness_of_Fit/", i, dec.tbl), recursive=TRUE, showWarnings=FALSE)
      grobs.to.print <- names(sgp_object@SGP[["Goodness_of_Fit"]][[i]])
      if (!is.null(grades)) {
        grobs.to.print <- grep(paste(paste(strsplit(i, "[.]")[[1]][2], sub("MATHEMATICS", "MATH", strsplit(i, "[.]")[[1]][1]), grades, sep="_"), collapse="|"), grobs.to.print, value=TRUE)
      }
      if (!is.null(norm_group)) {
        grobs.to.print <- grep(paste(norm_group, collapse="|"), grobs.to.print, value=TRUE)
      }

      for (j in grobs.to.print) {
        tmp.path <- file.path("Goodness_of_Fit", i, j)
        if (!identical(.Platform$OS.type, "unix") & nchar(tmp.path) > 250L) {
          tmp.content_area <- unlist(strsplit(j, "[.]"))[1L]
          tmp.path <- gsub(tmp.content_area, substr(tmp.content_area, 1, 1), tmp.path)
        }
        if ("PDF" %in% output_format) {
          grDevices::pdf(file=paste0(tmp.path, ".pdf"), width=8.5, height=11)
          if ("PLOT" %in% names(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])) {
            grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]][["PLOT"]])
          } else grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
          dev.off()
        }
        if ("PNG" %in% output_format) {
          Cairo::Cairo(file=paste0(tmp.path, ".png"),
                width=8.5, height=11, units="in", dpi=144, pointsize=10.5, bg="transparent")
          if ("PLOT" %in% names(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])) {
            grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]][["PLOT"]])
          } else grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
          dev.off()
        }
        if ("DECILE_TABLES" %in% output_format) {
          decile.table <- sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]][["TABLE"]]
          save(decile.table, file=paste0("Goodness_of_Fit/", i, "/Decile_Tables/", j, "_Decile_Table.Rdata"))
        }
      }
    } else {
      message(paste0("\tNOTE: No Goodness of Fit tables available to print for ", i, ". No tables will be produced."))
    }
  }
}
