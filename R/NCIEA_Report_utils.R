################################################################################
#####                                                                      #####
###  Ultility functions for setting up or updating a NCIEA report directory  ###
#####                                                                      #####
################################################################################

setupReportDirectory <- function(
  dir=getwd(),
  new.report=TRUE,
  update.assets=FALSE,
  custom.content=TRUE,
  custom.content.path,
  overwrite.custom=FALSE) {
  if (!identical(dir, getwd())) {
    tmp.dir <- getwd()
    setwd(dir)
    change.wd <- TRUE
  } else change.wd <- FALSE

  if (new.report) {
    Literasee::new_nciea_report()
    unlink("./new_report.Rmd") # delete skeleton.Rmd
  } else {
    if (update.assets) {
      updateAssets()
    }
  }

  if (custom.content) {
    #  Create a directory for customized RMD content
    if (!dir.exists(file.path("assets", "rmd", "Custom_Content"))) dir.create(file.path("assets", "rmd", "Custom_Content"))

    #  Copy existing templates for custom content.  Could also copy from existing (similar) state report
    file.copy(
      list.files(custom.content.path, full.names=TRUE),
      file.path("assets", "rmd", "Custom_Content"), recursive=TRUE, overwrite=overwrite.custom)
  }

  if (change.wd) {
    setwd(tmp.dir)
  }
}

updateAssets <- function(asset.type=c("css", "js", "pandoc", "rmd"), asset.directory = "assets") {
  if ("css" %in% asset.type) {
    if (!dir.exists(file.path(asset.directory, "css"))) dir.create(file.path(asset.directory, "css"), recursive = TRUE)
    file.copy(
      list.files(system.file("rmarkdown", "templates", "nciea_report", "skeleton", "assets", "css" , package = "Literasee"), full.names=TRUE),
      file.path(asset.directory, "css"), recursive = TRUE)
  }
  if ("js" %in% asset.type) {
    if (!dir.exists(file.path(asset.directory, "js"))) dir.create(file.path(asset.directory, "js"), recursive = TRUE)
    file.copy(
      list.files(system.file("rmarkdown", "templates", "nciea_report", "skeleton", "assets", "js" , package = "Literasee"), full.names=TRUE),
      file.path(asset.directory, "js"), recursive = TRUE)
  }
  if ("pandoc" %in% asset.type) {
    if (!dir.exists(file.path(asset.directory, "pandoc"))) dir.create(file.path(asset.directory, "pandoc"), recursive = TRUE)
    file.copy(
      list.files(system.file("rmarkdown", "templates", "nciea_report", "skeleton", "assets", "pandoc" , package = "Literasee"), full.names=TRUE),
      file.path(asset.directory, "pandoc"), recursive = TRUE)
  }
  if ("rmd" %in% asset.type) {
    if (!dir.exists(file.path(asset.directory, "rmd"))) dir.create(file.path(asset.directory, "rmd"), recursive = TRUE)
    file.copy(
      list.files(system.file("rmarkdown", "templates", "nciea_report", "skeleton", "assets", "rmd" , package = "Literasee"), full.names=TRUE),
      file.path(asset.directory, "rmd"), recursive = TRUE)
  }
  if ("images" %in% asset.type) {
    if (!dir.exists(file.path(asset.directory, "images"))) dir.create(file.path(asset.directory, "images"), recursive = TRUE)
    file.copy(
      list.files(system.file("rmarkdown", "templates", "nciea_report", "skeleton", "assets", "images" , package = "Literasee"), full.names=TRUE),
      file.path(asset.directory, "images"), recursive = TRUE)
  }
}

getPath <- function(path=".", relation=".") {
  tmp.wd <- getwd()
  setwd(path)
  requested.path <- normalizePath(relation)
  setwd(tmp.wd)
  return(requested.path)
}
