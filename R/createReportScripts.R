################################################################################
#####                                                                      #####
###    Function for creating parent/top-level report .Rmd and .yml scripts   ###
#####                                                                      #####
################################################################################

createReportScripts <- function(report_config, rmd_file_list, bookdown=TRUE, pagedown=TRUE) {
  # require(ymlthis) # need `%>%` pipe exported

  ###   Determine file order and paths before any YAML

  if (length(rmd_file_list$report$file.order) != length(c(rmd_file_list$report.source$universal, rmd_file_list$report.source$custom))) {
    stop("Length of rmd_file_list$report$file.order and all rmd_file_list$report.source must be the same!")
  }
  if (length(intersect(rmd_file_list$report.source$universal, rmd_file_list$report.source$custom)) != 0) {
    stop("There is overlap between rmd_file_list$report.source$universal and rmd_file_list$report.source$custom")
  }
  if (is.null(rmd_file_list$report$references)) rmd_file_list$report$references <- FALSE

  tmp.report.paths <- vector(mode = "character", length = length(rmd_file_list$report$file.order))
  tmp.report.paths[rmd_file_list$report.source$universal] <- report_config$params$unvrsl.rmd.path
  tmp.report.paths[rmd_file_list$report.source$custom] <- report_config$params$custom.rmd.path
  report.files <- file.path(tmp.report.paths, rmd_file_list$report$file.order)

  if (length(rmd_file_list$appendices) > 0) {
    appdx.tf <- TRUE
    appdx.files <- list()
    for (apdx in seq(length(rmd_file_list$appendices))) {
      tmp.appdx.paths <- vector(mode = "character", length = length(rmd_file_list$appendices[[apdx]]$file.order))
      tmp.appdx.paths[rmd_file_list$appendices[[apdx]]$file.source$universal] <- report_config$params$unvrsl.rmd.path
      tmp.appdx.paths[rmd_file_list$appendices[[apdx]]$file.source$custom] <- report_config$params$custom.rmd.path
      appdx.files[[apdx]] <- file.path(tmp.appdx.paths, rmd_file_list$appendices[[apdx]]$file.order)
    }
  } else appdx.tf <- FALSE

  ###   Output information

  ##    Determine pagedown title (can be supplied or automatic based on title)
  if (is.null(report_config$output$bookdown$directory)) {
    bd.output.dir <- "site"
  } else bd.output.dir <- report_config$output$bookdown$directory

  if (is.null(report_config$output$pagedown$directory)) {
    pd.output.dir <- "report"
  } else pd.output.dir <- report_config$output$pagedown$directory

  if (is.null(report_config$output$pagedown$file)) {
    pd.output.file <- gsub(" ", "_", report_config$top.level$title)
  } else pd.output.file <- report_config$output$pagedown$file


  ###   Top-level YAML used in both bookdown and pagedown

  top.yml <-
    ymlthis::yml_empty() %>%
      ymlthis::yml_title(addQuote(report_config$top.level$title)) %>%
      ymlthis::yml_subtitle(addQuote(report_config$top.level$subtitle)) %>%
      ymlthis::yml_author(
          name=addQuote(report_config$top.level$author.names),
          affiliation=addQuote(report_config$top.level$author.affil)) %>%
      ymlthis::yml_date(ifelse(is.null(report_config$top.level$date), format(Sys.time(), format = "%B %Y"), addQuote(report_config$top.level$date)))

  #####
  ###   Bookdown files
  #####

  ###   index.Rmd
  if (bookdown) {
    index.rmd.yml <- top.yml %>%
      ymlthis::yml_toplevel(favicon = "assets/images/favicon.png")
    if (report_config$top.level$executive.summary) {
      index.rmd.yml <- ymlthis::yml_toplevel(.yml=index.rmd.yml, `abstract-title`= "Executive Summary")
    }

    bd.downloads <- paste0("[\"downloads/", paste0(pd.output.file, ".pdf"), "\", \"Report\"]")
    if (appdx.tf) {
      for (apdx in seq(length(rmd_file_list$appendices))) {
        tmp.apdx.label <- names(rmd_file_list$appendices[apdx])
        bd.downloads <- c(bd.downloads, paste0("[\"downloads/", paste0(gsub(" ", "_", rmd_file_list$appendices[[apdx]]$title), "_APPENDIX_",  tmp.apdx.label, ".pdf"), "\", \"Appendix ",  tmp.apdx.label, "\"]"))
      }
    } # paste0("downloads/", paste0pd.output.file, ".html"))) # Add link to HTML formats?
    if (length(bd.downloads) == 1L) bd.downloads <- paste0("[", bd.downloads, "]")
    ##  Scoping issue when trying to send in objects like 'report_config$top.level$title' with ymlthis::yml_output(bookdown::gitbook(...))

    index.rmd.yml <- index.rmd.yml %>%
      ymlthis::yml_toplevel(
        list(output=list(
          `bookdown::gitbook` = list(
            fig_caption = FALSE,
            css = "assets/css/literasee_bookdown_style.css",
            config = list(
              toc = list(
                before = paste0("<li><a href=\"./\"><strong>", report_config$top.level$title, "</strong></a></li>"),
                after =  paste0("<li><a href=\"https://github.com/", report_config$client.info$github.repo, "\" target=\"blank\">Analysis Code on Github</a></li>")),
              download = bd.downloads),
            split_bib = FALSE)
        ))
      ) %>%
        ymlthis::yml_citations(
          bibliography = report_config$top.level$bibliography,
          link_citations = TRUE)

    ##    clean up (nested) params
    tmp.params <- report_config$params
    for (prm in seq(length(tmp.params))) {
      if (is.character(tmp.params[[prm]])) tmp.params[[prm]] <- addQuote(tmp.params[[prm]])
      if (is.list(tmp.params[[prm]])) {
        tmp.params[[prm]] <- gsub("'c[(]", "c(", gsub("[)]'", ")", paste0("!r list(", paste(names(tmp.params[[prm]]), "=", addQuote(tmp.params[[prm]]), collapse = ", "), ")XXX")))
      }
    }
    # index.rmd.yml <- do.call(ymlthis::yml_params, c(list(.yml=index.rmd.yml), sapply(report_config$params, function(f) ifelse(is.character(f), addQuote(f), f)), render.format = "'bookdown'"))
    index.rmd.yml <- do.call(ymlthis::yml_params, c(list(.yml=index.rmd.yml), tmp.params, render.format = "'bookdown'"))

    writeYAML(yaml = index.rmd.yml, filename = "index.Rmd")
    addCurrentDraft(config=report_config, filename = "index.Rmd")
    # Make params and setup child RMD files to be included in the rmd_file_list/rmd.files argument/object
    # addChildChunk(rmd_file=file.path(report_config$params$unvrsl.rmd.path[[1]], "report_setup.Rmd"), comments = "Set up report params, packages, cache, etc.", filename = "index.Rmd")
    cat("\n# {.unlisted .unnumbered}\n", file="index.Rmd", append=TRUE)

    ###   Create _bookdown.yml for website/book

    if (!is.null(rmd_file_list$bookdown$file.order)) {
      bd.files <- report.files[rmd_file_list$bookdown$file.order]
    } else bd.files <- report.files

    bd.files <- c(
      ifelse(file.exists("index.Rmd"), "index.Rmd", ""),
      bd.files,
      ifelse(rmd_file_list$report$references, file.path(rmd_file_list$bookdown$rmd.path, "references.Rmd"), "")
    )
    bd.files <- bd.files[bd.files!=""]
    if (appdx.tf) { # ifelse doesn't work with character vectors length > 1
      if (length(rmd_file_list$appendices) > 1) {
        apdx.placeholder <- "appendices.Rmd"
      } else apdx.placeholder <- "appendix.Rmd"
      bd.files <- c(bd.files, file.path(rmd_file_list$bookdown$rmd.path, apdx.placeholder), setdiff(unlist(appdx.files), report.files)) # setdiff to weed out params.Rmd, setup.Rmd, etc. (possibly needed for pagedown)
    }

    extra.bd.files <- grep("references.Rmd|appendix.Rmd|appendices.Rmd", list.files(rmd_file_list$bookdown$rmd.path), invert=TRUE, value = TRUE)
    if (length(extra.bd.files) > 0) {
      bd.files <- c(bd.files, file.path(rmd_file_list$bookdown$rmd.path, extra.bd.files))
    }

    ymlthis::yml_empty() %>%
      ymlthis::yml_bookdown_opts(
        output_dir = bd.output.dir,
        delete_merged_file = TRUE,
        rmd_files = addQuote(bd.files) # paste0("[\n", paste0(addQuote(bd.files), collapse = ",\n"), "\n]")
      ) %>%
        writeYAML(filename = "_bookdown.yml", fences=FALSE, remove_scalar_style=TRUE)
  }


  #####
  ###   Pagedown files
  #####

  if (pagedown) {
    pgdwn.rmd.yml <- top.yml %>%
      ymlthis::yml_toplevel(
        knit= "pagedown::chrome_print",
        client_city = report_config$client.info$city.name,
        client_state = report_config$client.info$state.abv,
        client_organization = addQuote(report_config$client.info$organization))

    if (!is.null(report_config$client.info$org.head)) {
      pgdwn.rmd.yml <- ymlthis::yml_toplevel(.yml=pgdwn.rmd.yml, client_name = addQuote(report_config$client.info$org.head))
    }
    if (!is.null(report_config$top.level$project.team)) {
      pgdwn.rmd.yml <- ymlthis::yml_toplevel(.yml=pgdwn.rmd.yml, project_team = addQuote(report_config$top.level$project.team))
    }
    if (!is.null(report_config$client.info$github.repo)) {
      pgdwn.rmd.yml <- ymlthis::yml_toplevel(.yml=pgdwn.rmd.yml, project_code = addQuote(paste0("[Github](https://github.com/", report_config$client.info$github.repo, ")")))
    }
    if (!is.null(report_config$top.level$project.email)) {
      pgdwn.rmd.yml <- ymlthis::yml_toplevel(.yml=pgdwn.rmd.yml, project_email = report_config$top.level$project.email)
    }
    if (!is.null(report_config$client.info$acknowledgements)) {
      pgdwn.rmd.yml <- ymlthis::yml_toplevel(.yml=pgdwn.rmd.yml, acknowledgements = addQuote(report_config$client.info$acknowledgements))
    }

    pgdwn.rmd.yml <- pgdwn.rmd.yml %>%
        ymlthis::yml_output(
          pagedown::html_paged(
            css = c("../assets/css/nciea-style.css", "../assets/css/nciea-page.css", "../assets/css/nciea-default.css"),
            template = "../assets/pandoc/nciea.html",
            toc = TRUE,
            toc_depth = 2L,
            self_contained = TRUE,
            number_sections = FALSE,
            fig_caption = TRUE,
            includes = ymlthis::includes2(
              in_header = "../assets/js/meta_lof_js.html")
          )
        ) %>%
        ymlthis::yml_toplevel(
          customjs = "../assets/js/lof.js",
          lof = TRUE,
          `lof-title` = "Tables and Figures"
        ) %>%
        ymlthis::yml_pagedown_opts(
          toc_title = addQuote("Inside"),
          paged_footnotes = TRUE
        )
    if (rmd_file_list$report$references) {
      pgdwn.rmd.yml <-  pgdwn.rmd.yml %>%
         ymlthis::yml_citations(
           bibliography = report_config$top.level$bibliography,
           link_citations = TRUE)
    }

    ##    clean up (nested) params
    if (!exists("tmp.params")) {
      tmp.params <- report_config$params
      for (prm in seq(length(tmp.params))) {
        if (is.character(tmp.params[[prm]])) tmp.params[[prm]] <- addQuote(tmp.params[[prm]])
        if (is.list(tmp.params[[prm]])) {
          tmp.params[[prm]] <- gsub("'c[(]", "c(", gsub("[)]'", ")", paste0("!r list(", paste(names(tmp.params[[prm]]), "=", addQuote(tmp.params[[prm]]), collapse = ", "), ")XXX")))
        }
      }
    }
    # pgdwn.rmd.yml <- do.call(ymlthis::yml_params, c(list(.yml=pgdwn.rmd.yml), sapply(report_config$params, function(f) ifelse(is.character(f), addQuote(f), f)), render.format = "pagedown"))
    pgdwn.rmd.yml <- do.call(ymlthis::yml_params, c(list(.yml=pgdwn.rmd.yml), tmp.params, render.format = "pagedown"))
    pgdwn.rmd.yml <- ymlthis::yml_toplevel(.yml=pgdwn.rmd.yml, abstract = code_chunk(chunk_args = list(child="'../assets/rmd/pagedown/abstract.Rmd'"))) # file.path("assets", "pagedown", "rmd", "abstract.Rmd")
    pd.filename <- file.path(pd.output.dir, paste0(pd.output.file, ".Rmd"))
    if (!dir.exists(pd.output.dir)) dir.create(pd.output.dir)

    writeYAML(yaml = pgdwn.rmd.yml, filename = pd.filename)

    addCurrentDraft(config=report_config, filename = pd.filename)

    ##  Make params and setup child RMD files to be included in the rmd_file_list/rmd.files argument/object
    # addChildChunk(rmd_file=file.path("..", report_config$params$unvrsl.rmd.path[[1]], "report_setup.Rmd"), comments = "Set up report params, packages, cache, etc.", filename = pd.filename)

    # setup_chunk() # can't get to work. Just as easy without wrapper:
    # cat(ymlthis::code_chunk({
    #   knitr::opts_chunk$set(
    #     echo = FALSE,
    #     fig.topcaption = TRUE,
    #     fig.cap = TRUE,
    #     dpi = 150)},
    #   chunk_name = "setup", chunk_args = list(include = FALSE)), "\n\n", file=pd.filename, append=TRUE)

    ###   Add in content child chunks

    if (!is.null(rmd_file_list$pagedown$file.order)) {
      pgd.files <- report.files[rmd_file_list$pagedown$file.order]
    } else pgd.files <- report.files

    for (chld in pgd.files) {
      ##  Move child files back an additional level ("..") since rendered from the "./report/" directory
      addChildChunk(rmd_file=file.path("..", chld), filename = pd.filename)
    }

    if (rmd_file_list$report$references) {
      addReferences(pd.filename)
    }

    ###   Pagedown appendices
    if (appdx.tf) {
      for (apdx in seq(length(rmd_file_list$appendices))) {
        tmp.apdx.label <- names(rmd_file_list$appendices[apdx])
        if (!is.null(rmd_file_list$appendices[[apdx]]$references)) {
          if (is.logical(rmd_file_list$appendices[[apdx]]$references)) {
            apdx.refs <- rmd_file_list$appendices[[apdx]]$references
          } else apdx.refs <- TRUE
        } else apdx.refs <- FALSE
        tmp.appdx.yml <-
          ymlthis::yml_empty() %>%
            ymlthis::yml_title(addQuote(rmd_file_list$appendices[[apdx]]$title)) %>%
            ymlthis::yml_subtitle(addQuote(report_config$top.level$title)) %>%
            ymlthis::yml_toplevel(
              `appendix-prefix` = addQuote(tmp.apdx.label),
              knit= "pagedown::chrome_print"
            ) %>%
            ymlthis::yml_pagedown_opts(
              toc_title = addQuote("Table of Contents"),
              paged_footnotes = TRUE
            ) %>%
            ymlthis::yml_output(
              pagedown::html_paged(
                css = c("../assets/css/nciea-style.css", "../assets/css/nciea-appendix-page.css",
                        "../assets/css/nciea-default.css", "../assets/css/literasee_bookdown_style.css"),
                template = "../assets/pandoc/nciea_appendix.html",
                toc = TRUE,
                toc_depth = 2L,
                self_contained = TRUE,
                number_sections = FALSE,
                fig_caption = TRUE
              )
            )
        if (apdx.refs) {
          tmp.appdx.yml <-  tmp.appdx.yml %>%
             ymlthis::yml_citations(
               bibliography = report_config$top.level$bibliography,
               link_citations = TRUE)
        }
        tmp.appdx.yml <- do.call(ymlthis::yml_params, c(list(.yml=tmp.appdx.yml), report_config$params, render.format = "pagedown"))

        if (!is.na(tmp.apdx.label)) {
          tmp.apdx.fname <- file.path(pd.output.dir, paste0(gsub(" ", "_", rmd_file_list$appendices[[apdx]]$title), "_APPENDIX_",  tmp.apdx.label, ".Rmd"))
        } else tmp.apdx.fname <- file.path(pd.output.dir, paste0(gsub(" ", "_", rmd_file_list$appendices[[apdx]]$title), "_APPENDIX", ".Rmd"))
        writeYAML(yaml = tmp.appdx.yml, filename = tmp.apdx.fname)

        addCurrentDraft(config=report_config, filename = tmp.apdx.fname)

        # setup_chunk() # can't get to work. Just as easy without wrapper:
        # cat(ymlthis::code_chunk({knitr::opts_chunk$set(echo = FALSE, fig.topcaption = TRUE, fig.cap = TRUE, dpi = 150)},
        #   chunk_name = "setup", chunk_args = list(include = FALSE)), "\n\n", file=tmp.apdx.fname, append=TRUE)

        tmp.code_chunk <- paste0("ymlthis::yml_empty() %>%\n\t\tymlthis::yml_bookdown_opts(\n\t\t\tlanguage=list(label=list(fig='Figure ", tmp.apdx.label, "', tab='Table ", tmp.apdx.label, "'))\n\t\t) %>%\n\t\t\tLiterasee:::writeYAML(filename = '_bookdown.yml', fences=FALSE)")
        addCodeChunk(chunk_args= "include=FALSE", code_chunk=tmp.code_chunk,
                     comments="create _bookdown.yml file for labeling Figures and Tabels with appendix prefix.", filename=tmp.apdx.fname)

        ##  Make params and setup child RMD files to be included in the rmd_file_list/rmd.files argument/object
        # addChildChunk(rmd_file=file.path("..", report_config$params$unvrsl.rmd.path[[1]], "report_setup.Rmd"), comments = "Set up report params, packages, cache, etc.", filename = tmp.apdx.fname)

        ##  Make user specify counter setup in appendix specific setup.Rmd (e.g., setup_appendix_a.Rmd)
        # tmp.code_chunk <- paste0("setCounters()\n\toptions(table_counter=FALSE)\n\toptions(table_counter_str = '", paste0("**Table ", tmp.apdx.label, "%s:**"),
        #                 "')\n\toptions(fig_caption_no_sprintf = '", paste0("**Figure ", tmp.apdx.label, "%d:** %s"), "')")
        # addCodeChunk(chunk_args= "cache=FALSE, results='asis', echo=FALSE", code_chunk=tmp.code_chunk,
        #              comments=paste0("Initialize appendix format: re-start counters and change to alphabetical (", tmp.apdx.label, ")"), filename=tmp.apdx.fname)

        ###   Add in content child chunks

        for (apdxchld in appdx.files[[apdx]]) {
          addChildChunk(rmd_file=file.path("..", apdxchld), filename = tmp.apdx.fname)
        }

        tmp.code_chunk <- "options(table_counter_str = NULL)\n\toptions(fig_caption_no_sprintf = NULL)"
        addCodeChunk(chunk_args = "cache=FALSE, results='asis', echo=FALSE", code_chunk = tmp.code_chunk,
                     comments = "End appendix format: re-start counters and change back to numeric for subsequent re-rendering", filename = tmp.apdx.fname)

        if (apdx.refs) {
          addReferences(tmp.apdx.fname)
        }
      }
    }
  }
}
