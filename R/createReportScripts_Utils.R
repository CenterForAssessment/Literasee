################################################################################
#####                                                                      #####
###     Ultility functions for producing configuration lists and scripts     ###
#####                                                                      #####
################################################################################

writeYAML <- function(yaml=ymlthis::last_yml(), filename, fences = TRUE, remove_scalar_style=FALSE, append_yaml=FALSE){
  clean.yaml <- gsub("'''", "\"", yaml::as.yaml(yaml, line.sep = "\n", column.major = FALSE))
  clean.yaml <- gsub("''", "\"", clean.yaml)
  clean.yaml <- gsub("'[[]", "[", clean.yaml)
  clean.yaml <- gsub("[]]'", "]", clean.yaml)
  clean.yaml <- gsub("\n\n", "\n", clean.yaml)
  clean.yaml <- gsub("'TRUE'", "yes", clean.yaml)
  clean.yaml <- gsub("'FALSE'", "no", clean.yaml)
  if (remove_scalar_style) {
    clean.yaml <- gsub("[|]-\n", "", clean.yaml)
  }
  if (fences) {
    cat(paste0("---\n", clean.yaml, "---\n"), file=filename, append=append_yaml)
  } else {
    cat(clean.yaml, file=filename, append=append_yaml)
  }
}

addQuote <- function(text) {
  paste0("'", text, "'")
}

addCurrentDraft <- function(config, filename) {
  auth.list <- config$top.level$author.names
  cat(paste0("
<!--
  This document was written by ", paste(paste(head(auth.list, -1), collapse=", "), tail(auth.list, 1), sep=" and "), " for the ", config$client.info$organization, "\n\t",
  "Current Draft:  ", format(Sys.time(), format = "%B %d, %Y"), "\n",
"-->\n\n"), file=filename, append=TRUE)
}

addChildChunk <- function(rmd_file, comments=NULL, label=NULL, eval_statement="TRUE", filename) {
  if (!is.null(comments)) {
    for (cmnt in comments) {
      cat(paste0("<!--  ", cmnt, "-->\n"), file=filename, append=TRUE)
    }
  }

  cat(paste0("
```{r ", label, " child = '",rmd_file, "', eval=", eval_statement, "}\n```\n"), file=filename, append=TRUE)
}

addCodeChunk <- function(chunk_args, code_chunk, comments=NULL, label=NULL, filename) {
  if (!is.null(comments)) {
    for (cmnt in comments) {
      cat(paste0("\n<!--  ", cmnt, "-->\n"), file=filename, append=TRUE)
    }
  }
  cat(paste0("
```{r, ", label, chunk_args, "}\n\t", code_chunk, "\n```\n"), file=filename, append=TRUE)
}

addReferences <- function(filename) {
  cat("
<!--  References  -->

# References {-}
::: {#refs}
:::\n", file=filename, append=TRUE)
}
