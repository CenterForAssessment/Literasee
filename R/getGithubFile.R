###  Get (download) one or more files from a Github repo

`getGithubFile` <- function(
	github.user,
	repo,
	branch="master",
	github.directory=NULL,
	file.to.get=NULL,
	local.directory="github_files",
	file.type = "[.]R$|[.]Rmd$",
	quiet=FALSE) {

	###   Get list of files from github.user/repo
	if (is.null(file.to.get)) {
		#   adapted from https://stackoverflow.com/a/25485782
	  req <- httr::GET(paste0("https://api.github.com/repos/", github.user, "/", repo, "/git/trees/", branch, "?recursive=1"))
	  httr::stop_for_status(req)
	  github.files <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = FALSE)
	  if (!is.null(github.directory)) {
	    github.files <- grep(github.directory, github.files, value = TRUE, fixed = TRUE)
	  }
	  if (!is.null(github.directory)) {
	    github.files <- grep(github.directory, github.files, value = TRUE, fixed = TRUE)
	  }
	  if (!is.null(file.type)) {
	    github.files <- grep(file.type, github.files, value=TRUE, ignore.case = TRUE)
	  }
	} else github.files <- file.to.get

	###   Create local.directory if necessary
  if (!dir.exists(file.path(local.directory))) {
    dir.create(file.path(local.directory), showWarnings = FALSE, recursive = TRUE)
  }

	###   Download files from github.user/repo/github.directory
  #  adapted from https://stackoverflow.com/a/29478407
  for (gf in github.files) {
    tmp.file <- tail(strsplit(gf, "[/]|[\\]")[[1]], 1)
    utils::download.file(url=paste("https://raw.githubusercontent.com", github.user, repo, branch, github.directory, tmp.file, sep = "/"),
                         destfile = file.path(local.directory, tmp.file), method="auto", quiet=quiet)
  }
}
