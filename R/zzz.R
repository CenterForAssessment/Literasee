`.onAttach` <-
	function(libname, pkgname) {
		if (interactive()) {
			packageStartupMessage(magenta$bold('Literasee',paste(paste0(unlist(strsplit(as.character(packageVersion("Literasee")), "[.]")),
														c(".", "-", ".", "")), collapse=""),' (7-8-2021). For help: >help("Literasee") or visit https://centerforassessment.github.io/Literasee'))
	}
}
