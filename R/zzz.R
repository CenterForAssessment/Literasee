`.onAttach` <-
	function(libname, pkgname) {
		if (interactive()) {
			packageStartupMessage(magenta$bold('Literasee',paste(paste0(unlist(strsplit(as.character(packageVersion("Literasee")), "[.]")),
														c(".", "-", ".", "")), collapse=""),' (2-26-2020). For help: >help("Literasee") or visit https://centerforassessment.github.io/Literasee'))
	}
}
