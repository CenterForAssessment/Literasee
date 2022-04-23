`.onAttach` <-
	function(libname, pkgname) {
		if (interactive()) {
			packageStartupMessage(magenta$bold('Literasee',paste(paste0(unlist(strsplit(as.character(packageVersion("Literasee")), "[.]")),
														c(".", "-", ".", "")), collapse=""),' (4-23-2022). For help: >help("Literasee") or visit https://centerforassessment.github.io/Literasee'))
	}
}
