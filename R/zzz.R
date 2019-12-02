`.onAttach` <-
	function(libname, pkgname) {
		if (interactive()) {
			packageStartupMessage(magenta$bold('Literasee',paste(paste0(unlist(strsplit(as.character(packageVersion("Literasee")), "[.]")), c(".", "-", ".", "")), collapse=""),' (7-17-2019). For help: >help("Literasee") or visit https://centerforassessment.github.io/Literasee'))
	}
}
