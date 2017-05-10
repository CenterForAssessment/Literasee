`.onAttach` <-
	function(libname, pkgname) {
		if (interactive()) {
			packageStartupMessage(magenta$bold('Literasee',paste(paste0(unlist(strsplit(as.character(packageVersion("Literasee")), "[.]")), c(".", "-", ".", "")), collapse=""),' (5-8-2017). For help: >help("Literasee") or visit https://github.com/CenterForAssessment/Literasee/wiki'))
	}
}

