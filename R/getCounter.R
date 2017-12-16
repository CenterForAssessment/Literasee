`getCounter` <- function(
        type = c("table", "figure", "equation")
    ) {
	       if (tolower(type) == "table") return(getOption("table_number"))
	       if (tolower(type) == "figure") return(getOption("fig_caption_no"))
	       if (tolower(type) == "equation") return(getOption("equation_counter"))
} ### END getCounter
