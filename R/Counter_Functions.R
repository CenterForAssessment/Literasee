 ### Get and Set Counters

`getCounter` <- function(
        type = c("table", "figure", "equation")
    ) {
	       if (tolower(type) == "table") return(getOption("table_number"))
	       if (tolower(type) == "figure") return(getOption("fig_caption_no"))
	       if (tolower(type) == "equation") return(getOption("equation_counter"))
}


`setCounters` <- function(tbl.counter=0, fig.counter=0, eqn.counter=0) {
  options("table_number" = tbl.counter)
  options("fig_caption_no" = fig.counter)
  options("equation_counter" = eqn.counter)
}
