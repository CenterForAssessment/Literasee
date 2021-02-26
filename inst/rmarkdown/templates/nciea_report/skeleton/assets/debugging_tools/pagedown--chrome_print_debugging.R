#####
###   Two small tests for debugging pagedown::chrome_print
#####

###   https://github.com/rstudio/pagedown/issues/157#issuecomment-570634712

#  Run this to make sure the small test works:
pagedown::chrome_print("test.html")


###   https://github.com/rstudio/pagedown/issues/157#issuecomment-566418376

#  Run this on the problematic 'my_headache.html' file to get an extensive (and cryptic) log file.
#  The problem is probably at the bottom of the log.

local({
  # Is the latest development version used?
  stopifnot(packageVersion("pagedown") >= "0.6.4")
  # Does the current working directory contains my_headache.html file?
  stopifnot(file.exists("my_headache.html"))

  chrome_print_log <- function(input, log_file) {
    out <- file(log_file, open = "wt")
    sink(out, type = "message")
    on.exit({
      sink(NULL, type = "message")
      close(out)
    })
    tryCatch(
      pagedown::chrome_print(input = input, output = tempfile(), verbose = 2),
      error = function(e) message("Error: ", e$message)
    )
  }

  # test
  chrome_print_log("my_headache.html", "chrome_print_test_2.log")
})
