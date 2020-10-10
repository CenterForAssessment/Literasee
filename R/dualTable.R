##  dualTable function for HTML and LaTeX output

`dualTable` <- function(
  my_tbl,
  where="H",
  align=paste(rep('c', ncol(my_tbl)), collapse=''),
  caption="",
  css.class="gmisc_table breakboth",
  use.xtable=FALSE,
  files="",
  ...) {
  tmp_caption <- tblCap(caption)
  if (all(nchar(align) > 1) & length(align)==1){
  	tex.align <- strsplit(align, "")[[1]]
  	html.align<- align
  } else {
  	tex.align <- align
  	html.align<- paste(align,collapse='')
  }

  ###  Utility function - cat to files, including stdout, if requested (for RMD output format)
  ccat <- function(text, all.files, append = FALSE) {for(f in all.files) cat(text, file=f, append=append)}

  ###  Output HTML and Latex formatted tables
  ccat("\n<!-- HTML_Start -->\n<!-- dualTable -->\n", all.files=files)
  html.text <- htmlTable::htmlTable(my_tbl, title="", align=html.align, caption=tmp_caption, css.class=css.class, ...)
  suppressWarnings(ccat(html.text, all.files=files, append = TRUE))
  ccat("\n<!-- LaTeX_Start dualTable\n", all.files=files, append = TRUE)
  if (use.xtable) {
    latex.text <- xtable::print.xtable(xtable::xtable(my_tbl, align=c("r", tex.align), caption=tmp_caption),
                                       table.placement = where, caption.placement = "top", file=f, append = TRUE)
  } else {
    options(omitlatexcom=TRUE)
    for (f in files) {
      if (f == "") {
        latex.text <- print(Hmisc::latex(my_tbl, file=f, append = TRUE, where=where, col.just=tex.align, caption=tmp_caption, ...))
      } else {
        latex.text <- Hmisc::latex(my_tbl, file=f, append = TRUE, where=where, col.just=tex.align, caption=tmp_caption, ...)
      }
    }
  }
  ccat("\nLaTeX_End -->\n", all.files=files, append = TRUE)
}
