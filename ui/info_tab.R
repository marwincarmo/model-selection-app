info_tab <- tabItem(
  tabName = "info_tab",
  #setBackgroundColor("#808080"),
  fluidRow(
    column(width = 12,
           withMathJax(includeMarkdown("R/info.rmd")))
  ))
      