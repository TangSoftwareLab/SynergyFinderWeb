faqUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    style = "margin-top: 15px;",
    column(
      width = 10,
      offset = 1,
      box(
        id = "faq1",
        title = "Mathematical modelling in SynergyFinderPlus and SynergyFinder2",
        solidHeader = TRUE,
        width =12,
        height = NULL,
        collapsible = TRUE,
        fluidRow(
          tags$div(
            class = "faq",
            withMathJax(
              includeMarkdown(
                "./doc/FAQ.Rmd"
              )
            )
          )
        )
      )
    )
  )
}
