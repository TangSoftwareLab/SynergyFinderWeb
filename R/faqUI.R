faqUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    style = "margin-top: 15px;",
    column(
      width = 10,
      offset = 1,
      box(
        id = "faq2",
        title = "How do I report a technical issue on SynergyFinder?",
        solidHeader = TRUE,
        width =12,
        height = NULL,
        collapsible = TRUE,
        collapsed = TRUE,
        fluidRow(
          tags$div(
            class = "faq",
            tags$p(
              "You can report the technical issues to our GitHub repositories: ",
              tags$a(
                href = "https://github.com/TangSoftwareLab/SynergyFinderWeb/issues",
                "TangSoftwareLab/SynergyFinderWeb",
                target="_blank"),
              "web application, or ",
              tags$a(
                href = "https://github.com/TangSoftwareLab/SynergyFinderR/issues",
                "TangSoftwareLab/SynergyFinderR",
                target="_blank"),
              "for R package."
            )
          )
        )
      ),
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
