aboutUsUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    style = "margin-top: 15px;",
    column(
      width = 8,
      offset = 2,
      tags$div(
        class = "textCard",
        tags$h1(class = "bottom-blue", "ABOUT US"),
        tags$p(
          style = 'padding-left:30px; padding-right: 30px;',
          "SynergyFinder is developed by the ",
          tags$a(
            "Network Pharmacology for Precision Medicine",
            href = 'https://www.helsinki.fi/en/researchgroups/network-pharmacology-for-precision-medicine',
            target = '_blank'
          ),
          " in the Research Programs Unit, the Faculty of Medicine at the ",
          tags$a(
            "University of Helsinki",
            href = 'https://www.helsinki.fi/en',
            target = '_blank'
          ),
          ", Helsinki, Finland.",
          tags$br(), tags$br(),
          "SynergyFinder is an interactive to evaluate the significance of combined drugs,",
          " the observed drug combination responses are usually with expected responses ",
          "calculated by using synergy scoring models deviation of observed and expected ",
          "responses one may classify the drug combination as synergistic or antagonistic. ",
          "It provides efficient implementations for all the popular synergy scoring models, ",
          "including HSA, Loewe, Bliss and ZIP to quantify the degree of drug synergy.",
          "The R package 'synergyfinder' is available for R programmer."
        ),
        tags$h1(class = "bottom-blue", "OTHER TOOLS"),
        fluidRow(
          column(
            width = 6,
            tags$a(
              tags$p(
                class = 'textCard left-light-green',
                tags$b("synergyfinder R package"),
                tags$br(),
                "An R package for calculate and visualize synergy cores for drug combinations."
              ),
              tags$br(), tags$br(), tags$br(), tags$br(),
              href="https://www.bioconductor.org/packages/release/bioc/html/synergyfinder.html",
              target="_blank"
            )
          ),
          column(
            width = 6,
            tags$a(
              tags$p(class='textCard left-light-green',
                tags$b("DrugComb"),
                tags$br(),
                "An integrated data portal aimed at collecting, ",
                "analysing and distributing the results of drug combination screens on cancer cell lines. ",
                "Its main purpose is harmonisation of drug combination research results."
              ),
              href="https://www.drugcomb.org",
              target="_blank"
            )
          )
        ),
        tags$h1(class="bottom-blue", "THE TEAM"),
        fluidRow(
          column(
            width = 6,
            tags$p(
              class="textCard left-deep-orange",
              tags$b("Jing Tang"),
              tags$br(),
              "Group Leader",
              tags$br(),
              "Assistant Professor"
            ),
            tags$p(
              class ="textCard left-light-orange",
              tags$b("Jehad Aldahdooh"),
              tags$br(),
              "Server Manager",
              tags$br(),
              "PhD student"
            )
          ),
          column(
            width = 6,
            tags$p(
              class='textCard left-light-orange',
              tags$b("Shuyu Zheng"),
              tags$br(),
              "Software Development & R Package Maintainer",
              tags$br(),
              "PhD student"
            ),
            tags$p(
              class='textCard left-light-orange',
              tags$b("Liye He"),
              tags$br(),
              "R Package Development",
              tags$br(),
              "PostDoc"
            )
          )
        )
      )
    )
  )
}
