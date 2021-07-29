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
          " in the Research Program of System Oncology, Faculty of Medicine at ",
          tags$a(
            "University of Helsinki",
            href = 'https://www.helsinki.fi/en',
            target = '_blank'
          ),
          ", Helsinki, Finland.",
          tags$br(), tags$br(),
          "SynergyFinder is an interactive tool for analyzing drug combination dose response data. It enables efficient implementations for all the popular synergy scoring models, ",
          "including HSA, Loewe, Bliss and ZIP to quantify the degree of drug synergy.",
          "The R package 'synergyfinder' is available for R programmers."
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
                "An R package for calculating and visualizing synergy scores for drug combination screening data."
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
                "An integrated data portal aimed at collecting and, ",
                "and harmonizing the results of drug combination screens on cancer cell lines. ",
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
              class='textCard left-light-orange',
              tags$b("Wenyu Wang"),
              tags$br(),
              "R Package Development",
              tags$br(),
              "PhD Student"
            ),
            tags$p(
              class ="textCard left-light-orange",
              tags$b("Alina Malyutina"),
              tags$br(),
              "Method Development",
              tags$br(),
              "PhD Student"
            ),
            tags$p(
              class ="textCard left-light-orange",
              tags$b("Ziaurrehman Tanoli"),
              tags$br(),
              "Database Manager",
              tags$br(),
              "Senior Researcher"
            ),
            tags$p(
              class ="textCard left-light-orange",
              tags$b("Liye He"),
              tags$br(),
              "Package Developer",
              tags$br(),
              "Former PhD student"
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
              "PhD Student"
            ),
            tags$p(
              class ="textCard left-light-orange",
              tags$b("Jehad Aldahdooh"),
              tags$br(),
              "Server Manager",
              tags$br(),
              "PhD Student"
            ),
            tags$p(
              class ="textCard left-light-orange",
              tags$b("Tolou Shadbahr"),
              tags$br(),
              "Method Development",
              tags$br(),
              "Research Assistant"
            ),
            tags$p(
              class ="textCard left-light-orange",
              tags$b("Alberto Pessia"),
              tags$br(),
              "Method Development",
              tags$br(),
              "Former Postdoc Researcher"
            )
          )
        )
      )
    )
  )
}
