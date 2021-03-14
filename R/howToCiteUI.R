howToCiteUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    style = "margin-top: 15px;",
    column(
      width = 8, offset = 2,
      tags$div(
        class = "textCard",
        tags$h1(
          class = "bottom-blue", "HOW TO CITE",
          style = "text-align: center;"
        ),
        tags$h3("For use of Synergyfinder web application:"),
        tags$p(
          "Ianevski, A.; He, L.; Aittokallio, T.; Tang, J. ",
          "SynergyFinder: A Web Application for Analyzing Drug Combination Dose-Response Matrix Data",
          "Bioinformatics 2017, 33 (15), 2413–2415.",
          tags$a(
            "doi:10.1093/bioinformatics/btx162",
            href = "https://doi.org/10.1093/bioinformatics/btx162",
            target = "_blank"
          )
        ),
        tags$h3("For use of SynergyFinder R pacakge:"),
        tags$p(
          "He, L.; Kulesskiy, E.; Saarela, J.; Turunen, L.; Wennerberg, K.; Aittokallio, T.; Tang, J. ",
          "Methods for High-Throughput Drug Combination Screening and Synergy Scoring. ",
          "Methods Mol Biol 2018, 1711, 351–398.",
          tags$a(
            "doi:10.1007/978-1-4939-7493-1_17",
            href = "https://doi.org/10.1007/978-1-4939-7493-1_17",
            target = "_blank"
          )
        ),
        tags$h3("For use of ZIP synergy scoring:"),
        tags$p(
          "Yadav, B.; Wennerberg, K.; Aittokallio, T.; Tang, J. ",
          "Searching for Drug Synergy in Complex Dose-Response Landscapes Using an Interaction Potency Model. ",
          "Comput Struct Biotechnol J 2015, 13, 504–513.",
          tags$a(
            "doi:10.1016/j.csbj.2015.09.001",
            href = "https://doi.org/10.1016/j.csbj.2015.09.001",
            target = "_blank"
          )
        ),
        tags$h3("For how to harmonize the different synergy scoring methods:"),
        tags$p(
          "Tang, J.; Wennerberg, K.; Aittokallio, T. ",
          "What Is Synergy? The Saariselkä Agreement Revisited. ",
          "Front Pharmacol 2015, 6, 181.",
          tags$a(
            "doi:10.3389/fphar.2015.00181",
            href = "https://doi.org/10.3389/fphar.2015.00181",
            target = "_blank")
          ),
        tags$h3("For general ideas of drug combination therapies:"),
        tags$p(
          "Tang, J. ",
          "Informatics Approaches for Predicting, Understanding, and Testing Cancer Drug Combinations. ",
          "Methods Mol Biol 2017, 1636, 485–506.",
          tags$a(
            "doi:10.1007/978-1-4939-7154-1_30",
            href = "https://doi.org/10.1007/978-1-4939-7154-1_30",
            target = "_blank"
          )
        ),
        tags$h3(
          "For retrieving the most comprehensive drug combination data ",
          "resources and their sensitivity and synergy results by SynergyFinder, please go to ",
          tags$a("DrugComb", href = "http://drugcomb.org/"), " :"
        ),
        tags$p(
          "Zagidullin, B.; Aldahdooh, J.; Zheng, S.; Wang, W.; Wang, Y.; Saad, J.; Malyutina, A.; Jafari, M.; Tanoli, Z.; Pessia, A.; Tang, J. ",
          "DrugComb: An Integrative Cancer Drug Combination Data Portal. ",
          "Nucleic Acids Res 2019, 47 (W1), W43–W51.",
          tags$a(
            "doi:10.1093/nar/gkz337",
            href = "https://doi.org/10.1093/nar/gkz337",
            target = "_blank"
          )
        ),
        tags$h3("For use of combination sensitivity score:"),
        tags$p(
          "Malyutina, A.; Majumder, M. M.; Wang, W.; Pessia, A.; Heckman, C. A.; Tang, J. ",
          "Drug Combination Sensitivity Scoring Facilitates the Discovery of Synergistic and Efficacious Drug Combinations in Cancer. ",
          "PLOS Computational Biology 2019, 15 (5), e1006752.",
          tags$a(
            "10.1371/journal.pcbi.1006752",
            href = "https://doi.org/10.1371/journal.pcbi.1006752",
            target = "_blank"
          )
        )
      )
    )
  )
}
  
