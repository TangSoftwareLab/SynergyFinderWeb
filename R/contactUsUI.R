contactUsUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    style = "margin-top: 15px;",
    column(
      width = 8, offset = 2, class = "textCard", style = "text-align: center;",
      tags$div(
        style = "margin-bottom: 15xp;",
        tags$h1(class = "bottom-blue", "CONTACT US")
      ),
      fluidRow(
        style="margin-top: 15xp; text-align: left;",
        column(
          width = 6,
          tags$p(
            "Building: Biomedicum Helsinki 1 Office B325",
            tags$br(),
            "Strees Address: Haartmaninkatu 8",
            tags$br(),
            "Faculty of Medicine",
            tags$br(),
            "00014 University of Helsinki",
            tags$br(),
            "Finland",
          )
        ),
        column(
          width = 6,
          tags$a(
            icon("github"),
            "TangSoftwareLab",
            target = "_blank",
            href = "https://github.com/TangSoftwareLab/SynergyFinderWeb"
          ),
          tags$br(),
          tags$a(
            icon("users"),
            "Network Pharmacology for Precision Medicine (Tang Lab)",
            href = "https://www.helsinki.fi/en/researchgroups/network-pharmacology-for-precision-medicine",
            target='_blank'
          ),
          tags$br(),
          tags$a(
            icon("twitter"),
            "@SynergyFinder",
            target="_blank",
            href="https://twitter.com/SynergyFinder"
          ),
          tags$br(),
          tags$p(
            icon("envelope"), style = "color:#0277bd",
            "jing(dot)tang(at)helsinki(dot)fi"
          )
        )
      ),
      fluidRow(
        style = "text-align: center;",
        column(
          width = 4, offset = 2,
          HTML(
            '<iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d15871.560054352698!2d24.910854188321558!3d60.1816424856201!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x46920a1fa6324c07%3A0x5e3bcefb5b26e3e1!2sBiomedicum!5e0!3m2!1szh-CN!2sfi!4v1609586539331!5m2!1szh-CN!2sfi"',
            ' width="600" height="450" frameborder="0" style="border:0;" ',
            'allowfullscreen="" aria-hidden="false" tabindex="0"></iframe>'
          )
        )
      )
    )
  )
}

