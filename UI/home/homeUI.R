homeUI <- tags$div(id = "home",
  fluidPage(
    fluidRow(class = "row", 
      column(width = 5, offset = 1, align = "center",
             h1("SynergyFinder"),
             br(),
             p("an interactive tool for analyzing drug combination dose-response data"),
             br(),
             actionButton("getStart", "Analyze", type="primary", class = "btn-lg shadow lift mr-1",
                          style="color:'white';"),
             actionButton("toGuide", "User Guide", class="btn-primary-soft lif"),
             # actionButton("toVideoGuide", "Video Guid", onclick="window.open('https://player.vimeo.com/video/192242384')")#,
      ),
      column(width = 6, align = "left",
             # 3D plot:
             includeHTML("./UI/home/homePlot.html")
             )
    )
      ),
    )

