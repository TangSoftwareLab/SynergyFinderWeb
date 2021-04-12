# Load required packages
dependency <- c(
  "shiny", "shinyjs", "shinytoastr", "shinyWidgets", "shinydashboard",
  "shinyBS", "DT", "synergyfinder", "htmltools", "TidyComb", #"kableExtra",
  "dashboardthemes", "ggplot2", "scales", "gplots", "lattice", "plotly", "grid",
  "xtable", "shinybusy", "writexl", "dplyr", "shinycssloaders")
sapply(
  dependency,
  library,
  character.only = T
)

# Load local R scripts
sapply(
  list.files("./R", pattern = ".*\\.R$", recursive = TRUE, full.names = TRUE),
  source,
  .GlobalEnv
)

# Path to temporarily store pdf reports.
reportspath <- "~/Desktop/test_synergyfinder/"
cellosauruspath <- "~/Documents/work/TidyComb_related/cellosaurus.xml"