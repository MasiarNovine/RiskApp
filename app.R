library(shiny)
library(shinydashboard)
library(thematic)
source("ui.R")

# Theming
options(shiny.useragg = TRUE)
thematic::thematic_shiny(font = "auto")

# Set up app
shiny::shinyApp(ui = ui, server = server)