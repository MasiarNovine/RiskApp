# UI object
# --------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = "Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Model", tabName = "model", icon = icon("diagram-project")),
      menuItem("Results", tabName = "results", icon = icon("chart-simple"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      # First tab
      tabItem(tabName = "data",
        wellPanel(
          wellPanel(
            fluidRow(
              column(
                h2("Data"),
                fileInput("d1", "Read file (.rds)", buttonLabel = "Choose", width = "250px"),
                h3("Options"),
                numericInput("dig", "Max digits", 2, min = 0, max = 10, width = "80px"),
                width = 8, height = 2
              )
            )
          ),
          DT::dataTableOutput("tbl", fill = TRUE, width = "100%")
        )
      ),
      # Second tab
      tabItem(tabName = "model",
        fluidRow(
          wellPanel(
            wellPanel(
              fluidRow(
                column(
                  h2("Model"),
                  networkD3::simpleNetworkOutput("network", height = "400px"),
                  width = 8, height = 10
                ),
                column(
                  wellPanel(
                    selectInput(
                      "sim",
                      "Similarity index",
                      c("Spearman", "Pearson", "Hoeffding D")
                    ),
                    selectInput(
                      "trans",
                      "Transformation",
                      c("square", "absolute", "none")
                    ),
                    sliderInput(
                      "thres",
                      "Edge threshold",
                      min = 0, max = 1, 0.5, step = 0.01
                    ),
                    checkboxInput("showsim", "Show similarities", FALSE)
                  ),
                  width = 4, height = 10
                )
              )
            ),
            #DT::dataTableOutput("vartbl"),
            rhandsontable::rHandsontableOutput("vars"),
            textAreaInput("mf", "Model formula")
          )
        )
      ),
      tabItem(tabName = "results"
        ## TODO results
      )
    )
  )
)
