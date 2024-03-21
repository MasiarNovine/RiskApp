library(shiny)
library(shinydashboard)
library(visNetwork)
library(networkD3)
library(mcgraph)
library(DT)
library(data.table)
library(thematic)

# BACKEND LOGIC
# --------------------------------------
# Function to load the Rdata file
load_data <- function(f) {
  ext <- tools::file_ext(f$datapath)
  # 'req' ensures, that 'f' exists
  req(f)
  d <- readRDS(f$datapath)
  setDT(d)
  cnms <- colnames(d)
  dnum <- d[, round(.SD, 2), .SDcols = is.numeric]
  dch <- d[, .SD, .SDcols = !is.numeric]
  dend <- data.table(dch, dnum)
  dend[, .SD, .SDcols = cnms]
}

# Create model graph
create_vis_network <- function(d) {
  vars <- colnames(d)
  # Create a graph based on the adjacency matrix
  m <- matrix(0, ncol = length(vars), nrow = length(vars))
  colnames(m) <- rownames(m) <- vars
  diag(m) <- 0
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected")
  d <- visNetwork::toVisNetworkData(g)
  visNetwork(nodes = d$nodes, edges = d$edges)
}

# Create model graph
create_network_3d <- function(d, ...) {
  vars <- colnames(d)
  # Create a graph based on the adjacency matrix
  m <- mcgraph::mcg.ct(d[, .SD, .SDcols = is.numeric], method = "spearman", is.squared = TRUE, rs = 0.4)
  #m <- matrix(0, ncol = length(vars), nrow = length(vars))
  #colnames(m) <- rownames(m) <- vars
  diag(m) <- 0
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected")
  dd <- networkD3::igraph_to_networkD3(g)
  dt <- data.table(dd[[1]])
  nd <- dd[[2]]
  dt[, source := nd[dt$source + 1 , ]]
  dt[, target := nd[dt$target + 1 , ]]
  simpleNetwork(dt, opacity = 0.9, ...)
}

# We want to order the variables based on their type
# numeric
# character
# factors
# This can be based on Hmisc::describe
create_variable_table <- function(d) {
  vars <- colnames(d)
  vartab <- data.table(symbol = "", name = vars)
}

# get_numeric_table <- function(d) {
#   dd <- d[, .SD, .SDcols = is.numeric]
#   n <- length(colnames())
#   dscr <- Hmisc::describe(dd)
#   res <- data.table(name = rep(0, times = length(colnames)), )
# }

# UI functions
# --------------------------------------
options(shiny.useragg = TRUE)
thematic_shiny(font = "auto")

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
                fileInput("d1", "Read file (.rds)", buttonLabel = "Choose"),
                width = 3, height = 2
              ),
              column(
                numericInput("dig", "Max digits", 2, min = 0, max = 10, width = "80px"),
                width = 3
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
                  #visNetwork::visNetworkOutput("vis_network", height = "600px"),
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
            DT::dataTableOutput("vartbl"),
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

# Connection
# -------------------------------------------------
server <- function(input, output, session) {
  output$tbl <- DT::renderDataTable({
    d <- load_data(f = input$d1)
    DT::datatable(d, options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100), scrollX = TRUE))
  })
  # output$vis_network <- visNetwork::renderVisNetwork({
  #   g <- create_vis_network(load_data(f = input$d1))
  #   g
  # })
  output$network <- networkD3::renderSimpleNetwork({
    g <- create_network_3d(load_data(f = input$d1), width = NULL, height = NULL, zoom = TRUE)
    g
  })
  output$form <- renderText({
    htmltools::code(input$mf)
  })
  output$vartbl <- DT::renderDataTable({
    d <- load_data(f = input$d1)
    DT::datatable(
      create_variable_table(d),
      options = list(searchable = FALSE, pageLength = 10, lengthMenu = c(5, 10), scrollX = TRUE)
    )
  })
  # The option 'force' can be used for local testing w/o a Shiny server, for 'TRUE'
  #session$allowReconnect(TRUE)
}

# Set up app
shinyApp(ui = ui, server = server)