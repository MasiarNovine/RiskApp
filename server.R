library(mcgraph)        # https://github.com/MasiarNovine/mcgraph
library(networkD3)
library(data.table)
library(DT)

# BACKEND

# Function to load the Rdata file
load_data <- function(f, digits=2) {
  ext <- tools::file_ext(f$datapath)
  # 'req' ensures, that 'f' exists
  req(f)
  d <- readRDS(f$datapath)
  setDT(d)
  cnms <- colnames(d)
  dnum <- d[, round(.SD, digits), .SDcols = is.numeric]
  dch <- d[, .SD, .SDcols = !is.numeric]
  dend <- data.table(dch, dnum)
  dend[, .SD, .SDcols = cnms]
}

# Create model graph
create_network_3d <- function(d,
                              similarity=c("spearman", "pearson", "hoeffding"),
                              trans=c("square", "abs", "none"),
                              threshold=0.4,
                              ...)
{
  similarity <- match.arg(similarity)
  trans <- match.arg(trans)
  vars <- colnames(d)
  # Create a graph based on the adjacency matrix
  dnum <- d[, .SD, .SDcols = is.numeric]
  # TODO: Include also negative correlations
  adj <- ifelse(Hmisc::varclus(as.matrix(dnum), similarity = similarity, trans = trans)$sim >= threshold, 1, 0)
  #m <- mcgraph::mcg.ct(, method = similarity, is.squared = TRUE, rs = threshold)
  #m <- matrix(0, ncol = length(vars), nrow = length(vars))
  #colnames(m) <- rownames(m) <- vars
  diag(adj) <- 0
  g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
  dd <- networkD3::igraph_to_networkD3(g)
  d <- data.table::data.table(dd[[1]])
  nd <- dd[[2]]
  d[, source := nd[d$source + 1 , ]]
  d[, target := nd[d$target + 1 , ]]
  ntw <- networkD3::simpleNetwork(d, opacity = 0.9, ...)

  return(ntw)
}

# Convenience function aggregate the data for each variable
bawl <- function(d, fun, digits = 2, ...) {
  return(round(unlist(lapply(d, fun, ...)), digits))
}

create_numeric_table <- function(d, digits = 2) {
  dd <- d[, .SD, .SDcols = is.numeric]
  res <- dd[,
    .(
      "Name" = colnames(dd),
      "Observations" = bawl(.SD, length),
      "Missing" = bawl(.SD, function(x) length(which(is.na(x)))),
      "Mean" = bawl(.SD, mean, na.rm = TRUE),
      "GMD" = bawl(.SD, Hmisc::GiniMd, na.rm = TRUE),
      "Min" = bawl(.SD, min, na.rm = TRUE),
      "Q1" = bawl(.SD, quantile, probs = 0.25, na.rm = TRUE),
      "Median" = bawl(.SD, median, na.rm = TRUE),
      "Q3" = bawl(.SD, quantile, probs = 0.75, na.rm = TRUE),
      "Max" = bawl(.SD, max, na.rm = TRUE)
    )
  ]
  return(res)
}

create_character_table <- function(d, digits = 2) {
  dd <- d[, .SD, .SDcols = !is.numeric]
  res <- dd[,
    .(
      "Name" = colnames(dd),
      "Observations" = bawl(.SD, length),
      "Missing" = bawl(.SD, function(x) length(which(is.na(x)))),
      "Distinct" = bawl(.SD, function(x) length(unique(x))),
      "Frequency (max)" = bawl(.SD, function(x) max(table(x))),
      "Frequency (min)" = bawl(.SD, function(x) min(table(x)))
    )
  ]
  return(res)
}

# Alternatively: rhandsontable
create_rhandsontable <- function(d) {
  vars <- colnames(d)
  n <- length(vars)
  vlev <- c("DV", "IV")
  linklev <- c("identity", "log", "poly", "rcs")
  # Columns: name = {...}, selected = {TRUE, FALSE}, type = {DV, IDV}, link = {log, sqrt, poly, rcs}, ... = {...}
  d <- data.table("Selected" = rep(TRUE, n),
                   "Name" = vars,
                   "Type" = rep(factor(rep("IV", n), levels = vlev)),
                   "Link" = rep(factor(rep("identity", n), levels = linklev)),
                   "1. Argument" = rep("", n)
  )
  rhand <- rhandsontable::rhandsontable(d, rowheaders = NULL) %>%
    rhandsontable::hot_col(col = "Type", type = "dropdown", source = vlev, readOnly = FALSE, allowInvalid = FALSE, strict = TRUE) %>%
    rhandsontable::hot_col(col = "Link", type = "dropdown", source = linklev, readOnly = FALSE, allowInvalid = FALSE, strict = TRUE)

  return(rhand)
}

server <- function(input, output, session) {
  output$tbl <- DT::renderDataTable({
    d <- load_data(f = input$d1, digits = input$dig)
    DT::datatable(d, options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100), scrollX = TRUE))
  })
  output$network <- networkD3::renderSimpleNetwork({
    g <- create_network_3d(
      load_data(f = input$d1, digits = input$dig),
      similarity = input$sim,
      trans = input$trans,
      threshold = input$thres,
      width = NULL,
      height = NULL,
      zoom = TRUE)
    g
  })
  output$form <- renderText({
    htmltools::code(input$mf)
  })
  # output$vars <- rhandsontable::renderRHandsontable({
  #   d <- load_data(f = input$d1)
  #   create_rhandsontable(d)
  # })
  output$num <- DT::renderDataTable({
    d <- load_data(f = input$d1, digits = input$dig)
    DT::datatable(
      create_numeric_table(d),
      options = list(searchable = FALSE, pageLength = 10, lengthMenu = c(5, 10), scrollX = TRUE)
    )
  })
  output$char <- DT::renderDataTable({
    d <- load_data(f = input$d1, digits = input$dig)
    DT::datatable(
      create_character_table(d),
      options = list(searchable = FALSE, pageLength = 10, lengthMenu = c(5, 10), scrollX = TRUE)
    )
  })
  # The option 'force' can be used for local testing w/o a Shiny server, for 'TRUE'
  #session$allowReconnect(TRUE)
}