library(shiny)
library(shinydashboard)
library(mcgraph)        # https://github.com/MasiarNovine/mcgraph
library(networkD3)
library(data.table)
library(DT)
library(thematic)
source("ui.R")
source("server.R")

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
create_network_3d <- function(d, ...) {
  vars <- colnames(d)
  # Create a graph based on the adjacency matrix
  m <- mcgraph::mcg.ct(d[, .SD, .SDcols = is.numeric], method = "spearman", is.squared = TRUE, rs = 0.4)
  #m <- matrix(0, ncol = length(vars), nrow = length(vars))
  #colnames(m) <- rownames(m) <- vars
  diag(m) <- 0
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected")
  dd <- networkD3::igraph_to_networkD3(g)
  dta <- data.table(dd[[1]])
  nd <- dd[[2]]
  dta[, source := nd[dta$source + 1 , ]]
  dta[, target := nd[dta$target + 1 , ]]
  ntw <- networkD3::simpleNetwork(dta, opacity = 0.9, ...)

  return(ntw)
}

# Ordering of variables should be based on their type
# numeric
# character
# factors
# This can be based on Hmisc::describe
create_variable_table <- function(d) {
  vars <- colnames(d)
  vartab <- data.table(symbol = "", name = vars)

  return(vartab)
}

create_numeric_table <- function(d) {
  dta <- data.table(d)
  dd <- dta[, .SD, .SDcols = is.numeric]
  res <- dd[,
    .(
      n = unlist(lapply(.SD, length)),
      missing = unlist(lapply(.SD, function(x) length(which(is.na(x))))),
      mean = unlist(lapply(.SD, mean, na.rm = TRUE)),
      gmd = lapply(.SD, GiniMd, na.rm = TRUE),
      min = lapply(.SD, min, na.rm = TRUE),
      Q1 = lapply(.SD, quantile, probs = 0.25, na.rm = TRUE),
      median = lapply(.SD, median, na.rm = TRUE),
      Q3 = lapply(.SD, quantile, probs = 0.75, na.rm = TRUE),
      max = lapply(.SD, max, na.rm = TRUE)
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
  dta <- data.table("Selected" = rep(TRUE, n),
                   "Name" = vars,
                   "Type" = rep(factor(rep("IV", n), levels = vlev)),
                   "Link" = rep(factor(rep("identity", n), levels = linklev)),
                   "1. Argument" = rep("", n)
  )
  rhand <- rhandsontable::rhandsontable(dta, rowheaders = NULL) %>%
    rhandsontable::hot_col(col = "Type", type = "dropdown", source = vlev, readOnly = FALSE, allowInvalid = FALSE, strict = TRUE) %>%
    rhandsontable::hot_col(col = "Link", type = "dropdown", source = linklev, readOnly = FALSE, allowInvalid = FALSE, strict = TRUE)

  return(rhand)
}

# UI functions
# --------------------------------------
options(shiny.useragg = TRUE)
thematic::thematic_shiny(font = "auto")

# Set up app
shinyApp(ui = ui, server = server)