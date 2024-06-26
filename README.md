# RiskApp

This is a R shiny app to model a statistical risk model based on a logistic ordinal proportional odds (PO) model. Currently, there are three basic functionailities implemented: 

* Loading a .rds file containing a data.frame or data.table.
* Based on the numeric data, a model graph is automatically constructed using correlation thresholding based on two similarity indices: Spearman rho and Pearsons r. The correlations might either be squared, taken as absolute values or kept as is.

Features, which are not yet implemented:

* Evaluating a model formulation.
* Fit a proportional odds model based on the implementation described in Harrell (2015).
* Visualization of the model results.

The app uses the following R packages: shiny (Chang et al, 2023), networkD3 (Allaire et al, 2017) for building the model graph, rms (Harrell, 2023) for the similarity indices and the implementation of the PO model.

# Literature

1. J.J. Allaire, Christopher Gandrud, Kenton Russell and CJ Yetman (2017). networkD3: D3 JavaScript Network Graphs from R. R package version 0.4. https://CRAN.R-project.org/package=networkD3.
2. Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff Allen, Jonathan McPherson, Alan Dipert and Barbara Borges (2023). shiny: Web Application Framework for R. R package version 1.8.0. https://CRAN.R-project.org/package=shiny.
3. Frank E. Harrell Jr. (2015) Regression Modeling Strategies 2 (Springer Cham).
4. Frank E. Harrell Jr. (2023). rms: Regression Modeling Strategies. R package version 6.7-1. https://CRAN.R-project.org/package=rms.
