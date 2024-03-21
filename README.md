# RiskApp

This is a R shiny app for modeling. Currently, there are three basic functionailities: 

(1) Loading a .rds file containing a data.frame or data.table.
(2) Based on the numeric values in the data, a model graph is automatically constructed using correlation thresholding based on three similarity indices: Spearman rho, Pearsons r and Hoeffding's D (independence test). It is possible to define to use the square, absolute or no transformation.
(3) Defining a model formulation.
(4) Fit a logistic ordinal proportional odds model based on the implementation described in Harrell (2015).

# Literature
Harrell, Frank E. Jr., Regression Modeling Strategies 2 (Springer Cham, 2015).
