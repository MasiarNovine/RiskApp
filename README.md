# RiskApp

This is a R shiny app to model a statistical risk model based on a logistic ordinal proportional odds model. Currently, there are three basic functionailities implemented: 

1. Loading a .rds file containing a data.frame or data.table.
2. Based on the numeric values in the data, a model graph is automatically constructed using correlation thresholding based on three similarity indices: Spearman rho, Pearsons r and Hoeffding's D (independence test). It is possible to to use the square, absolute or no transformation of the similarity values.
3. Defining a model formulation.
4. Fit a logistic ordinal proportional odds model based on the implementation described in Harrell (2015).

# Literature
Harrell, Frank E. Jr., Regression Modeling Strategies 2 (Springer Cham, 2015).
