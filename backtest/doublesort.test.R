source('doublesort.R')


targetValues <- runif(10000)
rowCriterias <- sample(1:1000, 1000, replace=T)
columnCriterias <- sample(1:1000, 1000, replace=T)

doublesort.uncond(targetValues, rowCriterias, columnCriterias, mean, n.rows=10, n.columns=10)
doublesort.cond(targetValues, rowCriterias, columnCriterias, mean, n.rows=10, n.columns=10)
