library(dplyr)


doublesort.uncond <- function(targetValues, rowCriterias, columnCriterias, aggregationFunc, n.rows, n.columns) {
  output <- matrix(NA, nrow=n.rows, ncol=n.columns)
  
  row.ranks <- ntile(rowCriterias, n.rows)
  column.ranks <- ntile(columnCriterias, n.columns)
  
  for (row in 1:n.rows) {
    row.matches <- which(row.ranks == row)
    
    for (column in 1:n.columns) {
      column.matches <- which(column.ranks == column)
      
      # merge row and column matches to get cell values
      cell.matches <- intersect(row.matches, column.matches)
      
      # aggregate cell matches to get cell value
      output[row, column] <- aggregationFunc(targetValues[cell.matches])
    }
  }
  
  return(output)
}

doublesort.cond <- function(targetValues, rowCriterias, columnCriterias, aggregationFunc, n.rows, n.columns) {
  output <- matrix(NA, nrow=n.rows, ncol=n.columns)
  
  column.ranks <- ntile(columnCriterias, n.columns)
  
  for (column in 1:n.columns) {
    column.matches <- which(column.ranks == column)
    column.rowCriterias <- rowCriterias[column.matches]
    column.targetValues <- targetValues[column.matches]
    
    row.ranks <- ntile(column.rowCriterias, n.rows)
    
    for (row in 1:n.rows) {
      row.matches <- which(row.ranks == row) # equals cell matches, since conditional sort
      
      # aggregate cell matches to get cell value
      output[row, column] <- aggregationFunc(column.targetValues[row.matches])
    }
  }
  
  return(output)
}

doublesort.cond2 <- function(targetValues, rowCriterias, columnCriterias, marketCaps, aggregationFunc, n.rows, n.columns) {
  output <- matrix(NA, nrow=n.rows, ncol=n.columns)
  
  column.ranks <- ntile(columnCriterias, n.columns)

  for (column in 1:n.columns) {
    column.matches <- which(column.ranks == column)
    column.rowCriterias <- rowCriterias[column.matches]
    column.targetValues <- targetValues[column.matches]
    column.marketCaps <- marketCaps[column.matches]

    row.sort.idxs <- sort(column.rowCriterias, index.return=T)$ix
    row.sort.marketCaps <- column.marketCaps[row.sort.idxs]
    row.deciles <- ceiling(cumsum(row.sort.marketCaps) / sum(row.sort.marketCaps) * n.rows)
    
    for (row in 1:n.rows) {
      decile.idxs <- which(row.deciles == row)
      
      # aggregate cell matches to get cell value
      if (length(decile.idxs) > 0) {
        output[row, column] <- aggregationFunc(column.targetValues[decile.idxs])
      }
      else {
        output[row, column] <- 0
      }
    }
  }
  
  return(output)
}

# output <- as.list(matrix(NA, nrow=10, ncol=10))
# dim(output) <- c(10, 10)
# 
# output[[2,2]] <- c(1,2,3)





