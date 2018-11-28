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

doublesort.cond2 <- function(targetValues, rowCriterias, rowBreakpoints, columnCriterias, columnBreakpoints, aggregationFunc, n.rows, n.columns) {
  output <- matrix(NA, nrow=n.rows, ncol=n.columns)
  
  column.ranks <- as.numeric(cut(columnCriterias, c(-Inf, as.vector(columnBreakpoints), Inf)))
  
  for (column in 1:n.columns) {
    column.matches <- which(column.ranks == column)
    column.rowCriterias <- rowCriterias[column.matches]
    column.targetValues <- targetValues[column.matches]
    
    row.ranks <- as.numeric(cut(column.rowCriterias, c(-Inf, as.vector(rowBreakpoints), Inf)))
    
    for (row in 1:n.rows) {
      row.matches <- which(row.ranks == row) # equals cell matches, since conditional sort
      
      # aggregate cell matches to get cell value
      output[row, column] <- aggregationFunc(column.targetValues[row.matches])
    }
  }
  
  return(output)
}

# output <- as.list(matrix(NA, nrow=10, ncol=10))
# dim(output) <- c(10, 10)
# 
# output[[2,2]] <- c(1,2,3)





