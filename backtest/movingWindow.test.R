# source('movingWindow.R')

movingWindow <- function(FUN, data, windowSize, stepSize=1, windowOffset=0, keepFirstWindowRows=F, returnAsMatrix=F) {
  if (is.null(dim(data)))
    stop('Parameter `data` needs to have dimensions.')
  
  if (windowSize <= 0)
    stop('Parameter `windowSize` cannot be negative or zero.')
  
  if (stepSize <= 0)
    stop('Parameter `stepSize` cannot be negative or zero.')
  
  if (windowOffset < 0)
    stop('Parameter `windowOffset` cannot be negative.')
  
  N <- nrow(data)
  i <- windowSize + windowOffset
  
  out <- list()
  
  # fill output with NA for rows with not enough data (e.g. first rows smaller than window size)
  if (keepFirstWindowRows && i > 1) {
    for (r in 1:(i - 1)) {
      if (returnAsMatrix) {
        out[[r]] <- rep(NA, ncol(data))
      }
      else {
        out[[r]] <- NA
      }
    }
  }
  
  # do moving window calls
  while (i <= N) {
    windowData <- data[(i - windowSize + 1):i - windowOffset, ]
    
    out[[length(out) + 1]] <- FUN(windowData)
    
    i <- i + stepSize
  }
  
  # return result as data matrix
  if (returnAsMatrix) {
    if (length(out) > 0) {
      out <- matrix(unlist(out), byrow=TRUE, nrow=length(out))
      
      if (!is.null(colnames(data)))
        colnames(out) <- colnames(data)
    }
    else {
      out <- NULL
    }
  }
  
  return(out)
}


test.equal <- function(actual, expected) {
  values <- unlist(actual)
  
  if (length(expected) != length(values) ||
      (length(expected) > 0 && !all(expected == values, na.rm=T)) ||
      (length(expected) > 0 && !all(which(is.na(values)) == which(is.na(expected)))))
  {
    cat('Expected --------------------------\n')
    print(expected)
    cat('Actual (unlisted) -----------------\n')
    print(values)
    stop("Expected does not equal actual values, check previous output.")
  }
}

test.equal.matrix <- function(actual, expected) {
  if (!is.null(actual) && !is.null(expected)) {  
    if (!is.matrix(actual)) {
      cat('Actual ----------------------------\n')
      print(actual)
      stop("Actual needs to be a matrix, check previous output.");
    }
    
    if (!is.matrix(expected)) {
      cat('Expected --------------------------\n')
      print(expected)
      stop("Expected needs to be a matrix, check previous output.");
    }
    
    if (dim(actual) != dim(expected) || !all(actual == expected, na.rm=T) || !all(which(is.na(actual)) == which(is.na(expected)))) {
      cat('Expected --------------------------\n')
      print(expected)
      cat('Actual (unlisted) -----------------\n')
      print(expected)
      stop("Expected does not equal actual values, check previous output.")
    }
  }
}



# #################################################
# # Test: General
# #################################################

# empty result: windowSize > # data rows
test.equal(movingWindow(sum, matrix(nrow=1, ncol=1), windowSize=2), c())

# empty result: windowSize + offset > # data rows
test.equal(movingWindow(sum, matrix(nrow=1, ncol=1), windowSize=1, windowOffset=1), c())

# return current value (windowSize=1)
test.equal(movingWindow(function(x) x, as.matrix(1:10), windowSize=1), 1:10)

# return every 3th value (windowSize=1, stepSize=3)
test.equal(movingWindow(function(x) x, as.matrix(1:10), windowSize=1, stepSize=3), c(1, 4, 7, 10))

# return previous value (windowSize=1, windowOffset=1)
test.equal(movingWindow(function(x) x, as.matrix(1:10), windowSize=1, windowOffset=1), 1:9)

# return previous value every 3th time (windowSize=1, windowOffset=1)
test.equal(movingWindow(function(x) x, as.matrix(1:10), windowSize=1, stepSize=3, windowOffset=1), c(1, 4, 7))

# sum current and previous values (windowSize=2)
test.equal(movingWindow(sum, as.matrix(1:10), windowSize=2), c(1+2, 2+3, 3+4, 4+5, 5+6, 6+7, 7+8, 8+9, 9+10))

# sum past two values, currentValue is ignored (windowSize=2, windowOffset=1)
test.equal(movingWindow(sum, as.matrix(1:10), windowSize=2, windowOffset=1), c(1+2, 2+3, 3+4, 4+5, 5+6, 6+7, 7+8, 8+9))

# sum past 3 values every 2nd time (windowSize=3, stepSize=2, windowOffset=1)
test.equal(movingWindow(sum, as.matrix(1:10), windowSize=3, stepSize=2, windowOffset=1), c(1+2+3, 3+4+5, 5+6+7, 7+8+9))


#################################################
# Test: keepFirstWindowRows
#################################################

# return current value (windowSize=1)
test.equal(movingWindow(function(x) x, as.matrix(1:10), windowSize=1, keepFirstWindowRows=T), 1:10)

# return every 3th value (windowSize=1, stepSize=3)
test.equal(movingWindow(function(x) x, as.matrix(1:10), windowSize=1, stepSize=3, keepFirstWindowRows=T), c(1, 4, 7, 10))

# return previous value (windowSize=1, windowOffset=1)
test.equal(movingWindow(function(x) x, as.matrix(1:10), windowSize=1, windowOffset=1, keepFirstWindowRows=T), c(NA, 1:9))

# sum previous 3 values (windowSize=1, windowOffset=1)
test.equal(movingWindow(sum, as.matrix(1:10), windowSize=3, windowOffset=1, keepFirstWindowRows=T), c(NA, NA, NA, 1+2+3, 2+3+4, 3+4+5, 4+5+6, 5+6+7, 6+7+8, 7+8+9))



#################################################
# Test: returnAsMatrix
#################################################

# empty result: windowSize > # data rows
test.equal.matrix(movingWindow(sum, matrix(nrow=1, ncol=1), windowSize=2, returnAsMatrix=T), NULL)

# empty result: windowSize + offset > # data rows
test.equal.matrix(movingWindow(sum, matrix(nrow=1, ncol=1), windowSize=1, windowOffset=1, returnAsMatrix=T), NULL)

# return current value (windowSize=1)
test.equal.matrix(movingWindow(function(x) x, as.matrix(1:10), windowSize=1, returnAsMatrix=T), matrix(1:10, nrow=10, ncol=1))

# return previous value (windowSize=1, windowOffset=1)
test.equal.matrix(movingWindow(function(x) x, as.matrix(1:10), windowSize=1, windowOffset=1, keepFirstWindowRows=T, returnAsMatrix=T), matrix(c(NA, 1:9), nrow=10, ncol=1))

# sum current and previous values (windowSize=2) of two columns
test.equal.matrix(movingWindow(colSums, as.matrix(data.frame(a=1:10, b=11:20)), windowSize=2, returnAsMatrix=T),
                  as.matrix(data.frame(a=c(1+2, 2+3, 3+4, 4+5, 5+6, 6+7, 7+8, 8+9, 9+10),
                                       b=c(11+12, 12+13, 13+14, 14+15, 15+16, 16+17, 17+18, 18+19, 19+20))))











