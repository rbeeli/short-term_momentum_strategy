movingWindow <- function(FUN, data, windowSize, stepSize, windowOffset, keepFirstWindowRows) {
  if (is.null(dim(data))) {
    stop('Parameter `data` needs to have dimensions.')
  }
  
  if (windowSize <= 0) {
    stop('Parameter `windowSize` cannot be negative or zero.')
  }
  
  if (missing(windowOffset)) {
    windowOffset <- 0
  }
  
  if (missing(stepSize)) {
    stepSize <- 1
  }
  
  if (missing(keepFirstWindowRows)) {
    keepFirstWindowRows <- FALSE
  }
  
  if (stepSize <= 0) {
    stop('Parameter `stepSize` cannot be negative or zero.')
  }
  
  if (windowOffset < 0) {
    stop('Parameter `windowOffset` cannot be negative.')
  }
  
  N <- nrow(data)
  i <- windowSize + windowOffset
  
  out <- list()
  
  # fill output with NA for rows with not enough data (e.g. first rows smaller than window size)
  if (keepFirstWindowRows && i > 1) {
    for (r in 1:(i - 1)) {
      out[[r]] <- NA
    }
  }
  
  while (i <= N) {
    windowData <- data[(i - windowSize + 1):i - windowOffset, ]
    
    out[[length(out) + 1]] <- FUN(windowData)
    
    i <- i + stepSize
  }
  
  return(out)
}
