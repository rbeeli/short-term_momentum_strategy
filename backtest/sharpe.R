# Sharpe ratio
sharpe.ratio <- function(returns, rf, scale) {
  return(mean(returns - rf) / sd(returns - rf) * sqrt(scale))
}
