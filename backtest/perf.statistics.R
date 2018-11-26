# Sortino ratio
sortino.ratio <- function(returns, MAR, scale) {
  ex.rets <- returns - MAR
  downside.risk <- sqrt(sum(1/length(returns) * ex.rets[which(ex.rets < 0)]^2))
  ratio <- mean(returns - MAR) / downside.risk
  return(ratio * sqrt(scale))
}

# Sharpe ratio
sharpe.ratio <- function(returns, rf, scale) {
  ratio <- mean(returns - rf) / sd(returns - rf)
  return(ratio * sqrt(scale))
}
