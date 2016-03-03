
mgf_norm <- function(t, mu, sigma2){
  return(exp(mu * t + 1/2 * sigma2 * t^2))
}

curve(mgf_norm(t = x, mu = 0, sigma2 = 1), from = -10, to = 20)
curve(mgf_norm(t = x, mu = 2, sigma2 = 1), add = TRUE, col="red")
