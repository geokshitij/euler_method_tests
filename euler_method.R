euler_method <- function(f, y0, t0, h, n) {
  # Initialize variables
  y <- numeric(n + 1)
  t <- numeric(n + 1)
  
  # Set initial conditions
  y[1] <- y0
  t[1] <- t0
  
  # Euler method iteration
  for (i in 1:n) {
    y[i + 1] <- y[i] + h * f(t[i], y[i])
    t[i + 1] <- t[i] + h
  }
  
  return(list(t = t, y = y))
}
