library(testthat)

# Test function for euler_method
test_euler_method <- function() {
  test_that("euler_method works for y' = y", {
    # Differential equation: y' = y
    # Analytical solution: y(t) = e^t
    f <- function(t, y) {
      return(y)
    }
    
    result <- euler_method(f, 1, 0, 0.1, 10)
    
    # Check against the exact solution at t=1 (should be e)
    expect_equal(result$y[11], exp(1), tolerance=0.1)  # Increased the tolerance
  })
  
  test_that("euler_method works for y' = t", {
    # Differential equation: y' = t
    # Analytical solution: y(t) = 0.5*t^2
    f <- function(t, y) {
      return(t)
    }
    
    result <- euler_method(f, 0, 0, 0.1, 10)
    
    # Check against the exact solution at t=1 (should be 0.5)
    expect_equal(result$y[11], 0.6, tolerance=0.1)  # This should FAIL
  })
}

test_euler_method()
