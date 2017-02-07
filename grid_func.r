grid1 <- function(n, range = c(0., 1.), edge = TRUE)
{
  if(any(n < 0. | round(n) != n))
    stop("n must be nonnegative and integer")
  G <- rep(0., n)
  if(edge) {
    G <- seq(from = min(range), to = max(range), by = abs(diff(
                                                   range))/(n - 1.))
  }
  else {
    lj <- abs(diff(range))
    incr <- lj/(2. * n)
    G <- seq(from = min(range) + incr, to = max(range) - incr,
             by = 2. * incr)
  }
  G
}

grid2 <- function(x, y)
{
  lx <- length(x)
  ly <- length(y)
  xy <- matrix(0, nrow = lx * ly, ncol = 2)
  l <- 0
  for(j in 1:ly) {
    for(i in 1:lx) {
      l <- l + 1
      xy[l,  ] <- c(x[i], y[j])
    }
  }
  xy
}
