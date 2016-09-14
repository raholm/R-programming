euclidian <- function(a, b)
{
  euclidian.check.input(a, b)
  
  while (b != 0)
  {
    tmp <- b
    b <- a %% b
    a <- tmp
  }
  
  return(a)
}

euclidian.check.input <- function(a, b)
{
  stopifnot(is.numeric(a), length(a) == 1)
  stopifnot(is.numeric(b), length(b) == 1)
}

euclidian(123612, 13892347912)
euclidian(100, 1000)