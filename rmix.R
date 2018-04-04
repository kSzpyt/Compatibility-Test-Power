
rmix <- function(n, family_1, par_1, family_2, par_2, p)
{
  pp <- sample(c(1,2), n, replace = TRUE, prob = c(p, 1-p))
  # set.seed(10)
  if(family_1 == "norm"){
    r1 <- rnorm(length(pp[pp == 1]), mean = par_1[1], sd = par_1[2])
  } 
  else if(family_1 == "pois"){
    r1 <- rpois(length(pp[pp == 1]), par_1)
  }
  #set.seed(10)
  if(family_2 == "norm"){
    r2 <-  rnorm(length(pp[pp == 2]), mean = par_2[1], sd = par_2[2])
  } 
  else if(family_2== "pois"){
    r2 <- rpois(length(pp[pp == 2]), par_2)
  }
  
  r <- c(r1, r2)
  return(r)
  
}