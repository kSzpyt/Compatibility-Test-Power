rmix <- function(n, family_1, par_1, family_2, par_2, p)
{
  set.seed(10)
  pp <- sample(c(1,2), n, replace = TRUE, prob = c(p, 1-p))
  n1 <- length(pp[pp==1])
  n2 <- length(pp[pp==2])
  
  set.seed(10)
  eval(parse(text = paste0("fun1 <- r", family_1)))
  r1 <- do.call(fun1, as.list(c(n1, par_1)))
  
  set.seed(10)
  eval(parse(text = paste0("fun2 <- r", family_2)))
  r2 <- do.call(fun2, as.list(c(n2, par_2)))
  
  return(c(r1,r2))
}