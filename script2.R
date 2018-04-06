source("rmix.R")
norm_sample <- lapply(rep(1, 300), function(x){
  rnorm(300)
})

ctp <- function(datalist){
  
  a <- sapply(datalist, function(x)
  {
    shapiro.test(x)$p.value
  })
  sh <- mean(a > 0.05)
  
  a <- sapply(datalist, function(x)
  {
    ks.test(x, pnorm)$p.value
  })
  ks <- mean(a > 0.05)
  
  a <- sapply(datalist, function(x)
  {
    jarque.bera.test(x)$p.value
  })
  jb <- mean(a > 0.05)
  
  r <- data.frame(
    shapiro = sh,
    kolgomorasfas = ks,
    jarquebera = jb
  )
  
  return(r)
}


ctp(norm_sample)
