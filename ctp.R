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
  jb <- mean(a < 0.05)
  
  r <- tibble(
    sw_test = sh,
    ks_test = ks,
    jb_test = jb
  )
  
  return(r)
}




