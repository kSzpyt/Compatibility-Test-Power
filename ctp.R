ctp <- function(datalist){
  
  a <- sapply(datalist, function(x)
  {
    shapiro.test(x)$p.value
  })
  
  b <- sapply(datalist, function(x)
  {
    ks.test(x, pnorm)$p.value
  })
  
  c <- sapply(datalist, function(x)
  {
    jarque.bera.test(x)$p.value
  })
  
  sw <- mean(a < 0.05)
  ks <- mean(b < 0.05)
  jb <- mean(c < 0.05)
  
  r <- tibble(sw_test = sw, ks_test = ks, jb_test = jb)
  
  return(r)
}
