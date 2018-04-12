ctp_wx <- function(datalist1, datalist2){
  
  a <- mapply(function(x, y)
  {
    wilcox.test(x, y, paired = T)$p.value
  }, datalist1, datalist2)
  
  wx <- mean(a < 0.05)
  
  r <- tibble(wx_test = wx)
  
  return(r)
}


