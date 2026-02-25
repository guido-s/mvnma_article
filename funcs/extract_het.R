extract_het <- function(x,...){
  
  method.model <- attributes(x)$method.model
  
  x <- x[names(x) != "cor"]
  
  if (method.model == "DM") {
    x <- x[names(x) != "sigma"]
  }
  
  het <- vector("list")
  
  for(i in 1:length(x)){
    
  het[[i]] <- x[[i]]$heterogeneity  
    
  }
  
  het <- list_rbind(het)

  het <- het %>% 
    dplyr::select(psi,lower,upper) %>% 
    mutate(psi = round(psi,digits = 2)) %>% 
    mutate(lower = round(lower, digits = 2)) %>% 
    mutate(upper = round(upper,digits = 2))
    
  return(het)
}


