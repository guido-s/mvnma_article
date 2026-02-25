extract_se <- function(x,outcome=NULL,...){
  
class <- class(x)

    
if(class == "mvnma"){
  
  
  if(is.null(outcome)){
    stop("Please specify outcome...")
  }
  
  method.model <- attributes(x)$method.model
  
  x <- x[names(x) != "cor"]
  
  if (method.model == "DM") {
    x <- x[names(x) != "sigma"]
  }
  
  outcomes <- names(x)
  
x <- x[[outcome]]

se_all <- x$seTE.random


se_all <- se_all[lower.tri(se_all,diag = FALSE)]

msg <- paste(class," model based on ",method.model," approach for outcome ", outcomes[outcome])
  
}  

if(class == "netmeta"){
  
se_all <- x$seTE.random

se_all <- se_all[lower.tri(se_all,diag = FALSE)]

msg <- paste("netmeta model")

  
}  

print(msg)
  
return(se_all)  
    
}




