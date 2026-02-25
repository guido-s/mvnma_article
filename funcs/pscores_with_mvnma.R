
x <- mvmod_DM_vague_corr

name <- row.names(x[[1]]$TE.random)

prep_new <- function(x,name,...){

method.model <- attributes(x)$method.model

x <- x[names(x) != "cor"]

if (method.model == "DM") {
  x <- x[names(x) != "sigma"]
}

n <- out_TE <- out_var <- vector("list")

for(i in 1:length(x)){
  
n[[i]] <- row.names(x[[i]]$basic_estimates)  
  
}

k = length(x)  

n = as.data.frame(table(unlist(n)))
  
p <- length(which(n$Freq == k))

outcomes <- var.outcomes <- array(rep(0,p^2*k),c(p,p,k))

for(i in 1:length(x)){
  
  x[[i]]$TE.random = x[[i]]$TE.random[lower.tri(x[[i]]$TE.random,diag = FALSE)]
  
  out_TE[[i]] <- matrix(rep(0,p^2),p,p,dimnames=list(name))  
  
  out_TE[[i]][lower.tri(out_TE[[i]])]=x[[i]]$TE.random
  
  out_TE[[i]] <- t(out_TE[[i]])
  # 
  out_TE[[i]][lower.tri(out_TE[[i]])]=x[[i]]$TE.random
  
  x[[i]]$var.random = (x[[i]]$seTE.random[lower.tri(x[[i]]$seTE.random,diag = FALSE)])^2
  
  out_var[[i]] <- matrix(rep(0,p^2),p,p,dimnames=list(name))  
  
  out_var[[i]][lower.tri(out_var[[i]])]=x[[i]]$var.random
  
  out_var[[i]] <- t(out_var[[i]])
  # 
  out_var[[i]][lower.tri(out_var[[i]])]=x[[i]]$var.random
  
  
  outcomes[,,i] = out_TE[[i]]
  
  var.outcomes[,,i] = out_var[[i]]
  
}
  

res <- list("outcomes" = outcomes,
            "var.outcomes" = var.outcomes
            )

return(res)

}



preps <- prep_new(mvmod_DM_vague_corr,name = name)

outcomes <- preps$outcomes
var.outcomes <- preps$var.outcomes
# correlation=matrix(c(1,0,0,1),2,2)
# correlation1=matrix(c(1,0.78,0.78,1),2,2)

pscores.pain_dis <-pscores(outcomes,var.outcomes,correlation,c(0,0),type=c("B","B"),name)
pscores.pain_dis1 <-pscores(outcomes,var.outcomes,correlation1,c(0,0),type=c("B","B"),name)

mean(pscores.pain_dis)
mean(pscores.pain_dis1)


# pscores.antipsychotics <- pscores(outcomes,var.outcomes,correlation,c(0,0,0,0,0),type=c("H","H","B","B","B"),name)


