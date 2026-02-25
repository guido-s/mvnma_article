# Function to calculate p-scores

p_scores <- function(x, CIV, type, correlation = NULL){
  prepare_data <- prep(x) 
  outcomes <- prepare_data$outcomes
  var.outcomes <- prepare_data$var.outcomes
  comm <- prepare_data$comm
  if (length(x) == 1)
    correlation <- diag(length(x)) # for outcomes we suppose that they are independent
  #
  if (is.null(correlation))
    correlation <- diag(length(x))
  # for multiple outcomes, if no information given we suppose that they are independent
  #
  ps <- pscores(outcomes, var.outcomes, correlation,
                -CIV, type, label = as.vector(comm))
  #
  ps <- as.data.frame(ps)
  names(ps) <- "P_score"
  #
  ps <- ps %>% 
    arrange(desc(P_score)) %>% 
    mutate(P_score = round(P_score,digits = 4))
  #
  ps
}
