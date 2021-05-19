#' Summarize significant correlations
#' 
#' @param cormat A matrix of significant correlations
#' 
#' @return A summary table of the correlated groups, as well as how big each group is
#' 
#' @export

summary_cors <- function(cormat){
  ns <- c()
  os <- c()
  gs <- c()
  # Get the names 
  for(otu in rownames(cormat)){
    grp <- names(which(cormat[otu,]>0))
    num <- length(grp)
    if(num > 0){
      ns <- c(ns, num)
      os <- c(os, otu)
      grp <- paste0(grp, sep = ";", collapse = "")
      gs <- c(gs, grp)
    }
  }
  s <- data.frame(OTU = os,
                  Group_Size = ns,
                  Groups = gs)
  s <- s[sort(s$Group_Size, decreasing=T, index.return=T)[[2]],]
  return(s)
}
