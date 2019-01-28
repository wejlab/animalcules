#' Get alpha diversity
#'
#' @param sam_table A dataframe with 2 cols, richness and condition
#' @param alpha_stat Wilcoxon rank sum test or T-test for the test
#' @return A dataframe
#'
#' @examples
#' df_test <- data.frame(richness = 1:10,
#' condition = c(rep(1,5), rep(0,5)))
#' alpha_div_test(df_test,alpha_stat="Wilcoxon rank sum test")
#'
#' @export

alpha_div_test <- function(sam_table, alpha_stat){
    if (length(unique(sam_table$condition)) == 2){
    if (alpha_stat == "Wilcoxon rank sum test"){
      tmp <- wilcox.test(richness ~ condition, data = sam_table)
      output <- c(tmp$method, tmp$p.value)
      output.table <- data.frame(output)
      rownames(output.table) <- c("Method", "P-value")
      output.table
    } else if (alpha_stat == "T-test"){
      tmp <- t.test(richness ~ condition, data = sam_table)
      output <- c(tmp$method, tmp$p.value)
      output.table <- data.frame(output)
      rownames(output.table) <- c("Method", "P-value")
      output.table
    } else{
      output.table = data.frame(("Condition level number is 2, please use Wilcoxon rank sum test."))
      colnames(output.table) <- "Note"
      output.table
    }

    } else if (length(unique(sam_table$condition)) > 2){
    if (alpha_stat == "Wilcoxon rank sum test"){
      result.list <- list()
      sam_table.list <- list()
      for (i in 1:length(unique(sam_table$condition))){
        sam_table.list[[i]] <- sam_table[which(sam_table$condition != unique(sam_table$condition)[i]),]
        result.list[[i]] <- wilcox.test(richness ~ condition, data = sam_table.list[[i]])
      }
      output.table <- NULL
      group.name <- c()
      for (i in 1:length(result.list)){
        output.tmp <- c(result.list[[i]]$method, result.list[[i]]$p.value)
        output.table <- cbind(output.table, output.tmp)
        group.name[i] <- paste(unique(sam_table.list[[i]]$condition), collapse = " and ")
      }
      rownames(output.table) <- c("Method", "P-value")
      colnames(output.table) <- group.name
      output.table

    } else if (alpha_stat == "T-test"){
              result.list <- list()
      sam_table.list <- list()
      for (i in 1:length(unique(sam_table$condition))){
        sam_table.list[[i]] <- sam_table[which(sam_table$condition != unique(sam_table$condition)[i]),]
        result.list[[i]] <- t.test(richness ~ condition, data = sam_table.list[[i]])
      }
      output.table <- NULL
      group.name <- c()
      for (i in 1:length(result.list)){
        output.tmp <- c(result.list[[i]]$method, result.list[[i]]$p.value)
        output.table <- cbind(output.table, output.tmp)
        group.name[i] <- paste(unique(sam_table.list[[i]]$condition), collapse = " and ")
      }
      rownames(output.table) <- c("Method", "P-value")
      colnames(output.table) <- group.name
      output.table

    } else{
      tmp <- kruskal.test(richness ~ condition, data = sam_table)
      output <- c(tmp$method, tmp$p.value)
      output.table <- data.frame(output)
      rownames(output.table) <- c("Method", "P-value")
      output.table
    }

    } else{
    "Condition level must be at least 2."
    }
}
