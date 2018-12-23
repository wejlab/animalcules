#' Greps the tid from the given identifier string
#'
#' @param id Given identifier string
#' @return tid string
#' @export
#' @examples
#' grep_tid("ti|700015|org|Coriobacterium_glomerans_PW2")

grep_tid <- function(id) {
    tid <- strsplit(id, "\\|")
    tid <- sapply(tid, function(x) x[2])
    return(tid)
}
