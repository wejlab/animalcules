#' Find the taxonomy for maximum 300 tids
#'
#' @param tids Given taxonomy ids
#' @return taxondata Data with the taxonomy information
#' @import rentrez
#' @import XML
#' @export
#' @examples
#' example_data_dir <- system.file("example/data", package = "PathoStat")
#' pathoreport_file_suffix <- "-sam-report.tsv"
#' datlist <- readPathoscopeData(example_data_dir,
#' pathoreport_file_suffix, input.files.name.vec = as.character(1:6))
#' dat <- datlist$data
#' ids <- rownames(dat)
#' tids <- unlist(lapply(ids, FUN = grepTid))
#' taxonLevels <- find_taxonomy_300(tids[1:5])

find_taxonomy_300 <- function(tids) {
    if (is.null(tids)) {
        return(NULL)
    }

    na.vec <- c()
    for (i in seq_len(length(tids))){
        if(is.na(tids[i])){
            na.vec <- c(na.vec, i)
        }
    }

    r_fetch <- entrez_fetch(db = "taxonomy", id = tids, rettype = "xml")
    dat <- xmlToList(r_fetch)
    taxonLevels <- lapply(dat, function(x) x$LineageEx)
    if(!is.null(na.vec)){
        for(i in seq_len(length(na.vec))){
        taxonLevels <- append(taxonLevels, list(NA), na.vec[i]-1)
        }
    }


    return(taxonLevels)
}
