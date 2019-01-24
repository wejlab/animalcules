#' Find the Taxonomy Information Matrix
#'
#' @param names Row names of the taxonomy matrix
#' @param taxonLevels Taxon Levels of all tids
#' @return taxmat Taxonomy Information Matrix
#' @export

find_taxon_mat <- function(names, taxonLevels) {
    # tax.name <- c('superkingdom', 'kingdom', 'phylum', 'class', 'order',
    #     'suborder', 'family', 'subfamily', 'genus', 'subgenus', 'species',
    #     'subspecies', 'no rank')
    tax.name <- c('superkingdom', 'kingdom', 'phylum', 'class', 'order',
        'family', 'genus', 'species', 'no rank')
    tl <- c()
    for (i in seq_len(length(tax.name))) {
        tl[tax.name[i]] <- "others"
    }
    taxmat <- NULL
    for (i in seq_len(length(taxonLevels))) {
        taxrow <- tl
        tLineageEx <- taxonLevels[[i]]
        for (j in seq_len(length(tLineageEx))) {
            rank <- tLineageEx[[j]]["Rank"]
            #taxid <- tLineageEx[[j]]["TaxId"]
            scientificName <- tLineageEx[[j]]["ScientificName"]
            if (!is.null(rank) && !is.na(rank) && rank %in% tax.name) {
                #taxrow[as.character(rank)] <- as.character(taxid)
                taxrow[as.character(rank)] <- as.character(scientificName)
            }
        }
        taxmat <- rbind(taxmat, taxrow)
    }

    rownames(taxmat) <- names
    #print(taxmat)
    return(taxmat)
}
