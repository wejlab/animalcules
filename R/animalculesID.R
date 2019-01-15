#' Animalcules ID 
#'
#' This function will read in a .bam file, annotate the taxonomy and genome names, reduce the mapping ambiguity using a mixture model, and output a .csv file with the results. Right now, it assumes that the genome library/.bam files uses NCBI accession names for reference names (rnames in .bam file). 
#' @param bam_file The .bam file that needs to be summarized, annotated, and needs removal of ambiguity
#' @param out_file The name of the .csv output file. Deacults to the bam_file basename plus "animalculesID.csv"
#' @param EMconv The convergence parameter of the EM algorithm. Default set at 0.001
#' @param EMmaxIts The maximum number of EM iterations, regardless of whether the EMconv is below the threshhold. Default set at 50. If set at 0, the algorithm skips the EM step and summarizes the .bam file 'as is'.  
#' 
#' @return
#' This function returns a .csv file with annotated read counts to genomes with mapped reads. The function iself returns the output .csv file name. 
#' 
#' @examples
#' ## Get a reference genome library
#' download_refseq('viral', compress = FALSE)
#' 
#' ## Make and align to a single a reference genome library
#' mk_subread_index('viral.fasta')
#' readPath <- system.file("extdata", "virus_example.fastq", package = "animalcules.preprocess")
#' viral_map <- align_target( readPath, "viral", "virus_example")
#' 
#' #### Apply animalcules ID:
#' animalcules_id( viral_map )
#' 
#' @export

animalcules_id <- function(bam_file, out_file = paste(tools::file_path_sans_ext(bam_file), 
                                                      ".animalculesID.csv", sep = ""), EMconv = 0.001, EMmaxIts = 50) {
  ## read in .bam file
  message("Reading .bam file: ", bam_file)
  reads <- Rsamtools::scanBam(bam_file, param = Rsamtools::ScanBamParam(what = c("qname", 
                                                                                 "rname")))
  unmapped <- is.na(reads[[1]]$rname)
  mapped_qname <- reads[[1]]$qname[!unmapped]
  mapped_rname <- reads[[1]]$rname[!unmapped]
  read_names <- unique(mapped_qname)
  accessions <- unique(mapped_rname)
  message("\tFound ", length(read_names), " reads aligned to ", length(accessions), 
          " NCBI accessions")
  
  ## convert accessions to taxids and get genome names
  message("Obtaining taxonomy and genome names")
  suppressMessages(tax_id_all <- taxize::genbank2uid(id = accessions))
  taxids <- sapply(tax_id_all, function(x) x[1])
  unique_taxids <- unique(taxids)
  taxid_inds <- match(taxids, unique_taxids)
  genome_names <- sapply(tax_id_all, function(x) attr(x, "name"))
  unique_genome_names <- genome_names[taxid_inds]
  message("\tFound ", length(unique_taxids), " unique NCBI taxonomy IDs")
  
  ## make an aligment matrix (rows: reads, cols: unique taxids)
  message("Setting up the EM algorithm")
  qname_inds <- match(mapped_qname, read_names)
  rname_inds <- match(mapped_rname, accessions)
  rname_tax_inds <- taxid_inds[rname_inds]
  gammas <- matrix(0, nrow = length(read_names), ncol = length(unique_taxids))
  # Make this faster or more memory efficient?
  for (i in 1:length(qname_inds)) {
    gammas[qname_inds[i], rname_tax_inds[i]] <- 1
  }
  
  ## EM algorithm for reducing abiguity in the alignments
  gammas_new <- gammas
  pi_old <- 1/nrow(gammas)
  pi_new <- apply(gammas_new, 2, mean)
  conv <- max(abs(pi_new - pi_old)/pi_old)
  it <- 0
  
  message("Starting EM iterations")
  while (conv > EMconv & it < EMmaxIts) {
    ## Expectation Step: Estimate the expected value for each read to each
    ## genome
    weighted_gamma <- gammas_new * matrix(pi_new, nrow(gammas), length(pi_new), 
                                          byrow = T)
    weighted_gamma_sums <- rowSums(weighted_gamma)
    gammas_new <- weighted_gamma/weighted_gamma_sums
    
    ## Maimization step: proportion of reads to each genome, ??Estimate
    ## thetas??
    pi_new <- apply(gammas_new, 2, mean)
    
    ## Check convergence
    it <- it + 1
    conv <- max(abs(pi_new - pi_old)/pi_old, na.rm = TRUE)
    pi_old <- pi_new
  }
  message("\tDONE!")
  
  ## Collect results
  best_hit <- table(unlist(apply(gammas_new, 1, function(x) which(x == 
                                                                    max(x)))))
  hits_ind <- as.numeric(names(best_hit))
  final_taxids <- unique_taxids[hits_ind]
  final_genomes <- unique_genome_names[hits_ind]
  proportion <- best_hit/sum(best_hit)
  EMreads <- round(colSums(gammas_new)[hits_ind], 1)
  EMprop <- colSums(gammas_new)[hits_ind]/sum(gammas_new)
  results <- cbind(TaxonomyID = final_taxids, Genome = final_genomes, 
                   read_count = best_hit, Proportion = proportion, EMreads = EMreads, 
                   EMProportion = EMprop)
  results <- results[order(best_hit, decreasing = TRUE), ]
  message("Found reads for ", length(best_hit), " genomes")
  
  ## Write to file
  write.csv(results, file = out_file, row.names = F)
  message("Results written to ", out_file)
  
  return(list(out_file, results))
}

