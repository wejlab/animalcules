#' Helper function for demultiplexing
#'
#' Helper function for demultiplexing sequencing reads, designed in a way to allow for parallelization accross barcodes (parallel extraction of reads by barcode). This function takes a specific barcode (numeric index) from lists of sample names/barcodes, a Biostrings::DNAStringSet of barcodes by sequence header, and a Biostrings::QualityScaledXStringSet of reads corresponding to the barcodes. Based on the barcode index given, it extracts all reads for the indexed barcode and writes all the reads from that barcode to a separate .fastq file. 
#' @param barcodeIndex Which barcode (integer number or index) in the barcodes or sample name to use for read extraction 
#' @param barcodes A list of all barcodes in the sequencing dataset. Correlates and in same order as sampleNames
#' @param sampleNames A list of sample names or identifiers associated with each barcode in the barcodes list
#' @param index A Biostrings::DNAStringSet that contains the read headers and barcode sequence for each header in the sequence slot
#' @param reads A Biostrings::QualityScaledXStringSet that has the same headers and order as the index file, but contains the read sequences and their quality scores
#' @param location A directory location to store the demuliplexed read files. Defaults to generate a new subdirectory at './demultiplex_fastq'
#' @param rcBarcodes Should the barcode indexes in the barcodes list be reverse complemented to match the sequences in the index DNAStringSet? Defaults to TRUE
#' @param hDist Uses a Hamming Distance or number of base differences to allow for inexact matches for the barcodes/indexes. Defaults to 0. Warning: if the Hamming Distance is >=1 and this leads to inexact index matches to more than one barcode, that read will be written to more than one demultiplexed read files 
#' 
#' @return Writes a single .fastq file that contains all reads whose index matches the barcode specified. This file will be written to the location directory, and will be named based on the specified sampleName and barcode, e.g. './demultiplex_fastq/SampleName1_GGAATTATCGGT.fastq.gz' 
#'
#' @examples
#' ## Load example barcode, index, and read data into R session:
#' barcodePath <- system.file("extdata", "barcodes.txt", package = "animalcules.preprocess")
#' bcFile <- read.table(barcodePath, sep = "\t", header = T)
#' 
#' indexPath <- system.file("extdata", "virus_example_index.fastq", package = "animalcules.preprocess")
#' inds <- Biostrings::readDNAStringSet(indexPath, format = "fastq")
#' 
#' readPath <- system.file("extdata", "virus_example.fastq", package = "animalcules.preprocess")
#' reads <- Biostrings::readQualityScaledDNAStringSet(readPath)
#' 
#' ## Extract reads from the first barcode
#' results <- extractReads(1, bcFile[, 2], bcFile[, 1], inds, reads, rcBarcodes = FALSE, 
#'     location = ".")
#' results
#' 
#' ## Extract reads from multiple barcodes
#' more_results <- lapply(1:6, extractReads, bcFile[, 2], bcFile[, 1], inds, 
#'     reads, rcBarcodes = FALSE, location = ".")
#'                        
#' ## BiocParallel application
#' multicoreParam <- BiocParallel::MulticoreParam(workers = 3)
#' parallel_results <- BiocParallel::bplapply(1:6, extractReads, bcFile[, 
#'     2], bcFile[, 1], inds, reads, rcBarcodes = FALSE, location = ".", BPPARAM = multicoreParam)
#' @export
#' 
extractReads <- function(barcodeIndex, barcodes, sampleNames, index, reads, 
                         location = "./demultiplex_fastq", rcBarcodes = TRUE, hDist = 0) {
  barcode <- barcodes[barcodeIndex]
  sampleName <- sampleNames[barcodeIndex]
  message("Finding reads for barcode: ", barcode)
  if (rcBarcodes) {
    rci <- as.character(Biostrings::reverseComplement(Biostrings::DNAString(barcode)))
  } else {
    rci <- barcode
  }
  # ind_match <- as.character(index) == rci
  ind_match <- adist(as.character(index), rci) <= hDist
  
  numReads <- sum(ind_match)
  outFileName <- paste(location, "/", sampleName, "_", barcode, ".fastq.gz", 
                       sep = "")
  if (numReads == 0) {
    message("\tFound 0 reads for this barcode, no file will be written")
  } else {
    message("\tFound ", sum(ind_match), " reads, writing reads to: ", 
            outFileName)
    Biostrings::writeQualityScaledXStringSet(reads[c(ind_match)], outFileName, 
                                             compress = T)
  }
  return(list(output_file = outFileName, numberOfReads = numReads, matchedIndexes = ind_match))
}


#' Demultiplexing sequencing reads
#'
#' Function for demultiplexing sequencing reads arranged in a common format provided by sequencers (such as Illumina) generally for 16S data. This function takes a matrix of sample names/barcodes, a .fastq file of barcodes by sequence header, and a .fastq file of reads corresponding to the barcodes. Based on the barcodes given, the function extracts all reads for the indexed barcode and writes all the reads from that barcode to separate .fastq files. 
#' @param barcodeFile File name for a file containing a .tsv matrix with a header row, and then sample names (column 1) and barcodes (column 2).
#' @param indexFile Location to a .fastq file that contains the barcodes for each read. The headers should be the same (and in the same order) as the readFile, and the sequence in the indexFile should be the corresponding barcode for each read. Quality scores are not considered
#' @param readFile Location to the sequencing read .fastq file that corresponds to the indexFile
#' @param rcBarcodes Should the barcode indexes in the barcodeFile be reverse complemented to match the sequences in the indexFile? Defaults to TRUE
#' @param location A directory location to store the demuliplexed read files. Defaults to generate a new subdirectory at './demultiplex_fastq'
#' @param cores The number of cores to use for parallelization (BiocParallel). This function will parallelize over the barcodes and extract reads for each barcode separately and write them to separate demultiplexed files
#' @param hammingDist Uses a Hamming Distance or number of base differences to allow for inexact matches for the barcodes/indexes. Defaults to 0. Warning: if the Hamming Distance is >=1 and this leads to inexact index matches to more than one barcode, that read will be written to more than one demultiplexed read files 
#' 
#' @return Returns multiple .fastq files that contain all reads whose index matches the barcodes given. These files will be written to the location directory, and will be named based on the given sampleNames and barcodes, e.g. './demultiplex_fastq/SampleName1_GGAATTATCGGT.fastq.gz' 
#'
#' @examples
#' ## Get barcode, index, and read data locations
#' barcodePath <- system.file("extdata", "barcodes.txt", package = "animalcules.preprocess")
#' indexPath <- system.file("extdata", "virus_example_index.fastq", package = "animalcules.preprocess")
#' readPath <- system.file("extdata", "virus_example.fastq", package = "animalcules.preprocess")
#' 
#' ## Get barcode, index, and read data locations
#' demult <- demultiplex(barcodePath, indexPath, readPath, rcBarcodes = FALSE, 
#'     hammingDist = 2)
#' demult
#'
#' @export

demultiplex <- function(barcodeFile, indexFile, readFile, rcBarcodes = TRUE, 
                        location = "./demultiplex_fastq", cores = 1, hammingDist = 0) {
  message("Reading Sample Names and Barcodes from: ", barcodeFile)
  bcFile <- read.table(barcodeFile, sep = "\t", header = T)
  barcodes <- bcFile[, 2]
  samNames <- bcFile[, 1]
  message("\tFound information for ", length(barcodes), " samples/barcodes")
  
  message("Reading Index File: ", indexFile)
  inds <- Biostrings::readDNAStringSet(indexFile, format = "fastq")
  message("\tFound indexes for ", length(inds), " reads")
  
  message("Reading Sequence File: ", readFile)
  reads <- Biostrings::readQualityScaledDNAStringSet(readFile)
  message("\tFound ", length(reads), " reads")
  
  ## make output directory if nessary
  if (!dir.exists(location)) {
    dir.create(location)
  }
  
  # Loop over barcodes
  numReads <- NULL
  ind_no_match <- numeric(length(reads))
  for (i in 1:length(barcodes)) {
    extracted <- extractReads(i, barcodes, samNames, inds, reads, rcBarcodes = rcBarcodes, 
                              location = location, hDist = hammingDist)
    numReads <- c(numReads, extracted$numberOfReads)
    ind_no_match <- ind_no_match + extracted$matchedIndexes
  }
  message(sum(ind_no_match > 1))
  ind_no_match <- (ind_no_match == 0)
  
  # sapply over barcodes and writing to file -- for multi-threading if
  # (cores == 1){ extracted <- lapply(1:length(barcodes), extractReads,
  # barcodes, samNames, inds, reads, hammingDist) }else{ message('Using
  # ',cores,' cores') multicoreParam <- MulticoreParam(workers = cores)
  # extracted <- bplapply(1:length(barcodes),extractReads, barcodes,
  # samNames, inds, reads, BPPARAM = multicoreParam) } numReads <-
  # sapply(extracted, function(x) x$numberOfReads) ind_no_match <- (
  # rowSums(sapply(extracted, function(x) x$matchedIndexes)) == 0 )
  
  # number of reads for each barcode
  if (any(numReads == 0)) {
    message("Did not find any reads for the following barcodes: ", 
            paste(barcodes[numReads == 0], collapse = " "))
    message("Did not find any reads for the following samples: ", paste(samNames[numReads == 
                                                                                   0], collapse = " "))
    write(paste("Did not find any reads for the following barcodes:", 
                paste(barcodes[numReads == 0], collapse = " "), "\n", "Did not find any reads for the following samples: ", 
                paste(samNames[numReads == 0], collapse = " ")), file = "demultiplex_fastq/unmapped_barcodes_samples.txt")
  }
  
  # Track reads without matches, and write them to an 'orphan' file
  message(paste("Found ", sum(ind_no_match), " reads without a matching barcode (", 
                100 * round(mean(ind_no_match), 4), "%), writing reads to: ", location, 
                "/orpahns.fastq.gz", sep = ""))
  Biostrings::writeQualityScaledXStringSet(reads[c(ind_no_match)], paste(location, 
                                                                         "/orpahns.fastq.gz", sep = ""), compress = T)
  
  summaryMat <- cbind(bcFile[1:length(barcodes), ], NumberOfReads = numReads)
  write.table(summaryMat, file = paste(location, "/summary.txt", sep = ""), 
              col.names = F, row.names = T, quote = F)
  return(summaryMat)
}
