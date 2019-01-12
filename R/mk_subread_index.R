#' Make a Subread index
#'
#' This function is a wrapper for the Rsubread::buildindex function. It will generate one or more Subread indexes from a .fasta file. If the library is too large (default >4GB) it will automatically be split into multiple indexes, with _1, _2, etc at the end of the ref_lib basename. 
#' @param ref_lib The name/location of the reference library file, in (uncompressed) .fasta format
#' @param split The maximum allowed size of the genome file (in GB). If the ref_lib file is larger than this, the function will spolt the library into multiple parts 
#' @param mem The maximum amount of memory (in MB) that can be used by the index generation process (used by the Rsubread::buildindex function)
#' @return Returns one or more Subread indexes for the supplied reference .fasta file. If multiple indexes are created, the libraries will be named the ref_lib basename plus _1, _2, etc.  
#'
#' @examples
#' ## Download all RefSeq reference viral genomes and make an index
#' download_refseq('viral', compress = FALSE)
#' mk_subread_index('viral.fasta')
#' 
#' ## Download all RefSeq reference viral genomes and make more than one index
#' download_refseq('viral', compress = FALSE)
#' mk_subread_index('viral.fasta', split = .0005)
#'
#' @export


mk_subread_index <- function(ref_lib, split = 4, mem = 8000) {
  GB <- 1073741824
  ref_size <- file.info(ref_lib)$size
  split_libs <- ceiling(ref_size/(split * GB))
  
  if (ref_size > (split * GB)) {
    print(paste("Library size is ", round(ref_size/GB, 1), " GBs. Splitting the library into ", 
                split_libs, " separate components.", sep = ""))
    
    ## Making connections to the .fasta library and generating the new split
    ## .fasta files
    con <- file(ref_lib, open = "r")
    out_cons <- paste("outcon_", 1:split_libs, sep = "")
    out_files <- paste(tools::file_path_sans_ext(ref_lib), "_", 1:split_libs, 
                       ".", tools::file_ext(ref_lib), sep = "")
    for (i in 1:split_libs) {
      assign(out_cons[i], file(out_files[i], open = "w"))
    }
    
    ## Reading ref library and splitting to split libraries
    nGenomes <- -1
    while ((length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 
            0)) {
      if (substr(oneLine, 1, 1) == ">") {
        nGenomes <- nGenomes + 1
      }
      writeLines(oneLine, get(out_cons[(nGenomes%%split_libs) + 1]))
    }
    print(paste("Printed ", nGenomes + 1, " sequences to ", split_libs, 
                " genome files", sep = ""))
    
    ## Closing file connections
    close(con)
    for (ocon in out_cons) {
      close(get(ocon))
    }
    
    ## Building Rsubread indexes--should parallelize this!
    for (lib in out_files) {
      Rsubread::buildindex(basename = tools::file_path_sans_ext(lib), 
                           reference = lib, memory = mem)
    }
    
  } else {
    print(paste("Library size is ", round(ref_size/GB, 1), " GBs. Building the Rsubread index", 
                sep = ""))
    Rsubread::buildindex(basename = tools::file_path_sans_ext(ref_lib), 
                         reference = ref_lib, memory = mem)
  }
}
