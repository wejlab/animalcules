#' Filter unmapped reads
#'
#' This function will to remove all unmapped reads or lines in a .bam file (warning: overwrites the original file!). This function is needed because combining multiple .bam files from different microbial libraries may lead to some reads that mapped to one library and have unmapped entries from another library. This will just remove any unmapped entries and leave all referene mapped lines in the .bam file.  
#' @param bamfile Location for the .bam file to filter/remove all unmapped reads
#' 
#' @return
#' This function will overwrite the existing .bam file with a new .bam file in the same location that has only mapped lines. The function iself returns the output .bam file name. 
#' 
#' @examples
#' download_refseq('viral', compress = FALSE)
#' mk_subread_index('viral.fasta')
#' readPath <- system.file("extdata", "virus_example.fastq", package = "animalcules.preprocess")
#' Rsubread::align(index = "viral", readfile1 = readPath, output_file = "virus_example.bam")
#' filtered <- filter_unmapped_reads("virus_example.bam")
#' 

filter_unmapped_reads <- function(bamfile) {
  # sort bam file
  sorted_bamfile <- Rsamtools::sortBam(bamfile, paste(tools::file_path_sans_ext(bamfile), 
                                                      ".sorted", sep = ""))
  # index bam file
  bam_index <- Rsamtools::indexBam(sorted_bamfile)
  # filter umapped reads
  filtered_bam <- Rsamtools::filterBam(sorted_bamfile, bamfile, index = bam_index, 
                                       indexDestination = FALSE, param = Rsamtools::ScanBamParam(flag = Rsamtools::scanBamFlag(isUnmappedQuery = F)))
  # clean up
  file.remove(sorted_bamfile)
  file.remove(bam_index)
  # return filtered file name
  return(filtered_bam)
}


#' Create a combined .bam header
#' 
#' This function generates a combined header from multiple .bam files from different reference libraries (e.g. a split bacterial library)
#' @param bamfiles A list of the locations/file names of .bam files from which to combine the headers
#' @param header_file A file name and location for the output file for the combined header. This will be a .sam format file without any reads. Defaults to 'header_tmp.sam'.
#' 
#' @return
#' This function will return a combined header from all the supplied .bam files
#' 
#' @examples
#' download_refseq('viral', compress = FALSE)
#' mk_subread_index('viral.fasta', split = .0005)
#' readPath <- system.file("extdata", "virus_example.fastq", package = "animalcules.preprocess")
#' Rsubread::align(index = "viral_1", readfile1 = readPath, output_file = "virus_example1.bam")
#' Rsubread::align(index = "viral_2", readfile1 = readPath, output_file = "virus_example2.bam")
#' bam_files <- c('virus_example1.bam','virus_example2.bam')
#' com_head <- combined_header(bam_files)

combined_header <- function(bam_files, header_file = "header_tmp.sam") {
  print(paste("Making a combined header file:", header_file))
  # get first and last line of header
  bam_head <- Rsamtools::scanBamHeader(bam_files[1])
  n <- length(bam_head[[1]]$text)
  last <- c(names(bam_head[[1]]$text)[n], bam_head[[1]]$text[[n]])
  # open and print the first line of header
  head_con <- file(header_file, open = "w")
  cat(c(names(bam_head[[1]]$text)[1], bam_head[[1]]$text[[1]]), file = head_con, 
      sep = "\t")
  cat("\n", file = head_con, sep = "")
  # print genomes from all .bam files
  for (bfile in bam_files) {
    # print(paste('Reading/writing genomes from', bfile))
    bam_head <- Rsamtools::scanBamHeader(bfile)
    for (j in 2:(length(bam_head[[1]]$text) - 1)) {
      cat(c(names(bam_head[[1]]$text)[j], bam_head[[1]]$text[[j]]), 
          file = head_con, sep = "\t")
      cat("\n", file = head_con, sep = "")
    }
  }
  cat(last, file = head_con, sep = "\t")
  close(head_con)
  return(header_file)
}

#' Replace the header from a .bam file
#' 
#' This function replaces the header from one .bam file with a header from a different .sam file. This function mimicks the function of the 'reheader' function in samtools.
#' @param head A file name and location for the .sam file with the new header
#' @param old_bam A file name and location for the .bam file for which you want to reheader
#' @param new_bam A file name for the new .bam file with a replaced header. Defaults to the same base filename plus 'h.bam'. For example, 'example.bam' will be written as 'exampleh.bam'  
#' 
#' @return
#' This function will return a new .bam file with a replaced header. The function also outputs the new .bam filename.  
#' 
#' @examples
#' download_refseq('viral', compress = FALSE)
#' mk_subread_index('viral.fasta', split = .0005)
#' readPath <- system.file("extdata", "virus_example.fastq", package = "animalcules.preprocess")
#' Rsubread::align(index = "viral_1", readfile1 = readPath, output_file = "virus_example1.bam")
#' Rsubread::align(index = "viral_2", readfile1 = readPath, output_file = "virus_example2.bam")
#' bam_files <- c('virus_example1.bam','virus_example2.bam')
#' com_head <- combined_header(bam_files)
#' bam_reheader_R(com_head, 'virus_example2.bam')
#' ## Note that the following would be an equivalent command if samtools is installed
#' #system("samtools reheader header_tmp.sam virus_example2.bam > virus_example2h.bam")
#' 
bam_reheader_R <- function(head, old_bam, new_bam = paste(tools::file_path_sans_ext(old_bam), 
                                                          "h.bam", sep = "")) {
  # system(paste('samtools reheader ' , head, ' ', old_bam,' > ',
  # new_bam, sep=''))
  new_sam <- paste(tools::file_path_sans_ext(new_bam), ".sam", sep = "")
  new_sam_con <- file(new_sam, open = "w")
  head_con <- file(head, open = "r")
  while ((length(oneLine <- readLines(head_con, n = 1, warn = FALSE)) > 
          0)) {
    writeLines(oneLine, new_sam_con)
  }
  close(head_con)
  old_sam <- Rsamtools::asSam(old_bam, overwrite = T)
  old_sam_con <- file(old_sam, open = "r")
  while ((length(oneLine <- readLines(old_sam_con, n = 1, warn = FALSE)) > 
          0)) {
    if (substr(oneLine, 1, 1) != "@") {
      writeLines(oneLine, new_sam_con)
    }
  }
  close(new_sam_con)
  close(old_sam_con)
  file.remove(old_sam)
  new_bam <- Rsamtools::asBam(new_sam, overwrite = T)
  file.remove(new_sam)
  file.remove(paste(new_bam, ".bai", sep = ""))
  return(new_bam)
}

#' Merge multiple .bam files
#' 
#' This function merges .bam files. It first used the combined_header funtion to generate a combined header for all the files, reheaders the files, and then merges and sorts the .bam files. This is similar to the 'samtools merge' function, but it allows the .bam files to have different headers. 
#' @param bam_files A list of file names for the .bam files to be merged
#' @param destination A file name and location for the merged .bam file
#' @param head_file A file name and location for the combined header file. Defaults to the destiation . For example, 'example.bam' will be written as 'exampleh.bam'  
#' 
#' @return
#' This function merges .bam files and combines them into a single file. The function also outputs the new .bam filename.  
#' 
#' @examples
#' download_refseq('viral', compress = FALSE)
#' mk_subread_index('viral.fasta', split = .0005)
#' readPath <- system.file("extdata", "virus_example.fastq", package = "animalcules.preprocess")
#' Rsubread::align(index = "viral_1", readfile1 = readPath, output_file = "virus_example1.bam", maxMismatches = 3)
#' Rsubread::align(index = "viral_2", readfile1 = readPath, output_file = "virus_example2.bam")
#' bam_files <- c('virus_example1.bam','virus_example2.bam')
#' com_head <- combined_header(bam_files)
#' bam_reheader_R(com_head, 'virus_example1.bam')
#' bam_reheader_R(com_head, 'virus_example1.bam')
#' bam_files <- c('virus_example1h.bam','virus_example2h.bam')
#' merged_all <- merge_bam_files(bam_files, 'virus_example_merged')
#' 

merge_bam_files <- function(bam_files, destination, head_file = paste(destination, 
                                                                      "_header.sam", sep = "")) {
  com_head <- combined_header(bam_files, header_file = head_file)
  print("Merging and sorting .bam files")
  bam_files_h <- sam_files_h <- NULL
  for (i in 1:length(bam_files)) {
    new_bam_h <- bam_reheader_R(com_head, bam_files[i])
    bam_files_h <- c(bam_files_h, new_bam_h)
    file.remove(bam_files[i])
    suppressWarnings(file.remove(paste(bam_files[i], ".indel.vcf", sep = "")))  # remove .bam and .vcf files for each alignment 
  }
  merged_bam <- Rsamtools::mergeBam(bam_files_h, paste("unsorted_", destination, 
                                                       ".bam", sep = ""), overwrite = T)
  # clean up
  file.remove(com_head)
  for (i in bam_files_h) {
    file.remove(i)
  }
  # sort merged bam file
  merged_bam_sorted <- Rsamtools::sortBam(merged_bam, destination, byQname = T)
  # clean up
  file.remove(merged_bam)
  # return merged and sorted bam
  return(merged_bam_sorted)
}

#' Align microbiome reads to a set of reference libraries 
#' 
#' This is the main animalcules target library mapping function, using Rsubread and multiple libraries. Aligns to each library separately, filters unmapped reads from each file, and then merges and sorts the .bam files from each library into one output file.
#' @param reads Location to the .fastq file to align 
#' @param libs A list of Subread index headers for alignment
#' @param project_name A name for the project, which names the output .bam file (e.g. project_name.bam). Defaults to the basename of the reads file.
#' @param threads The number of threads for the Subread alignment. Defaults to 8
#' @param mismatch Numeric value giving the maximum number of mis-matched bases allowed in the alignment. Default is 3. Mis-matches found in soft-clipped bases are not counted.
#' 
#' @return
#' This function writes to file a merged and sorted .bam file after aligning to all reference libraries given. The function also outputs the new .bam filename.  
#' 
#' @examples
#' ## Get a reference genome library
#' download_refseq('viral', compress = FALSE)
#' 
#' ## Make and align to a single a reference genome library
#' mk_subread_index('viral.fasta')
#' readPath <- system.file("extdata", "virus_example.fastq", package = "animalcules.preprocess")
#' viral_map <- align_target( readPath, "viral", "virus_example")
#' viral_map_sam <- Rsamtools::asSam(viral_map, overwrite=T)
#' 
#' ## Make and align to a multiple reference genome libraries
#' mk_subread_index('viral.fasta', split=0.005)
#' targLibs <- c("viral_1", "viral_2")
#' readPath <- system.file("extdata", "virus_example.fastq", package = "animalcules.preprocess")
#' viral_map <- align_target( readPath, targLibs, "virus_example")
#' 
#' @export
#' 
align_target <- function(reads, libs, project_name = tools::file_path_sans_ext(reads), 
                         threads = 8, mismatch = 5) {
  ## needs to make a system call to samtools to merge
  bam_files <- NULL
  for (i in 1:length(libs)) {
    bam_files <- c(bam_files, paste(tools::file_path_sans_ext(reads), 
                                    ".", libs[i], ".bam", sep = ""))
    Rsubread::align(index = libs[i], readfile1 = reads, output_file = bam_files[length(bam_files)], 
                    type = "dna", nthreads = threads, unique = FALSE, nBestLocations = 16, 
                    maxMismatches = mismatch)
    ## remove umapped reads
    filter_unmapped_reads(bam_files[length(bam_files)])
  }
  # merge bam files if needed; rename if not
  if (length(bam_files) > 1) {
    print(paste("Merging the bam files into", paste(project_name, ".bam", 
                                                    sep = "")))
    merged_all <- merge_bam_files(bam_files, project_name)
  } else {
    file.rename(bam_files, paste(project_name, ".bam", sep = ""))
    file.remove(paste(bam_files, ".indel.vcf", sep = ""))  # remove Rsubread .vcf files for now
  }
  message(paste("DONE! Alignments written to ", project_name, ".bam", 
                sep = ""))
  return(paste(project_name, ".bam", sep = ""))
}

