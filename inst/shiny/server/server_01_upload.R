
observeEvent(input$uploadPathoStat,{
  withBusyIndicatorServer("uploadPathoStat", {
    if (input$rdtype == 'rda') {
      load(input$rdfile$datapath)
    }
    if (input$rdtype == 'rds') {
      MAE <- readRDS(input$rdfile$datapath)
    }
    vals$MAE <- MAE
    vals$MAE_backup <- MAE
    # Update ui
    updateCovariate()
    updateSample()
  })
})

observeEvent(input$uploadDataCount,{
  withBusyIndicatorServer("uploadDataCount", {

  df.input <- read.csv(input$countsfile$datapath,
                       header = input$header.count,
                       row.names = 1,
                       stringsAsFactors = FALSE,
                       sep = input$sep.count,
                       comment.char="",
                       check.names = FALSE)

  df.taxon.input <- read.csv(input$taxon.table$datapath,
                            header = input$header.count,
                            sep = input$sep.count,
                            row.names= 1,
                            stringsAsFactors=FALSE,
                            comment.char="",
                            check.names = FALSE)

  df.meta.input <- read.csv(input$annotfile.count$datapath,
                            header = input$header.count,
                            sep = input$sep.count,
                            row.names=input$metadata_sample_name_col_count,
                            stringsAsFactors=FALSE,
                            comment.char="",
                            check.names = FALSE)

  # Choose only the samples in metadata that have counts data as well
  df.meta.input <- df.meta.input[match(colnames(df.input), rownames(df.meta.input)), ]

  # Test and fix the constant/zero row
  row.remove.index <- c()
  if (sum(rowSums(as.matrix(df.input)) == 0) > 0){
      row.remove.index <- which(rowSums(as.matrix(df.input)) == 0)
      df.input <- df.input[-row.remove.index,]
  }

  OTU <- otu_table(df.input, taxa_are_rows = TRUE)
  TAX <- tax_table(as.matrix(df.taxon.input))
  physeq <- phyloseq(OTU, TAX)

  # Change NA/NULL to 0
  # Remove variables with identical values
  col.remove.index <- c()
  for (i in 1:ncol(df.meta.input)){
      if(length(unique(df.meta.input[,i])) < 2){
          col.remove.index <- c(col.remove.index, i)
      }
  }
  if (!is.null(col.remove.index)){
      df.meta.input <- df.meta.input[,-col.remove.index]
  }

  sampledata = sample_data(data.frame(df.meta.input))
  random_tree = rtree(ntaxa(physeq), rooted=TRUE, tip.label=
                          taxa_names(physeq))
  physeq1 <- merge_phyloseq(physeq, sampledata, random_tree)
  pstat <- pathostat1(physeq1)
  shinyInput <- list(pstat = pstat)
  vals$shiny.input <- shinyInput
  vals$shiny.input.backup <- shinyInput
  # Update ui
  updateCovariate()
  updateSample()
  updateTaxLevel()
  })
})

observeEvent(input$uploadDataPs, {
  withBusyIndicatorServer("uploadDataPs", {

    df.path.vec <- c()
    df.name.vec <- c()
    for(i in 1:length(input$countsfile.pathoscope[,1])){
        df.path.vec[i] <- input$countsfile.pathoscope[[i, 'datapath']]
        df.name.vec[i] <- input$countsfile.pathoscope[[i, 'name']]
    }

    datlist <- readPathoscopeData(input_dir, pathoreport_file_suffix = input$report_suffix,
                                  use.input.files = TRUE,
                                  input.files.path.vec = df.path.vec,
                                  input.files.name.vec = df.name.vec)
    countdat <- datlist$countdata

    df.meta.input <- read.csv(input$annotfile.ps$datapath,
                              header = input$header.ps,
                              sep = input$sep.ps,
                              row.names=input$metadata_sample_name_col,
                              stringsAsFactors=FALSE)

    # Choose only the samples in metadata that have counts data as well
    df.meta.input <- df.meta.input[match(colnames(countdat), rownames(df.meta.input)), ]

    # Test and fix the constant/zero row
    row.remove.index <- c()
    if (sum(rowSums(as.matrix(countdat)) == 0) > 0){
        row.remove.index <- which(rowSums(as.matrix(countdat)) == 0)
        countdat <- countdat[-row.remove.index,]
    }

    ids <- rownames(countdat)
    tids <- unlist(lapply(ids, FUN = grepTid))
    taxonLevels <- findTaxonomy(tids)
    taxmat <- findTaxonMat(ids, taxonLevels)
    # Test and fix the constant/zero row
    if (!is.null(row.remove.index)){
        taxmat <- taxmat[-row.remove.index,]
    }

    OTU <- otu_table(countdat, taxa_are_rows = TRUE)
    TAX <- tax_table(taxmat)
    physeq <- phyloseq(OTU, TAX)

    # Change NA/NULL to 0
    # Remove variables with identical values
    col.remove.index <- c()
    for (i in 1:ncol(df.meta.input)){
        if(length(unique(df.meta.input[,i])) < 2){
            col.remove.index <- c(col.remove.index, i)
        }
    }
    if (!is.null(col.remove.index)){
        df.meta.input <- df.meta.input[,-col.remove.index]
    }

    sampledata = sample_data(data.frame(df.meta.input))
    random_tree = rtree(ntaxa(physeq), rooted=TRUE, tip.label=
                            taxa_names(physeq))
    physeq1 <- merge_phyloseq(physeq, sampledata, random_tree)
    pstat <- pathostat1(physeq1)
    shinyInput <- list(pstat = pstat)
    vals$shiny.input <- shinyInput
    vals$shiny.input.backup <- shinyInput
    # Update ui
    updateCovariate()
    updateSample()
    updateTaxLevel()
  })
})
