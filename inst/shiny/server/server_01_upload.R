
observeEvent(input$upload_animalcules,{
  withBusyIndicatorServer("upload_animalcules", {
    MAE <- readRDS(input$rdfile$datapath)
    vals$MAE <- MAE
    vals$MAE_backup <- MAE
    # Update ui
    updateCovariate()
    updateSample()
  })
})

observeEvent(input$uploadDataCount,{
  withBusyIndicatorServer("uploadDataCount", {

  count_table <- read.csv(input$countsfile$datapath,
                       header = input$header.count,
                       row.names = 1,
                       stringsAsFactors = FALSE,
                       sep = input$sep.count,
                       comment.char="",
                       check.names = FALSE)

  tax_table <- read.csv(input$taxon.table$datapath,
                            header = input$header.count,
                            sep = input$sep.count,
                            row.names= 1,
                            stringsAsFactors=FALSE,
                            comment.char="",
                            check.names = FALSE)

  metadata_table <- read.csv(input$annotfile.count$datapath,
                            header = input$header.count,
                            sep = input$sep.count,
                            row.names=input$metadata_sample_name_col_count,
                            stringsAsFactors=FALSE,
                            comment.char="",
                            check.names = FALSE)

  # Choose only the samples in metadata that have counts data as well
  metadata_table <- metadata_table[match(colnames(count_table), rownames(metadata_table)), ]

  # Test and fix the constant/zero row
  row.remove.index <- c()
  if (sum(rowSums(as.matrix(count_table)) == 0) > 0){
      row.remove.index <- which(rowSums(as.matrix(count_table)) == 0)
      count_table <- count_table[-row.remove.index,]
  }

  # create MAE object
  se_mgx <-
      count_table %>%
      base::data.matrix() %>%
      S4Vectors::SimpleList() %>%
      magrittr::set_names("MGX")

  se_colData <-
      metadata_table %>%
      S4Vectors::DataFrame()

  se_rowData <-
      tax_table %>%
      base::data.frame() %>%
      dplyr::mutate_all(as.character) %>%
      #dplyr::select(superkingdom, phylum, class, order, family, genus) %>%
      S4Vectors::DataFrame()

  microbe_se <-
      SummarizedExperiment::SummarizedExperiment(assays = se_mgx,
                                               colData = se_colData,
                                               rowData = se_rowData)
  mae_experiments <-
      S4Vectors::SimpleList(MicrobeGenetics = microbe_se)

  MAE <-
      MultiAssayExperiment::MultiAssayExperiment(experiments = mae_experiments,
                                               colData = se_colData)

  # update vals
  vals$MAE <- MAE
  vals$MAE_backup <- MAE
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

    datlist <- read_pathoscope_data(input_dir, pathoreport_file_suffix = input$report_suffix,
                                  use.input.files = TRUE,
                                  input.files.path.vec = df.path.vec,
                                  input.files.name.vec = df.name.vec)
    count_table <- datlist$countdata

    metadata_table <- read.csv(input$annotfile.ps$datapath,
                              header = input$header.ps,
                              sep = input$sep.ps,
                              row.names=input$metadata_sample_name_col,
                              stringsAsFactors=FALSE)

    # Choose only the samples in metadata that have counts data as well
    metadata_table <- metadata_table[match(colnames(count_table), rownames(metadata_table)), ]

    # Test and fix the constant/zero row
    row.remove.index <- c()
    if (sum(rowSums(as.matrix(count_table)) == 0) > 0){
        row.remove.index <- which(rowSums(as.matrix(count_table)) == 0)
        count_table <- count_table[-row.remove.index,]
    }

    ids <- rownames(count_table)
    tids <- unlist(lapply(ids, FUN = grep_tid))
    taxonLevels <- find_taxonomy(tids)
    tax_table <- find_taxon_mat(ids, taxonLevels)
    # Test and fix the constant/zero row
    if (!is.null(row.remove.index)){
        tax_table <- tax_table[-row.remove.index,]
    }

  # create MAE object
  se_mgx <-
      count_table %>%
      base::data.matrix() %>%
      S4Vectors::SimpleList() %>%
      magrittr::set_names("MGX")

  se_colData <-
      metadata_table %>%
      S4Vectors::DataFrame()

  se_rowData <-
      tax_table %>%
      base::data.frame() %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::select(superkingdom, phylum, class, order, family, genus) %>%
      S4Vectors::DataFrame()

  microbe_se <-
      SummarizedExperiment::SummarizedExperiment(assays = se_mgx,
                                               colData = se_colData,
                                               rowData = se_rowData)
  mae_experiments <-
      S4Vectors::SimpleList(MicrobeGenetics = microbe_se)

  MAE <-
      MultiAssayExperiment::MultiAssayExperiment(experiments = mae_experiments,
                                               colData = se_colData)

  # update vals
  vals$MAE <- MAE
  vals$MAE_backup <- MAE
  # Update ui
  updateCovariate()
  updateSample()
  updateTaxLevel()


  })
})





# Data input summary
output$contents.count <- DT::renderDataTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    if (!is.null(input$countsfile.pathoscope)){
        if (input$uploadChoice == "pathofiles"){
        req(input$countsfile.pathoscope)
        df <- read.csv(input$countsfile.pathoscope[[1, 'datapath']],
                       skip = 1,
                       header = TRUE,
                       sep = input$sep.ps)
        return(df)
        }
    }
},
options = list(
    paging = TRUE, scrollX = TRUE, pageLength = 5
))

output$contents.meta <- DT::renderDataTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    if (!is.null(input$annotfile.ps)){
        if (input$uploadChoice == "pathofiles"){
        req(input$countsfile.pathoscope)

        df <- read.csv(input$annotfile.ps$datapath,
                       header = input$header.ps,
                       sep = input$sep.ps)
        return(df)
        }
    }
},
options = list(
    paging = TRUE, scrollX = TRUE, pageLength = 5
))


### data input summary
output$contents.count.2 <- DT::renderDataTable({

  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.

  if (!is.null(input$countsfile)){
    if (input$uploadChoice == "count"){
      req(input$countsfile)
      df <- read.csv(input$countsfile$datapath,
                     header = input$header.count,
                     sep = input$sep.count)
      return(df)
    }
  }
},
options = list(
  paging = TRUE, scrollX = TRUE, pageLength = 5
))

output$contents.meta.2 <- DT::renderDataTable({

  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.

  if (!is.null(input$annotfile.count)){
    if (input$uploadChoice == "count"){
      req(input$annotfile.count)
      df <- read.csv(input$annotfile.count$datapath,
                     header = input$header.count,
                     sep = input$sep.count)
      return(df)
    }
  }
},
options = list(
  paging = TRUE, scrollX = TRUE, pageLength = 5
))

output$contents.taxonomy <- DT::renderDataTable({

  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.


  if (!is.null(input$taxon.table)){
    if (input$uploadChoice == "count"){
      req(input$taxon.table)

      df <- read.csv(input$taxon.table$datapath,
                     header = input$header.count,
                     sep = input$sep.count)
      return(df)
    }
  }
},
options = list(
  paging = TRUE, scrollX = TRUE, pageLength = 5
))
