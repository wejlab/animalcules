abc <- base::readRDS("data/animalcules_sample_lung_cancer_dataset.RDS")
count_table <- abc$count_table
tax_table <- abc$tax_table
metadata_table <- abc$metadata_table
gene_expression_table <- abc$gene_expression_table

count_table <- data.matrix(count_table)
tax_table <- DataFrame(tax_table)
metadata_table <- DataFrame(metadata_table)
gene_expression_table <- data.matrix(gene_expression_table)

dim(count_table)
dim(tax_table)
dim(metadata_table)
dim(gene_expression_table)
