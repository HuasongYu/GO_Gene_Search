# if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
# BiocManager::install(c("clusterProfiler", "org.Hs.eg.db", "AnnotationDbi", "GO.db"))
# Gene symbols associated with GO terms were retrieved using AnnotationDbi v1.64.1, clusterProfiler v4.10.0, org.Hs.eg.db v3.18.0, and GO.db v3.18.0. GO annotations were based on GOALL mappings.

library(clusterProfiler)
library(org.Hs.eg.db)
library(AnnotationDbi)
library(GO.db)

get_go_gene_symbols <- function(go_ids,
                                OrgDb = org.Hs.eg.db,
                                keytype = "GOALL") {
  
  valid_go <- keys(GO.db, keytype = "GOID")
  invalid_go <- setdiff(go_ids, valid_go)
  
  if (length(invalid_go) > 0) {
    warning("The following GO IDs were not found in GO.db:",
            paste(invalid_go, collapse = ", "))
  }
  
  go_ids <- intersect(go_ids, valid_go)
  
  if (length(go_ids) == 0) {
    return(data.frame())
  }
  

  gene_df <- AnnotationDbi::select(
    OrgDb,
    keys = go_ids,
    keytype = keytype,
    columns = c("SYMBOL", "ENTREZID", "GOALL", "ONTOLOGYALL")
  )
  
  return(gene_df)
}

result <- get_go_gene_symbols("GO:0001817")

