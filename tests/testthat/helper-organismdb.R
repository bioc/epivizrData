make_test_gene_info <- function() {
  library(bumphunter)

  tt <- bumphunter::TT$transcripts
  seqlevels(tt, pruning.mode="coarse") <- paste0("chr",c(1:22,"X","Y"))
  tt
}

