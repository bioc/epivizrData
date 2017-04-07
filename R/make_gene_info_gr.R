.cleanup_gene_info_gr <- function(gr, keepSeqlevels = NULL) {
  if (any(tmp <- isTRUE(GenomeInfoDb::isCircular(seqinfo(gr))))) {
    keep <- names(tmp)[!tmp]
    gr <- keepSeqlevels(gr, keep, pruning.mode="coarse")
  }
  
  if (!is.null(keepSeqlevels)) {
    gr <- keepSeqlevels(gr, keepSeqlevels, pruning.mode="coarse")
  }
  
  gr
}

setGeneric(".make_gene_info_gr", signature=c("object"),
           function(object, kind, keepSeqlevels) 
             standardGeneric(".make_gene_info_gr")
)

.make_gene_info_org_helper <- function(object, kind = c("gene","tx"), keepSeqlevels = NULL) {
  kind <- match.arg(kind)
  gr <- GenomicFeatures::genes(object, columns=c("GENEID", "SYMBOL"))
  exons <- GenomicFeatures::exonsBy(object, by=kind)
  
  ids <- as.character(gr$GENEID)
  exons <- reduce(ranges(exons)[ids])
  gr$Exons <- exons
  
  gr <- .cleanup_gene_info_gr(gr, keepSeqlevels)
  
  nms <- names(mcols(gr))
  geneNameIdx <- match("SYMBOL", nms)
  nms[geneNameIdx] <- "Gene"
  names(mcols(gr)) <- nms
  gr
}

.make_gene_info_ensdb_helper <- function(object, kind = c("gene","tx"), keepSeqlevels = NULL) {
  kind <- match.arg(kind)
  gr <- ensembldb::genes(object, columns=c("gene_id", "gene_name"))
  exons <- ensembldb::exonsBy(object, by=kind)
  
  names(mcols(gr)) <- c("GENEID","SYMBOL")
  ids <- as.character(gr$GENEID)
  exons <- reduce(ranges(exons)[ids])
  gr$Exons <- exons
  
  gr <- .cleanup_gene_info_gr(gr, keepSeqlevels)
  
  nms <- names(mcols(gr))
  geneNameIdx <- match("SYMBOL", nms)
  nms[geneNameIdx] <- "Gene"
  names(mcols(gr)) <- nms
  gr
}

setMethod(".make_gene_info_gr", "OrganismDb", .make_gene_info_org_helper)
setMethod(".make_gene_info_gr", "EnsDb", .make_gene_info_ensdb_helper)

setMethod(".make_gene_info_gr", "TxDb", 
  function(object, kind = c("gene", "tx"), keepSeqlevels = NULL) {
    gr <- GenomicFeatures::genes(object)
    kind <- match.arg(kind)
    exons <- GenomicFeatures::exonsBy(object, by=kind)
    
    ids <- as.character(gr$gene_id)
    exons <- reduce(ranges(exons)[ids])
    gr$Exons <- exons
    
    gr <- .cleanup_gene_info_gr(gr, keepSeqlevels)
    
    nms <- names(mcols(gr))
    geneNameIdx <- match("gene_id", nms)
    nms[geneNameIdx] <- "Gene"
    names(mcols(gr)) <- nms
    gr
  }
)
