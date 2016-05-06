context("measurement registering")

test_that("register measurement works for block", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=100))
  ms_obj <- epivizrData:::register(gr)
  expect_true(validObject(ms_obj))
  
  expect_is(ms_obj, "EpivizBlockData")
  expect_is(ms_obj$.object, "GNCList")
  
  newgr <- as(ms_obj$.object, "GRanges")
  mcols(newgr) <- mcols(ms_obj$.object)
  expect_equal(newgr, gr)
  
  expect_true(is.null(ms_obj$.columns))
})

test_that("register works for bp data", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1),score=rnorm(10))
  ms_obj <- epivizrData::register(gr, columns="score", type="bp")
  expect_true(validObject(ms_obj))
  
  expect_is(ms_obj, "EpivizBpData")
  expect_is(ms_obj$.object, "GNCList")
  
  newgr <- as(ms_obj$.object, "GRanges")
  mcols(newgr) <- mcols(ms_obj$.object)
  
  expect_equal(newgr, unname(gr))
  expect_equal(ms_obj$.columns, "score")
  rng <- range(pretty(range(gr$score)))
  expect_equal(ms_obj$.ylim, unname(cbind(rng)))
})

test_that("register works for gene info granges", {
  skip_if_not_installed("bumphunter")
  gr <- make_test_gene_info()
  ms_obj <- epivizrData::register(gr, type="gene_info")
  expect_true(validObject(ms_obj))
  
  expect_is(ms_obj, "EpivizGeneInfoData")
  expect_is(ms_obj$.object, "GNCList")
  expect_true(is.null(ms_obj$.columns))
})

test_that("register works for RangedSummarizedExperiment", {
  sset <- make_test_SE()
  ms_obj <- epivizrData::register(sset, columns=c("A","B"), assay="counts2")
  expect_true(validObject(ms_obj))
  
  order <- order(start(rowRanges(sset)))
  sset <- sset[order,]
  
  expect_is(ms_obj, "EpivizFeatureData")
  expect_is(ms_obj$.object, "RangedSummarizedExperiment")
  
  gr <- as(rowRanges(ms_obj$.object), "GRanges")
  mcols(gr) <- mcols(rowRanges(ms_obj$.object))
  
  expect_false(is.null(gr$probeid))
  
  tmp <- rowRanges(sset)
  strand(tmp) <- "*"
  o <- order(tmp)
  
  expect_identical(gr, rowRanges(sset)[o,])
  
  #  expect_identical(assays(dev$object), assays(sset)[o,])
  expect_identical(colData(ms_obj$.object), colData(sset))
  
  columns <- c("A","B")
  expect_identical(ms_obj$.columns, columns)
  emat <- assay(sset,"counts2")[,c("A","B")]
  mat <- assay(ms_obj$.object,"counts2")[,c("A","B")]
  expect_equal(emat[o,], mat)
  
  rngs <- apply(emat, 2, function(x) range(pretty(range(x))))
  expect_equal(ms_obj$.ylim, rngs, check.attributes=FALSE)
})

test_that("register works for ExpressionSet", {
  skip_if_not_installed("hgu133plus2.db")
  
  eset <- make_test_eset()
  ms_obj <- epivizrData::register(eset, columns=c("SAMP_1", "SAMP_2"))
  expect_true(validObject(ms_obj))
  
  expect_is(ms_obj, "EpivizFeatureData")
  expect_is(ms_obj$.object, "RangedSummarizedExperiment")
  
  obj <- ms_obj$.object
  gr <- rowRanges(obj)
  
  m <- match(gr$PROBEID, featureNames(eset))
  mat <- assay(obj)
  
  expect_equal(exprs(eset)[m,"SAMP_1"], mat[,"SAMP_1"], check.names=FALSE, check.attributes=FALSE)
  expect_equal(exprs(eset)[m,"SAMP_2"], mat[,"SAMP_2"], check.names=FALSE, check.attributes=FALSE)
  
  rngs <- apply(exprs(eset)[m,c("SAMP_1","SAMP_2")], 2, function(x) range(pretty(range(x))))
  expect_equal(ms_obj$.ylim, rngs, check.attributes=FALSE)
})

test_that("register works for OrganismDb object", {
  skip_on_cran()
  skip_if_not_installed("Mus.musculus")
  require(Mus.musculus)
  ms_obj <- epivizrData::register(Mus.musculus, keepSeqlevels=paste0("chr",c(1:19,"X","Y")))
  expect_true(validObject(ms_obj))
  
  expect_is(ms_obj, "EpivizGeneInfoData")
  expect_is(ms_obj$.object, "GNCList")
  expect_true(is.null(ms_obj$.columns))
})

test_that("register works for a TxDb object", {
  skip_on_cran()
  skip_if_not_installed("TxDb.Mmusculus.UCSC.mm10.knownGene")
  require(TxDb.Mmusculus.UCSC.mm10.knownGene)
  ms_obj <- epivizrData::register(TxDb.Mmusculus.UCSC.mm10.knownGene, 
                                  keepSeqlevels = paste0("chr", c(1:19, "X", "Y")))
  expect_true(validObject(ms_obj))
  
  expect_is(ms_obj, "EpivizGeneInfoData")
  expect_is(ms_obj$.object, "GNCList")
  expect_true(is.null(ms_obj$.columns))
})
