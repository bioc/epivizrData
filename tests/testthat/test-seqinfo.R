context("disconnected seqinfo")

test_that("add_seqinfo works", {
  skip_if_not_installed("Mus.musculus")
  require(Mus.musculus)
  
  server <- epivizrServer::createServer()
  mgr <- createMgr(server=server)
  
  seqinfo <- seqinfo(Mus.musculus)
  seqlevels <- paste0("chr", c(1:19,"X","Y", "M"))
  
  mgr$add_seqinfo(seqinfo, keep_seqlevels = seqlevels, send_request=FALSE)
  seqinfo <- keepSeqlevels(seqinfo, seqlevels)
  expect_equal(mgr$.seqinfo, seqinfo)
})

test_that("rm_seqinfo works",{
  skip_if_not_installed("Mus.musculus")
  require(Mus.musculus)
  
  server <- epivizrServer::createServer()
  mgr <- createMgr(server=server)
  
  seqinfo <- seqinfo(Mus.musculus)
  seqlevels <- paste0("chr", c(1:19,"X","Y", "M"))
  
  mgr$add_seqinfo(seqinfo, keep_seqlevels = seqlevels, send_request=FALSE)
  mgr$rm_seqinfo(send_request=FALSE)
  expect_equal(mgr$.seqinfo, Seqinfo())
})

test_that("get_seqinfo works", {
  skip_if_not_installed("Mus.musculus")
  require(Mus.musculus)
  
  server <- epivizrServer::createServer()
  mgr <- createMgr(server=server)
  
  seqinfo <- seqinfo(Mus.musculus)
  seqlevels <- paste0("chr", c(1:19,"X","Y", "M"))
  
  mgr$add_seqinfo(seqinfo, keep_seqlevels = seqlevels, send_request=FALSE)
  res <- mgr$get_seqinfo()
  
  seqinfo <- keepSeqlevels(seqinfo, seqlevels)
  seqlengths <- seqlengths(seqinfo)
  expected_res <- mapply(function(nm,ln) list(nm,1,ln+1), names(seqlengths), seqlengths, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  expect_equal(res, expected_res)
})

