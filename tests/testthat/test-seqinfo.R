context("disconnected seqinfo")

test_that("add_seqinfo works", {
  skip("gc error")
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
  skip("gc error")
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
  skip("gc error")
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
  
  expected_res <- lapply(seqlengths+1, function(l) c(1,l))
  names(expected_res) <- names(seqlengths)
  expect_equal(res, expected_res)
})

