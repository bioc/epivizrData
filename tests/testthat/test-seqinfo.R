context("disconnected seqinfo")

test_that("add_seqinfo works", {
  skip_if_not_installed("Mus.musculus")
  require(Mus.musculus)
  
  server <- epivizrServer::createServer()
  mgr <- createMgr(server=server)
  
  seqinfo <- seqinfo(Mus.musculus)
  seqlevels_to_keep <- paste0("chr", c(1:19,"X","Y", "M"))
  
  mgr$add_seqinfo(seqinfo, keep_seqlevels = seqlevels_to_keep, send_request=FALSE)
  seqlevels(seqinfo) <- seqlevels_to_keep
  expect_equal(mgr$.seqinfo, seqinfo)
})

test_that("rm_seqinfo works",{
  skip_if_not_installed("Mus.musculus")
  require(Mus.musculus)
  
  server <- epivizrServer::createServer()
  mgr <- createMgr(server=server)
  
  seqinfo <- seqinfo(Mus.musculus)
  seqlevels_to_keep <- paste0("chr", c(1:19,"X","Y", "M"))
  
  mgr$add_seqinfo(seqinfo, keep_seqlevels = seqlevels_to_keep, send_request=FALSE)
  mgr$rm_seqinfo(send_request=FALSE)
  expect_equal(mgr$.seqinfo, Seqinfo())
})

test_that("get_seqinfo works", {
  skip_if_not_installed("Mus.musculus")
  require(Mus.musculus)
  
  server <- epivizrServer::createServer()
  mgr <- createMgr(server=server)
  
  seqinfo <- seqinfo(Mus.musculus)
  seqlevels_to_keep <- paste0("chr", c(1:19,"X","Y", "M"))
  
  mgr$add_seqinfo(seqinfo, keep_seqlevels = seqlevels_to_keep, send_request=FALSE)
  res <- mgr$get_seqinfo()
  
  seqlevels(seqinfo) <- seqlevels_to_keep
  seqlengths <- seqlengths(seqinfo)
  
  expected_res <- lapply(seqlengths+1, function(l) c(1,l))
  names(expected_res) <- names(seqlengths)
  expect_equal(res, expected_res)
})

