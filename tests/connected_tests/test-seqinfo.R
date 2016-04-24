context("connected seqinfo")

test_that("add_seqinfo works", {
  skip_if_not_installed("Mus.musculus")
  require(Mus.musculus)
  
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment")
  }
  
  server <- epivizrServer::createServer(port=7123L, 
                                        static_site_path=".",
                                        daemonized=TRUE, 
                                        verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test only works for daemonized servers")
  }
  
  .startRemoteDriver()
  on.exit({cat("stopping remDr\n"); .stopPhantomJS()})
  
  server$start_server()
  on.exit({cat("stopping server\n"); server$stop_server()}, add=TRUE)
  
  mgr <- createMgr(server)
  .navigateRemoteDriver(port=server$.port)
  wait_until(mgr$.server$is_socket_connected())
  
  seqinfo <- seqinfo(Mus.musculus)
  seqlevels <- paste0("chr", c(1:19,"X","Y", "M"))
  
  mgr$add_seqinfo(seqinfo, keep_seqlevels = seqlevels)
  wait_until(!mgr$.server$has_request_waiting())
  
  seqinfo <- keepSeqlevels(seqinfo, seqlevels)
  
  outputEl <- remDr$findElement(using="id", "add_seqinfo_output")
  res <- outputEl$getElementText()[[1]]
  
  seqlengths <- seqlengths(mgr$.seqinfo)+1
  expected_res <- paste0(names(seqlengths), ":1-", seqlengths, collapse=",")
  expect_equal(res, expected_res)
})

test_that("rm_seqinfo works",{
  skip_if_not_installed("Mus.musculus")
  require(Mus.musculus)
  
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment")
  }
  
  server <- epivizrServer::createServer(port=7123L, 
                                        static_site_path=".",
                                        daemonized=TRUE, 
                                        verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test only works for daemonized servers")
  }
  
  .startRemoteDriver()
  on.exit({cat("stopping remDr\n"); .stopPhantomJS()})
  
  server$start_server()
  on.exit({cat("stopping server\n"); server$stop_server()}, add=TRUE)
  
  mgr <- createMgr(server)
  .navigateRemoteDriver(port=server$.port)
  wait_until(mgr$.server$is_socket_connected())

  seqinfo <- seqinfo(Mus.musculus)
  seqlevels <- paste0("chr", c(1:19,"X","Y", "M"))
  
  mgr$add_seqinfo(seqinfo, keep_seqlevels = seqlevels)
  wait_until(!mgr$.server$has_request_waiting())

  mgr$rm_seqinfo()
  wait_until(!mgr$.server$has_request_waiting())
  
  outputEl <- remDr$findElement(using="id", "add_seqinfo_output")
  res <- outputEl$getElementText()[[1]]
  
  expect_equal(res, "")
})

test_that("get_seqinfo works", {
  skip_if_not_installed("Mus.musculus")
  require(Mus.musculus)
  
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment")
  }
  
  server <- epivizrServer::createServer(port=7123L, 
                                        static_site_path=".",
                                        daemonized=TRUE, 
                                        verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test only works for daemonized servers")
  }
  
  .startRemoteDriver()
  on.exit({cat("stopping remDr\n"); .stopPhantomJS()})
  
  server$start_server()
  on.exit({cat("stopping server\n"); server$stop_server()}, add=TRUE)
  
  mgr <- createMgr(server)
  .navigateRemoteDriver(port=server$.port)
  wait_until(mgr$.server$is_socket_connected())
  
  seqinfo <- seqinfo(Mus.musculus)
  seqlevels <- paste0("chr", c(1:19,"X","Y", "M"))
  
  mgr$add_seqinfo(seqinfo, keep_seqlevels = seqlevels)
  wait_until(!mgr$.server$has_request_waiting())
  
  server$register_action("getSeqInfos", function(request_data) {
    mgr$get_seqinfo()
  })
  
  buttonEl <- remDr$findElement(using="id", "get_seqinfo_btn")
  buttonEl$clickElement()
  Sys.sleep(1)

  outputEl <- remDr$findElement(using="id", "get_seqinfo_output")
  res <- outputEl$getElementText()[[1]]
  
  seqlengths <- seqlengths(mgr$.seqinfo)+1
  expected_res <- paste0(names(seqlengths), ":1-", seqlengths, collapse=",")
  expect_equal(res, expected_res)
})

