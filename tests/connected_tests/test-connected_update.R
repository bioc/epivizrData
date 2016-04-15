context("connected update measurement")

test_that("update block works with connection", {
  skip_on_cran()
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
  
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr12", ranges=IRanges::IRanges(start=1:1000,width=10))
  
  ms_obj <- mgr$add_measurements(gr1, "dev1", send_request=TRUE)
  wait_until(!mgr$.server$has_request_waiting())
  
	ms_obj$update(gr2)
	wait_until(!mgr$.server$has_request_waiting())
	
	outEl <- remDr$findElement(using="id", "clear_cache_output")
	res <- outEl$getElementText()[[1]]
	expect_equal(res, paste(ms_obj$get_id(), "cache cleared."))
})

test_that("update block works with connection through mgr", {
  skip_on_cran()
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
  
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr12", ranges=IRanges::IRanges(start=1:1000,width=10))
  
  ms_obj <- mgr$add_measurements(gr1, "dev1", send_request=TRUE)
  wait_until(!mgr$.server$has_request_waiting())
  
  mgr$update_measurements(ms_obj, gr2)
  wait_until(!mgr$.server$has_request_waiting())
  
  outEl <- remDr$findElement(using="id", "clear_cache_output")
  res <- outEl$getElementText()[[1]]
  expect_equal(res, paste(ms_obj$get_id(), "cache cleared."))
  
  ms_obj2 <- mgr$add_measurements(gr1, "dev2", send_request=TRUE)
  wait_until(!mgr$.server$has_request_waiting())

  mgr$update_measurements(ms_obj2$get_id(), gr2)
  wait_until(!mgr$.server$has_request_waiting())
  
  outEl <- remDr$findElement(using="id", "clear_cache_output")
  res <- outEl$getElementText()[[1]]
  expect_equal(res, paste(ms_obj2$get_id(), "cache cleared."))
})

