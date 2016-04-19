context("connected measurement management")

test_that("add measurement works with connection", {
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
  
  se <- make_test_SE()
  ms_obj <- mgr$add_measurements(se, "example", columns=c("A", "B"), assay="counts2", send_request=TRUE)
  ms_id <- ms_obj$get_id()
  
  wait_until(!mgr$.server$has_request_waiting())
  ms_record <- mgr$.ms_list[[ms_id]]
  expect_true(ms_record$connected)

  expect_true(mgr$is_ms_connected(ms_obj))
  expect_true(mgr$is_ms_connected(ms_id))
  
  outputEl <- remDr$findElement(using="id", "add_measurements_output")
  ms_list <- outputEl$getElementText()[[1]]
  exp_list <- paste0(sprintf("%s:%s", ms_id, c("A","B")), collapse=",")
  expect_equal(ms_list, exp_list)
})


test_that("get_measurements works with connection", {
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  skip_if_not_installed("hgu133plus2.db")
  
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
  
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges::IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), 
                 score=rnorm(length(seq(1,100,by=25))))
  eset <- make_test_eset()
  
  msObj1 <- mgr$add_measurements(gr1, "dev1", send_request=TRUE); msId1=msObj1$get_id()
  msObj2 <- mgr$add_measurements(gr2, "dev2", send_request=TRUE); msId2=msObj2$get_id()
  msObj3 <- mgr$add_measurements(gr3, "dev3", send_request=TRUE, type="bp"); msId3=msObj3$get_id()
  msObj4 <- mgr$add_measurements(eset, "dev4", send_request=TRUE, columns=c("SAMP_1", "SAMP_2")); msId4=msObj4$get_id()
    
  wait_until(!mgr$.server$has_request_waiting())
  for (id in ls(mgr$.ms_list)) {
    expect_true(mgr$.ms_list[[id]]$connected)
  }
  
  mgr$.server$register_action("getMeasurements", function(request_data) {
    list(measurements=epivizrServer::json_writer(mgr$get_measurements()))
  })
  
  buttonEl <- remDr$findElement(using="id", "get_measurements_btn")
  buttonEl$clickElement()
  Sys.sleep(1)
  
  outputEl <- remDr$findElement(using="id", "get_measurements_output")
  ms_list <- outputEl$getElementText()[[1]]
  cat(ms_list, "\n")
  
  ms <- mgr$get_measurements()
  exp_list <- paste0(sprintf("%s:%s", ms$datasourceId, ms$id), collapse=",")
  cat(exp_list, "\n")
  
  expect_equal(ms_list, exp_list)
})

test_that("rm_measurements works with connection", {
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
  
  gr <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1),
                score1=rnorm(length(seq(1,100,by=25))),
                score2=rnorm(length(seq(1,100,by=25))))

  ms_obj <- mgr$add_measurements(gr, "dev1", send_request=TRUE, type="bp")
  wait_until(!mgr$.server$has_request_waiting())
  id <- ms_obj$get_id()
  expect_true(mgr$.ms_list[[id]]$connected)

  outputEl <- remDr$findElement(using="id", "add_measurements_output")
  ms_list <- outputEl$getElementText()[[1]]
  expect_equal(ms_list, paste0(sprintf("%s:score%d", id, c(1,2)), collapse=","))
  
  mgr$rm_measurements(id)
  wait_until(!mgr$.server$has_request_waiting())
  ms_list <- outputEl$getElementText()[[1]]
  expect_equal(ms_list, "")
  
  ms_obj <- mgr$add_measurements(gr, "dev1", send_request=TRUE, type="bp")
  wait_until(!mgr$.server$has_request_waiting())
  id <- ms_obj$get_id()
  ms_record <- mgr$.ms_list[[id]]
  expect_true(ms_record$connected)
  
  ms_list <- outputEl$getElementText()[[1]]
  expect_equal(ms_list, paste0(sprintf("%s:score%d", id, c(1,2)), collapse=","))
  
  mgr$rm_measurements(ms_obj)
  wait_until(!mgr$.server$has_request_waiting())
  ms_list <- outputEl$getElementText()[[1]]
  expect_equal(ms_list, "")
})

test_that("rm_allMeasurements works with connection", {
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
  
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges::IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), 
                 score=rnorm(length(seq(1,100,by=25))))

  ms1 <- mgr$add_measurements(gr1, "dev1", send_request=TRUE); msId1 <- ms1$get_id()
  ms2 <- mgr$add_measurements(gr2, "dev2", send_request=TRUE); msId2 <- ms2$get_id()
  ms3 <- mgr$add_measurements(gr3, "dev3", send_request=TRUE, type="bp"); msId3 <- ms3$get_id()
  wait_until(!mgr$.server$has_request_waiting())
  
  outputEl <- remDr$findElement(using="id", "add_measurements_output")
  ms_list <- outputEl$getElementText()[[1]]
  exp_list <- paste0(sprintf("%s:%s", c(msId1, msId2, msId3), c(msId1, msId2, "score")), collapse=",")
  cat(ms_list, "\n")
  cat(exp_list, "\n")
  expect_equal(ms_list, exp_list)
  
  mgr$rm_all_measurements()
  wait_until(!mgr$.server$has_request_waiting())
  
  ms_list <- outputEl$getElementText()[[1]]
  expect_equal(ms_list, "")
})

