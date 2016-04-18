context("connected manager fetch")

test_that("connected mgr fetch rows works", {
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  skip_if_not_installed("rjson")
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
  
  eset <- make_test_eset()
  
  ms_obj <- mgr$add_measurements(eset, "dev4", send_request=TRUE, columns=c("SAMP_1", "SAMP_2")); 
  ms_id <- ms_obj$get_id()
  wait_until(!mgr$.server$has_request_waiting())
  
  query <- GRanges(seqnames="chr6", ranges=IRanges::IRanges(start=30000000,end=40000000))
  hits <- unique(subjectHits(findOverlaps(query, ms_obj$.object)))
  hits <- seq(min(hits), max(hits))
  
  mgr$.server$register_action("getRows", function(request_data) {
    mgr$get_rows(request_data$seqName,
                 request_data$start,
                 request_data$end,
                 request_data$metadata, request_data$datasource)
  })

  inputEl <- remDr$findElement(using="id", "_rows_input_location")
  inputEl$sendKeysToElement(list(sprintf("%s:%d-%d", "chr6", 30000000, 40000000)))

  inputEl <- remDr$findElement(using="id", "_rows_input_metadata")
  inputEl$sendKeysToElement(list("PROBEID,SYMBOL"))

  inputEl <- remDr$findElement(using="id", "_rows_input_datasource")
  inputEl$sendKeysToElement(list(ms_id))

  buttonEl <- remDr$findElement(using="id", "_rows_input_btn")
  buttonEl$clickElement()
  Sys.sleep(1)

  outputEl <- remDr$findElement(using="id", "get_rows_output")
  rows_info <- outputEl$getElementText()[[1]]
  cat(rows_info, "\n")

  exp_rows_info <- list(
    globalStartIndex = hits[1],
    useOffset = FALSE,
    nhits = length(hits),
    metadata = list(PROBEID=length(hits), SYMBOL=length(hits))
  )
  exp_rows_info <- rjson::toJSON(exp_rows_info)
  cat(exp_rows_info, "\n")
  expect_equal(rows_info, exp_rows_info)
})

test_that("connected mgr fetch values works", {
  skip("for now")
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  skip_if_not_installed("rjson")
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
  
  eset <- make_test_eset()
  
  ms_obj <- mgr$add_measurements(eset, "dev4", send_request=TRUE, columns=c("SAMP_1", "SAMP_2")); 
  ms_id <- ms_obj$get_id()
  wait_until(!mgr$.server$has_request_waiting())
  
  query <- GRanges(seqnames="chr6", ranges=IRanges::IRanges(start=30000000,end=40000000))
  hits <- unique(subjectHits(findOverlaps(query, ms_obj$.object)))
  hits <- seq(min(hits), max(hits))
  
  mgr$.server$register_action("getValues", function(request_data) {
    mgr$get_values(request_data$seqName, request_data$start, request_data$end,
                   request_data$datasource, request_data$measurement)
  })
  
  inputEl <- remDr$findElement(using="id", "_values_input_location")
  inputEl$sendKeysToElement(list(sprintf("%s:%d-%d", "chr6", 30000000, 40000000)))
  
  inputEl <- remDr$findElement(using="id", "_values_input_measurement")
  inputEl$sendKeysToElement(list("SAMP_1"))

  inputEl <- remDr$findElement(using="id", "_values_input_datasource")
  inputEl$sendKeysToElement(list(ms_id))

  buttonEl <- remDr$findElement(using="id", "_values_input_btn")
  buttonEl$clickElement()
  cat("getValues form submitted\n")
  Sys.sleep(1)

  outputEl <- remDr$findElement(using="id", "get_values_output")
  values_info <- outputEl$getElementText()[[1]]
  cat(values_info, "\n")

  exp_values_info <- list(
    globalStartIndex=hits[1],
    nvalues=length(hits)
  )
  exp_values_info <- rjson::toJSON(exp_values_info)
  cat(exp_values_info, "\n")
  expect_equal(values_info, exp_values_info)
})
