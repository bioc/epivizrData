context("connection")

test_that("server connection works", {
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
    
  title <- remDr$getTitle()[[1]]
  expect_equal(title, "EpivizServer Test Page")
    
  remDr$close()
})