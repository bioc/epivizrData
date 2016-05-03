context("manager creation")

test_that("new creates a proper object", {
  skip("gc error")
  server <- epivizrServer::createServer()
  mgr <- createMgr(server=server)
  expect_is(mgr, "EpivizDataMgr")
  
  expect_is(mgr$.ms_list, "environment")
  expect_equal(mgr$num_datasources(), 0)
  
  expect_equal(mgr$.ms_idCounter, 0)
  
  expect_is(mgr$.server, "EpivizServer")
  expect_true(mgr$is_server_closed())
  
  expect_is(mgr$.seqinfo, "Seqinfo")
  expect_equal(mgr$.seqinfo, GenomeInfoDb::Seqinfo())
})

test_that("server opening works as expected", {
  skip("gc error")
  server <- epivizrServer::createServer(try_ports=TRUE)
  mgr <- createMgr(server=server)
  expect_true(mgr$is_server_closed())
  
  server$start_server()
  on.exit(server$stop_server())
  
  expect_false(mgr$is_server_closed())
})

