library(testthat)
library(dplyr)

test_that("Global_stat", {
      data  <- readRDS(  testthat::test_path("Nterm_DDa.Rds"))

      out <- global_stat( data) 
  
   expected_first_row <- c(a = 'pyroglu_', b  = 1421, c = 3.12383213524149)
  
  expect_equal(out$status,0) # Result is done 
  expect_equal( colnames(out$res), c( 'label','count_absolute','percentage') ) # column names  expected
  expect_equal(dim(out$res), c(5, 3)) ## expected dimention 
  expect_equal(as.character(out$res[1, ]), as.character(expected_first_row)) # check fist row


} )


test_that("Group_stat", {
      data  <- readRDS(  testthat::test_path("Nterm_DDa.Rds"))
     
      out <- group_stat( data) 
   
  #print( length( out$l_uniq_protein$NR ))
  expect_protein_id <- c('P07108','P62316','P04424','P51888')
  expect_equal(out$status,0) # Result is done 
  expect_equal(dim(out$res), c(24, 4)) ## expected dimention 

  expect_false( is.null( out$l_uniq_protein)  ) # list uniq protein should not be NULL
  expect_equal(length( out$l_uniq_protein$RPE ),2198) # uniq protein list RPE
  expect_contains( out$l_uniq_protein$RPE, expect_protein_id) # expected protein ID
  expect_equal(length( out$l_uniq_protein$NR ),1833) # uniq protein list NR

} )


test_that("Sample_stat", {
      data  <- readRDS(  testthat::test_path("Nterm_DDa.Rds"))
     
      out <- sample_stat( data) 
   
  expect_protein_id <- c('P07108','P62316','P63215','Q14012')
  expect_equal(out$status,0) # Result is done 
  expect_equal(dim(out$res), c(64, 4)) ## expected dimention 

  expect_false( is.null( out$l_uniq_protein)  ) # list uniq protein should not be NULL
  expect_equal(length(out$l_uniq_protein$NR_1),1268) # uniq protein list RPE_1
  expect_equal(length(out$l_uniq_protein$NR_2),1337)# uniq protein list RPE_2
  expect_equal(length(out$l_uniq_protein$NR_3),1460)# uniq protein list RPE_3
  expect_contains( out$l_uniq_protein$NR_1, expect_protein_id) # expected protein ID

} )


test_that("acetyl_stat", {
      data  <- readRDS(  testthat::test_path("Nterm_DDa.Rds"))
     
      out <- acetyl_stat_( data) 

  expect_protein_id <- c('P07108','P62316','P63215','Q14012')
  expect_equal(out$status,0) # Result is done 
  expect_equal(length(out$res), 3) ## expected dimention 
  expect_equal(dim(out$res$NR$p_start_), c(637, 63)) ## expected dimention
  expect_equal(dim(out$res$NR$p_atis), c(51, 63)) ## expected dimention
  expect_equal(dim(out$res$RPE$p_start_), c(634, 63)) ## expected dimention
  expect_equal(dim(out$res$RPE$p_atis), c(53, 63)) ## expected dimention

} )

test_that("acd4_stat", {
      data  <- readRDS(  testthat::test_path("Nterm_DDa.Rds"))
     
      out <- acd4_stat_( data) 

  expect_equal(out$status,0) # Result is done 
  expect_equal(length(out$res), 3) ## expected dimention 
  expect_equal(dim(out$res$NR$p_start_), c(160, 63)) ## expected dimention
  expect_equal(dim(out$res$NR$p_atis), c(59, 63)) ## expected dimention
  expect_equal(dim(out$res$RPE$p_start_), c(149, 63)) ## expected dimention
  expect_equal(dim(out$res$RPE$p_atis), c(55, 63)) ## expected dimention

} )