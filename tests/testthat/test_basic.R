library(testthat)
library(dplyr)
library(stringr)

test_that("process_nterm_data integration base", {
    mock_pep <- readRDS(testthat::test_path("pep_small.Rds"))
    mock_psm <- readRDS(testthat::test_path("psm_small.Rds"))
    mock_design <- readRDS(testthat::test_path("design_small.Rds"))
    
   my_test_path <- file.path(tempdir(), paste0("run_test_", Sys.getpid()))
    
    if (!dir.exists(my_test_path)) {
        dir.create(my_test_path, recursive = TRUE)
    }
    
    # 3. Define the specific filenames
    # Now file.path will definitely see a character string
    path_psm  <- file.path(my_test_path, "mock_data_psm.tsv")
    path_pep  <- file.path(my_test_path, "mock_output_peptide.tsv")
    path_csv  <- file.path(my_test_path, "design.csv")
  
    write.table(mock_pep, path_pep, sep = "\t", row.names = FALSE)
    write.table(mock_psm, path_psm, sep = "\t", row.names = FALSE)
    write.table(mock_design, path_csv, sep = ";", row.names = FALSE)
    
  
   params <- list(
    input_file = path_psm,
    design_file = path_csv,
    title = 'PRC-6253 ',
    subtitle = "N-Terminal Evy Dev",
    author = 'Dev',
    select_group= list(A_ = c('WT','NAA80KO'))
  )
  
  out <- process_nterm_data(params, analysis_type = "id_")

  expect_type(out, "list")
  expect_named(out$quarto_data, c("glb_stat", "grb_stat", "pep_id", 
                                  "pep_id_group", "ace_group", "ace_sample"))
  expect_equal(nrow(out$quarto_data$glb_stat),9)
  expect_equal(nrow(out$quarto_data$grb_stat),54)
  expect_equal(nrow(out$quarto_data$pep_id),6)
  expect_equal(nrow(out$quarto_data$pep_id_group),6)
  expect_equal(length(out$quarto_data$ace_group),6)
  expect_equal(length(out$quarto_data$ace_sample),6)

  expect_named(out$export_data, c("acetyl_table", "pep_dump"))
  expect_equal(nrow(out$export_data$pep_dump),800)
  # Cleanup (Optional, R usually cleans tempdir() on exit)
  unlink(c(path_psm, path_pep, path_csv))
} 
)

