#' @author andrea argentini
#' @title process_nterm_data
#' 
#' @description
#' Internal helper function that orchestrates the data analysis pipeline. It reads the raw input,
#' applies the statistical logic based on the `analysis_type`, and structures the results into
#' two distinct "data bags".
#'
#' @param params_report A named list containing input parameters (input_file, design_file, select_group).
#' @param analysis_type Character string specifying the logic branch. Defaults to "id_".
#'
#' @return A named list containing `quarto_data` and `export_data`.
#' 
#' @keywords internal
#' 
#' @importFrom rlang expr .data
#' @importFrom stringr str_ends str_detect fixed
#' @importFrom methods is
#' @importFrom logger log_info
process_nterm_data <- function(params_report, analysis_type = "id_") {
  # 1. Read Data
  res <- read_data(params_report$input_file, params_report$design_file, params_report$select_group)
  if (res$status == 1) stop(res$error)

  # 2. Logic Branching
  if (analysis_type == "id_report") {
    logger::log_info("Starting Analysis for: {analysis_type}")
    stat_reg <- list(
      list(logic = expr(grepl('N-term*', .data$pep_var_mod, fixed = FALSE)), calc_pct = TRUE),
      list(logic = expr(grepl('*Ace*', .data$pep_var_mod, fixed = FALSE)),   calc_pct = FALSE),
      list(logic = expr(grepl('*Gln*', .data$pep_var_mod, fixed = FALSE)),   calc_pct = FALSE),
      list(logic = expr(.data$mascot_task == task[2] & !str_ends(.data$pep_seq, "R")), calc_pct = TRUE),
      list(logic = expr(.data$mascot_task == task[1] & str_detect(.data$pep_seq, 'H')), calc_pct = TRUE),
      list(logic = expr(.data$mascot_task == task[2]), calc_pct = TRUE)
    )
    
    stat_name <- c('N-terminally', 'Ace', 'Gln-NtermQ', 'C-terminal', 'N-terminally with H', 'NH2')
    
    # 3. Execute Analysis
    res_a <- global_PSM_general(res$nterm_data, stat_reg, stat_name)
    if (res_a$status == 1) stop(res_a$error)

    res_bb <- global_PEP_general(res$nterm_pep, res$df_design)
    if (res_bb$status == 1) stop(res_bb$error)
    
    # 4. Construct the "Data Bags"
    quarto_bag <- list(
      glb_stat      = res_a$res,
      grb_stat      = res_a$res_sample,
      pep_id        = res_bb$pep_cnt_sample,
      pep_id_group  = res_bb$pep_cnt_group,
      ace_group     = res_bb$ace_group,
      ace_sample    = res_bb$ace_sample
    )
    
    export_bag <- list(
      acetyl_table  = res_bb$ace_all,
      pep_dump      = res$nterm_pep
    )
    
  } else {
    stop(paste("Analysis type not supported:", analysis_type))
  }
  
  return(list(
    quarto_data = quarto_bag,
    export_data = export_bag
  ))
}


#' @title render_quarto_template
#' 
#' @description
#' This internal function manages the heavy lifting of the reporting layer. It 
#' creates a temporary sandbox, bundles all analysis results into a single 
#' RDS "data bag," updates the Quarto parameters, and renders the report. 
#' Finally, it cleans up the temporary directory and moves the output to 
#' the designated folder.
#'
#' @param data_list A named list containing all data objects (tables, stats) 
#' required by the Quarto template.
#' @param template_name Character string. The name of the `.qmd` file located 
#' within the package's `quarto_template` directory.
#' @param report_fld Character string. The final destination directory for 
#' the rendered report and its associated files.
#' @param report_fname Character string. The name of the resulting HTML file 
#' (e.g., "Nterm_Report.html").
#' @param params_report A list of parameters to be passed to Quarto. This 
#' function automatically appends a `data_path` element to this list.
#'
#' @return The function returns `NULL` invisibly. Its primary purpose is 
#' the side effect of file creation and directory management.
#' 
#' @keywords internal
#' 
#' @importFrom quarto quarto_render
#' @importFrom logger log_info log_error
#' @importFrom withr with_dir
#' @importFrom tools file_path_sans_ext
render_quarto_template <- function(data_list, template_name, report_fld, report_fname, params_report) {
  # 1. Setup Temp Dir (Your current logic is good here)
  template_source_folder <- system.file("quarto_template", package = "ntermreport")
  if (template_source_folder == "") {
    stop("Template folder not found in the package.")
  }

  temp_work_dir <- file.path(tempdir(), paste0("quarto_temp_", Sys.getpid()))
  dir.create(temp_work_dir, recursive = TRUE, showWarnings = FALSE)
  log_info('Temp folder created : {temp_work_dir}')
  success <- file.copy(from = template_source_folder,
                       to = temp_work_dir,
                       recursive = TRUE)
   if (!success) {
    stop("Failed to copy the template folder to the temporary directory.")
  }
  log_info('Copy Template file ...done')
  
  # 2. THE BIG CHANGE: Save everything into ONE file
  data_rds_path <- file.path(temp_work_dir, "report_data.rds")
  saveRDS(data_list, data_rds_path)
  
  log_info('Copy Rds Results ...done')
  # 3. Add that path to parameters
  params_report$data_path <- data_rds_path

    # Construct the path to the copied template file in the temp directory.
  # Assumes that the template file is directly inside the copied folder.
  temp_template_path <- file.path(temp_work_dir, basename(template_source_folder), template_name)
  if (!file.exists(temp_template_path)) {
    stop("Template file not found in the temporary directory: ", temp_template_path)
  }

  path <- file.path(temp_work_dir, basename(template_source_folder))
  
    tryCatch({
    withr::with_dir(path, {
      quarto_render(
        input = temp_template_path,
        output_format = "html",
        output_file = report_fname,
        execute_params = params_report,
        quarto_args = c( "--no-clean", "--output-dir", path)
      )
    })
  }, error = function(e) {
    log_error("Error in Quarto rendering: {e$message}")
    unlink(temp_work_dir, recursive = TRUE)
    stop(e)
  })

  resource_folder_name <- paste0(tools::file_path_sans_ext(report_fname), "_files")
  rendered_report_path <- file.path(path, report_fname)

  if (!dir.exists(report_fld)) {
    dir.create(report_fld, recursive = TRUE)
  }
    log_info('Copying rendered html report ...')
  # Copy the rendered HTML report to the target folder
  file.copy(from = rendered_report_path, to = file.path(report_fld, report_fname), overwrite = TRUE)
  # If a resource folder was generated, copy it as well
  temp_resource_path <- file.path(temp_work_dir, resource_folder_name)
  if (dir.exists(temp_resource_path)) {
    file.copy(from = temp_resource_path,
              to = file.path(report_fld, resource_folder_name),
              recursive = TRUE, overwrite = TRUE)
  }
  log_info('Cleaning temp folder ...')
  # Optionally, remove the temporary working directory to clean up
  unlink(temp_work_dir, recursive = TRUE)
  return(invisible(NULL))
  
}


#' @author andrea argentini
#' @title view_intersection
#' @description
#' Given a membership table (logical columns for sets and an "Elements" column), 
#' this function extracts the elements belonging to the intersection of sets 
#' defined in `sets`. 
#' 
#' @param df A data.frame containing at least one column named "Elements" and logical columns representing set membership.
#' @param sets A named list of logicals indicating for each set whether it should be included (`TRUE`) or excluded (`FALSE`) in the intersection.
#' @param exclusive Logical; currently, the logic applies the provided set conditions strictly (included if TRUE, excluded if FALSE).
#' @return A character vector of element identifiers satisfying the intersection conditions.
#' @export

view_intersection <- function(df, sets = list(), exclusive = TRUE) {
  # 1. Initialize the mask
  mask <- rep(TRUE, nrow(df))
  
  # 2. Iterate through sets using string indexing (Safe for R CMD Check)
  for (set_name in names(sets)) {
    # Ensure the column actually exists to avoid errors
    if (!set_name %in% names(df)) {
      stop(sprintf("Column '%s' not found in the data frame.", set_name))
    }
    
    if (sets[[set_name]]) {
      mask <- mask & df[[set_name]]
    } else {
      mask <- mask & !df[[set_name]]
    }
  }
  
  # 3. Access 'Elements' using double brackets to avoid Global Variable notes
  # This is the equivalent of df$Elements but invisible to the 'check' warning
  return(df[["Elements"]][mask])
}
#' @author andrea argentini
#' @title get_all_intersections
#' @description
#' Computes all non-empty intersections of logical set membership columns 
#' in a given data frame.
#' 
#' @param df A data.frame with one "Elements" column and one or more logical columns.
#' @return A named list where each entry corresponds to a non-empty intersection.
#' @export

get_all_intersections <- function(df) {
  # 1. Use vapply for strictness (replaces sapply)
  is_log <- vapply(df, is.logical, FUN.VALUE = logical(1))
  set_names <- names(df)[is_log]
  n_sets <- length(set_names)
  
  if (n_sets == 0) return(list())
  
  n_combinations <- 2^n_sets - 1
  combinations <- list()
  
  for (i in 1:n_combinations) {
    binary <- as.integer(intToBits(i))[1:n_sets]
    combo <- as.list(as.logical(binary))
    names(combo) <- set_names
    
    present_sets <- set_names[binary == 1]
    
    # 2. Fix non-ASCII intersection symbol using Unicode escape
    if (length(present_sets) == 1) {
      label <- paste(present_sets, "only")
    } else {
      # \u2229 is the intersection symbol (∩)
      label <- paste(present_sets, collapse = " \u2229 ")
    }
    
    combinations[[label]] <- combo
  }
  
  results <- list()
  for (name in names(combinations)) {
    # 3. Ensure view_intersection handles columns safely
    elements <- view_intersection(df, combinations[[name]])
    
    # Filter out empty or NA elements
    elements <- elements[!is.na(elements) & elements != ""]
    
    if (length(elements) >= 1) {
      results[[name]] <- list(
        elements = elements,
        count = length(elements),
        sets = combinations[[name]]
      )
    }
  }
  return(results)
}

#' @author andrea argentini
#' @title create_set_indicators
#' @description
#' Creates a compact human-readable summary of set membership indicators.
#' Each set is annotated with a checkmark (\u2713) if included (TRUE) or
#' a cross (\u2717) if excluded (FALSE).
#' 
#' @param sets A named logical list indicating membership of each set.
#' @return A character string with formatted set indicators.
#' @export
create_set_indicators <- function(sets) {
  # We use vapply instead of sapply in packages for strict type safety
  indicators <- vapply(sets, function(x) {
    if (x) "\u2713" else "\u2717"
  }, FUN.VALUE = character(1))
  
  paste(names(indicators), indicators, sep = ": ", collapse = " | ")
}


#' @author andrea argentini
#' @title render_child
#' @description
#'  This function allows to render other template.Rmd in the main quarto document
#' @param data  DE result for each comparison
#' @param path PAth where to store the result
#' @param label layer name of layer in the qfeature object
#' @param template name of the template .Rms file (contrast,heatmap etc)
#' @return none
#'  @importFrom xfun read_utf8
#' @export
render_child <- function(data, path , label, template ) {
    # _templateVEEN diagram.Rmd 
    res = knitr::knit_child(
      text = xfun::read_utf8( template),
      envir = rlang::env(data = data,  label = label,  path = path),
      quiet = TRUE
    )
    cat(res, sep = '\n')
    cat("\n")
 
}

#' @author Andrea Argentini
#' @title annotate_df
#' @description
#' This function annotates a list of data frames with a "Venn" column indicating 
#' in which experimental groups each peptide sequence was identified.
#' 
#' @param res A nested list of data frames containing proteomics results.
#' @param type Character string; the sub-list key to access (e.g., "p_start_").
#' @param groups A character vector of group labels to compare.
#' 
#' @return The modified `res` list with the added `Venn` column in the specified sub-data frames.
#' 
#' @importFrom dplyr mutate distinct c_across ungroup left_join rowwise select all_of
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @export

annotate_df <- function(res, type, groups) {
  
  # 1. Gather all pep_seq into a long table
  all_pep_seqs <- map_dfr(groups, function(g) {
    tibble(
      Group = g,
      pep_seq = res[[g]][[type]]$pep_seq
    )
  })  
  
  # 2. Create a wide presence/absence table
  # We use .data$Group and "present" as a string for pivot_wider
  venn_table <- all_pep_seqs %>%
    distinct() %>%
    mutate(present = TRUE) %>%
    pivot_wider(
      names_from = .data$Group, 
      values_from = "present", 
      values_fill = FALSE
    )

  # 3. Add a Venn annotation column
  # rowwise() and c_across are tricky: 
  # all_of(groups) tells dplyr "look at the external vector named groups"
  venn_table <- venn_table %>%
    rowwise() %>%
    mutate(
      Venn = paste(groups[as.logical(c_across(all_of(groups)))], collapse = "_")
    ) %>%
    ungroup()

  # 4. Annotate each group's data frame with Venn
  for (g in groups) {
    res[[g]][[type]] <- res[[g]][[type]] %>%
      left_join(
        venn_table %>% select(.data$pep_seq, .data$Venn), 
        by = "pep_seq"
      )
  }

  return(res)
}

#' @author Andrea Argentini
#' @title write_final_result
#' @description
#' Processes acetylation data by splitting it into N-terminal (positions 1, 2) and 
#' internal (positions > 2) peptides. Calculates group-wise statistics (median, SD, count) 
#' and exports the results, along with a peptide dump, into a multi-sheet Excel workbook.
#'
#' @param acetyl A data frame containing acetylation data. Must include columns: 
#' `pep_start`, `pep_seq`, `Group`, `prot_acc`, `prot_isoforms`, and `Percent_Acetylation`.
#' @param pep_dump A data frame containing the raw peptide dump information for the third sheet.
#' @param quant_base Reserved for future use (currently not used in the active logic).
#' @param group_l A vector of group names used for filtering or labeling (currently not used in the active logic).
#' @param path Character string specifying the directory path where the Excel file should be saved.
#'
#' @return A named list containing:
#' \itemize{
#'   \item \code{error}: Character string with the error message if the function fails, otherwise empty.
#'   \item \code{status}: Integer; 0 for success, 1 for failure.
#'   \item \code{res}: Integer; 1 for success, NULL for failure.
#' }
#'
#' @importFrom dplyr %>% filter group_by summarise n
#' @importFrom tidyr pivot_wider
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom stats median sd
#' @importFrom rlang .data
#' 
write_final_result = function(  acetyl, pep_dump , quant_base ,  group_l, path  ){

## make it only for specific pairwise condition like A, B and A in B and B in A 
tryCatch( expr = {  
  
  sheet_a <- acetyl %>% filter(.data$pep_start %in% c(1, 2)) %>%  
        group_by(.data$pep_seq, .data$Group, .data$prot_acc,.data$prot_isoforms,.data$pep_start) %>%
         summarise(
          median = median(.data$Percent_Acetylation, na.rm = TRUE),
          SD = sd(.data$Percent_Acetylation, na.rm = TRUE),
          n_observations =n(),
          .groups = "drop"
        ) %>%
        # 2. Pivot to wide format
        pivot_wider(
          names_from = .data$Group, 
          values_from = c("median", "SD", "n_observations"),
          names_glue = "{Group}_{.value}",  # This creates 'WT_median', 'WT_SD', etc.
          names_vary = "slowest"
        )
  sheet_b <- acetyl %>% filter(.data$pep_start > 2 ) %>%  
      group_by(.data$pep_seq, .data$Group, .data$prot_acc,.data$prot_isoforms,.data$pep_start) %>%
      summarise(
        median = median(.data$Percent_Acetylation, na.rm = TRUE),
        SD = sd(.data$Percent_Acetylation, na.rm = TRUE),
        n_observations =n(),
        .groups = "drop"
      ) %>%
      # 2. Pivot to wide format
      pivot_wider(
        names_from = .data$Group, 
        values_from = c("median", "SD", "n_observations"),
        names_glue = "{Group}_{.value}",  # This creates 'WT_median', 'WT_SD', etc.
        names_vary = "slowest"
      )

  # acetyl <- annotate_df(acetyl, type= 'p_start_', groups = group_l)
    
  # acd4 <- annotate_df(acd4, type= 'p_atis', groups =  group_l )
    
  wb <- createWorkbook()
  addWorksheet(wb, 'S1 1,2 starting N-term...')
  writeData(wb, 'S1 1,2 starting N-term...', sheet_a)
   addWorksheet(wb, 'S2 _2 starting N-term...')
  writeData(wb, 'S2 _2 starting N-term...', sheet_b)
   addWorksheet(wb, 'S3 PEP file...')
  writeData(wb, 'S3 PEP file...', pep_dump)
  
  # for (g in group_l ) {
  #   log_info(g)
  #   addWorksheet(wb, paste("cTIS", g,collapse = ' '))
  #   writeData(wb, paste("cTIS", g,collapse = ' '), acetyl[[g]]$p_start_)
    
  #   addWorksheet(wb, paste("aTIS", g,collapse = ' '))
  #   writeData(wb, paste("aTIS", g,collapse = ' '), acetyl[[g]]$p_atis)
    
  #   addWorksheet(wb, paste("Low Confidence cTIS", g,collapse = ' '))
  #   writeData(wb, paste("Low Confidence cTIS", g,collapse = ' '), acd4[[g]]$p_start_)
    
  #   addWorksheet(wb, paste("Low Confidence aTIS", g,collapse = ' '))
  #   writeData(wb, paste("Low Confidence aTIS", g,collapse = ' '), acd4[[g]]$p_atis) 

  #   addWorksheet(wb, paste("Valid Quant PSM",g,collapse = ' '))
  #   writeData(wb, paste("Valid Quant PSM",g,collapse = ' '), quant_base[[g]]$quant)


  # } 
    
  saveWorkbook(wb, file.path(path ,  "Report_Quantitative_peptides.xlsx"), overwrite = TRUE)
    
return( list(error= '', status= 0 ,   res = 1  ))
    
},error = function(err){
  print(paste("Annotating / Saving file : ",err))
  return( list(error= err, status= 1,  res = NULL  ))

} )
  
}

#' @author Andrea Argentini
#' @title check_dependencies
#' @description
#' This function gets a vector with names of packages and it
#' Check if packages are available, if not it installs them.
#' Otherwise, it loads the  requiredpackages.
#' @param required_packages list with packages to be installed
#' @return none
#' @importFrom utils install.packages
#' @importFrom BiocManager install
#' @export
check_dependencies = function(required_packages = required_packages){
  suppressPackageStartupMessages(
    for(i in required_packages){
      # require returns TRUE invisibly if it was able to load package
      if(! require(i, character.only = TRUE, quietly = TRUE)){
        #  If package was not able to be loaded then re-install
        tryCatch(install.packages(i , dependencies = TRUE), error = function(e) { NULL })
        tryCatch(BiocManager::install(i), error = function(e) { NULL })
        require(i, character.only = TRUE, quietly = TRUE)
      }
    }
  )

}



#' @author Andrea Argentini
#' @title check_length_design_data
#' @description
#' This function checks the names and the number of samples in Mascot report and experiment design data,
#' if Mascot report has more samples than design file, only sample present in design file are kept.
#'Remark : Model result are supposed to be in proteinRS layer.
#' @param data_ data frame containing the Mascot report data
#' @param design data frame containing experiment design data
#' @return status  int 0 / 1: error found, 2: samples in Mascot report are more than samples in experiment design data
#' @return error error message
#' @return message: message returned if data frame containing the Mascot report data is filtered
#' @importFrom dplyr %>% select pull distinct filter
#' @importFrom rlang .data
check_length_design_data <- function(data_, design) {
  status <- 0
  error <- ""
  message <- ""

  # 1. Extract Run names using .data pronoun
  data_sample <- data_ %>% distinct(.data$Run) %>% pull()
  d_sample <- design %>% distinct(.data$Run) %>% pull()

  # 2. Check: Mascot has FEWER samples than Design (Fatal Error)
  if (length(data_sample) < length(d_sample)) {
    error <- paste0(
      "Number of samples in the design file and in Mascot result does not match.\n",
      "Samples detected in Mascot:\n", paste(unlist(data_sample), collapse = "\n"),
      "\n\nSamples detected in Design File:\n", paste(unlist(d_sample), collapse = "\n")
    )
    status <- 1
    return(list(status = status, error = error, message = message))
  }

  # 3. Check: Names don't match (Fatal Error)
  if (any(!d_sample %in% data_sample)) {
    error <- "Samples in the design file and in Mascot do not match names."
    status <- 1
    return(list(status = status, error = error, message = message))
  }

  # 4. Check: Mascot has MORE samples than Design (Warning/Filter)
  if (length(data_sample) > length(d_sample)) {
    status <- 2
    # Use .data pronoun here as well
    df_mod <- data_ %>% filter(.data$Run %in% d_sample) 
    
    message <- paste0(
      "Number of samples in the Mascot result is bigger than number of samples in design file.\n", 
      "ONLY DATA RELATED TO SAMPLES IN DESIGN FILE IS KEPT"
    )
    return(list(status = status, error = error, message = message, data_ = df_mod))
  } else {
    # Perfect match
    return(list(status = status, error = error, message = message))
  }
}

#' @author Andrea Argentini
#' @title check_columns_presence
#' @description This helper function checks for the presence of a required set 
#' of column names in the input data frame.
#' 
#' @param df Input data frame to be checked.
#' @param min_features Character vector of required column names.
#' 
#' @return A named list containing:
#' \itemize{
#'   \item \code{status}: Integer; 0 for success, 1 if columns are missing.
#'   \item \code{error}: Character string containing the error message and missing columns.
#' }
#' 
#' @importFrom logger log_info
check_columns_presence  <- function  ( df, min_features){
  status <- 0
  #type_raw <- NA
  error <- ''
  # 1. Identify which required features are missing
  missing_cols <- min_features[!(min_features %in% colnames(df))]
  
  if (length(missing_cols) > 0) {
    log_info("Missing columns detected. Total columns in DF: {length(colnames(df))}")
    
    # 2. Create a helpful error message
    error <- paste0(
      "Required columns missing from the data frame: ", 
      paste(missing_cols, collapse = ", ")
    )
    status <- 1
    return(list(status=status ,error=error))
  }
  return(list(status=status, error= ''))
}


#' @author Andrea Argentini
#' @title read_data
#' @description Imports the Mascot PSM report, discovers the peptide summary file, 
#' and joins them with the experimental design file.
#' 
#' @param file_nterm Path to input Mascot PSM report (.tsv).
#' @param file_expdesign Path to design file (.csv or .tsv).
#' @param grp_selected A named list of group labels for validation.
#' 
#' @return A named list containing:
#' \itemize{
#'   \item \code{error}: Error message string (empty if success).
#'   \item \code{status}: 0 for success, 1 for error.
#'   \item \code{nterm_data}: Processed PSM data frame.
#'   \item \code{df_design}: Processed design data frame.
#'   \item \code{nterm_pep}: Processed peptide data frame.
#' }
#'
#' @importFrom dplyr mutate left_join join_by select distinct pull .data
#' @importFrom utils read.csv read.csv2 read.table read.delim
#' @importFrom stringr str_detect fixed
#' @importFrom yaml read_yaml
#' @importFrom logger log_info
read_data <- function(file_nterm, file_expdesign, grp_selected) {

  tryCatch( expr = {
    
   folder_path <- dirname(file_nterm)

    # 3. Search for the other file using a regular expression
    # Example: Look for any PDF file that contains the word "Summary"
    target_pattern <- ".*_output_peptide.tsv"

    matching_files <- list.files(
      path = folder_path, 
      pattern = target_pattern , 
      full.names = TRUE  # This returns the absolute path
    )
  if (length(matching_files) == 0) {
      return(list(
        error = paste0("Peptide tsv file not found. Check for *_output_peptide.tsv in: ", folder_path),
        status = 1, nterm_data = NULL, df_design = NULL, nterm_pep = NULL
      ))
    } else if (length(matching_files) > 1) {
      stop("Multiple peptide files found! Please isolate the relevant file.")
    }

    file_ntermpep <- matching_files[1]

    yaml_path <- system.file("config", "default_input_file.yaml", package = "ntermreport")
    if (yaml_path == "") stop("Internal configuration file 'default_input_file.yaml' not found.")
    default_file_input_col <- yaml::read_yaml(yaml_path)

    ## reading psm 
    df <-  read.delim(file_nterm, header = TRUE, stringsAsFactors = FALSE, check.names = TRUE)
    check_psm_file <- check_columns_presence (df, min_features =  default_file_input_col$psm_file_col )

  if (check_psm_file$status == 1) {
      return(list(
        error = paste("PSM file missing columns:", paste(default_file_input_col$psm_file_col[1:5], collapse = ', ')), 
        status = 1, nterm_data = NULL, nterm_pep = NULL, df_design = NULL
      ))
    }
    ## reading pep file 
    df_pep <- read.delim(file_ntermpep, header = TRUE, stringsAsFactors = FALSE, check.names = TRUE)
    check_pep_file <- check_columns_presence (df_pep, min_features =  default_file_input_col$psm_pep_col )

    if (check_pep_file$status == 1) {
      return(list(
        error = paste("Peptide file missing columns:", paste(default_file_input_col$psm_pep_col[1:5], collapse = ', ')), 
        status = 1, nterm_data = NULL, nterm_pep = NULL, df_design = NULL
      ))
    }

    #" add exp design data

L <- readLines(file_expdesign, n = 1)
    design <- if (grepl(";", L)) read.csv2(file_expdesign) else read.csv(file_expdesign)
    
    log_info('Checking Design File (EDF)...')
    min_col_need_design <- c('Run', 'Sample', 'Group', 'Replicate')
    result_check <- check_columns_presence(design, min_features = min_col_need_design)

    if (result_check$status == 1) {
      return(list(
        error = paste("Design file missing columns:", paste(min_col_need_design, collapse = ', ')), 
        status = 1, nterm_data = NULL, nterm_pep = NULL, df_design = NULL
      ))
    }
    
    if (any(str_detect(design$Run, fixed(r"(\\)")))) {
          design <- design %>% 
            mutate(Run = basename(.data$Run)) %>% 
            mutate(Run = gsub('.raw', '', .data$Run))
        }
  

   valid_groups <- design %>% distinct(.data$Group) %>% pull(.data$Group)
    for (nm in names(grp_selected)) {
      invalid <- setdiff(grp_selected[[nm]], valid_groups)
      if (length(invalid) > 0) {
        return(list(
          error = paste0("Groups in '", nm, "' not found in design: ", paste(invalid, collapse = ", ")),
          status = 1, nterm_data = NULL, nterm_pep = NULL, df_design = NULL
        ))
      }
    }
    
    # add exp design to output
    
    df <- df %>% mutate(Run = basename(.data$input_file) ) %>% mutate (Run =  gsub('.raw','',.data$Run))
    ## do the same in pep file 
    
    df_pep <- df_pep %>% mutate(Run = basename(.data$input_file) ) %>% mutate (Run =  gsub('.raw','',.data$Run))

    checkedLength_pep  <- check_length_design_data (df_pep, design)
 
    # sanity check between data and exp design info 
    checkedLength_psm  <- check_length_design_data (df, design)

    # 3. Handle CRITICAL ERRORS (Status 1)
    # If either file fails completely, return the error immediately
    if (checkedLength_psm$status == 1 || checkedLength_pep$status == 1) {
        # Determine which error to show (prioritize df, then pep)
        err_msg <- if(checkedLength_psm$status == 1) checkedLength_psm$error else checkedLength_psm$error
        
        return(list(
            error      = err_msg, 
            status     = 1, 
            nterm_data =  NULL , 
            nterm_pep =  NULL , 
            df_design = NULL,
            nterm_pep =NULL 
        ))
    }
   
        # If status is 2, the function usually returns a filtered version of the data
    if (checkedLength_psm$status == 2) {
        df <- checkedLength_psm$data_
        log_info(paste("PSM:", checkedLength_psm$message))
    }

    if (checkedLength_pep$status == 2) {
        df_pep <- checkedLength_pep$data_
        log_info(paste("Peptide:", checkedLength_pep$message))
    }

    ## steps shared chen the data is ok. 
    df <- df %>% left_join(design %>% select(.data$Run, .data$Group, .data$Sample), by = "Run")
    df <- df %>% mutate(Percent_Acetylation = (.data$L.H * 100) / (.data$L.H + 1))
    
    df_pep <- df_pep %>% left_join(design %>% select(.data$Run, .data$Group, .data$Sample), by = "Run")
    df_pep <- df_pep %>% mutate(Percent_Acetylation = (.data$Median.L.H * 100) / (.data$Median.L.H + 1))
    return( list(error= '', status= 0, nterm_data =  df, df_design = design , nterm_pep = df_pep))
  },error = function(err){
    print(paste("Reading Design / Nterm PSM  file :  ",err))
    return( list(error= err, status= 1, 
          nterm_data =  NULL , 
          nterm_pep =  NULL , 
          df_design = NULL,
          nterm_pep =NULL ))

  } )

}



#'@author Andrea Argentini
#' @title  process_filter_wip
#'
#' @description 
#' Applies a specific filtering logic to the input data frame and computes 
#' the absolute count of the remaining rows. This function supports 
#' tidy evaluation for dynamic filtering.
#' @param filter_item A list containing the filtering logic (e.g., `filter_item$logic`).
#' @param filter_label A character string used as a label for the specific metric.
#' @param data The input data frame to be filtered.
#' @param task An array or vector of mascot tasks used for context (if applicable within the logic).
#' @return A list containing:
#' \itemize{
#'   \item{filter_name} The provided `filter_label`.
#'   \item{val_count} The number of rows (integer) after applying the filter.
#' }
#' @importFrom dplyr %>% filter
#' @importFrom logger log_info


process_filter_wip <- function(filter_item, filter_label, data , task) {
    ## total
  
     print(filter_item$logic)
    val_count <-  data %>% filter(!! filter_item$logic)  %>% nrow()
   
    return(list(
      filter_name = filter_label,
      val_count = val_count
    ))
}




#'@author Andrea Argentini
#' @title  global_PEP_general
#'
#' @description  Calculates the number of identified precursors per Sample and Group,
#' and filters for N-terminally acetylated peptides based on specific 
#' methionine (M) cleavage logic

#' @param pep_nterm input n-terminal data frame parsed from peptide file 
#' @param design dataframe realted to the design experiment 
#' @return A list containing:
#' \itemize{
#'   \item{error} Error message string.
#'   \item{status} Status code (0 for success).
#'   \item{pep_cnt_sample} Precursor counts per sample.
#'   \item{pep_cnt_group} Precursor counts per group.
#'   \item{ace_group} Acetylated peptides split by group.
#'   \item{ace_sample} Acetylated peptides split by sample.
#' }
#' @importFrom  logger log_info
#' @importFrom dplyr %>% distinct count arrange filter select .data
#'
global_PEP_general <- function(pep_nterm, design) {

  log_info('Global Statistics Start ...')

  # 1. Precursor Counting per Sample
  df_count_run <- pep_nterm %>%
    distinct(.data$Sample, .data$pep_seq, .data$All_Nterm_mods_identified) %>%
    count(.data$Sample, name = "n_precursors") %>%
    arrange(.data$Sample)
  
  # 2. Precursor Counting per Group
  df_count_group <- pep_nterm %>%
    distinct(.data$Group, .data$pep_seq, .data$All_Nterm_mods_identified) %>%
    count(.data$Group, name = "n_precursors") %>%
    arrange(.data$Group)
  
  # 3. Filter for Acetylation and Validity
  # Using .data$ for columns with dots to avoid R CMD check notes
  all_ace <- pep_nterm %>% 
    filter(.data$Nterm_mod.HS == "Acetyl") %>% 
    filter(.data$quant_valid.HS == TRUE) 
    
  # 4. Filter for N-terminal Start positions (1 or 2)
  all_filtered <- all_ace %>% 
    filter(.data$pep_start %in% c(1, 2)) %>% 
    select(
      .data$pep_modified_seq, 
      .data$Nterm_mod.HS, 
      .data$pep_start, 
      .data$pep_res_before, 
      .data$prot_acc, 
      .data$prot_desc, 
      .data$pep_seq, 
      .data$Sample, 
      .data$Group
    ) 

  # 5. Split data into lists for the Quarto report "Data Bag"
  ace_groups_pep <- split(all_filtered, all_filtered$Group)
  ace_sample_pep <- split(all_filtered, all_filtered$Sample)
 
  return(list(
    error          = '', 
    status         = 0,
    pep_cnt_sample = df_count_run,  
    pep_cnt_group  = df_count_group, 
    ace_group      = ace_groups_pep,  
    ace_sample     = ace_sample_pep,
    ace_all        = all_ace
  ))
}



#' @author Andrea Argentini
#' @title global_PSM_general
#'
#' @description 
#' Computes global PTM statistics from PSM data using a list of regular expressions. 
#' Calculates specific proteomics metrics including SCX enrichment efficiency 
#' and N-terminal acetylation percentages across the entire dataset and per sample.
#'
#' @param d_nterm A data frame containing PSM-level N-terminal data.
#' @param stat_reg A list of regular expressions (captured as expressions) defining the modifications.
#' @param stat_name A character vector of names corresponding to the `stat_reg` patterns.
#' 
#' @return A list containing:
#' \itemize{
#'   \item{error} Character string containing error messages, if any.
#'   \item{status} Integer status code (0 for success, 1 for error).
#'   \item{res} Dataframe of global summary statistics.
#'   \item{res_sample} Dataframe of summary statistics broken down by sample.
#' }
#' 
#' @importFrom logger log_info
#' @importFrom dplyr %>% distinct pull filter mutate case_when .data
#' @importFrom tibble add_row


global_PSM_general  <- function(d_nterm, stat_reg, stat_name) {

  log_info('Global Statistics Start ...')
  #  sorted by num id 
  # task[1] KNfix
  # task[2] NH2
  task <- d_nterm %>% distinct(.data$mascot_task) %>% pull(.data$mascot_task) %>% 
              as.integer() %>% sort() %>% 
                as.array()
   ## compute the statistics 
 # Internal helper to standardize the results table
  summarize_peptide_stats <- function(counts_list) {
    # 1. Convert list to vector
    counts <- vapply(counts_list, function(x) x$val_count, numeric(1))
    
    # 2. Pre-calculate the denominator constant
    # Ensure keys exist to avoid NA errors
    needed <- c('N-terminally', 'C-terminal', 'NH2')
    counts_safe <- counts
    counts_safe[is.na(counts_safe)] <- 0
    
    total_val <- sum(counts_safe[needed], na.rm = TRUE)
    
    # 3. Build the data frame
    df <- data.frame(
      label = names(counts_list),
      count_absolute = counts,
      stringsAsFactors = FALSE
    ) %>%
      # Add the custom enrichment calculation row
      add_row(
        label = "%.enrich.SCX.step", 
        count_absolute = (counts_safe['N-terminally'] + counts_safe['C-terminal'] - counts_safe['N-terminally with H'])
      ) %>%
      # Calculate percentages using the pre-calculated constant
      mutate(
        percentage = case_when(
          total_val == 0 ~ 0,
          .data$label %in% c('N-terminally', 'C-terminal', 'N-terminally with H', 'NH2', '%.enrich.SCX.step') ~ (.data$count_absolute / total_val) * 100,
          TRUE ~ NA_real_
        )
      ) %>%
      add_row(
        label = "Total PSM", 
        count_absolute = total_val,
        percentage = 100.00
      ) %>%
      add_row(
        label = "Nterminally Acetylated",
        percentage = if(total_val > 0) (counts_safe['Ace'] / total_val) * 100 else 0
      )
      
    return(df)
  }
   
  tryCatch(expr = {
    # 1. Global Processing
    results_list <- lapply(seq_along(stat_reg), function(i) {
      process_filter_wip(stat_reg[[i]], stat_name[i], d_nterm, task)
    })
    names(results_list) <- stat_name
    res <- summarize_peptide_stats(results_list) 
    
    # 2. Per-Sample Processing
    samples <- d_nterm %>% distinct(.data$Sample) %>% pull(.data$Sample)

    results_sample_df <- lapply(samples, function(sample_name) {
      sample_data <- d_nterm %>% filter(.data$Sample == sample_name)
      
      raw_counts <- lapply(seq_along(stat_reg), function(i) {
        process_filter_wip(stat_reg[[i]], stat_name[i], sample_data, task)
      })
      names(raw_counts) <- stat_name
      
      final_table <- summarize_peptide_stats(raw_counts)
      final_table$sample <- sample_name
      
      return(final_table)
    })
  
    res_df_sample <- do.call(rbind, results_sample_df)

    # Cleanup formatting
    rownames(res) <- NULL
    rownames(res_df_sample) <- NULL

    log_info('Global Statistics End ...')
    return(list(error = '', status = 0, res = res, res_sample = res_df_sample))

  }, error = function(err) {
    log_info("Global Stat Error: {err$message}")
    return(list(error = err$message, status = 1, res = NULL, res_sample = NULL))
  })
}

