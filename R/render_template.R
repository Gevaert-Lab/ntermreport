#' Validate file name
#'
#' @param filename The file name to be validated.
#' @return TRUE if the file name is valid, otherwise stops with an error message.
#' @importFrom assertthat assert_that is.string
validate_filename <- function(filename) {
  # Define invalid characters for file names
  invalid_chars <- "[<>:\"/\\|?*]"

  # Check if filename is a string
  assertthat::assert_that(assertthat::is.string(filename), msg = "filename must be a string.")

  # Check if filename length is less than 40 characters
  if (nchar(filename) > 40) {
    stop("The file name must be less than 40 characters.")
  }

  # Check if filename contains invalid characters
  if (grepl(invalid_chars, filename)) {
    stop("The file name contains invalid characters. Invalid characters are: <>:\"/\\|?*")
  }

  TRUE
}

#' Validate report folder path
#'
#' @param report_folder The folder path to be validated.
#' @return TRUE if the folder path is valid and writable, otherwise stops with an error message.
#' @importFrom assertthat assert_that is.writeable
validate_folder <- function(report_folder) {
  # Define invalid characters for Windows file system
  invalid_chars <- "[<>:\"/\\|?*]"

  # # Check if the folder path contains invalid characters
  # if (grepl(invalid_chars, report_folder)) {
  #   stop("The folder path contains invalid characters. Invalid characters are: <>:\"/\\|?*")
  # }

  if (!dir.exists(file.path(report_folder))) {
    dir.create(file.path( report_folder),recursive = TRUE)
  }
  dir.create(file.path( report_folder, "Result"),recursive = TRUE)

  # Check if the folder path is writable
  #assertthat::assert_that(assertthat::is.writeable(report_folder), msg = "The folder path is not writable.")

  TRUE
}



#' @title validate_template
#' @description Maps a user-friendly template ID to an actual .qmd file name.
#' @param t_id Character string representing the template ID.
#' @return The filename (string) of the corresponding Quarto template.
#' @importFrom assertthat assert_that is.string
#' @export
validate_template <- function(t_id) {
  
  # 1. The Dictionary: Mapping ID to physical .qmd file
  valid_templates <- list(
    "id_report"    = "DEV_DDA.qmd",
    "quant_report" = "DEV_QUANT_DDA.qmd"
  )
  # 2. Guard: Ensure the input is a single string
  assertthat::assert_that(
    assertthat::is.string(t_id), 
    msg = "Template ID (t_id) must be a single character string."
  )

  # 3. Guard: Check if the ID exists in our dictionary
  if (!t_id %in% names(valid_templates)) {
    stop(
      "Invalid template ID: '", t_id, "'. \n",
      "Available IDs are: ", paste(names(valid_templates), collapse = ", ")
    )
  }

  # 4. Return the filename string
  # We use [[ ]] to extract just the string value, not a list
  return(valid_templates[[t_id]])
}



#' Validate minimal parameters for the DIA-NN report
#'
#' @param params A list of parameters to be validated.
#' @return TRUE if all parameters are valid, otherwise stops with an error message.
#' @importFrom assertthat assert_that is.string
validate_params_minimal <- function(params) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  is_empty <- function(x) {
    is.character(x) && length(x) == 1 && x == ''
  }

  check_path <- function(x) {
    if (length(x) == 1 && x == '') {
      TRUE
    } else {
      file.exists(x)
    }
  }

   check_select_group <- function(x) {

    # Must be a named list
    if (!is.list(x) || is.null(names(x))) {
      return(FALSE)
    }else{
      return (TRUE)
    }
   }
  requirements <- list(
    input_file = list(
      type = "string",
      check = function(x) file.exists(x),
      msg = "Input file does not exist or is not specified."
    ),
    design_file = list(
      type = "string",
      check = function(x) file.exists(x),
      msg = "Design file does not exist or is not specified."
    ),
    folder_prj = list(type= "string"
    )
    ,

    select_group =list(type="list",
     check = check_select_group,
     msg = "select_group must be a *named list* of group sets"
    )
    ,
    description= list(
      type = "string"
    ),
    title= list(
      type = "string"
    ),
    subtitle = list(
      type = "string"
    ),
    author = list(
      type = "string"
    )
  
  )

  for (p in names(requirements)) {
    val <- params[[p]]
    req <- requirements[[p]]

    # Type check
    if (req$type == "string") {
      assertthat::assert_that(assertthat::is.string(val), msg = paste0("'", p, "' must be a string."))
    } else if (req$type == "numeric") {
      assertthat::assert_that(is.numeric(val), msg = paste0("'", p, "' must be numeric."))
    } else if (req$type == "list") {
      assertthat::assert_that(is.list(val), msg = paste0("'", p, "' must be a list."))
    }

    # Value check (if provided)
    if (!is.null(req$check)) {
      assertthat::assert_that(req$check(val), msg = req$msg %||% paste0("Invalid value for '", p, "'."))
    }
  }
  TRUE
}


#' @author andrea Argentini
#' @title render_dia_report
#' @param params_report List of parameters for the report.
#' @param template_file Path to the .qmd template.
#' @param report_target_folder Directory to save the output.
#' @param report_filename Name of the output file
#' @export
render_ntermdia_report <- function(params_report, template_file, report_target_folder, report_filename){

  diareport::render_dia_report(params_report, template = template_file,
     report_folder = report_target_folder, report_filename = report_filename)

}

#' @author andrea Argentini
#' @title render_quarto_index
#' @param report_info report 
#' @param out_file output file 
#' @param title title of the 
#' @importFrom  glue  glue_collapse glue
render_quarto_index <- function(report_info, out_file = "", title = "") {
  title <- paste(" N-terminal Analysis Projects ",title,collapse= "")
  links <- glue_collapse(
    glue('- [{report_info$name}]({report_info$file}): {report_info$summary}'),
    sep = "\n"
  )
  md <- glue("
---
title: \"{title}\"
---

# Welcome

This site contains the following reports:

{links}

*Generated on {Sys.Date()}*
")
  writeLines(md, out_file)
}

#' @author andrea Argentini
#' @title render_nterm_website
#' @param params_dda A named list of parameters to pass to the N-Term (DDA) report rendering function.
#' @param params_dia A named list of parameters to pass to the DIA report rendering function.
#' @param template_dda Path to the Quarto template (.qmd) for the N-Term (DDA) report.
#' @param report_folder Directory where the final rendered website will be copied. Should be writable.
#' @param template_dia Path to the Quarto template (.qmd) for the DIA report.
#' @param project_id 
#'  @return
#' Invisibly returns the path to the final website in \code{report_folder}.
#' @importFrom withr with_dir
#' @export
render_nterm_website <- function ( params_dda , params_dia , template_dda, report_folder, template_dia , project_id ){
  temp_work_dir_web <- file.path(tempdir(), paste0("website_", Sys.getpid()))
  dir.create(temp_work_dir_web, recursive = TRUE, showWarnings = FALSE)

  # 2. Copy Quarto project skeleton (index.qmd, _quarto.yml, etc.) and reports to temp
  site_src <- system.file("reports_site", package = "ntermreport")
  file.copy(list.files(site_src, full.names = TRUE), temp_work_dir_web, recursive = TRUE)
  log_info('Created temp folder for website')
  log_info(temp_work_dir_web)
  # Render reports inside site directory
  #params_dda, template_dda, report_folder, "nterm_report.html"
  render_nterm_report(params_dda, template_dda, report_folder, "nterm_report.html"  )
  file.copy(from =  file.path(report_folder,"nterm_report.html" ), to = temp_work_dir_web, overwrite = TRUE)


  #params_dia, template_dia, report_target_folder, "dia_report.html"
  render_ntermdia_report(  params_dia, template_dia, report_folder,  "dia_report.html" )
  file.copy(from =  file.path(report_folder,"dia_report.html" ), to = temp_work_dir_web, overwrite = TRUE)

  # Optionally: update index.qmd with custom info

  report_info <- data.frame(
  name = c("N-Term Report", "DIA Report"),
  file = c("nterm_report.html", "dia_report.html"),
  summary = c("Summary for N-Terminal DDA ", "Summary for N-terminal DIA LFQ")
)
 render_quarto_index(report_info, out_file = file.path(temp_work_dir_web, "index.qmd"), title = project_id )
  # Render Quarto website
 with_dir(temp_work_dir_web, {
   quarto::quarto_render(input = ".", as_job = FALSE)
  })


  # 1. Path to rendered site
  site_path <- file.path(temp_work_dir_web, "_site")

 # 2. Copy all files to report_folder
  file.copy(
  from = list.files(site_path, full.names = TRUE),
  to = report_folder,
  recursive = TRUE,
  overwrite = TRUE
)
  log_info('Cleaning temp folder ...')
  # Optionally, remove the temporary working directory to clean up
  unlink(temp_work_dir_web, recursive = TRUE)


}



#' @title merge_default_parameters
#'
#' @param params_int parameters
#' @return merged parameters
#' @importFrom yaml read_yaml
merge_default_parameters <- function  ( params_int  ){

  yaml_path <- system.file("config", "default_parameter.yaml", package = "ntermreport")
  default_p <- read_yaml(yaml_path )

  miss <- base::setdiff(names(default_p$params), names(params_int))
   for (a in miss) {
     params_int[[a]] <- default_p$params[[a]] }

  return (params_int)
}


#' @author andrea Argentini
#' @title Render a DDA report using a Quarto template
#'
#' @param params_report parameters
#' @param template_ids template ids can be a vector 
#' @param report_folder description
#' @param report_filename output report file
#' @details
#' The `params` list must contain the following elements:
#' \describe{
#'   \item{\code{test}}{test}
#'   \item{\code{test}}{test}
#'
#' }
#'
#' @return The full path to the rendered report.
#'
#' @importFrom quarto quarto_render
#' @importFrom fs file_move
#' @importFrom logger log_info log_threshold log_appender log_formatter INFO appender_console appender_file
#' @importFrom yaml as.yaml
#' @importFrom rlang expr
#' @importFrom utils modifyList
#' @importFrom stringr str_ends str_detect
#' @importFrom assertthat assert_that is.string
#' @export
render_nterm_report <- function(params_report, template_ids, report_folder, report_filename ) {

  # --- 1. Validation & Setup ---
  #validate_template( template)
  validate_folder(report_folder)
  validate_filename( filename = report_filename)
  params_report <- merge_default_parameters(params_report)

  validate_params_minimal(params_report)
  
  # Logger set up
  logger::log_threshold(logger::INFO)
  logger::log_appender(logger::appender_console)
  logger::log_formatter(logger::formatter_glue)
  logfile <- file.path(report_folder, "logfile_nterm.log")
  file.create(logfile)
  logger::log_appender(logger::appender_file(logfile ), index = 2)

  log_info ('Start processing...')
  for (t_id in template_ids) {
    log_info("Starting processing for template : {t_id}")

    # 1. Resolve the filename for Quarto
    t_file <- validate_template(t_id)
    # --- 2. Data Processing (The "Logic" Layer) ---
    # This returns the named list "Data Bag"
    # You can change 'analysis_type' to trigger different logic branches later
    data_ <- process_nterm_data(params_report, analysis_type = t_id)

    unique_output <- paste0(report_filename, "_", t_id, ".html")
    log_info('End processing for {t_id}...')
      res_write <- write_final_result(acetyl = data_$export_data$acetyl_table, 
                          pep_dump = data_$export_data$pep_dump,
                          quant_base = NULL,
                          group_l = NULL,
                          path =  report_folder )

    log_info ('Starting Visualization  ...')

    render_quarto_template( data_list = data_$quarto_data,
      template_name = t_file,
      report_fld = report_folder, 
      report_fname= unique_output, 
      params_report = params_report
    )
  }
  }
