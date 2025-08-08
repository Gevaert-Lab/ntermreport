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
  assertthat::assert_that(assertthat::is.writeable(report_folder), msg = "The folder path is not writable.")

  TRUE
}



#' Validate template parameter
#'
#' @param template The template parameter to be validated.
#' @return TRUE if the template is valid, otherwise stops with an error message.
#' @importFrom assertthat assert_that is.string
validate_template <- function(template) {
  # Define the list of valid templates
  valid_templates <- c( "Template_DDA.qmd")

  # Check if template is a string
  assertthat::assert_that(assertthat::is.string(template), msg = "template must be a string.")

  # Check if template belongs to the list of valid templates
  if (!template %in% valid_templates) {
    stop("Invalid template. The template must be one of the following: ", paste(valid_templates, collapse = ", "))
  }

  TRUE
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
#' @export
render_ntermdia_report <- function(params_report, template_file, report_target_folder, report_filename){

  diareport::render_dia_report(params_report, template = template_file,
     report_folder = report_target_folder, report_filename = report_filename)

}

#' @author andrea Argentini
#' @title render_quarto_index
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
#'  @return
#' Invisibly returns the path to the final website in \code{report_folder}.
#' @importFrom withr with_dir
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



#' @author andrea Argentini
#' @title Render a DDA report using a Quarto template
#'
#' @param params_report parameters
#' @param template template name
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
#' @importFrom utils modifyList
#' @importFrom assertthat assert_that is.string
#' @export
render_nterm_report <- function(params_report, template, report_folder, report_filename ) {

  # Validate parameters

  validate_template( template)
  validate_folder(report_folder)
  validate_filename( filename = report_filename)
  ## to be changed
  #params_report <- merge_default_parameters(params_report)

  validate_params_minimal(params_report)

  # other set up
  logger::log_threshold(logger::INFO)
  logger::log_appender(logger::appender_console)
  logger::log_formatter(logger::formatter_glue)
  logfile <- file.path(report_folder, "logfile_nterm.log")
  file.create(logfile)
  logger::log_appender(logger::appender_file(logfile ), index = 2)


  log_info ('Start ...')
  #debug(read_data)
  res <- read_data( params_report$input_file, params_report$design_file)
  #undebug(read_data)
  if (res$status == 1) stop(res$error)
  ## get global statistics

  res_a <- global_stat( res$nterm_data)
  if (res_a$status == 1) stop(res_a$error)
  ## group stats
  res_b <- group_stat( res$nterm_data)
  if (res_b$status == 1) stop(res_b$error)

  res_c <- sample_stat( res$nterm_data)
  if (res_c$status == 1) stop(res_c$error)

  res_acetyl <- acetyl_stat_( res$nterm_data)
  if (res_acetyl$status == 1) stop(res_acetyl$error)

  res_acd4  <- acd4_stat_( res$nterm_data)
  if (res_acd4$status == 1) stop(res_acd4$error)

  res_base <- quant_base_(res$nterm_data)
  if (res_base$status == 1) stop(res_base$error)

  list_group <- res$df_design %>% dplyr::distinct(Group) %>% pull()
  res_write <- write_final_result(acetyl = res_acetyl$res, acd4 = res_acd4$res,
                      quant_base = res_base$res,
                      group_l = list_group,
                      path =  report_folder )
  if (res_write$status == 1) stop(res_write$error)

  log_info ('End processing  ...')
  log_info ('Starting Visualization  ...')

  template_source_folder <- system.file("quarto_template", package = "ntermreport")
  if (template_source_folder == "") {
    stop("Template folder not found in the package.")
  }


  # Create a unique temporary working directory
  temp_work_dir <- file.path(tempdir(), paste0("quarto_temp_", Sys.getpid()))
  dir.create(temp_work_dir, recursive = TRUE, showWarnings = FALSE)
  log_info('Temp folder created : {temp_work_dir}')

  # Copy the entire template folder content to the temporary directory
  # This copies all files and subfolders (e.g., resource folders with JS/CSS files)
  success <- file.copy(from = template_source_folder,
                       to = temp_work_dir,
                       recursive = TRUE)
  if (!success) {
    stop("Failed to copy the template folder to the temporary directory.")
  }
  log_info('Copy Template file ...done')
  saveRDS(res_a$res, file.path(temp_work_dir,basename(template_source_folder), 'glob_stat.RDS'  ))
  saveRDS(res_b$res, file.path(temp_work_dir,basename(template_source_folder), 'group_stat.RDS'  ))
  saveRDS(res_c$res, file.path(temp_work_dir,basename(template_source_folder), 'sample_stat.RDS'  ))
  saveRDS(res_acetyl$res, file.path(temp_work_dir,basename(template_source_folder), 'acetyl_stat.RDS'  ))
  saveRDS(res_acd4$res, file.path(temp_work_dir,basename(template_source_folder), 'acd4_stat.RDS'  ))

  ## unique protein export

  saveRDS(res_b$l_uniq_protein, file.path(temp_work_dir,basename(template_source_folder), 'l_uniqueprot_group.RDS'  ))
  saveRDS(res_c$l_uniq_protein, file.path(temp_work_dir,basename(template_source_folder), 'l_uniqueprot_sample.RDS'  ))
  saveRDS(res_c$l_uniq_protein,  'l_uniqueprot_sample.RDS'  )


  params_report$glb_stat <-   file.path(temp_work_dir,basename(template_source_folder),'glob_stat.RDS'  )
  params_report$grp_stat<-   file.path(temp_work_dir,basename(template_source_folder),'group_stat.RDS'  )
  params_report$sample_stat <-   file.path(temp_work_dir,basename(template_source_folder),'sample_stat.RDS' )
  params_report$grp_uniqprot <-   file.path(temp_work_dir,basename(template_source_folder),'l_uniqueprot_group.RDS'  )
  params_report$sample_uniqprot<-   file.path(temp_work_dir,basename(template_source_folder),'l_uniqueprot_sample.RDS'  )
  params_report$acetyl_stat <-   file.path(temp_work_dir,basename(template_source_folder),'acetyl_stat.RDS'  )
  params_report$acd4_stat <-   file.path(temp_work_dir,basename(template_source_folder),'acd4_stat.RDS'  )

  log_info('Copy Rds Results ...done')

   # Construct the path to the copied template file in the temp directory.
  # Assumes that the template file is directly inside the copied folder.
  temp_template_path <- file.path(temp_work_dir, basename(template_source_folder), template)
  if (!file.exists(temp_template_path)) {
    stop("Template file not found in the temporary directory: ", temp_template_path)
  }

  path <- file.path(temp_work_dir, basename(template_source_folder))

  tryCatch({
    withr::with_dir(path, {
      quarto_render(
        input = temp_template_path,
        output_format = "html",
        output_file = report_filename,
        execute_params = params_report,
        quarto_args = c("--output-dir", path)
      )
    })
  }, error = function(e) {
    print("Error in Quarto rendering:")
    print(e$message)
    print("Cleaning Temp folder")
    unlink(temp_work_dir, recursive = TRUE)
    stop(e)
  })

  resource_folder_name <- paste0(tools::file_path_sans_ext(report_filename), "_files")
  rendered_report_path <- file.path(path, report_filename)


  if (!dir.exists(report_folder)) {
    dir.create(report_folder, recursive = TRUE)
  }


  log_info('Copying rendered html report ...')
  # Copy the rendered HTML report to the target folder
  file.copy(from = rendered_report_path, to = file.path(report_folder, report_filename), overwrite = TRUE)
  # If a resource folder was generated, copy it as well
  temp_resource_path <- file.path(temp_work_dir, resource_folder_name)
  if (dir.exists(temp_resource_path)) {
    file.copy(from = temp_resource_path,
              to = file.path(report_folder, resource_folder_name),
              recursive = TRUE, overwrite = TRUE)
  }

  log_info('Cleaning temp folder ...')
  # Optionally, remove the temporary working directory to clean up
  unlink(temp_work_dir, recursive = TRUE)

  return (-1)

}
