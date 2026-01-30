#' @author andrea argentini
#' @title view_intersection
#' @description
#' Given a membership table (logical columns for sets and an `Elements` column), 
#' this function extracts the elements belonging to the intersection of sets 
#' defined in `sets`. If `exclusive = TRUE`, the mask is applied strictly: 
#' - `TRUE` means the element must belong to the set, 
#' - `FALSE` means the element must not belong to the set.  
#' The result is the subset of `Elements` that satisfy all inclusion/exclusion conditions.
#' @param df A data.frame containing at least one column named `Elements` and logical columns representing set membership.
#' @param sets A named list of logicals indicating for each set whether it should be included (`TRUE`) or excluded (`FALSE`) in the intersection.
#' @param exclusive Logical; if `TRUE` (default), elements must match the exact inclusion/exclusion pattern defined in `sets`.
#' @return A character vector of element identifiers satisfying the intersection conditions.


view_intersection <- function(df, sets = list(), exclusive = TRUE) {
  mask <- rep(TRUE, nrow(df))
  for (set_name in names(sets)) {
    if (sets[[set_name]]) {
      mask <- mask & df[[set_name]]
    } else {
      mask <- mask & !df[[set_name]]
    }
  }
  return(df$Elements[mask])
}



#' @author andrea argentini
#' @title get_all_intersections
#' @description
#' Computes all non-empty intersections of logical set membership columns 
#' in a given data frame.  
#' It systematically generates all possible combinations of inclusion/exclusion 
#' across the sets (2^n - 1 combinations for n sets), evaluates which elements 
#' belong to each combination using `view_intersection()`, and returns only 
#' those intersections that contain one or more elements.  
#' Each intersection includes its name, the matched elements, their count, 
#' and the logical definition of the combination.
#' @param df A data.frame with one `Elements` column and one or more logical columns (each representing membership in a set).
#' @return A named list where each entry corresponds to a non-empty intersection, 
#'   containing:
#'   \describe{
#'     \item{elements}{Character vector of elements in the intersection}
#'     \item{count}{Integer count of elements}
#'     \item{sets}{Named logical list describing which sets were included/excluded}
#'   }
#' @export

get_all_intersections <- function(df) {
  # Get set names (column names that are logical/boolean)
  set_names <- names(df)[sapply(df, is.logical)]
  n_sets <- length(set_names)
  
  n_combinations <- 2^n_sets - 1
  combinations <- list()
  
  for(i in 1:n_combinations) {
    binary <- as.integer(intToBits(i))[1:n_sets]
    combo <- as.list(as.logical(binary))
    names(combo) <- set_names
    
    present_sets <- set_names[binary == 1]
    if(length(present_sets) == 1) {
      label <- paste(present_sets, "only")
    } else {
      label <- paste(present_sets, collapse = " ∩ ")
    }
    
    combinations[[label]] <- combo
  }
  
  results <- list()
  for (name in names(combinations)) {
    elements <- view_intersection(df, combinations[[name]])
        elements <- elements[!is.na(elements) & elements != ""]
    if ( length(elements) >= 1) {  # keep only intersections with more than one element
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
#' Each set is annotated with a checkmark ("✓") if included (`TRUE`) or 
#' a cross ("×") if excluded (`FALSE`), and the results are concatenated 
#' into a single string.
#' @param sets A named logical list indicating membership of each set.
#' @return A character string with formatted set indicators, e.g. 
#'   `"SetA: ✓ | SetB: × | SetC: ✓"`.
#' @export
create_set_indicators <- function(sets) {
  indicators <- sapply(sets, function(x) if(x) "✓" else "×")
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
#' This function annotate the df with N groups 
#' @param  res list with packages to be installed
#' @param  groups  vector of group label  
#' @return none
#' @importFrom dplyr case_when mutate distinct c_across ungroup left_join rowwise
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom tidyr  pivot_wider


annotate_df <- function(res, type ,groups) {
 
# Gather all pep_seq into a long table
all_pep_seqs <- map_dfr(groups, function(g) {
  tibble(
    Group = g,
    pep_seq = res[[g]][[type]]$pep_seq
  )
})  
  
# Create a wide presence/absence table
venn_table <- all_pep_seqs %>%
  distinct() %>%
  mutate(present = TRUE) %>%
  pivot_wider(names_from = Group, values_from = present, values_fill = FALSE)

# Add a Venn annotation column
venn_table <- venn_table %>%
  rowwise() %>%
  mutate(
    Venn = paste(groups[as.logical(c_across(all_of(groups)))], collapse = "_")
  ) %>%
  ungroup()
# 3. Annotate each group's data frame with Venn

  for (g in groups) {
 res[[g]][[type]]<- res[[g]][[type]] %>%
    left_join(venn_table %>% select(pep_seq, Venn), by = "pep_seq")
}

  return(res)
}


#' @author Andrea Argentini
#' @title write_final_result
#' @description
#' This function save all result in a excell file
#' @param required_packages list with packages to be installed
#' @return int 
#' @importFrom  openxlsx createWorkbook addWorksheet writeData saveWorkbook

write_final_result = function(  acetyl, acd4 , quant_base ,  group_l, path  ){

## make it only for specific pairwise condition like A, B and A in B and B in A 

tryCatch( expr = {  
  
  acetyl <- annotate_df(acetyl, type= 'p_start_', groups = group_l)
    
  acd4 <- annotate_df(acd4, type= 'p_atis', groups =  group_l )
    
  wb <- createWorkbook()  
  for (g in group_l ) {
    log_info(g)
    addWorksheet(wb, paste("cTIS", g,collapse = ' '))
    writeData(wb, paste("cTIS", g,collapse = ' '), acetyl[[g]]$p_start_)
    
    addWorksheet(wb, paste("aTIS", g,collapse = ' '))
    writeData(wb, paste("aTIS", g,collapse = ' '), acetyl[[g]]$p_atis)
    
    addWorksheet(wb, paste("Low Confidence cTIS", g,collapse = ' '))
    writeData(wb, paste("Low Confidence cTIS", g,collapse = ' '), acd4[[g]]$p_start_)
    
    addWorksheet(wb, paste("Low Confidence aTIS", g,collapse = ' '))
    writeData(wb, paste("Low Confidence aTIS", g,collapse = ' '), acd4[[g]]$p_atis) 

    addWorksheet(wb, paste("Valid Quant PSM",g,collapse = ' '))
    writeData(wb, paste("Valid Quant PSM",g,collapse = ' '), quant_base[[g]]$quant)


  } 
    
  saveWorkbook(wb, file.path(path ,  "Quantitative_peptides.xlsx"), overwrite = TRUE)
    
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
#' @importFrom dplyr  %>%  select pull
check_length_design_data  <- function  (data_ , design){
  status <- 0
  error <- ''
  message <- ''

  data_sample <-  data_ %>% dplyr::distinct(Run) %>% pull()
  ## filename does not exist
  d_sample <- design %>% dplyr::distinct(Run) %>% pull()
  #log_info(paste0(data_sample,collapse = ' '))
  #log_info(paste0(d_sample,collapse = ' '))

  if (length(data_sample) < length(d_sample)){

    error <- paste0('Number of samples in the design file and in Mascot result  does not match \n  Samples detected in DIANN :\n',  paste(unlist(data_sample), collapse = "\n")  ,  '\n Samples detected in EDF',paste(unlist(d_sample), collapse = "\n")  , '\n')
    status <- 1
    return(list(status=status,error=error,message=message))
  }

  if (length(data_sample[!d_sample %in% data_sample]) >= 1){
    error <- 'Samples in the design file and in Mascot do not match'
    status <- 1
    return(list(status=status,error=error,message=message))
  }
  ### pay attention here
  if (length(data_sample) > length(d_sample)){
    status <- 2
    df_mod <- data_ %>% filter(Run %in% d_sample) 
    
    message <-  paste0('Number of samples in the Mascot result is bigger than number of samples in design file.\n', 
      'ONLY DATA RELATED TO SAMPLES IN DESIGN FILE IS KEPT')
    return(list(status=status, error = error, message=message , data_ = df_mod))
  }else{
    return(list(status=status,error=error,message=message))
  }

}


#'@author Andrea Argentini
#' @title read_data
#'
#' @description It imports the mascot PSM report and the experiments design file.
#' @param file_nterm Path to input mascot report
#' @param file_expdesign Path to design TSV file
#' @return Data frame with imported data
#' @importFrom  dplyr mutate left_join join_by select
#' @importFrom utils read.csv read.csv2 read.table
read_data <- function(file_nterm, file_expdesign, grp_selected) {

  tryCatch( expr = {
    
   folder_path <- dirname(file_nterm)

    # 3. Search for the other file using a regular expression
    # Example: Look for any PDF file that contains the word "Summary"
    target_pattern <- "*_output_peptide.tsv"

    matching_files <- list.files(
      path = folder_path, 
      pattern = target_pattern , 
      full.names = TRUE  # This returns the absolute path
    )
    if (length(matching_files) > 0) {
      file_ntermpep <- matching_files[1] 
      } else if (length(matching_files) > 1) {
          stop("Multiple peptide files found! Please include only the one relevant with the current experiment.")
      }else{
        return(list(
            error = paste0(" Peptide tsv file not found.\n Check the presence of *_output_peptide.tsv in : ", folder_path, '\n'),
            status = 1, nterm_data = NULL, df_design = NULL, nterm_pep =NULL
          ))

    }
    df <-  read.delim(file_nterm, header = TRUE, stringsAsFactors = FALSE, check.names = TRUE)

    df_pep <- read.delim(file_ntermpep, header = TRUE, stringsAsFactors = FALSE, check.names = TRUE)
    #" add exp design data

    ## add  possible control
    L <- readLines(file_expdesign, n = 1)
    if (grepl(";", L)) design <- read.csv2(file_expdesign) else design <- read.csv(file_expdesign)
    
    valid_groups <- design %>% dplyr::distinct(Group) %>% dplyr::pull(Group)

    for (nm in names(grp_selected)) {
      grp_set <- grp_selected[[nm]]
    
      # Check that each group set has ≥ 2 values
      if (length(grp_set) < 1) {
        return(list(
          error = paste0(" select_group '", nm, "' must contain at least one list"),
          status = 1, nterm_data = NULL, df_design = NULL, nterm_pep =NULL
        ))
      }
    
    # Check that all values exist in design
      invalid <- setdiff(grp_set, valid_groups)
      if (length(invalid) > 0) {
        1
      }
    }
    
    # add exp design to output
    
    df <- df %>% mutate(Run = basename(input_file) ) %>% mutate (Run =  gsub('.raw','',Run))
    ## do the same in pep file 
    
    df_pep <- df_pep %>% mutate(Run = basename(input_file) ) %>% mutate (Run =  gsub('.raw','',Run))

    checkedLength_pep  <- check_length_design_data (df_pep, design)
 
    # sanity check between data and exp design info 
    checkedLength_psm  <- check_length_design_data (df, design)

    # 3. Handle CRITICAL ERRORS (Status 1)
    # If either file fails completely, return the error immediately
    if (checkedLength_psm$status == 1 || checkedLength_pep$status == 1) {
        # Determine which error to show (prioritize df, then pep)
        err_msg <- if(checkedLength_psm$status == 1) checked_df$error else checkedLength_pep$error
        
        return(list(
            error      = err_msg, 
            status     = 1, 
            nterm_data = NULL, 
            df_design  = NULL, 
            nterm_pep  = NULL
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
    df <- df %>%
      left_join(design %>%
                  select(Run, Group, Sample), join_by(Run))
    
    df$Percent_Acetylation <- (df$L.H * 100) / (df$L.H + 1)
    
    #" other step for peptide file 
    df_pep <- df_pep %>%
      left_join(design %>%
                  select(Run, Group, Sample), join_by(Run))

    return( list(error= '', status= 0, nterm_data =  df, df_design = design , nterm_pep = df_pep))
  },error = function(err){
    print(paste("Reading Design / Nterm PSM  file :  ",err))
    return( list(error= err, status= 1,  nterm_data =  NULL , nterm_pep =  NULL , df_design = NULL, nterm_pep =NULL ))

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
#'   \item{filter_name}{ The provided `filter_label`.}
#'   \item{val_count}{ The number of rows (integer) after applying the filter.}
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

#' @param d_nterm input n-terminal data frame parsed from peptide file 
#' @param design dataframe realted to the design experiment 
#' @return A list containing:
#' \itemize{
#'   \item{error}{ Error message string.}
#'   \item{status}{ Status code (0 for success).}
#'   \item{pep_cnt_sample}{ Precursor counts per sample.}
#'   \item{pep_cnt_group}{ Precursor counts per group.}
#'   \item{ace_group}{ Acetylated peptides split by group.}
#'   \item{ace_sample}{ Acetylated peptides split by sample.}
#' }
#' @importFrom  logger log_info
#' @importFrom dplyr %>% distinct count arrange filter select
#'

global_PEP_general  <- function(pep_nterm, design ) {

  log_info('Global Statistics Start ...')

  # table logic    
  df_count_run <- pep_nterm %>%
  distinct(Sample, pep_seq, All_Nterm_mods_identified) %>%
  count(Sample, name = "n_precursors") %>%
  arrange(Sample)
  
  df_count_group <- pep_nterm %>%
  distinct(Group, pep_seq, All_Nterm_mods_identified) %>%
  count(Group, name = "n_precursors") %>%
  arrange(Group)
  
  # ration acetilation 
  
  # pep_nterm %>% filter(Nterm_mod.HS == "Acetyl")  %>%  filter (quant_valid.HS == TRUE ) %>%  
  #   filter(pep_start %in% c(1, 2)) %>%  filter(substr(pep_seq, 1, 1) == "M" | pep_res_before == "M") 

  all_ <- pep_nterm %>% filter(Nterm_mod.HS == "Acetyl")  %>%  filter (quant_valid.HS == TRUE ) %>%  
    filter(pep_start %in% c(1, 2))  %>% 
    select(pep_modified_seq, Nterm_mod.HS, pep_start,pep_res_before,prot_acc, prot_desc,pep_seq ,Sample, Group) 

  ace_groups_pep <- split(all_, all_$Group)
  ace_sample_pep <- split(all_, all_$Sample)
 
  return(list(error= '', status= 0,pep_cnt_sample = df_count_run  ,  pep_cnt_group = df_count_group, 
                       ace_group =  ace_groups_pep ,  ace_sample =  ace_sample_pep ) )
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
#' @param stat_reg A list of regular expressions defining the modifications to search for.
#' @param stat_name A character vector of names corresponding to the `stat_reg` patterns.
#' 
#' @return A list containing:
#' \itemize{
#'   \item{error}{ Character string containing error messages, if any.}
#'   \item{status}{ Integer status code (0 for success, 1 for error).}
#'   \item{res}{ Dataframe of global summary statistics.}
#'   \item{res_sample}{ Dataframe of summary statistics broken down by sample.}
#' }
#' 
#' @importFrom logger log_info
#' @importFrom dplyr %>% distinct pull filter mutate case_when
#' @importFrom tibble add_row


global_PSM_general  <- function(d_nterm, stat_reg, stat_name) {

  log_info('Global Statistics Start ...')
  #  sorted by num id 
  # task[1] KNfix
  # task[2] NH2
  task <- d_nterm %>% distinct(mascot_task) %>% pull() %>% as.integer() %>% sort() %>%   as.array()
  #browser()
   ## compute the statistics 
  summarize_peptide_stats <- function(counts_list) {
        # 1. Convert list to vector
        counts <- vapply(counts_list, function(x) x$val_count, numeric(1))
        
        # 2. Pre-calculate the denominator constant
        # This avoids the "object not found" error later
        total_val <- sum(counts[c('N-terminally', 'C-terminal', 'NH2')], na.rm = TRUE)
        
        # 3. Build the data frame
        df <- data.frame(
          label = names(counts_list),
          count_absolute = counts,
          stringsAsFactors = FALSE
        ) %>%
          # Add the custom enrichment calculation row
          add_row(
            label = "%.enrich.SCX.step", 
            count_absolute = (counts['N-terminally'] + counts['C-terminal'] - counts['N-terminally with H'])
          ) %>%
          # Calculate percentages using the pre-calculated constant
          mutate(
            percentage = case_when(
              total_val == 0 ~ 0,
              label %in% c('N-terminally', 'C-terminal', 'N-terminally with H', 'NH2', '%.enrich.SCX.step') ~ (count_absolute / total_val) * 100,
              TRUE ~ NA_real_
            )
          ) %>%
          # Add the Total row at the very bottom using the constant
          add_row(
            label = "Total PSM", 
            count_absolute = total_val,
            percentage = 100.00
          )     %>%
          # Add the Total row at the very bottom using the constant
          add_row(
            label = "Nterminally Acetylated",
            percentage = (counts['Ace'] / total_val) * 100
          )
          
        return(df)

      }
   
  log_info('Global Statistics Sample  ...')
  tryCatch( expr = {

  results_list <- lapply(seq_along(stat_reg), function(i) {
    process_filter_wip(stat_reg[[i]], stat_name[i], d_nterm, task  )
  })
  names(results_list) <- stat_name

  res <- summarize_peptide_stats(results_list)   
    
  samples <- d_nterm %>% distinct(Sample) %>% pull()

   results_sample_df <- lapply(samples, function(sample_name) {
        # 1. Filter data for this sample
        sample_data <- d_nterm %>% filter(Sample == sample_name)
        
        # 2. Get raw counts using your existing logic
        raw_counts <- lapply(seq_along(stat_reg), function(i) {
          process_filter_wip(stat_reg[[i]], stat_name[i], sample_data, task)
        })
        names(raw_counts) <- stat_name
        
        # 3. Use the new function to get the final table for this sample
        final_table <- summarize_peptide_stats(raw_counts)
        
        # Add a column so we know which sample this belongs to
        final_table$sample <- sample_name
        
        return(final_table)
      })
  
res_df_sample <- do.call(rbind, results_sample_df)
    

rownames(res) <- NULL
rownames(res_df_sample) <- NULL

log_info('Global Statistics End ...')
return( list(error= '', status= 0,  res = res ,
                           res_sample = res_df_sample))
},error = function(err){
    print(paste("Global Stat :  ",err))
    return( list(error= err, status= 1,res =NULL,res_sample=NULL  ))
  })
}



#'@author Andrea Argentini
#' @title  process_filter
#'
#' @description  This function apply the right filtering and computed the metrics related,
#' bot as absolute and percentage values.
#' @param filter_pattern  reg exp to filter
#' @param filter_label label of the filter / metrics
#' @param data  inpute dataframe to filter
#' @param countpeptides  denominator cardinality of the input dataframe
#' @return list with computed metrics
#' @importFrom  dplyr distinct filter join_by select
#' @importFrom utils read.csv read.csv2 read.table
#' @importFrom  logger log_info


process_filter <- function(filter_pattern, filter_label, data,countpeptides ) {
    ## total
    if (filter_label == 'total') {
      #log_info(filter_label)
      #data %>% dim(data)[1]
      count_main <- dim( data)[1]
      percentage_main = NaN
    }
    if (filter_label == 'unique_peptide') {
      #log_info(filter_label)
      # unique peptide
      #dim(data %>%  distinct(,pep_modified_seq, .keep_all= TRUE))[1]
      count_main <- dim(data %>%  distinct(pep_modified_seq, .keep_all= TRUE))[1]
      percentage_main = NaN
    }
    if (filter_label == 'unique_protein') {
      #log_info(filter_label)
      # uniqueprotein
      #dim(data %>%  distinct(prot_acc, .keep_all= TRUE))[1]
      count_main = dim(data %>%  distinct(prot_acc, .keep_all= TRUE))[1]
      percentage_main = NaN
    }
    if (filter_label %in% c('pyroglu_','ace','AcD4','C-terminal','NH2')){

      if (filter_label == 'C-terminal') {
        #log_info(paste(filter_pattern, collapse = ' '))
        filtered_data <- data %>%
          filter(
            !grepl(filter_pattern[[1]], pep_seq, fixed = FALSE),
            !grepl(filter_pattern[[2]], pep_var_mod, fixed = FALSE)
          )
      }else if (filter_label == "NH2"){
        #log_info(paste(filter_pattern, collapse = ' '))
        filtered_data <- data %>%
          filter(
            grepl(filter_pattern[[1]], pep_seq, fixed = FALSE),
            !grepl(filter_pattern[[2]], pep_var_mod, fixed = FALSE)
          )
      } else {
        #log_info(paste(filter_pattern, collapse = ' '))
        filtered_data <- data %>%
          filter(grepl(filter_pattern, pep_var_mod, fixed = FALSE))
      }

      # Calculate counts and percentages
      count_main <- nrow(filtered_data)
      percentage_main <- count_main / countpeptides * 100
    }
    return(list(
      filter_name = filter_label,
      count_main = count_main,
      percentage_main = percentage_main
    ))
}


#'@author Andrea Argentini
#' @title  quant_base_
#'
#' @description quant base dataframe per group
#' @param d_nterm input data frame
#' @return list of dataframes.For each group , cTIS and aTIS dataframe are included.
#' @importFrom  dplyr filter

quant_base_ <- function(d_nterm){
    log_info('Quant Valid Base Extraction Start...')

  tryCatch( expr = {
    groups <- d_nterm %>% distinct(Group) %>% pull()

    res__ <- lapply(groups, function(group_name) {
      g_ <- d_nterm %>% filter(Group == group_name)
    
      quant <- g_ %>% filter (quant_valid == TRUE )  
      return(list( quant = quant ))
    })

    names(res__) <- groups
     log_info('Quant Valid Base Extraction End ...')
        return( list(error= '', status= 0, res =  res__ ))

    },error = function(err){
      print(paste("quant_base  :  ", err))
      return( list(error= err, status= 1,res =NULL ))
  } )
}

