

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
  #log_info(paste(group_l, collapse = ' '))
  
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
  log_info(paste0(data_sample,collapse = ' '))
  log_info(paste0(d_sample,collapse = ' '))

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
read_data <- function(file_nterm, file_expdesign) {

  tryCatch( expr = {

    df <- read.table(file_nterm, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    #" add exp design data

    ## add  possible control
    L <- readLines(file_expdesign, n = 1)
    if (grepl(";", L)) design <- read.csv2(file_expdesign) else design <- read.csv(file_expdesign)

    # add exp design to output

    df <- df %>% mutate(Run = basename(input_file) ) %>% mutate (Run =  gsub('.raw','',Run))
    # sanity check between data and exp design info 
    checkedLength  <- check_length_design_data (df, design)

    if (checkedLength$status==1){

    return( list(error= checkedLength$error, status= checkedLength$status ,nterm_data =  NULL, df_design = NULL ))
    }
   
    if (checkedLength$status==2){
       df <- checkedLength$data_
      log_info(checkedLength$message)
    }

    ## steps shared chen the data is ok. 
    df <- df %>%
      left_join(design %>%
                  select(Run, Group, Sample), join_by(Run))

    df$Percent_Acetylation <- (df$L.H * 100) / (df$L.H + 1)


    return( list(error= '', status= 0, nterm_data =  df, df_design = design ))
  },error = function(err){
    print(paste("Reading Design / Nterm PSM  file :  ",err))
    return( list(error= err, status= 1,  nterm_data =  df, df_design = design ))

  } )

}

#'@author Andrea Argentini
#' @title  group_stat
#' @description Compute statistics at group level. Statistic and its labels are
#' parameters at the moment.
#'
#' @param d_nterm input n-terminal data frame
#' @return res dataframe with all the computed statistics
#' @return l_uniq_protein list of all the unique protein per group
#' @importFrom  dplyr distinct pull filter
#' @importFrom magrittr  %>% 
#' @importFrom  logger log_info

group_stat <- function(d_nterm) {

  log_info('Group Statistics Starting ...')

  filter_<- list( '','','',   '*Gln->pyro-Glu*',
                  '*Acetyl (N-term)*',
                  '*Acetyl:2H\\(3\\)C13\\(1\\) \\(N-term\\)*',
                  list('*R$|K$','*pyro-Glu*|*Acetyl*'),
                  list('*R$|K$','*pyro-Glu*|*Acetyl*'))
  label_ <- c('total','unique_peptide','unique_protein',
              'pyroglu_','ace','AcD4','C-terminal','NH2')

  tryCatch( expr = {
  groups <- d_nterm %>% distinct(Group) %>% pull()

  all_results <- lapply(groups, function(group_name) {
    g_ <- d_nterm %>% filter(Group == group_name)
    countpeptides <- nrow(g_)
    log_info(paste0('statistics for: ',group_name, ' -->  \n'))
    results_list <- lapply(seq_along(filter_), function(i) {
      process_filter(filter_[[i]], label_[i], g_, countpeptides)
    })
    names(results_list) <- label_

    return(list(group = group_name, results = results_list))
  })
  names(all_results) <- groups

  ## create a df

  res_df <- do.call(rbind, lapply(all_results, function(group_entry) {
    group_name <- group_entry$group
    results_list <- group_entry$results

    data.frame(
      group = group_name,
      metric = names(results_list),
      count_absolute = sapply(results_list, function(x) x$count_main),
      percentage = sapply(results_list, function(x) x$percentage_main),
      stringsAsFactors = FALSE
    )
  }))
  rownames(res_df) <- NULL
  log_info('Group Statistics Ends ...')
  },error = function(err){
    print(paste("Group Statistics :  ",err))
    return( list(error= err, status= 1,res =NULL ,l_uniq_protein =NULL))
  })

  tryCatch( expr = {
  log_info('Group Unique protein Starting ...')

  ## protein unique list
  distinct_proteins_list <- lapply(groups, function(group_name) {
    g_ <- d_nterm %>% filter(Group == group_name)
    unique_proteins <- g_ %>% distinct(prot_acc) %>% pull(prot_acc)
    return(unique_proteins)
  })
  names(distinct_proteins_list) <- groups
  log_info('Group Unique protein Ends ...')

  },error = function(err){
    print(paste("Group unique protein :  ",err))
    return( list(error= err, status= 1,res =NULL ,l_uniq_protein =NULL))
  })



  return( list(error= '', status= 0,  res = res_df, l_uniq_protein = distinct_proteins_list ))
}



#'@author Andrea Argentini
#' @title  sample_stat
#'
#' @description Compute statistics at sample level.Statistic and its labels are
#' parameters at the moment.
#' @param d_nterm input n-terminal data frame
#' @return res dataframe with all the computed statistics
#' @return l_uniq_protein list of all the unique protein per group
#' @importFrom  dplyr distinct pull filter
#' @importFrom  logger log_info



sample_stat <- function(d_nterm) {

  log_info('Sample Statistics Starting ...')

  filter_<- list( '','','',   '*Gln->pyro-Glu*',
                  '*Acetyl (N-term)*',
                  '*Acetyl:2H\\(3\\)C13\\(1\\) \\(N-term\\)*',
                  list('*R$|K$','*pyro-Glu*|*Acetyl*'),
                  list('*R$|K$','*pyro-Glu*|*Acetyl*'))
  label_ <- c('total','unique_peptide','unique_protein',
              'pyroglu_','ace','AcD4','C-terminal','NH2')

  tryCatch( expr = {
    samples <- d_nterm %>% distinct(Sample) %>% pull()

    all_results <- lapply(samples, function(sample_name) {
      g_ <- d_nterm %>% filter(Sample == sample_name)
      countpeptides <- nrow(g_)
      log_info(paste0('statistics for Sample : ',sample_name, ' -->  \n'))
      results_list <- lapply(seq_along(filter_), function(i) {
        process_filter(filter_[[i]], label_[i], g_, countpeptides)
      })
      names(results_list) <- label_

      return(list(sample = sample_name, results = results_list))
    })
    names(all_results) <- samples

    ## create a df

    res_df <- do.call(rbind, lapply(all_results, function(group_entry) {
      sample_name <- group_entry$sample
      results_list <- group_entry$results

      data.frame(
        sample = sample_name,
        metric = names(results_list),
        count_absolute = sapply(results_list, function(x) x$count_main),
        percentage = sapply(results_list, function(x) x$percentage_main),
        stringsAsFactors = FALSE
      )
    }))
    rownames(res_df) <- NULL
    log_info('Sample Statistics Ends ...')
    },error = function(err){
      print(paste("Sample Stats Computation :  ",err))
      return( list(error= err, status= 1,q_feat =NULL ))
    })

  ## protein unique list

  log_info('Sample Unique protein  Starting ...')
  tryCatch( expr = {
  distinct_proteins_list <- lapply(samples, function(sample_name) {
    g_ <- d_nterm %>% filter(Sample == sample_name)
    unique_proteins <- g_ %>% distinct(prot_acc) %>% pull(prot_acc)
    return(unique_proteins)
  })
  names(distinct_proteins_list) <- samples
  log_info('Sample Unique protein Ends ...')

  },error = function(err){
    print(paste("Unique protein  samples :  ",err))
    return( list(error= err, status= 1,q_feat =NULL ))
  })

  return( list(error= '', status= 0,  res = res_df, l_uniq_protein = distinct_proteins_list ))
}


#'@author Andrea Argentini
#' @title  global_stat
#'
#' @description Global statistics computed for the entire dataset.Statistic and its labels are
#' parameters at the moment.
#' @param d_nterm input n-terminal data frame
#' @return res dataframe with all the computed metrics
 #' @importFrom  logger log_info

#'

global_stat <- function(d_nterm) {

  log_info('Global Statistics Starting ...')

  filter_option <- list('*Gln->pyro-Glu*',
                        '*Acetyl (N-term)*',
                        '*Acetyl:2H\\(3\\)C13\\(1\\) \\(N-term\\)*',
                        list('*R$|K$','*pyro-Glu*|*Acetyl*'),
                        list('*R$|K$','*pyro-Glu*|*Acetyl*'))
  label_filter <- c('pyroglu_','ace','AcD4','C-terminal','NH2')

  countpeptides <- nrow(d_nterm)

  tryCatch( expr = {

    results_list <- lapply(seq_along(filter_option), function(i) {
      process_filter(filter_option[[i]], label_filter[i], d_nterm,countpeptides )
    })
    names(results_list) <- label_filter

    ## result
    res <- data.frame(
      label = names(results_list),
      count_absolute = sapply(results_list, function(x) x$count_main),
      percentage = sapply(results_list, function(x) x$percentage_main)
    )

    rownames(res) <- NULL

    return( list(error= '', status= 0,  res = res ))
    log_info('Global Statistics Ends ...')
  },error = function(err){
    print(paste("Global Stat :  ",err))
    return( list(error= err, status= 1,res =NULL ))
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
      log_info(filter_label)
      #data %>% dim(data)[1]
      count_main <- dim( data)[1]
      percentage_main = NaN
    }
    if (filter_label == 'unique_peptide') {
      log_info(filter_label)
      # unique peptide
      #dim(data %>%  distinct(,pep_modified_seq, .keep_all= TRUE))[1]
      count_main <- dim(data %>%  distinct(pep_modified_seq, .keep_all= TRUE))[1]
      percentage_main = NaN
    }
    if (filter_label == 'unique_protein') {
      log_info(filter_label)
      # uniqueprotein
      #dim(data %>%  distinct(prot_acc, .keep_all= TRUE))[1]
      count_main = dim(data %>%  distinct(prot_acc, .keep_all= TRUE))[1]
      percentage_main = NaN
    }
    if (filter_label %in% c('pyroglu_','ace','AcD4','C-terminal','NH2')){

      if (filter_label == 'C-terminal') {
        log_info(paste(filter_pattern, collapse = ' '))
        filtered_data <- data %>%
          filter(
            !grepl(filter_pattern[[1]], pep_seq, fixed = FALSE),
            !grepl(filter_pattern[[2]], pep_var_mod, fixed = FALSE)
          )
      }else if (filter_label == "NH2"){
        log_info(paste(filter_pattern, collapse = ' '))
        filtered_data <- data %>%
          filter(
            grepl(filter_pattern[[1]], pep_seq, fixed = FALSE),
            !grepl(filter_pattern[[2]], pep_var_mod, fixed = FALSE)
          )
      } else {
        log_info(paste(filter_pattern, collapse = ' '))
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
    log_info('Quant Valid Base Extraction ...')

  tryCatch( expr = {
    groups <- d_nterm %>% distinct(Group) %>% pull()

    res__ <- lapply(groups, function(group_name) {
      g_ <- d_nterm %>% filter(Group == group_name)
    
      quant <- g_ %>% filter (quant_valid == TRUE )  
      return(list( quant = quant ))
    })

    names(res__) <- groups

        return( list(error= '', status= 0, res =  res__ ))

    },error = function(err){
      print(paste("quant_base  :  ", err))
      return( list(error= err, status= 1,res =NULL ))
  } )
}



#'@author Andrea Argentini
#' @title  acetyl_stat_
#'
#' @description Process statistics for percentage of acetylation
#' @param d_nterm input data frame
#' @return list of dataframes.For each group , cTIS and aTIS dataframe are included.
#' @importFrom  dplyr filter

acetyl_stat_ <- function(d_nterm) {

  log_info('Acetyl Statistics ...')

  tryCatch( expr = {
    groups <- d_nterm %>% distinct(Group) %>% pull()

    res__ <- lapply(groups, function(group_name) {
      g_ <- d_nterm %>% filter(Group == group_name)
      start_ <- g_ %>% filter(grepl("Acetyl \\(N-term\\)", pep_modified_seq, fixed = FALSE)) %>%
        filter (quant_valid == TRUE )  %>%
        filter(pep_res_before != "R") %>%
        filter(substr(pep_seq, 1, 1) == "M" | pep_res_before == "M") %>%
        filter(pep_start %in% c(1, 2))
      aTIS_ <- g_ %>% filter(grepl('Acetyl \\(N-term\\)', pep_modified_seq, fixed = FALSE)) %>%
        filter (quant_valid == TRUE )  %>%
        filter(pep_res_before != "R") %>%
        filter(substr(pep_seq, 1, 1) == "M" | pep_res_before == "M") %>%
        filter(pep_start > 2)
      return(list( atis = aTIS_, start_= start_))
    })

    names(res__) <- groups

    res__processed <- lapply(res__, function(sublist) {
      list(
        p_start_ = process_dataset(sublist$start_),
        p_atis = process_dataset(sublist$atis)
      )
    })
    names(res__processed) <- groups


    return( list(error= '', status= 0, res = res__processed ))
  }, error = function(err){
    print(paste("acetyl_statisics  :  ",err))
    return( list(error= err, status= 1,res =NULL ))
  } )
}



#'@author Andrea Argentini
#' @title  acd4_stat_
#'
#' @description Process statistics for ACd4 low confident
#' @param d_nterm input data frame
#' @return list of dataframes.For each group , cTIS and aTIS dataframe are included.
#' @importFrom  dplyr filter pull distinct
#' @importFrom logger log_info


acd4_stat_ <- function(d_nterm) {

  log_info('acd4 Statistics ...')

  tryCatch( expr = {
    groups <- d_nterm %>% distinct(Group) %>% pull()

    res__ <- lapply(groups, function(group_name) {
      g_ <- d_nterm %>% filter(Group == group_name)
      start_ <- g_ %>% filter(grepl("Acetyl:2H\\(3\\)C13\\(1\\) \\(N-term\\)", pep_modified_seq, fixed = FALSE)) %>%
        filter (quant_valid == TRUE )  %>%
        filter(pep_res_before != "R") %>%
        filter(substr(pep_seq, 1, 1) == "M" | pep_res_before == "M") %>%
        filter(pep_start %in% c(1, 2))
      aTIS_ <- g_ %>% filter(grepl('Acetyl:2H\\(3\\)C13\\(1\\) \\(N-term\\)', pep_modified_seq, fixed = FALSE)) %>%
        filter (quant_valid == TRUE )  %>%
        filter(pep_res_before != "R") %>%
        filter(substr(pep_seq, 1, 1) == "M" | pep_res_before == "M") %>%
        filter(pep_start > 2)
      return(list( atis = aTIS_, start_= start_))
    })

    names(res__) <- groups

    res__processed <- lapply(res__, function(sublist) {
      list(
        p_start_ = process_dataset(sublist$start_),
        p_atis = process_dataset(sublist$atis)
      )
    })
    names(res__processed) <- groups


    return( list(error= '', status= 0,  res = res__processed ))
   } ,error = function(err){
      print(paste("acd4 Statistics :  ",err))
      return( list(error= err, status= 1,res =NULL ))
    })

}

#'@author Lode
#' @title  process_dataset
#'
#' @description Add futher aggregated metrics from acytilated and ACd4 statistics.
#' @param dataset input  data frame
#' @return Data frame with aggregatted information
#' @importFrom  dplyr group_by summarise distinct left_join select mutate
#' @importFrom stats  median
#'
process_dataset <- function(dataset) {
  # Step 1: Summarize Percent_Acetylation by concatenating values
  unique_filtered <- dataset %>%
    group_by(pep_modified_seq) %>%
    summarise(
      Percent_Acetylation = paste(round(as.numeric(Percent_Acetylation), 2), collapse = ", "),
      .groups = "drop"  # Drop grouping after summarization
    )

  # Step 2: Merge back the concatenated Percent_Acetylation
  merged <- dataset %>%
    distinct(pep_modified_seq, .keep_all = TRUE) %>%  # Retain unique pep_modified_seq rows
    left_join(unique_filtered, by = "pep_modified_seq") %>%  # Add concatenated Percent_Acetylation
    select(-Percent_Acetylation.x)  # Remove old Percent_Acetylation column if duplicated

  # Step 3: Calculate Median_Percentage_Acetylation from the concatenated Percent_Acetylation
  merged <- merged %>%
    mutate(
      # Split the concatenated string into individual numeric values
      Percent_Acetylation_List = strsplit(as.character(Percent_Acetylation.y), ", "),

      # Calculate the median for each group based on the Percent_Acetylation values
      Median_Percentage_Acetylation = sapply(Percent_Acetylation_List, function(x) {
        # Convert the string list to numeric and calculate median
        round(median(as.numeric(x), na.rm = TRUE), 2)
      })
    ) %>%
    select(-Percent_Acetylation_List)  # Drop the list column for cleanliness

  return(merged)
}
