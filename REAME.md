🎯 Ntermreport R Package — Reports for N-terminal and LFQ Experiments Welcome to Ntermreport — your ultimate companion for comprehensive reports on DDA enriched N-terminal experiments and non-enriched LFQ proteomics experiments. 🧑‍🔬🔬

With Ntermreport, you can generate in-depth analysis reports for both N-terminal–enriched datasets and standard LFQ workflows with just one magical command. ✨

## 🛠️ Requirements Installation

1.  **Install R version \>= 4.4.0**

2.  **Install Bioconductor:** `install.packages("BiocManager")`

3.  **Install PhantomJS:** `webshot::install_phantomjs()`

4.  **Install devtools:** `install.packages("devtools")`

5.  **Install Quarto:**

    Follow the instructions on the [Quarto website](https://quarto.org/docs/download/).

6. **Install diareport** `devtools::install_github('Gevaert-Lab/diareport')`  

------------------------------------------------------------------------

## 🛠 How to Install the R Package

**For development use:**

1.  Clone the GitHub repository.

2.  Use `devtools::install()` to install the diareport package on your system.

**To test the R package:**

``` r
devtools::install_github('Gevaert-Lab/ntermreport')
```

------------------------------------------------------------------------

## 📂 Analysis Templates Available

The repository contains four Quarto templates:

-   `Template_DDA.qmd`: Template for DDA analysis report from Mascot PSM result

Specify the template name via the `template_file` parameter in the `render_nterm_report` function

------------------------------------------------------------------------

## 🚀 How to Run an Analysis

The three main functions to render HTML reports are:

-   *render_nterm_report*

    It renders an HTML report for the N-terminal–enriched DDA analysis, starting from the Mascot PSM file.

| Prameter | Description |
|------------------|------------------------------------------------------|
| **`input_file`** | Full path to the **processed proteomics results file** (e.g., PSM, peptide, or protein table) generated from the N-terminal enriched DDA experiment. |
| **`design_file`** | Full path to the **experimental design CSV file**, containing sample names, conditions, and grouping information. |
| **`folder_prj`** | Path to the folder where the **report output and intermediate results** will be saved. |
| **`description`** | Short text describing the experiment’s purpose or biological context (appears in the report header). |
| **`title`** | Main title for the report. |
| **`subtitle`** | Secondary title, often used to indicate experiment type (e.g., “N-Terminal”). |
| **`author`** | Name(s) of the report author(s), shown in the report header. |
| **`template`** | Name of the **Quarto template file** used to generate the report (e.g., `Template_DDA.qmd`). |
| **`report_folder`** | Folder where the final report will be stored (usually the same as `folder_prj`). |
| **`report_filename`** | Name of the output report file (e.g., `test.html`). |

Example:

``` r
report_target_folder  <- 'C:\\path\\to\\your\\data\\N-term\\AnalysisFolder'
template_file = "Template_DDA.qmd"
output_filename = "test.html"

params <- list(
  input_file  = 'C:\\path\\to\\your\\data\\N-term\\Nterminal_output_PSM.tsv',
  design_file = 'C:\\path\\to\\your\\data\\N-term\\exp_design_dda_2groups.csv',
  folder_prj  = report_target_folder, 
  description = 'N-terminal enrichment on retina samples',
  title       = 'CMB - XXYY',
  subtitle    = "N-Terminal",
  author      = 'Your Name'
)

# To run the analysis:
render_nterm_report(
  params,
  template       = template_file,
  report_folder  = report_target_folder,
  report_filename = output_filename
)
```

-   *render_nterm_website*

    This function renders both DDA and DIA reports in a small website created by Quarto and accepts the following parameters:

| Parameter | Description |
|------------------|------------------------------------------------------|
| **`params_dda`** | A **named list of parameters** passed to the N-terminal enriched **DDA** report rendering function. Defines input files, metadata, and analysis settings for the DDA workflow. |
| **`params_dia`** | A **named list of parameters** passed to the **DIA** report rendering function. Defines input files, metadata, and analysis settings for the DIA workflow. |
| **`template_dda`** | Full path to the **Quarto template** (`.qmd`) used to generate the N-terminal enriched DDA report. |
| **`report_folder`** | Directory where the **final rendered website/report** will be saved. Must be writable and preferably empty before rendering. |
| **`template_dia`** | Full path to the **Quarto template** (`.qmd`) used to generate the DIA report. |

Example:

``` r
# Output folder for the reports
report_folder  <- 'C:\\path\\to\\project\\Nterm_all'

# DIA parameters and template
template_dia <- "Template_DIA-NN_dev_A.qmd"

params_dia <- list(
  title        = "Project XYZ N-terminal DIA LFQ",
  subtitle     = "DE Analysis",
  author       = "Author Name",
  description  = "Retina Samples",
  input_file   = 'C:\\path\\to\\data\\nterm_lfq\\report.parquet',
  design_file  = 'C:\\path\\to\\data\\nterm_lfq\\exp_design_dia_2groups.csv',
  folder_prj   = report_folder,
  contrast     = 'Group',
  aggr_method  = 'medianPolish',
  normalization = 'center.median',
  formula      = '~ -1 + Group',
  confounder_list = '',
  PCA_comparison  = c('Group'),
  Proteotypic     = TRUE,
  pep_per_prot    = 2,
  nNonZero        = 50,
  FC_thr          = 2,
  comparisons     = c('GroupNR - GroupRPE'),
  filtPerGroup    = 'at_least_one',
  wildstr_run     = 'Project-',
  mbr             = TRUE,
  DIANN_ver2      = TRUE,
  comparison_label = c('NR - RPE'),
  filtering_contaminant = TRUE,
  contaminant_str = 'cRAP-',
  filt_NaNDE      = TRUE
)

# DDA parameters and template
template_dda <- "Template_DDA.qmd"

params_dda <- list(
  input_file  = 'C:\\path\\to\\data\\nterm_dda\\Nterminal_output_PSM.tsv',
  design_file = 'C:\\path\\to\\data\\nterm_dda\\exp_design_dda_2groups.csv',
  folder_prj  = report_folder,
  description = 'N-terminal enrichment on retina samples',
  title       = 'Project - ABCD',
  subtitle    = "N-Terminal",
  author      = 'Author Name'
)
render_nterm_website ( params_dda , params_dia , template_dda, report_folder, template_dia , 'CMB-1699 CMB-XXXX' )
```

-   *render_ntermdia_report*

    This function is a simple wrapper for the `diareport::render_dia_report` function. See the [documentation ](https://github.com/Gevaert-Lab/diareport)for more information about the parameters and how to use it.


------------------------------------------------------------------------

## 📝 Experiment Design File (EDF)

The experiment design file is a CSV that must include the following columns:

-   **Sample**: Sample name (used in all plots, should be meaningful and not too long)
-   **Run**: Raw file name *without file extension (mzML/.d/.raw)*
-   **Group**: Groups in the experiment (e.g., Cancer/control, mutation/WT)
-   **Replicate**: Label for the sample replicates

Example:

| Sample | Run | Group | Replicate |
|--------------|-------------------------------|-------------|-------------|
| B000250_ratio01_DIA | B000250_Ap_6883_EXT-765_DIA_Yeast_UPS2_ratio01_DIA | A | 1 |
| B000254_ratio02_DIA | B000254_Ap_6883_EXT-765_DIA_Yeast_UPS2_ratio02_DIA | B | 1 |
| B000258_ratio04_DIA | B000258_Ap_6883_EXT-765_DIA_Yeast_UPS2_ratio04_DIA | C | 1 |
| B000262_ratio08_DIA | B000262_Ap_6883_EXT-765_DIA_Yeast_UPS2_ratio08_DIA | D | 1 |
| B000266_ratio10_DIA | B000266_Ap_6883_EXT-765_DIA_Yeast_UPS2_ratio10_DIA | E | 1 |
