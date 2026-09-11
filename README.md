# Federal Sentencing Disparities Research

Research collaboration between Charlotte Lawrence and Linh Pham, conducted for
Fiona Doherty at Yale Law School (YLS). The project replicates the U.S.
Sentencing Commission's (USSC) methodology from its
[2017](https://www.ussc.gov/research/research-reports/2017-demographic-differences-federal-sentencing)
and
[2023](https://www.ussc.gov/research/research-reports/2023-demographic-differences-federal-sentencing)
reports on demographic differences in federal sentencing, then extends that
methodology to additional race-gender group comparisons not covered in the
original reports.

The pipeline downloads USSC individual offender data files, loads them into a
Google BigQuery warehouse, and processes them in R to fit and report on
sentencing-disparity regression models.

## Repository structure

- **`final_scripts_2017model/`** — Data download, preprocessing, and analysis
  scripts replicating the USSC's 2017 report methodology, plus the resulting
  R Markdown reports.
- **`final_scripts_2023model/`** — Same pipeline structure, updated for the
  USSC's 2023 report methodology.
- **`utils.R`** — Shared helper functions (variable construction, aggregation,
  plotting helpers) sourced by the scripts in both model folders.
- **`template/`** — Word template used to format knitted R Markdown reports.
- **`archive/`** — Earlier drafts and side analyses kept for historical
  reference; superseded by the folders above. See
  [`archive/README.md`](archive/README.md) for details.

## Pipeline

Each `final_scripts_*` folder follows the same numbered sequence:

1. **`0_*data_download*.R`** — Pull raw offender data and load it into
   BigQuery.
2. **`1_*prep*.R` / `1_*processing*.R`** — Clean and reshape the raw data.
3. **`2_io_download.R`** *(2017 model only)* — Download the combined
   individual offender file.
4. **`3_read_sentencing_rc*.R`** — Read and finalize the processed data used
   for modeling.
5. **`*.Rmd`** — Fit the regression models and render the final report
   (knitted `.docx` output included alongside each `.Rmd`).

## Data

Raw and intermediate USSC data files are not included in this repository.
Source files are available from the USSC's
[Individual Offender Datafiles](https://www.ussc.gov/research/datafiles/commission-datafiles).

## Notes on BigQuery

- [Notes on BigQuery-specific SQL](https://codingisforlosers.com/learn-bigquery-sql/)
- [Finding column names of a BigQuery table (Stack Overflow)](https://stackoverflow.com/questions/11338670/bigquery-query-to-find-the-column-names-of-a-table)
