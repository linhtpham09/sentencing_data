



#aggregate reasons function

# a little longer but doesn't require knowing the number of "REAS" columns
aggregate_reasons <- function(df){
  brace_open <- "{"
  brace_close <- "}"
  reas_cols <- df %>% 
    select(contains("REAS")) %>% 
    colnames() %>% 
    as_tibble() %>% 
    mutate(value = glue("{brace_open}{value}{brace_close}")) %>% 
    as.character()
  df %>% 
    mutate(reason = glue(reas_cols) %>% 
             str_remove_all("NA") %>% 
             str_remove_all("\\,") %>% 
             str_remove_all("\"") %>% 
             str_squish(),
           .keep = "unused") %>% 
    remove_empty()#move to beginning?
}


#aggregate CHOFF columns

aggregate_choff <- function(df){
  clean <- function(x){x %>% 
      str_remove_all("NA") %>% 
      str_remove_all("\\,") %>% 
      str_remove_all("\"") %>% 
      str_squish()}
  grabcols <- function(x){x %>% 
      colnames() %>% 
      as_tibble() %>% 
      mutate(value = glue("{brace_open}{value}{brace_close}")) %>% 
      as.character()}
  brace_open <- "{"
  brace_close <- "}"
  choff1_cols <- df %>% 
    select(contains("CHOFF1")) %>% 
    grabcols()
  choff2_cols <- df %>% 
    select(contains("CHOFF2")) %>% 
    grabcols()
  choff3_cols <- df %>% 
    select(contains("CHOFF3")) %>% 
    grabcols()
  choff4_cols <- df %>% 
    select(contains("CHOFF4")) %>% 
    grabcols()
  choff5_cols <- df %>% 
    select(contains("CHOFF5")) %>% 
    grabcols()
  choff6_cols <- df %>% 
    select(contains("CHOFF6")) %>% 
    grabcols()
  choff7_cols <- df %>% 
    select(contains("CHOFF7")) %>% 
    grabcols()
  choff8_cols <- df %>% 
    select(contains("CHOFF8")) %>% 
    grabcols()
  df %>% 
    mutate(CHOFF1 = glue(choff1_cols) %>% clean(),
           CHOFF2 = glue(choff2_cols) %>% clean(),
           CHOFF3 = glue(choff3_cols) %>% clean(),
           CHOFF4 = glue(choff4_cols) %>% clean(),
           CHOFF5 = glue(choff5_cols) %>% clean(),
           CHOFF6 = glue(choff6_cols) %>% clean(),
           CHOFF7 = glue(choff7_cols) %>% clean(),
           CHOFF8 = glue(choff8_cols) %>% clean(),
           .keep = "unused") %>% 
    remove_empty()#move to beginning?
}


#data2017_1 %>% head() %>% aggregate_reasons() %>% View()

# 
# aggregate_reasons <- function(df, num){
#   hold <- c()
#   for (i in 1:num){
#     hold[i] = paste('{REAS', as.character(i), '}', sep = '')
#   }
#   hold <- paste(hold, collapse = ' ')
#   df %>% 
#     #rename_all(str_to_upper) %>% 
#     mutate(reason = glue(hold) %>% 
#              str_remove_all("NA") %>% 
#              str_squish(),
#            .keep = "unused") %>% 
#     remove_empty()
# }

#coefficient interpretation table functions


logtrans <- function(x) {
  round((exp(as.numeric(x))-1)*100, 2)
}

loglogtrans <- function(x) {
  round((1.01^as.numeric(x) - 1)*100, 2)
}

extract_coeffs <- function(model){
  summary(model)[[4]] %>% 
    data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    clean_names() %>% 
    select(var, estimate, pr_t) %>% 
    mutate(
      coeff = case_when(
        pr_t<=0.01 ~ estimate,
        pr_t>0.01 ~ 0,
        TRUE ~ NA_real_),
      coeffinterp = ifelse(var=="logmin", loglogtrans(coeff), logtrans(coeff))) 
}

extract_coeffs_5 <- function(model){
  summary(model)[[4]] %>% 
    data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    clean_names() %>% 
    select(var, estimate, pr_t) %>% 
    mutate(
      coeff = case_when(
        pr_t<=0.05 ~ estimate,
        pr_t>0.05 ~ 0,
        TRUE ~ NA_real_),
      coeffinterp = ifelse(var=="logmin", loglogtrans(coeff), logtrans(coeff))) 
}

extract_coeffs_tbl <- function(model){
  summary(model)[[4]] %>% 
    data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    clean_names() %>% 
    select(var, estimate, pr_t) %>% 
    mutate(
      coeff = case_when(
        pr_t<=0.01 ~ estimate,
        pr_t>0.01 ~ NA_real_,
        TRUE ~ NA_real_),
      coeffinterp = ifelse(var=="logmin", loglogtrans(coeff), logtrans(coeff))) 
}
