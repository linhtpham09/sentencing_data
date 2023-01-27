



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
