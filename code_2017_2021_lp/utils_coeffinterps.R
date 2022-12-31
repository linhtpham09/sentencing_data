

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
        pr_t>0.01 ~ NA_real_,
        TRUE ~ NA_real_),
      coeffinterp = ifelse(var=="logmin", loglogtrans(coeff), logtrans(coeff))) 
}