library(tidyverse)
library(here)

df <- read.csv("data/io_2017_2021.csv")

#This is kind of extreme but I thought I'd try a more cautious version 
#of the log function 
naLog <- function(x){
  na_index <- which(is.na(x))
  non_na_val <- x[!is.na(x)]
  log_val <- log(non_na_val)
  result <- numeric(length(x))
  result[na_index] <- NA 
  result[!is.na(x)] <- log_val 
  return(result)
}

aggregate_columns <- function(df, column_name){
  brace_open <- "{"
  brace_close <- "}"
  cols <- df %>% 
    select(contains(column_name)) %>% 
    colnames() %>% 
    as_tibble() %>% 
    mutate(value = glue("{brace_open}{value}{brace_close}")) %>% 
    as.character() 
  df %>% 
    mutate(temp_name = glue(cols) %>% 
             str_remove_all("NA") %>% 
             str_remove_all("\\,") %>% 
             str_remove_all("\"") %>% 
             str_squish(),
           .keep = "unused") %>% 
    remove_empty() %>% 
  #move to beginning? %>% 
  #!! is the unquote operator
  #:= operator used for assignment within rename 
  rename(!!column_name := temp_name) %>% 
  mutate(USSCIDN = as.numeric(USSCIDN))
 
}



#----start-- 
temp <- 
  df %>% 
  mutate(USSCIDN= as.numeric(USSCIDN)) %>% 
#-----log length of incarceration---- 
#"SENSPLT0" and na in some rows
  mutate (
    numSENSPLT0 = as.numeric(SENSPLT0)
    ) %>% 
  mutate(
  logIncarceration = naLog(case_when(
    numSENSPLT0 > 470 ~ 470, 
    TRUE ~ numSENSPLT0 
    ))
  ) %>%
#-----probation only sentencing----
#sentimp = 4
  mutate(
    probationOnly = case_when(SENTIMP ==4 ~ 1)
  ) %>%
#-----race and gender--- -
  mutate(
    raceAndGender = case_when(
      NEWRACE == 1 & MONSEX == 0 ~ 'whiteMale',
      NEWRACE == 1 & MONSEX == 1 ~ 'whiteFemale',
      NEWRACE == 2 & MONSEX == 0 ~ 'blackMale',
      NEWRACE == 2 & MONSEX == 1 ~ 'blackFemale',
      NEWRACE == 3 & MONSEX == 0 ~ 'hispanicMale',
      NEWRACE == 3 & MONSEX == 1 ~ 'hispanicFemale',
      NEWRACE == 6 & MONSEX == 0 ~ 'otherMale',
      NEWRACE == 6 & MONSEX == 1 ~ 'otherFemale'
    )
  ) %>%
#----age---
  mutate(
    age = AGE # age at the time of sentencing
  ) %>%
#---presumptive sentence (log)--
  mutate(
    logPresumptiveSentence = naLog(as.numeric(GLMIN))
  ) %>% 
#---presumptive sentence -- 
  mutate( # taking as.numeric bc there are 5 rows with "GLMIN" 
    presumptiveSentence = as.numeric(GLMIN)
  ) %>% 
#---upward departure-- 
#for FY2004-FY2017 see DEPART_A or BOOKERCD 
#for FY2018-Present see SENTRNGE 
  mutate(
    upwardDeparture = case_when(
      SENTYR == 2017 & BOOKERCD == 1 ~ TRUE,
      SENTYR == 2017 & BOOKERCD == 2 ~ TRUE,
      SENTYR == 2017 & SENTRNGE == 1 ~ TRUE, 
      SENTYR == 2018 & SENTRNGE == 1 ~ TRUE, 
      SENTYR == 2019 & SENTRNGE == 1 ~ TRUE, 
      SENTYR == 2020 & SENTRNGE == 1 ~ TRUE, 
      SENTYR == 2021 & SENTRNGE == 1 ~ TRUE, 
      TRUE ~ FALSE
      )
  ) %>% 
#----substantial assistance---
#for FY2005-FY2017 see  BOOKERCD 
#for FY2018-Present see SENTRNGE   
  mutate(
    substantialAssist = case_when(
      SENTYR == 2017 & BOOKERCD == 5 ~ TRUE,
      SENTYR == 2017 & SENTRNGE == 2 ~ TRUE,
      SENTYR == 2018 & SENTRNGE == 2 ~ TRUE, 
      SENTYR == 2019 & SENTRNGE == 2 ~ TRUE, 
      SENTYR == 2020 & SENTRNGE == 2 ~ TRUE, 
      SENTYR == 2021 & SENTRNGE == 2 ~ TRUE, 
      TRUE ~ FALSE
    )
  ) %>% 
#---mandatory minimum penalty---- 
  mutate(
    mandMinPen = STATMIN
  ) %>% 
#---in custody--- 
  mutate(
    inCustody = case_when(PRESENT == 1 ~ TRUE, 
                               TRUE ~ FALSE)) %>% 
#----education-- 
  mutate(education = NEWEDUC) %>% 

#----citizenship-- 
  mutate(citizenship = NEWCIT) %>%  

#--- early disposition program-- 
  mutate(
    EDP = case_when(
      SENTYR == 2017 & BOOKERCD == 6 ~ TRUE,
      SENTYR == 2017 & SENTRNGE == 3 ~ TRUE, 
      SENTYR == 2018 & SENTRNGE == 3 ~ TRUE, 
      SENTYR == 2019 & SENTRNGE == 3 ~ TRUE, 
      SENTYR == 2020 & SENTRNGE == 3 ~ TRUE, 
      SENTYR == 2021 & SENTRNGE == 3 ~ TRUE, 
      TRUE ~ FALSE
    )
  )  %>% 
#---govt departure or variance--- 
  mutate(
    govtDepartVar = case_when(
      SENTYR == 2017 & BOOKERCD == 7 ~ TRUE,
      SENTYR == 2017 & SENTRNGE %in% c(4, 7) ~ TRUE, 
      SENTYR == 2018 & SENTRNGE %in% c(4, 7) ~ TRUE, 
      SENTYR == 2019 & SENTRNGE %in% c(4, 7) ~ TRUE,
      SENTYR == 2020 & SENTRNGE %in% c(4, 7) ~ TRUE,
      SENTYR == 2021 & SENTRNGE %in% c(4, 7) ~ TRUE
    )
  ) %>%  

#---non govt departure or variance --- 
 mutate( 
   nonGovtDepartVar = case_when(
     # downward depart, down depart w/ Booker, below w/Booker
     SENTYR == 2017 & BOOKERCD %in% c(8,9,10) ~ TRUE,
     # did downward departure and below range variance
     SENTYR == 2017 & SENTRNGE %in% c(5, 8) ~ TRUE, 
     SENTYR == 2018 & SENTRNGE %in% c(5, 8) ~ TRUE, 
     SENTYR == 2019 & SENTRNGE %in% c(5, 8) ~ TRUE,
     SENTYR == 2020 & SENTRNGE %in% c(5, 8) ~ TRUE,
     SENTYR == 2021 & SENTRNGE %in% c(5, 8) ~ TRUE
   )
 ) %>% 
#-----offense type--- 

  mutate(offType = substr(GDLINEHI, 1, 2)) %>%  
  mutate(offType = case_when( 
    offType %in% c("2B", '2C','2D','2E','2G','2K','2L','2N',
                   '2P','2Q','2R','2S','2T','2A') ~ offType,
    TRUE ~ "Other"
  )) %>% 

#--- Criminal history category-- 
  mutate(
    CHC = case_when(
    ACCCAT == 1 ~ "CHC I", 
    ACCCAT == 2 ~ 'CHC II', 
    ACCCAT == 3 ~ 'CHC III', 
    ACCCAT == 4 ~ 'CHC IV', 
    ACCCAT == 5 ~ 'CHC V', 
    ACCCAT == 6 ~ 'CHC VI', 
  )) %>%  

#--- Weapon -- 
  mutate(weapon = WEAPON) %>% 

#--- final offense level 
  mutate(finalOffenseLvl = XFOLSOR) %>%  

#--- Conviction type 
  mutate(convictionType = NEWCNVTN)

#build prior violence

choff17 <- read.csv("data/choff/data2017_choff.csv")
choff18 <- read.csv("data/choff/data2018_choff.csv")
choff19 <- read.csv("data/choff/data2019_choff.csv")
choff20 <- read.csv("data/choff/data2020_choff.csv")
choff21 <-read.csv("data/choff/data2021_choff.csv")
#merge choff columns together 


# a little longer but doesn't require knowing the number of "REAS" columns
aggregate <- function(df){
  brace_open <- "{"
  brace_close <- "}"
  reas_cols <- df %>% 
    select(contains("CHOFF")) %>% 
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

brace_open <- "{"
brace_close <- "}"
reas_cols <- choff17 %>% 
  select(contains("CHOFF")) %>% 
  colnames() %>% 
  as_tibble() %>% 
  mutate(value = glue("{brace_open}{value}{brace_close}")) %>% 
  as.character()


test <- 
  choff17 %>%  
  select(contains("CHOFF")) %>% 
  rowwise %>% 
  mutate(CHOFF = list(c_across(starts_with("CHOFF")))) 

#%>% 
  #ungroup() 

#instant violence 
#to determine instant violence, we will have to use GDLINEHI

#if GDLINEHI == xx than instant violence variable = 1 


    