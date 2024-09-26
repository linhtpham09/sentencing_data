library(tidyverse)
library(lubridate)
library(glue)
library(here)

io_combined <- read_csv(here::here("data/io_combined.csv"))

#to use in constructing the crime type variables, so that NAs are properly preserved as NAs when the indicators are created
matchwna <- function(string, list){
  case_when(
    string %in% list ~ TRUE,
    !is.na(string) ~ FALSE,
    is.na(string) ~ NA
  )
}

data <- io_combined %>% 
  remove_empty() %>% 
  mutate(
       #for FYs that use SENTDATE, converting to date data type
        sentdate = SENTDATE %>% 
          str_replace("Jan", "01") %>% 
          str_replace("Feb", "02") %>% 
          str_replace("Mar", "03") %>% 
          str_replace("Apr", "04") %>% 
          str_replace("May", "05") %>% 
          str_replace("Jun", "06") %>% 
          str_replace("Jul", "07") %>% 
          str_replace("Aug", "08") %>% 
          str_replace("Sep", "09") %>% 
          str_replace("Oct", "10") %>% 
          str_replace("Nov", "11") %>% 
          str_replace("Dec", "12") %>% 
          dmy(), 
        #for FYs that use SENTMON and SENTYR, make a variable for the floor of each month to use for splitting data into periods
         sentmonyr = ymd(glue("{SENTYR}-{SENTMON}-01")),
        #length of confinement
         logsplit = case_when(
          SENSPLT0==0.00 ~ log(0.01), 
          SENSPLT0>470 ~ log(470),
          TRUE ~ log(SENSPLT0)),
        #trumped guideline minimum
         logmin = case_when(
          GLMIN==0.00 ~ log(0.01), 
          GLMIN>470 ~ log(470),
          TRUE ~ log(GLMIN)),
        #variables for type of offense committed
         violent = matchwna(GDLINEHI, c("2A1.1", "2A1.2", "2A1.3", "2A1.4", "2A1.5", "2A2.1", "2A2.2", "2A2.3", "2A2.4", 
                                   "2A4.1", "2A4.2", "2A5.1", "2A5.2", "2A5.3", "2A6.1", "2A6.2", "2E1.3", "2E1.4", 
                                   "2E2.1", "2B3.1", "2B3.2", "2B3.3")) | str_detect(GDLINEHI, "^2K\\d{1}\\.\\d{1}$"),
         sexual2 = matchwna(GDLINEHI, c("2A3.1", "2A3.2", "2A3.3", "2A3.4", "2G1.1", "2G1.2", "2G1.3")),
         porn = matchwna(GDLINEHI, c("2G2.1", "2G2.2", "2G2.3", "2G2.4", "2G2.5")),
         drugtraff = matchwna(GDLINEHI, c("2D1.1", "2D1.2", "2D1.5", "2D1.6", "2D1.7", "2D1.8", "2D1.9", "2D1.10", "2D1.11", "2D1.12", "2D1.13")),
         whitecoll = matchwna(GDLINEHI, c("2B1.1", "2B1.6", "2B4.1", "2B5.1", "2B5.3", "2F1.1", "2F1.2", "2R1.1", "2S1.1", "2S1.2", "2S1.3", "2S1.4")) | 
           str_detect(GDLINEHI, "^2T\\d{1}\\.\\d{1}$"),
         immigration = str_detect(GDLINEHI, "^2L\\d{1}\\.\\d{1}$")) %>% 
       #time periods - note that both postbooker and postgall are include all of Dec, 2007 - maybe this is the wrong choice?
       #these are used to filter the data for each regression (not as indicators)
  mutate(postprotect = (!is.na(sentdate) & "2003-05-01"<=sentdate) | (is.na(sentdate) & sentmonyr<"2004-07-01"), 
         postbooker = "2005-01-01"<=sentmonyr & sentmonyr<="2007-12-01" & (BOOKPOST!=0 | is.na(BOOKPOST)), # excluding december makes the match worse
         postgall = "2007-12-01"<=sentmonyr & sentmonyr<="2011-09-01",
         postreport = "2011-10-01"<=sentmonyr & sentmonyr <="2016-09-01",
         present = "2017-10-01"<=sentmonyr & sentmonyr<= "2021-09-01",
        #crime type variable defined via the other categories
         othtype = !(violent|sexual2|porn|drugtraff|whitecoll|immigration), 
         custody = PRESENT==1,
        #SENTRNGE is included so we can extrapolate to the most recent data
         upward = case_when(
           SENTRNGE %in% c(1, 6) ~ TRUE,
           BOOKERCD %in% c(1:4) ~ TRUE,
           DEPART==1 ~ TRUE,
           DEPART_A==1 ~ TRUE,
           DEPART==8 ~ NA,
           DEPART_A==8 ~ NA,
           !SENTRNGE %in% c(1, 6) & !is.na(SENTRNGE) ~ FALSE,
           !BOOKERCD %in% c(1:4) & !is.na(BOOKERCD) ~ FALSE,
           DEPART!=1 & !is.na(DEPART) ~ FALSE,
           DEPART_A!=1 & !is.na(DEPART_A) ~ FALSE,
           TRUE ~ NA),
        downdep = 
          case_when( ###this is called downdep but really it's all below range 
            #sentences, not just those attributable only to departures, 
            SENTRNGE %in% c(3, 4, 5, 7, 8) ~ TRUE, 
            BOOKERCD %in% c(6:11) ~ TRUE,
            DEPART %in% c(2, 4, 6) ~ TRUE,
            DEPART_A %in% c(3, 4, 5) ~ TRUE,
            !SENTRNGE %in% c(3, 4, 5, 7, 8) & !is.na(SENTRNGE) ~ FALSE, 
            !BOOKERCD %in% c(6:11) & !is.na(BOOKERCD) ~ FALSE,
            !DEPART %in% c(2, 4, 6) & !is.na(DEPART) ~ FALSE,
            !DEPART_A %in% c(3, 4, 5) & !is.na(DEPART_A) ~ FALSE,
            DEPART==8 ~ NA,
            DEPART_A==8 ~ NA, #consider FALSE here?
            TRUE ~ NA),
         subasst = case_when(
           SENTRNGE ==2 ~ TRUE, # LP Edit here 
           BOOKERCD==5 ~ TRUE,
           DEPART %in% c(3, 5, 7, 9) ~ TRUE,
           DEPART_A==2 ~ TRUE,
           DEPART==8 ~ NA,
           DEPART_A==8 ~ NA,
           BOOKERCD !=5 ~ FALSE,
           SENTRNGE !=2 ~ FALSE,
           !DEPART %in% c(3, 5, 7, 9) & !is.na(DEPART) ~ FALSE,#add 8 to the nots for consistency
           DEPART_A!=2 & !is.na(DEPART_A) ~ FALSE,
           TRUE ~ NA),
         valve = case_when( #doesn't include expanded safety valve eligibility under the First Step Act
           SAFE %in% c(1, 2) ~ TRUE,
           SAFE == 0 ~ FALSE,
           TRUE ~ NA),
         agedummy = AGE>25,
         educ = case_when(
           EDUCATN %in% c(13:16, 23, 24, 34, 35) ~ TRUE,
           !EDUCATN %in% c(13:16, 23, 24, 34, 35) & !is.na(EDUCATN) ~ FALSE,
           TRUE ~ NA),
         citizen = NEWCIT==0,
         racesex = case_when(
           MONSEX==0 & NEWRACE==1 ~ "whitemale",
           MONSEX==1 & NEWRACE==1 ~ "whitefemale",
           MONSEX==0 & NEWRACE==2 ~ "blackmale",
           MONSEX==1 & NEWRACE==2 ~ "blackfemale",
           MONSEX==0 & NEWRACE==3 ~ "hispmale",
           MONSEX==1 & NEWRACE==3 ~ "hispfemale",
           MONSEX==0 & NEWRACE==6 ~ "othermale",
           MONSEX==1 & NEWRACE==6 ~ "otherfemale",
           is.na(MONSEX) | is.na(NEWRACE) ~ NA_character_),
        mandmin2 = case_when(
          is.na(STATMIN) ~ NA,
          STATMIN==0 ~ FALSE,
          STATMIN>0 & (SAFE==1 | SAFE==2) ~ FALSE,
          STATMIN>0 & subasst ~ FALSE,
          STATMIN>0 ~ TRUE,
          TRUE ~ NA)) 

write_csv(data, here::here("data/io_processed.csv"))




