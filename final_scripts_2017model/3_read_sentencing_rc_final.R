library(tidyverse)
library(lubridate)
library(glue)
library(here)

#read in the output from 2_io_download.R
io_raw_2002_2023 <- read_csv(here::here("data/io_2002_2023.csv"))

matchwna <- function(string, list){
  case_when(
    string %in% list ~ TRUE,
    !is.na(string) ~ FALSE,
    is.na(string) ~ NA
  )
}

data <- io_raw_2002_2023 %>%
  filter(SOURCES==1) %>% 
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
      TRUE ~ log(GLMIN))) %>% 
  #construct crime type variables
  #in the 2017 report, there are two different ways of breaking guidelines into crime categories
  mutate(PART = substr(GDLINEHI, 1, 2)) %>%
  mutate(VIOLENT = case_when(PART %in% c("2K", "2A") ~ TRUE, # ours has 4921 missings
                             GDLINEHI %in% c('2E1.3','2E1.4','2E2.1','2B3.1','2B3.2','2B3.3') ~ TRUE,
                             TRUE ~ FALSE)) %>%
  mutate(violent = case_when(GDLINEHI %in% c('2A3.1','2A3.2','2A3.3','2A3.4') ~ FALSE,
                             TRUE ~ VIOLENT)) %>% 
  
  mutate(SEXUAL = case_when(PART %in% c("2G") ~ TRUE, # ours has 4921 missings, and his has 4 more TRUEs
                            GDLINEHI %in% c('2A3.1','2A3.2','2A3.3','2A3.4') ~ TRUE,
                            TRUE ~ FALSE)) %>%
  mutate(sexual = case_when(GDLINEHI %in% c('2G3.1', '2G3.2') ~ FALSE,
                            TRUE ~ SEXUAL)) %>% 
  
  mutate(whitecoll = case_when(PART %in% c("2T", "2S") ~ TRUE, # ours has 4921 missings and Ryan's has 18 more TRUEs
                               GDLINEHI %in% c('2B1.1', 
                                               '2F1.1', 
                                               '2F1.2', 
                                               '2B1.4', 
                                               '2B1.6', 
                                               '2B4.1', 
                                               '2B5.1', 
                                               '2B5.3', 
                                               '2R1.1') ~ TRUE,
                               TRUE ~ FALSE)) %>%
  
  mutate(immigration = case_when(PART %in% c("2L") ~ TRUE,
                                 TRUE ~ FALSE)) %>%
  
  mutate(DRUGTRAFF = case_when(PART %in% c("2D") ~ TRUE, #ours has 4921 missings
                               TRUE ~ FALSE)) %>%
  mutate(drugtraff = case_when(GDLINEHI %in% c('2D2.1', 
                                               '2D2.2', 
                                               '2D2.3',
                                               '2D3.1',
                                               '2D3.2',
                                               '2D3.3',
                                               '2D3.4',
                                               '2D3.5') ~ FALSE,
                               TRUE ~ DRUGTRAFF)) %>% 
  
  mutate(drugposs =  case_when(GDLINEHI %in% c('2D2.1', '2D2.2') ~ TRUE,
                               TRUE ~ FALSE)) %>%
  
  mutate(PART1_3 = substr(GDLINEHI, 1, 3)) %>%
  
  mutate(SEXUAL2 = sexual) %>%
  mutate(sexual2 = case_when(PART1_3 %in% c("2G2") ~ FALSE,
                             TRUE ~ SEXUAL2)) %>%
  
  mutate(porn = case_when(PART1_3 %in% c("2G2") ~ TRUE,
                          TRUE ~ FALSE)) %>% 
  #note: our original script codes missing values as NA; RC's has no NAs
  
  mutate(othtype = !(whitecoll|immigration|drugtraff|sexual2|porn|violent)) %>% #should = drugposs? this is the 2012 Booker Report. in model, violent should be baseline
  ##time periods - note that both postbooker and postgall include all of Dec, 2007 - 
  #the case came down on Dec 10, but no way to reflect that in the data
  #also--not all dates are captured, but this is per the USSC's description of the boundaries of each period
  #see eg Booker Report Part E, page 1, note 2
  mutate(postprotect = (!is.na(sentdate) & "2003-05-01"<=sentdate) | (is.na(sentdate) & sentmonyr<"2004-07-01"), 
         postbooker = "2005-01-01"<=sentmonyr & sentmonyr<="2007-12-01" & (BOOKPOST!=0 | is.na(BOOKPOST)), # excluding december makes the match worse - decision on dec 10
         postgall = "2007-12-01"<=sentmonyr & sentmonyr<="2011-09-01",
         postreport = "2011-10-01"<=sentmonyr & sentmonyr <="2016-09-01",
         present = "2016-10-01"<=sentmonyr & sentmonyr<= "2023-09-01",
         #mandatory minimum
         mandmin = ifelse(STATMIN > 0, 1, 0)) %>% #this is a first step--straight from RC's script--see below
  mutate(CUSTODY = ifelse(PRESENT == 1, 1, 0)) %>% #RC's version eliminates 4761 missings
  mutate(custody = ifelse(is.na(CUSTODY), 0, CUSTODY)) %>%
  #because this script combines data from many years and the commission's approach towards the variance/departure variable has changed somewhat over the years,
  #this script is more complicated than what we have from Ryan, which only replicated years using BOOKERCD. this should do the same thing
   mutate(upward = case_when(
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
         down = case_when(
           SENTRNGE %in% c(3, 4, 5, 7, 8) ~ TRUE,
           BOOKERCD %in% c(6, 7, 8, 9, 10, 11) ~ TRUE,
           !SENTRNGE %in% c(3, 4, 5, 7, 8) & !is.na(SENTRNGE) ~ FALSE,
           !BOOKERCD %in% c(6, 7, 8, 9, 10, 11) & !is.na(BOOKERCD) ~ FALSE,
           DEPART %in% c(2, 4, 6) ~ TRUE,
           DEPART_A %in% c(3, 4, 5) ~ TRUE,
           DEPART==8 ~ NA,
           DEPART_A==8 ~ NA,
           !DEPART %in% c(2, 4, 6) & !is.na(DEPART) ~ FALSE,
           !DEPART_A %in% c(3, 4, 5) & !is.na(DEPART_A) ~ FALSE,
           TRUE ~ NA), 
         subasst = case_when(
           SENTRNGE ==2 ~ TRUE,
           BOOKERCD==5 ~ TRUE,
           DEPART %in% c(3, 5, 7, 9) ~ TRUE,
           DEPART_A==2 ~ TRUE,
           DEPART==8 ~ NA,
           DEPART_A==8 ~ NA,
           BOOKERCD !=5 ~ FALSE,
           SENTRNGE !=2 ~ FALSE, 
           !DEPART %in% c(3, 5, 7, 9) & !is.na(DEPART) ~ FALSE,
           DEPART_A!=2 & !is.na(DEPART_A) ~ FALSE,
           TRUE ~ NA)) %>% 
  mutate(VALVE = ifelse(SAFE > 0, 1, 0)) %>%
  mutate(valve = ifelse(is.na(VALVE), 0, VALVE)) %>%
  ##CML: does not capture expanded safety valve eligibility under the First Step Act, which became relevant in 2019
  ##see codebook entries for SAFE and FSASV
  mutate(agedummy = AGE>25,
         educ = case_when(
           EDUCATN %in% c(13:16, 23, 24, 34, 35) ~ TRUE,
           !EDUCATN %in% c(13:16, 23, 24, 34, 35) & !is.na(EDUCATN) ~ FALSE,
           TRUE ~ NA),
         citizen = NEWCIT==0,
         #the Commission's coding for this variable varies slightly
         #below, the race/sex variable is missing when either race or sex is missing
         #in the Commission's coding, the variables are coded as "no" is race is missing but sex isn't a match, or vice versa
         #so for us, where sex is male but race is missing, we'd code that as missing for race/sex, but the commission would code it as "No" for white female
         #either race or sex is missing for less than 1% of the total data, so this shouldn't make a big difference
         #and I think it's conceptually odd to have different amounts of missing data for different race/sex categories, when really race/sex is one variable that can have different values
         #so that's the rationale for the below
         #if you want to do it the way Ryan's script did instead, here's a sample:
         #mutate(WHITEFEMALE = ifelse(MONSEX==1 & NEWRACE==1, 1, 0)) %>%
         #note--this way also makes writing the baseline switch models a bit more tedious, since you have to switch out the baseline demographic manually
         #to give a sense of the stakes: Ryan's "WHITEFEMALE" var has ~2700 missings where our "racesex" var has ~10600 missings
         racesex = case_when(
           MONSEX==0 & NEWRACE==1 ~ "whitemale",
           MONSEX==1 & NEWRACE==1 ~ "whitefemale",
           MONSEX==0 & NEWRACE==2 ~ "blackmale",
           MONSEX==1 & NEWRACE==2 ~ "blackfemale",
           MONSEX==0 & NEWRACE==3 ~ "hispmale",
           MONSEX==1 & NEWRACE==3 ~ "hispfemale",
           MONSEX==0 & NEWRACE==6 ~ "othermale",
           MONSEX==1 & NEWRACE==6 ~ "otherfemale",
           is.na(MONSEX) | is.na(NEWRACE) ~ NA_character_
         )) %>% 
  ###maybe delete?
  mutate(crime_type = case_when( # uses the refined model typology
          drugtraff ~ "Drug Trafficking",
          whitecoll ~ "White Collar",
          othtype ~ "Other Types",
          sexual2 ~ "Sexual",
          porn ~ "Porn",
          violent ~ "Violent",
          immigration ~ "Immigration"),
        racesex_clean = case_when(
          str_detect(racesex, "whitemale") ~ "White Male",
          str_detect(racesex, "whitefemale") ~ "White Female",
          str_detect(racesex, "blackmale") ~ "Black Male",
          str_detect(racesex, "blackfemale") ~ "Black Female",
          str_detect(racesex, "hispmale") ~ "Hispanic Male",
          str_detect(racesex, "hispfemale") ~ "Hispanic Female",
          str_detect(racesex, "othermale") ~ "Other Male",
          str_detect(racesex, "otherfemale") ~ "Other Female"),
        ) %>% 
  mutate(MANDMIN2 = ifelse(valve == 1 | subasst == 1, 0, mandmin)) %>% 
  mutate(mandmin2 = ifelse(is.na(MANDMIN2), 0, MANDMIN2)) #%>% #NAs in our code but not Ryan's, + 9 extra TRUEs in ours %>%  #2012 Booker Report at 32 says BOOKERCD and DEPART used too, but model also uses subassist var?)
  #mutate(familyties = str_detect(reason, "\\b17\\b"))

write_csv(data, here::here("data/io_2002_2023.csv"))

# Sept 2024, CML:
#   I went over the script with a fine toothed comb. I compared the variable constructions to the script Ryan sent us and
# to the codebook. I checked for errors. I compared the processed variables to the raw inputs. There really shouldn't be substantial
# changes from earlier iterations. For instance, I switched the baseline used for citizen (to what I now believe is the correct baseline)
# This wouldn't affect the accuracy of the model, but it does affect the value of the coefficient on that variables because the interpretation is changed
# to reflect the new baseline.
