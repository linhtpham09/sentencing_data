library(tidyverse)
library(janitor)
library(glue)
period <- read.csv("data/io_2012_2016.csv")
 
table(period$racesex, useNA = 'ifany')
table(period$MONSEX, useNA = 'ifany')
table(period$NEWRACE, useNA = 'ifany')

df2017 <- read.csv("data/io_truncated/data2017.csv")

df2017$SENSPLT0 <- as.numeric(df2017$SENSPLT0)
df2017$GLMIN <- as.numeric(df2017$GLMIN)
df <- df2017  %>% 
  filter(SOURCES==1) %>% #removed 40,205 rows, 5 of which were NA 
  remove_empty() %>% 
  mutate(
    #for FYs that use SENTDATE, converting to date data type
    # sentdate = SENTDATE %>% 
    #   str_replace("Jan", "01") %>% 
    #   str_replace("Feb", "02") %>% 
    #   str_replace("Mar", "03") %>% 
    #   str_replace("Apr", "04") %>% 
    #   str_replace("May", "05") %>% 
    #   str_replace("Jun", "06") %>% 
    #   str_replace("Jul", "07") %>% 
    #   str_replace("Aug", "08") %>% 
    #   str_replace("Sep", "09") %>% 
    #   str_replace("Oct", "10") %>% 
    #   str_replace("Nov", "11") %>% 
    #   str_replace("Dec", "12") %>% 
    #   dmy(), 
    #for FYs that use SENTMON and SENTYR, make a variable for the floor of each
    #month to use for splitting data into periods
    sentmonyr = ymd(glue("{SENTYR}-{SENTMON}-01")),
    #length of confinement, confines range of sentencing 
    logsplit = case_when(
      SENSPLT0==0.00 ~ log(0.01), 
      SENSPLT0>470 ~ log(470),
      TRUE ~ log(SENSPLT0)),
    #trumped guideline minimum, confines range of guideline minimum 
    logmin = case_when(
      GLMIN==0.00 ~ log(0.01), 
      GLMIN>470 ~ log(470),
      TRUE ~ log(GLMIN))) %>% 
  #construct crime type variables
  #https://www.ussc.gov/guidelines/2023-guidelines-manual-annotated 
  #in the 2017 report, there are two different ways of breaking guidelines into crime categories
  mutate(PART = substr(GDLINEHI, 1, 2)) %>% #separates out the first part, so we can group into categories
  mutate(VIOLENT = case_when(PART %in% c("2K", "2A") ~ TRUE, # ours has 4921 missings
                             #2K - Offenses involving public safety 
                             #2A - Offenses against the person 
                             GDLINEHI %in% c('2E1.3','2E1.4','2E2.1','2B3.1','2B3.2','2B3.3') ~ TRUE,
                             TRUE ~ FALSE)) %>%
  #2E1.3 - gambling, 2E1.4 - trafficking tobacco, 2E2.1 -Extortion credit 
  #2B3.1- robbery, 2B3.2 - extortion by force, 2B3.3 - blackmail 
  mutate(violent = case_when(GDLINEHI %in% c('2A3.1','2A3.2','2A3.3','2A3.4') ~ FALSE,
                             TRUE ~ VIOLENT)) %>% 
  #2A3.1 - Criminal sexual abuse, 2A3.2 - Criminal sexual abuse of a minor 
  #2A3.3 - Criminal sexual abuse of a ward, 2A3.4 - Abusive sexual contact
  mutate(SEXUAL = case_when(PART %in% c("2G") ~ TRUE, # ours has 4921 missings, and his has 4 more TRUEs
                            GDLINEHI %in% c('2A3.1','2A3.2','2A3.3','2A3.4') ~ TRUE,
                            TRUE ~ FALSE)) %>%
  #2G - Offenses involving commercial sex acts, sexual explotation of minors
  # and obscenity 
  mutate(sexual = case_when(GDLINEHI %in% c('2G3.1', '2G3.2') ~ FALSE,
                            TRUE ~ SEXUAL)) %>% 
  #2G3.1 - transporting obscene matter 
  #2G3.2 - obscene phone comm for commercial purpose 
  mutate(whitecoll = case_when(PART %in% c("2T", "2S") ~ TRUE, # ours has 4921 missings and Ryan's has 18 more TRUEs
                               #2S - money laundering and monetary transaction reporting
                               #2T- offenses involving taxation 
                               GDLINEHI %in% c('2B1.1', #larceny, embezzlement and other forms of theft
                                               '2F1.1', #deleted
                                               '2F1.2', #deleted
                                               '2B1.4', #insider trading
                                               '2B1.6', #aggravated identity thet
                                               '2B4.1', #bribery in procurement of bank loan
                                               '2B5.1', #offenses involving counterfeit bearer obligations
                                               '2B5.3', #criminal infringement of copyright or trademark
                                               '2R1.1') ~ TRUE,#bid ridding, price fixing, market allocation
                               TRUE ~ FALSE)) %>%
  
  mutate(immigration = case_when(PART %in% c("2L") ~ TRUE, #offenses involving immigration, naturalization
                                 TRUE ~ FALSE)) %>%
  
  mutate(DRUGTRAFF = case_when(PART %in% c("2D") ~ TRUE, #ours has 4921 missings
                               TRUE ~ FALSE)) %>% #2D - offenses involving drugs and narco terrorism 
  mutate(drugtraff = case_when(GDLINEHI %in% c('2D2.1', #unlawful possession
                                               '2D2.2', #acquiring controlled substance by fraud
                                               '2D2.3', #operating operation of common carrier
                                               '2D3.1', #regulatory offenses involving registration numbers
                                               '2D3.2', # regulatory offenses involving controlled substances
                                               '2D3.3', #deleted
                                               '2D3.4',#deleted
                                               '2D3.5') ~ FALSE, #deleted
                               TRUE ~ DRUGTRAFF)) %>% 
  
  mutate(drugposs =  case_when(GDLINEHI %in% c('2D2.1', '2D2.2') ~ TRUE,
                               TRUE ~ FALSE)) %>%
  
  mutate(PART1_3 = substr(GDLINEHI, 1, 3)) %>% #selects substring of GDLINEHI code (ex.2D2.1 -> 2D2)
  
  mutate(SEXUAL2 = sexual) %>%
  mutate(sexual2 = case_when(PART1_3 %in% c("2G2") ~ FALSE,#sexual exploitation of a minor
                             TRUE ~ SEXUAL2)) %>%
  
  mutate(porn = case_when(PART1_3 %in% c("2G2") ~ TRUE,
                          TRUE ~ FALSE)) %>% 
  #note: our original script codes missing values as NA; RC's has no NAs
  
  mutate(othtype = !(whitecoll|immigration|drugtraff|sexual2|porn|violent)) %>% #should = drugposs? this is the 2012 Booker Report. in model, violent should be baseline
#mandatory minimum
mutate(mandmin = ifelse(STATMIN > 0, 1, 0)) %>% #this is a first step--straight from RC's script--see below
  mutate(CUSTODY = ifelse(PRESENT == 1, 1, 0)) %>% #RC's version eliminates 4761 missings
  mutate(custody = ifelse(is.na(CUSTODY), 0, CUSTODY)) %>%
 mutate(upward = case_when(
    BOOKERCD %in% c(1:4) ~ TRUE,

    !BOOKERCD %in% c(1:4) & !is.na(BOOKERCD) ~ FALSE, #everything is false 
    TRUE ~ NA),
    down = case_when(
      BOOKERCD %in% c(6, 7, 8, 9, 10, 11) ~ TRUE,

      !BOOKERCD %in% c(6, 7, 8, 9, 10, 11) & !is.na(BOOKERCD) ~ FALSE,
      TRUE ~ NA), 
    subasst = case_when(
      BOOKERCD==5 ~ TRUE,#5-5K1.1/substantial assistance 
      BOOKERCD !=5 ~ FALSE,
      TRUE ~ NA)) %>% 
  mutate(VALVE = ifelse(SAFE > 0, 1, 0)) %>% 
  mutate(valve = ifelse(is.na(VALVE), 0, VALVE)) %>%
  mutate(agedummy = AGE>25,
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
           is.na(MONSEX) | is.na(NEWRACE) ~ NA_character_
         )) %>% 
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
  mutate(mandmin2 = ifelse(is.na(MANDMIN2), 0, MANDMIN2))

#df2017 had 66874
#final result is 61917 obs with 61 variables 
table(df$racesex, useNA = 'ifany')
table(df$NEWRACE, useNA = 'ifany')
table(df$MONSEX, useNA = 'ifany')
