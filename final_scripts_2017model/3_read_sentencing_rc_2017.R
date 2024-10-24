library(tidyverse)
library(lubridate)
library(glue)
library(here)

#read in the output from 2_io_download.R
io_raw_2012_2016 <- read_csv(here::here("data/io_raw_2012_2016.csv"))

matchwna <- function(string, list){
  case_when(
    string %in% list ~ TRUE,
    !is.na(string) ~ FALSE,
    is.na(string) ~ NA
  )
}

#the primary differences between 2012-2016  
#and other periods are the date columns
#i.e instead of SENTDATE there is a 
#SENTMON and SENTYR
#and SENTRNGE (only available post 2018)-
#for this time period, we use BOOKERCD

data <- io_raw_2012_2016 %>%
  #sources- information represents known court findings 
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
  ##time periods - note that both postbooker and postgall include all of Dec, 2007 - 
  #the case came down on Dec 10, but no way to reflect that in the data
  #also--not all dates are captured, but this is per the USSC's description of the boundaries of each period
  #see eg Booker Report Part E, page 1, note 2
  
  #--LP commented out Oct 17 2024 bc this is one period---- 
  
  # mutate(postprotect = (!is.na(sentdate) & "2003-05-01"<=sentdate) | (is.na(sentdate) & sentmonyr<"2004-07-01"), 
  #        postbooker = "2005-01-01"<=sentmonyr & sentmonyr<="2007-12-01" & (BOOKPOST!=0 | is.na(BOOKPOST)), # excluding december makes the match worse - decision on dec 10
  #        postgall = "2007-12-01"<=sentmonyr & sentmonyr<="2011-09-01",
  #        postreport = "2011-10-01"<=sentmonyr & sentmonyr <="2016-09-01",
  #        present = "2016-10-01"<=sentmonyr & sentmonyr<= "2023-09-01",
  #--------------------------


         #mandatory minimum
         mutate(mandmin = ifelse(STATMIN > 0, 1, 0)) %>% #this is a first step--straight from RC's script--see below
  mutate(CUSTODY = ifelse(PRESENT == 1, 1, 0)) %>% #RC's version eliminates 4761 missings
  mutate(custody = ifelse(is.na(CUSTODY), 0, CUSTODY)) %>%
  #because this script combines data from many years and the commission's approach towards the variance/departure variable has changed somewhat over the years,
  #this script is more complicated than what we have from Ryan, which only replicated years using BOOKERCD. this should do the same thing
   mutate(upward = case_when(
     #BOOKERCD - Assigns cases to one of the 12 postBooker reporting categories
     #based on relationship between the sentence and guideline range and the 
     #reason(s) given for being outside of the range.
           BOOKERCD %in% c(1:4) ~ TRUE,
           #1-upward departure
           #2-upward departure w/Booker
           #3-above range w/Booker
           #4-Remaining above range 
           !BOOKERCD %in% c(1:4) & !is.na(BOOKERCD) ~ FALSE, #everything is false 
           TRUE ~ NA),
         down = case_when(
           BOOKERCD %in% c(6, 7, 8, 9, 10, 11) ~ TRUE,
           #6-early disposition 5K3.1 
           #7 - govt sponsored - below range
           #8 - downward departure 
           #9- downward departure w/booker 
           #10- below range w/Booker 
           #11- remaining below range 
           !BOOKERCD %in% c(6, 7, 8, 9, 10, 11) & !is.na(BOOKERCD) ~ FALSE,
           TRUE ~ NA), 
         subasst = case_when(
           BOOKERCD==5 ~ TRUE,#5-5K1.1/substantial assistance 
           BOOKERCD !=5 ~ FALSE,
           TRUE ~ NA)) %>% 
  mutate(VALVE = ifelse(SAFE > 0, 1, 0)) %>% #SAFE - indicator of safety valve application
                                            #under both 2D1.1 and 5C1.2 
  mutate(valve = ifelse(is.na(VALVE), 0, VALVE)) %>%
  ##CML: does not capture expanded safety valve eligibility under the First Step Act, which became relevant in 2019
  ##see codebook entries for SAFE and FSASV
  mutate(agedummy = AGE>25,
         educ = case_when(
           EDUCATN %in% c(13:16, 23, 24, 34, 35) ~ TRUE, 
           #13-One year of college 
           #14-two years of college 
           #15-three years of college 
           #16- college grad
           #23-associates degree 
           #24 - graduate degree 
           #34 - some college 
           #35- some grad school 
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

write_csv(data, here::here("data/io_2012_2016.csv"))

# Sept 2024, CML:
#   I went over the script with a fine toothed comb. I compared the variable constructions to the script Ryan sent us and
# to the codebook. I checked for errors. I compared the processed variables to the raw inputs. There really shouldn't be substantial
# changes from earlier iterations. For instance, I switched the baseline used for citizen (to what I now believe is the correct baseline)
# This wouldn't affect the accuracy of the model, but it does affect the value of the coefficient on that variables because the interpretation is changed
# to reflect the new baseline.
