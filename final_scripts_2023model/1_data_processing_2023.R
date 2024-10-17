library(tidyverse)
library(here)

source(here::here("utils.R"))

data <- read.csv(here::here("data/io_2017_2021.csv")) %>% 
  filter(AGE!="AGE") %>% 
  mutate(across(c(SENSPLT0, GLMIN, BOOKERCD, SENTRNGE), as.numeric),
         logsent = log(SENSPLT0),
         probation = SENSPLT0==0, #check on NAs
         whitemale = !is.na(MONSEX) & MONSEX==0 & !is.na(NEWRACE) & NEWRACE== 1,
         blackmale = !is.na(MONSEX) & MONSEX==0 & !is.na(NEWRACE) & NEWRACE== 2,
         hispmale = !is.na(MONSEX) & MONSEX==0 & !is.na(NEWRACE) & NEWRACE== 3,
         othermale = !is.na(MONSEX) & MONSEX==0 & !is.na(NEWRACE) & NEWRACE== 6,
         whitefemale = !is.na(MONSEX) & MONSEX==1 & !is.na(NEWRACE) & NEWRACE== 1,
         blackfemale = !is.na(MONSEX) & MONSEX==1 & !is.na(NEWRACE) & NEWRACE== 2,
         hispfemale = !is.na(MONSEX) & MONSEX==1 & !is.na(NEWRACE) & NEWRACE== 3,
         otherfemale = !is.na(MONSEX) & MONSEX==1 & !is.na(NEWRACE) & NEWRACE== 6,
         racesex = case_when(
           is.na(MONSEX) | is.na(NEWRACE) ~ NA_character_,
           MONSEX==0 & NEWRACE== 1 ~ "whitemale",
           MONSEX==0 & NEWRACE== 2 ~ "blackmale",
           MONSEX==0 & NEWRACE== 3 ~ "hispmale",
           MONSEX==0 & NEWRACE== 6 ~ "othermale",
           MONSEX==1 & NEWRACE== 1 ~ "whitefemale",
           MONSEX==1 & NEWRACE== 2 ~ "blackfemale",
           MONSEX==1 & NEWRACE== 3 ~ "hispfemale",
           MONSEX==1 & NEWRACE== 6 ~ "otherfemale"),
         age = as.numeric(AGE),
         agesq = as.numeric(AGE)^2, #why?? and are they controlling for both?
         logglmin = log(GLMIN), # produces negative infinities--check on this. should 0s be converted?
         glmin = GLMIN,
         upward = case_when(
           BOOKERCD %in% c(1,2) | SENTRNGE==1 ~ TRUE,
           is.na(BOOKERCD) & is.na(SENTRNGE) ~ NA,
           TRUE ~ FALSE), #confirm that it's upward departures only, not variances? SENTRNGE==6, BOOKERCD==3,4
         subasst = case_when(
           BOOKERCD==5 | SENTRNGE==2 ~ TRUE,
           is.na(BOOKERCD) & is.na(SENTRNGE) ~ NA,
           TRUE ~ FALSE),
         mandmin = case_when(
           is.na(STATMIN) ~ NA,
           is.na(BOOKERCD) & is.na(SENTRNGE) ~ NA,
           STATMIN > 0 & (BOOKERCD!=5 | SENTRNGE!=2) & (SAFE==0 | is.na(SAFE)) ~ TRUE,
           TRUE ~ FALSE), 
         #SAFE is missing where inapplicable bc not a drug case
         custody = PRESENT==1,
         educ1 = NEWEDUC==1,
         educ3 = NEWEDUC==3,
         educ5 = NEWEDUC==5,
         educ6 = NEWEDUC==6,
         citizen = NEWCIT==0,
         earlydisp = case_when(
           BOOKERCD==6 | SENTRNGE==3 ~ TRUE,
           is.na(BOOKERCD) & is.na(SENTRNGE) ~ NA,
           TRUE ~ FALSE),
         govtdown = case_when(
           BOOKERCD==7  | SENTRNGE %in% c(4,7) ~ TRUE,
           is.na(BOOKERCD) & is.na(SENTRNGE) ~ NA,
           TRUE ~ FALSE),
         down = case_when(
           BOOKERCD %in% c(8:11) | SENTRNGE %in% c(8,5) ~ TRUE,
           is.na(BOOKERCD) & is.na(SENTRNGE) ~ NA,
           TRUE ~ FALSE),
         offtype = GDLINEHI %>% 
           str_remove("§") %>% 
           str_remove("\\d\\.\\d*"), #double check greedy vs nongreedy matching
         crimhistcat = case_when(
           !is.na(ACCCAT) ~ as.character(ACCCAT),
           is.na(ACCCAT) ~ "CHC Inapplicable or Missing"),
         weapon = WEAPON,
         finalofflev = as.numeric(XFOLSOR), #coded as numeric/continuous
         cnvnt = as.numeric(NEWCNVTN)) %>% 
         mutate(PART = substr(GDLINEHI, 1, 2)) %>%
         mutate(instviol = case_when(PART %in% c("2K", "2A") ~ TRUE, 
                                      GDLINEHI %in% c('2E1.3','2E1.4','2E2.1','2B3.1','2B3.2','2B3.3') ~ TRUE,
                                     is.na(GDLINEHI) ~ NA, #about 4-5K rows per year have GDLINEHI missing. Ryan's construction seems to convert the NAs to false
                                      TRUE ~ FALSE)) %>%
         mutate(instviol = case_when(GDLINEHI %in% c('2A3.1','2A3.2','2A3.3','2A3.4') ~ FALSE,
                                      TRUE ~ instviol)) %>% 
  
        numvecs()

vec <- c(1:6, 8:11, 14:17, 19) # list of criminal history numbers designated violence
cols_to_convert <- data %>% select(contains("CHOFF")) %>% colnames()

result <- check_intersections(data, vec, cols_to_convert)

data <- data %>%
  mutate(intersects = result$intersects,
         intersect_elements = result$intersect_elements)


write_csv(data, here::here("data/io_2023.csv"))

         

### defining crimes of violence for federal data -- list taken from booker report        
#Chapter Two Part K offenders (“Offenses Involving Public Safety”), USSG §§2A1.1-2A1.5, 2A2.1-2A2.4, 2A4.1-
#2A4.2, 2A5.1-2A5.3, 2A6.1, 2A6.2, 2E1.3, 2E1.4, 2E2.1, 2B3.1, 2B3.2, and 2B3.3.
#double check against link Prof D sent
  
## defining crimes of violence for crim hist data
# c(1:6, 8:11, 14:17, 19)
#uncertain: intimidating a witness, intimidation (12, 13), rioting (29), public order (32)

  
#statutory rape (2A3.2) is not a crime of violence per the list from old commission reports. this is consistent with Guidelines definitions
           
         
         

