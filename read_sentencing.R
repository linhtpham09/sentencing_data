library(tidyverse)
library(lubridate)
library(glue)
library(here)
library(asciiSetupReader)

#works but is too slow
# fy21 <- read_ascii_setup(data = here::here("opafy21nid/opafy21nid.dat"), setup_file = here::here("opafy21nid/opafy21nid.sps"))
# fy02 <- read_ascii_setup(data = here::here("opafy02nid/opafy02nid.dat"), setup_file = here::here("opafy02nid/opafy02nid.sps"))

#non-comprehensive
#justfair <- read_csv(here::here("FinalDataset.csv"), guess_max = 50000)

#too big to do all at once
# read_io_data <- function(year){
#   read_csv(here::here(glue("data/individual_offenders/opafy{year}nid.csv")), guess_max = 50000)
# }
# yearlist <- c("02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21")
# io_list_raw <- map(yearlist, read_io_data)

grp1 <- c("02", "03")
grp3 <- c("05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17")
grp5 <- c("19", "20", "21")

read_io_data_1 <- function(year){
  read_csv(here::here(glue("data/individual_offenders/opafy{year}nid.csv")), guess_max = 50000) %>% 
    select(SENTDATE, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, DEPART, SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT)
}

fy02_raw <- read_csv(here::here("data/individual_offenders/opafy02nid.csv"), guess_max = 50000) %>% 
  select(SENTDATE, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, DEPART, 
         SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT) %>% 
  rename_all(str_to_upper)

fy03_raw <- read_csv(here::here("data/individual_offenders/opafy03nid.csv"), guess_max = 50000) %>% 
  select("sentdate", "sensplt0", "glmin", "gdlinehi", "totchpts", "is924c", "weapsoc", "statmin", "caroffap", "accap", 
         "depart", "safe", "newcnvtn", "present", "mitrolhi", "aggrolhi", "newrace", "monsex", "age", "educatn", 
         "newcit") %>% 
  rename_all(str_to_upper)

fy04_raw <- read_csv(here::here("data/individual_offenders/opafy04nid.csv"), guess_max = 50000) %>% 
  select("SENTMON", "SENTYR", "sensplt0", "glmin", "gdlinehi", "totchpts", "is924c", "weapsoc", "statmin", "caroffap", "accap", 
         "DEPART_A",  "safe", "newcnvtn", "present", "mitrolhi", "aggrolhi", "newrace", "monsex", "age", "educatn", 
         "newcit") %>% 
  rename_all(str_to_upper)

fy05_raw <- read_csv(here::here("data/individual_offenders/opafy05nid.csv"), guess_max = 50000) %>% 
  select(SENTMON, SENTYR, sensplt0, glmin, gdlinehi, totchpts, is924c, weapsoc, statmin, caroffap, accap, DEPART_A, BookerCD, 
         safe, newcnvtn, present, mitrolhi, aggrolhi, newrace, monsex, age, educatn, newcit, BOOKPOST) %>% 
  rename_all(str_to_upper)

fy06_raw <- read_csv(here::here("data/individual_offenders/opafy06nid.csv"), guess_max = 50000) %>% 
  select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD, 
         SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT) #left DEPART_A out by accident, starting here

fy07_raw <- read_csv(here::here("data/individual_offenders/opafy07nid.csv"), guess_max = 50000) %>% 
  select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD, 
         SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT)

fy08_raw <- read_csv(here::here("data/individual_offenders/opafy08nid.csv"), guess_max = 50000) %>% 
  select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD, 
         SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT)

fy09_raw <- read_csv(here::here("data/individual_offenders/opafy09nid.csv"), guess_max = 50000) %>% 
  select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD, 
         SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT)

fy10_raw <- read_csv(here::here("data/opafy10nid_downselected.csv")) %>% 
  rename_all(str_to_upper)

fy11_raw <- read_csv(here::here("data/individual_offenders/opafy11nid.csv"), guess_max = 50000) %>% 
  select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD, 
         SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT)

fy12_raw <- read_csv(here::here("data/opafy12nid_downselected.csv")) %>% 
  rename_all(str_to_upper)

fy13_raw <- read_csv(here::here("data/opafy13nid_downselected.csv")) %>% 
  rename_all(str_to_upper)

fy14_raw <- read_csv(here::here("data/opafy14nid_downselected.csv")) %>% 
  rename_all(str_to_upper)

fy15_raw <- read_csv(here::here("data/opafy15nid_downselected.csv")) %>% 
  rename_all(str_to_upper)

fy16_raw <- read_csv(here::here("data/opafy16nid_downselected.csv")) %>% 
  rename_all(str_to_upper)

fy17_raw <- read_csv(here::here("data/opafy17nid_downselected.csv")) %>% 
  rename_all(str_to_upper)

fy20_raw <- read_csv(here::here("data/individual_offenders/opafy20nid.csv"), guess_max = 50000) %>% 
  select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, 
         ACCAP, SENTRNGE, FSASV, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, 
         AGE, EDUCATN, NEWCIT)


#add bookpost into "This field is only available FY2005 (post Booker Supreme Court Decision)-FY2017."

io <- bind_rows(fy02_raw,
                fy03_raw,
                fy04_raw,
                fy05_raw,
                fy06_raw,
                fy07_raw,
                fy08_raw,
                fy09_raw,
                fy10_raw,
                fy11_raw,
                fy12_raw,
                fy13_raw,
                fy14_raw,
                fy15_raw,
                fy16_raw,
                fy17_raw,
                fy20_raw)

write_csv(io, here::here("data/io_raw.csv"))

matchwna <- function(string, list){
  case_when(
    string %in% list ~ TRUE,
    !is.na(string) ~ FALSE,
    is.na(string) ~ NA
  )
}


data <- io %>% 
  mutate(sentdate = SENTDATE %>% 
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
         sentmonyr = ymd(glue("{SENTYR}-{SENTMON}-01")),
         logsplit = case_when(
          SENSPLT0==0.00 ~ log(0.01), 
          SENSPLT0>470 ~ log(470),
          TRUE ~ log(SENSPLT0)),
         logmin = case_when(
          GLMIN==0.00 ~ log(0.01), 
          GLMIN>470 ~ log(470),
          TRUE ~ log(GLMIN)),
         violent = matchwna(GDLINEHI, c("2A1.1", "2A1.2", "2A1.3", "2A1.4", "2A1.5", "2A2.1", "2A2.2", "2A2.3", "2A2.4", 
                                   "2A4.1", "2A4.2", "2A5.1", "2A5.2", "2A5.3", "2A6.1", "2A6.2", "2E1.3", "2E1.4", 
                                   "2E2.1", "2B3.1", "2B3.2", "2B3.3")) | str_detect(GDLINEHI, "^2K\\d{1}\\.\\d{1}$"),
         sexual = matchwna(GDLINEHI, c("2A3.1", "2A3.2", "2A3.3", "2A3.4", "2G1.1", "2G1.2", "2G1.3", "2G2.1", "2G2.2", "2G2.3", "2G2.4", "2G2.5")),
         sexual2 = matchwna(GDLINEHI, c("2A3.1", "2A3.2", "2A3.3", "2A3.4", "2G1.1", "2G1.2", "2G1.3")),
         porn = matchwna(GDLINEHI, c("2G2.1", "2G2.2", "2G2.3", "2G2.4", "2G2.5")),
         drugtraff = matchwna(GDLINEHI, c("2D1.1", "2D1.2", "2D1.5", "2D1.6", "2D1.7", "2D1.8", "2D1.9", "2D1.10", "2D1.11", "2D1.12", "2D1.13")),
         othdrug = matchwna(GDLINEHI, c("2D2.1", "2D2.2")),
         whitecoll = matchwna(GDLINEHI, c("2B1.1", "2B1.6", "2B4.1", "2B5.1", "2B5.3", "2F1.1", "2F1.2", "2R1.1", "2S1.1", "2S1.2", "2S1.3", "2S1.4")) | 
           str_detect(GDLINEHI, "^2T\\d{1}\\.\\d{1}$"),
         immigration = str_detect(GDLINEHI, "^2L\\d{1}\\.\\d{1}$")) %>% 
  mutate(drug = drugtraff | othdrug,
         othtype = !(drugtraff|othdrug|violent|sexual|whitecoll|immigration), 
         othtype2 = !(violent|sexual2|porn|drugtraff|whitecoll|immigration), #incl othdrug, model 2 excludes violent
         mandmin = STATMIN>0,
         custody = PRESENT==1,
         upward = case_when(
           BOOKERCD %in% c(1:4) ~ TRUE,
           DEPART==1 ~ TRUE,
           DEPART_A==1 ~ TRUE,
           DEPART==8 ~ NA,
           DEPART_A==8 ~ NA,
           !BOOKERCD %in% c(1:4) & !is.na(BOOKERCD) ~ FALSE, #add 8 to the nots for consistency
           DEPART!=1 & !is.na(DEPART) ~ FALSE,
           DEPART_A!=1 & !is.na(DEPART_A) ~ FALSE,
           TRUE ~ NA),
         downgovt = case_when( #inconsistent with the coding of 8 as NA
           BOOKERCD %in% c(6, 7) ~ TRUE,
           !BOOKERCD %in% c(6, 7) & !is.na(BOOKERCD) ~ FALSE,
           TRUE ~ NA),
         downcourt = case_when(
           BOOKERCD %in% c(8:11) ~ TRUE,
           !BOOKERCD %in% c(8:11) & !is.na(BOOKERCD) ~ FALSE,
           TRUE ~ NA),
         subasst = case_when(
           BOOKERCD==5 ~ TRUE,
           DEPART %in% c(3, 5, 7, 9) ~ TRUE,
           DEPART_A==2 ~ TRUE,
           DEPART==8 ~ NA,
           DEPART_A==8 ~ NA,
           BOOKERCD !=5 ~ FALSE,
           !DEPART %in% c(3, 5, 7, 9) & !is.na(DEPART) ~ FALSE,#add 8 to the nots for consistency
           DEPART_A!=2 & !is.na(DEPART_A) ~ FALSE,
           TRUE ~ NA),
         valve = case_when(
           SAFE %in% c(1, 2) ~ TRUE,
           SAFE == 0 ~ FALSE,
           TRUE ~ NA),
         mitigate = MITROLHI==0,
         aggravate = AGGROLHI==0,
         agedummy = AGE>25,
         educ = case_when(
           EDUCATN %in% c(13:16, 23, 24, 34, 35) ~ TRUE,
           !EDUCATN %in% c(13:16, 23, 24, 34, 35) & !is.na(EDUCATN) ~ FALSE,
           TRUE ~ NA),
         citizen = NEWCIT==0,
         whitefemale = MONSEX==1 & NEWRACE==1,
         blackmale = MONSEX==0 & NEWRACE==2,
         blackfemale = MONSEX==1 & NEWRACE==2,
         hispmale = MONSEX==0 & NEWRACE==3,
         hispfemale = MONSEX==1 & NEWRACE==3,
         othermale = MONSEX==0 & NEWRACE==6,
         otherfemale = MONSEX==1 & NEWRACE==6,
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
         )) %>%  #NAs are messed up
  mutate(downdep = case_when(
          DEPART %in% c(2, 4, 6) ~ TRUE,
          DEPART_A %in% c(3, 4, 5) ~ TRUE,
          downgovt ~ TRUE,
          downcourt ~ TRUE,
          DEPART==8 ~ NA,
          DEPART_A==8 ~ NA,
          !DEPART %in% c(2, 4, 6) & !is.na(DEPART) ~ FALSE,
          !DEPART_A %in% c(3, 4, 5) & !is.na(DEPART_A) ~ FALSE,
          !downgovt & !downcourt ~ FALSE,
          TRUE ~ NA),
        mandmin2 = case_when(
          (SAFE==1 | SAFE==2) ~ FALSE,
          STATMIN>0 ~ TRUE,
          STATMIN==0 ~ FALSE,
          TRUE ~ NA)) #2012 Booker Report at 32 says BOOKERCD and DEPART used too, but model also uses subassist var?)

write_csv(data, here::here("data/io.csv"))



##?
postprotect %>% filter(MONSEX==1 & NEWRACE==1) %>% pull(SENSPLT0) %>% mean(na.rm=T)
postprotect %>% filter(MONSEX==0 & NEWRACE==1) %>% mutate(SENSPLT0 = ifelse(is.nan(SENSPLT0), NA, SENSPLT0)) %>% pull(SENSPLT0) %>% mean(na.rm=T)






#justfair has TOTCHPTS instead of SORCHPT for total criminal history points -- check on the difference
#also missing DEPART
#ols


#
model1 <- lm(logsplit ~ logmin + 
               sexual + #sexual2 used pg 33 of 2012 Booker Report
               drugtraff +
               whitecoll +
               immigration +
               othtype +
               TOTCHPTS +
               IS924C +
               WEAPSOC +
               valve + 
               CAROFFAP +
               ACCAP +
               upward +
               downgovt +
               downcourt +
               subasst +
               mandmin +
               NEWCNVTN +
               mitigate +
               aggravate +
               factor(NEWRACE) +
               MONSEX + 
               agedummy +
               educ +
               NEWCIT,
             data)



model2_protect <- lm(logsplit ~ logmin + 
                       drugtraff +
                       sexual2 + 
                       porn +
                       immigration +
                       othtype2 +
                       whitecoll +
                       upward +
                       downdep +
                       subasst +
                       mandmin2 +
                       NEWCNVTN +
                       custody +
                       whitefemale +
                       blackmale +
                       blackfemale +
                       hispmale +
                       hispfemale +
                       othermale +
                       otherfemale +
                       agedummy +
                       educ +
                       NEWCIT,
                     postprotect)
