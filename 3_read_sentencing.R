library(tidyverse)
library(lubridate)
library(glue)
library(here)
library(asciiSetupReader)
library(janitor)

io <- read_csv(here::here("data/io_2002_2021.csv"))
#add district
circdist <- read_csv(here::here("data/circdist.csv"))
io_combined <- left_join(io_combined, circdist, by = c('CIRCDIST' ='districtNum'))

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
#note to include a new time period for this data when we integrate with the main data file %>% 
  mutate(postprotect = (!is.na(sentdate) & "2003-05-01"<=sentdate) | (is.na(sentdate) & sentmonyr<"2004-07-01"), 
         postbooker = "2005-01-01"<=sentmonyr & sentmonyr<="2007-12-01" & (BOOKPOST!=0 | is.na(BOOKPOST)), # excluding december makes the match worse
         postgall = "2007-12-01"<=sentmonyr & sentmonyr<="2011-09-01",
         postreport = "2011-10-01"<=sentmonyr & sentmonyr <="2016-09-01",
         present = "2017-10-01"<=sentmonyr & sentmonyr<= "2021-09-01", #LP Edit here: Max month was 9 
         drug = drugtraff | othdrug,
         othtype = !(drugtraff|othdrug|violent|sexual|whitecoll|immigration), 
         othtype2 = !(violent|sexual2|porn|drugtraff|whitecoll|immigration), #incl othdrug, model 2 excludes violent
         mandmin = STATMIN>0,
         custody = PRESENT==1,
         upward = case_when(
           SENTRNGE %in% c(1, 6) ~ TRUE, # LP Edit here 
           BOOKERCD %in% c(1:4) ~ TRUE,
           DEPART==1 ~ TRUE,
           DEPART_A==1 ~ TRUE,
           DEPART==8 ~ NA,
           DEPART_A==8 ~ NA,
           !SENTRNGE %in% c(1, 6) & !is.na(SENTRNGE) ~ FALSE, #LP Edit here 
           !BOOKERCD %in% c(1:4) & !is.na(BOOKERCD) ~ FALSE, #add 8 to the nots for consistency
           DEPART!=1 & !is.na(DEPART) ~ FALSE,
           DEPART_A!=1 & !is.na(DEPART_A) ~ FALSE,
           TRUE ~ NA),
         downgovt = case_when( #inconsistent with the coding of 8 as NA
           SENTRNGE %in% c(3, 4, 7) ~ TRUE, # LP Edit here 
           BOOKERCD %in% c(6, 7) ~ TRUE,
           !SENTRNGE %in% c(3, 4, 7) & !is.na(SENTRNGE) ~ FALSE, #LP Edit here 
           #CML: double check that early disposition/5K3.1 should be classified as downgovt and not downcourt
           !BOOKERCD %in% c(6, 7) & !is.na(BOOKERCD) ~ FALSE,
           TRUE ~ NA),
         downcourt = case_when(
           SENTRNGE %in% c(5, 8) ~ TRUE,#LP Edit here 
           BOOKERCD %in% c(8:11) ~ TRUE,
           !SENTRNGE %in% c(5, 8) & !is.na(SENTRNGE) ~ FALSE, # LP Edit here
           !BOOKERCD %in% c(8:11) & !is.na(BOOKERCD) ~ FALSE,
           TRUE ~ NA),
         subasst = case_when(
           SENTRNGE ==2 ~ TRUE, # LP Edit here 
           BOOKERCD==5 ~ TRUE,
           DEPART %in% c(3, 5, 7, 9) ~ TRUE,
           DEPART_A==2 ~ TRUE,
           DEPART==8 ~ NA,
           DEPART_A==8 ~ NA,
           BOOKERCD !=5 ~ FALSE,
           SENTRNGE !=2 ~ FALSE, #LP edit here 
           !DEPART %in% c(3, 5, 7, 9) & !is.na(DEPART) ~ FALSE,#add 8 to the nots for consistency
           DEPART_A!=2 & !is.na(DEPART_A) ~ FALSE,
           TRUE ~ NA),
         valve = case_when( ##CML: may not fully capture valve cases post-2019, 
           #see codebook. note to ask prof doherty if she wants this var to 
           #include expanded safety valve eligibility under the First Step Act (it does not currently)
           SAFE %in% c(1, 2) ~ TRUE,
           SAFE == 0 ~ FALSE,
           TRUE ~ NA),
         mitigate = MITROLHI!=0,
         aggravate = AGGROLHI!=0,
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
  mutate(crime_type = case_when( # uses the refined model typology
          drugtraff ~ "Drug Trafficking",
          whitecoll ~ "White Collar",
          othtype2 ~ "Other Types",
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
        downdep = 
          case_when( ###this is called downdep but really it's all below range 
            #sentences, not just those attributable only to departures, 
            #so in addition to 3,4,5 you'd want  7, 8 (which is not covered by downcourt and downgovt)
            DEPART %in% c(2, 4, 6) ~ TRUE,
            DEPART_A %in% c(3, 4, 5) ~ TRUE,
            downgovt ~ TRUE,
            downcourt ~ TRUE,
            !DEPART %in% c(2, 4, 6) & !is.na(DEPART) ~ FALSE,
            !DEPART_A %in% c(3, 4, 5) & !is.na(DEPART_A) ~ FALSE,
            !downgovt & !downcourt ~ FALSE,
            DEPART==8 ~ NA,
            DEPART_A==8 ~ NA, #consider FALSE here?
            TRUE ~ NA),
        mandmin2 = case_when(
          is.na(STATMIN) ~ NA,
          STATMIN==0 ~ FALSE,
          STATMIN>0 & (SAFE==1 | SAFE==2) ~ FALSE,
          STATMIN>0 & subasst ~ FALSE,
          STATMIN>0 ~ TRUE,
          TRUE ~ NA)) %>%  #2012 Booker Report at 32 says BOOKERCD and DEPART used too, but model also uses subassist var?)
  mutate(familyties = str_detect(reason, "\\b17\\b"))


#--------OLD CODE---------------------

# 
# fy02_raw <- read_csv(here::here("data/individual_offenders/opafy02nid.csv"), guess_max = 50000) %>% 
#   select(SENTDATE, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, DEPART, 
#          SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT) %>% 
#   rename_all(str_to_upper)
# 
# fy03_raw <- read_csv(here::here("data/individual_offenders/opafy03nid.csv"), guess_max = 50000) %>% 
#   select("sentdate", "sensplt0", "glmin", "gdlinehi", "totchpts", "is924c", "weapsoc", "statmin", "caroffap", "accap", 
#          "depart", "safe", "newcnvtn", "present", "mitrolhi", "aggrolhi", "newrace", "monsex", "age", "educatn", 
#          "newcit") %>% 
#   rename_all(str_to_upper)
# 
# fy04_raw <- read_csv(here::here("data/individual_offenders/opafy04nid.csv"), guess_max = 50000) %>% 
#   select("SENTMON", "SENTYR", "sensplt0", "glmin", "gdlinehi", "totchpts", "is924c", "weapsoc", "statmin", "caroffap", "accap", 
#          "DEPART_A",  "safe", "newcnvtn", "present", "mitrolhi", "aggrolhi", "newrace", "monsex", "age", "educatn", 
#          "newcit") %>% 
#   rename_all(str_to_upper)
# 
# fy05_raw <- read_csv(here::here("data/individual_offenders/opafy05nid.csv"), guess_max = 50000) %>% 
#   select(SENTMON, SENTYR, sensplt0, glmin, gdlinehi, totchpts, is924c, weapsoc, statmin, caroffap, accap, DEPART_A, BookerCD, 
#          safe, newcnvtn, present, mitrolhi, aggrolhi, newrace, monsex, age, educatn, newcit, BOOKPOST, REAS1, REAS2, REAS3, 
#          REAS4, REAS5, REAS6, REAS7, REAS8, REAS9, REAS10, REAS11, REAS12) %>% 
#   rename_all(str_to_upper)
# #MITCAP not available til 06
# #add: REASON1 - REASONX, MITCAP
# 
# fy06_raw <- read_csv(here::here("data/individual_offenders/opafy06nid.csv"), guess_max = 50000) %>% 
#   select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD, 
#          SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT) #left DEPART_A out by accident, starting here
# 
# fy07_raw <- read_csv(here::here("data/individual_offenders/opafy07nid.csv"), guess_max = 50000) %>% 
#   select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD, 
#          SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT)
# 
# fy08_raw <- read_csv(here::here("data/individual_offenders/opafy08nid.csv"), guess_max = 50000) %>% 
#   select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD, 
#          SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT)
# 
# fy09_raw <- read_csv(here::here("data/individual_offenders/opafy09nid.csv"), guess_max = 50000) %>% 
#   select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD, 
#          SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT)
# 
# fy10_raw <- read_csv(here::here("data/opafy10nid_downselected.csv")) %>% 
#   rename_all(str_to_upper)
# 
# fy11_raw <- read_csv(here::here("data/individual_offenders/opafy11nid.csv"), guess_max = 50000) %>% 
#   select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD, 
#          SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT)
# 
# fy12_raw <- read_csv(here::here("data/opafy12nid_downselected.csv")) %>% 
#   rename_all(str_to_upper)
# 
# fy13_raw <- read_csv(here::here("data/opafy13nid_downselected.csv")) %>% 
#   rename_all(str_to_upper)
# 
# fy14_raw <- read_csv(here::here("data/opafy14nid_downselected.csv")) %>% 
#   rename_all(str_to_upper)
# 
# fy15_raw <- read_csv(here::here("data/opafy15nid_downselected.csv")) %>% 
#   rename_all(str_to_upper)
# 
# fy16_raw <- read_csv(here::here("data/opafy16nid_downselected.csv")) %>% 
#   rename_all(str_to_upper)
# 
# fy17_raw <- read_csv(here::here("data/opafy17nid_downselected.csv")) %>% 
#   rename_all(str_to_upper)
# 
# fy20_raw <- read_csv(here::here("data/individual_offenders/opafy20nid.csv"), guess_max = 50000) %>% 
#   select(SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, 
#          ACCAP, SENTRNGE, FSASV, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, 
#          AGE, EDUCATN, NEWCIT)
# 
# # write_csv(fy02_raw, "data/io_downselect1/fy02.csv")
# # write_csv(fy03_raw, "data/io_downselect1/fy03.csv")
# # write_csv(fy04_raw, "data/io_downselect1/fy04.csv")
# # write_csv(fy05_raw, "data/io_downselect1/fy05.csv")
# # write_csv(fy06_raw, "data/io_downselect1/fy06.csv")
# # write_csv(fy07_raw, "data/io_downselect1/fy07.csv")
# # write_csv(fy08_raw, "data/io_downselect1/fy08.csv")
# # write_csv(fy09_raw, "data/io_downselect1/fy09.csv")
# # write_csv(fy10_raw, "data/io_downselect1/fy10.csv")
# # write_csv(fy11_raw, "data/io_downselect1/fy11.csv")
# # write_csv(fy12_raw, "data/io_downselect1/fy12.csv")
# # write_csv(fy13_raw, "data/io_downselect1/fy13.csv")
# # write_csv(fy14_raw, "data/io_downselect1/fy14.csv")
# # write_csv(fy15_raw, "data/io_downselect1/fy15.csv")
# # write_csv(fy16_raw, "data/io_downselect1/fy16.csv")
# # write_csv(fy17_raw, "data/io_downselect1/fy17.csv")
# 
# #add bookpost into "This field is only available FY2005 (post Booker Supreme Court Decision)-FY2017."
# 
# io <- bind_rows(fy02_raw,
#                 fy03_raw,
#                 fy04_raw,
#                 fy05_raw,
#                 fy06_raw,
#                 fy07_raw,
#                 fy08_raw,
#                 fy09_raw,
#                 fy10_raw,
#                 fy11_raw,
#                 fy12_raw,
#                 fy13_raw,
#                 fy14_raw,
#                 fy15_raw,
#                 fy16_raw,
#                 fy17_raw,
#                 fy20_raw)
# 

#write_csv(data, here::here("data/io.csv"))

# 
# aggregate_reasons <- function(df){
#   df %>% 
#     #rename_all(str_to_upper) %>% 
#     mutate(reason = glue("{REAS1} {REAS2} {REAS3} {REAS4} {REAS5} {REAS6} {REAS7} {REAS8} {REAS9} {REAS10} {REAS11} {REAS12}") %>% 
#            str_remove_all("NA") %>% 
#            str_squish(),
#          .keep = "unused") %>% 
#     remove_empty()
# }
# # 
# fy05_raw <- read_csv(here::here("data/io_downselect2/opafy05_downselected.csv"), guess_max = 50000) %>% 
#   aggregate_reasons() %>% 
#   rename_all(str_to_upper) %>%
#   rename(reason = REASON) %>% 
#   select(-c(MITROL1:MITROL60, AGGROL1:AGGROL60))
# 
# fy06_raw <- read_csv(here::here("data/io_downselect2/opafy06_downselected.csv"), guess_max = 50000) %>% 
#   select(-c(MITROL1:MITROL41, AGGROL1:AGGROL41)) %>% 
#   aggregate_reasons() 
#   
# fy07_raw <- read_csv(here::here("data/io_downselect2/opafy07_downselected.csv"), guess_max = 50000) %>% 
#   select(-c(MITROL1:MITROL100, AGGROL1:AGGROL100)) %>% 
#   aggregate_reasons()
# 
# fy08_raw <- read_csv(here::here("data/io_downselect2/opafy08_downselected.csv"), guess_max = 50000) %>% 
#   select(-c(MITROL1:MITROL69, AGGROL1:AGGROL69)) %>% 
#   aggregate_reasons()
# 
# # write_csv(io, here::here("data/io_raw.csv"))

#long term - read in only the output of Linh's io_download file
#short term - use output of Linh's io_download file for fy2017-2021
# 
# io <- read_csv(here::here("data/io_raw.csv")) %>% 
#   filter(!SENTYR %in% c(2017, 2019, 2020) & !(SENTYR==2016 & SENTMON %in% c(10, 11, 12)))
# 
# #read in the raw file for FYs 17-21 here
# io_raw_2017_2021 <- read_csv(here::here("data/io_raw_2017_2021.csv"))
# 
# #merge the two as "io_combined"
# io_combined <- bind_rows(io, io_raw_2017_2021)

# ##### IGNORE BELOW HERE
# 
# ##?
# postprotect %>% filter(MONSEX==1 & NEWRACE==1) %>% pull(SENSPLT0) %>% mean(na.rm=T)
# postprotect %>% filter(MONSEX==0 & NEWRACE==1) %>% mutate(SENSPLT0 = ifelse(is.nan(SENSPLT0), NA, SENSPLT0)) %>% pull(SENSPLT0) %>% mean(na.rm=T)


#justfair has TOTCHPTS instead of SORCHPT for total criminal history points -- check on the difference

# 
# #this is the old Booker report model
# model1 <- lm(logsplit ~ logmin + 
#                sexual + #sexual2 used pg 33 of 2012 Booker Report
#                drugtraff +
#                whitecoll +
#                immigration +
#                othtype +
#                TOTCHPTS +
#                IS924C +
#                WEAPSOC +
#                valve + 
#                CAROFFAP +
#                ACCAP +
#                upward +
#                downgovt +
#                downcourt +
#                subasst +
#                mandmin +
#                NEWCNVTN +
#                mitigate +
#                aggravate +
#                factor(NEWRACE) +
#                MONSEX + 
#                agedummy +
#                educ +
#                NEWCIT,
#              data)
# 
# 
# 
