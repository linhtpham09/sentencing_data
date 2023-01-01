
data <- io_raw_2017_2021 %>% 
  mutate(SENTMON = SENTMON %>% 
           str_replace("01", "Jan") %>% 
           str_replace("02","Feb" ) %>% 
           str_replace("03","Mar") %>% 
           str_replace("04","Apr") %>% 
           str_replace("05","May") %>% 
           str_replace("06","Jun") %>% 
           str_replace("07","Jul") %>% 
           str_replace("08","Aug") %>% 
           str_replace("09","Sep") %>% 
           str_replace("10","Oct") %>% 
           str_replace("11","Nov") %>% 
           str_replace("12","Dec"), 
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
         immigration = str_detect(GDLINEHI, "^2L\\d{1}\\.\\d{1}$"),
      drug = drugtraff | othdrug,
         othtype = !(drugtraff|othdrug|violent|sexual|whitecoll|immigration), 
         othtype2 = !(violent|sexual2|porn|drugtraff|whitecoll|immigration),  
         mandmin = STATMIN>0, 
        custody = PRESENT==1, 
        upward = case_when(
           BOOKERCD %in% c(1:4) ~ TRUE,
           SENTRNGE==1 ~ TRUE,
           !BOOKERCD %in% c(1:4) & !is.na(BOOKERCD) ~ FALSE, 
           SENTRNGE!=1 & !is.na(SENTRNGE) ~ FALSE,
           TRUE ~ NA),
 downgovt = case_when( 
           BOOKERCD %in% c(6, 7) ~ TRUE,
           !BOOKERCD %in% c(6, 7) & !is.na(BOOKERCD) ~ FALSE,
           TRUE ~ NA), 
 downcourt = case_when(
           BOOKERCD %in% c(8:11) ~ TRUE,
           !BOOKERCD %in% c(8:11) & !is.na(BOOKERCD) ~ FALSE,
           TRUE ~ NA),
 subasst = case_when(
           BOOKERCD==5 ~ TRUE,
           SENTRNGE ==2 ~ TRUE,
           BOOKERCD !=5 ~ FALSE,
           !SENTRNGE ==2 & !is.na(SENTRNGE) ~ FALSE,
           TRUE ~ NA), 
         valve = case_when(
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
         )) %>%  
  mutate( 
    crime_type = case_when( 
    drugtraff ~ "Drug Trafficking",
    whitecoll ~ "White Collar",
    othtype2 ~ "Other Types",
    sexual2 ~ "Sexual",
    porn ~ "Porn",
    violent ~ "Violent",
    immigration ~ "Immigration"
    ),
    racesex_clean = case_when(
      str_detect(racesex, "whitemale") ~ "White Male",
      str_detect(racesex, "whitefemale") ~ "White Female",
      str_detect(racesex, "blackmale") ~ "Black Male",
      str_detect(racesex, "blackfemale") ~ "Black Female",
      str_detect(racesex, "hispmale") ~ "Hispanic Male",
      str_detect(racesex, "hispfemale") ~ "Hispanic Female",
      str_detect(racesex, "othermale") ~ "Other Male",
      str_detect(racesex, "otherfemale") ~ "Other Female"),
    downdep = case_when(
      SENTRNGE %in% c(3, 4, 5) ~ TRUE,
      downgovt ~ TRUE,
      downcourt ~ TRUE,
      !SENTRNGE %in% c(3, 4, 5) & !is.na(SENTRNGE) ~ FALSE,
      !downgovt & !downcourt ~ FALSE,
      TRUE ~ NA),
    mandmin2 = case_when(
      is.na(STATMIN) ~ NA,
      STATMIN==0 ~ FALSE,
      STATMIN>0 & (SAFE==1 | SAFE==2) ~ FALSE,
      STATMIN>0 & subasst ~ FALSE,
      STATMIN>0 ~ TRUE,
      TRUE ~ NA),
  familyties =  str_detect(reason, "\\b17\\b"))
