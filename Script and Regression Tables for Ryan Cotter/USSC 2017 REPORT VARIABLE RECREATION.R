


DATA <- FY12_16B %>%
  
  mutate(GLMIN = ifelse(GLMIN == 999, NA, 
                        ifelse(GLMIN == 996, 470,
                               ifelse(GLMIN == 995, STATMIN,
                                      ifelse(GLMIN > 470, 470, GLMIN))))) %>%
  
  mutate(GLMIN0 = ifelse(GLMIN == 0, 0.01, GLMIN)) %>%
  
  mutate(LOGMIN = log(GLMIN0)) %>%
  
  mutate(SENSPLT0 = ifelse(SENSPLT0 > 470, 470, SENSPLT0)) %>%
  
  mutate(SENSPLT = ifelse(SENSPLT0 >  470, 470,
                          ifelse(SENSPLT0 == 0, 0.01, SENSPLT0))) %>%
  
  
  mutate(LOGSPLIT = log(SENSPLT)) %>%
  
  mutate(UPWARD = case_when(BOOKERCD %in% c(1:4) ~ TRUE,
                            !BOOKERCD %in% c(1:4) & !is.na(BOOKERCD) ~ FALSE,
                            TRUE ~ NA)) %>%
  
  mutate(DOWNDEP = 0) %>%
  mutate(DOWNDEP = ifelse(BOOKERCD == 6 |
                            BOOKERCD == 7 |
                            BOOKERCD == 8 |
                            BOOKERCD == 9 |
                            BOOKERCD == 10 |
                            BOOKERCD == 11, 1, DOWNDEP)) %>%
  
  mutate(DOWNDEP = ifelse(BOOKERCD == 12 |
                            is.na(BOOKERCD) == TRUE, NA, DOWNDEP)) %>%
  
  
  mutate(SUBASST = case_when(BOOKERCD %in% c(5) ~ TRUE,
                             !BOOKERCD %in% c(5) & !is.na(BOOKERCD) ~ FALSE,
                             TRUE ~ NA)) %>%
  
  mutate(VALVE = ifelse(SAFE > 0, 1, 0)) %>%
  mutate(VALVE = ifelse(is.na(VALVE), 0, VALVE)) %>%
  
  mutate(MANDMIN = ifelse(STATMIN > 0, 1, 0)) %>%
  mutate(MANDMIN2 = ifelse(VALVE == 1 | SUBASST == 1, 0, MANDMIN)) %>%
  mutate(MANDMIN2 = ifelse(is.na(MANDMIN2), 0, MANDMIN2)) %>%
  
  mutate(CUSTODY = ifelse(PRESENT == 1, 1, 0)) %>%
  mutate(CUSTODY = ifelse(is.na(CUSTODY), 0, CUSTODY)) %>%
  
  mutate(AGEDUMMY = ifelse(AGE > 25, 1, 0)) %>%
  
  mutate(EDUC = case_when(
    EDUCATN %in% c(13:16, 23, 24, 34, 35) ~ TRUE,
    !EDUCATN %in% c(13:16, 23, 24, 34, 35) & !is.na(EDUCATN) ~ FALSE,
    TRUE ~ NA)) %>%
  
  mutate(WHITEMALE = ifelse(MONSEX==0 & NEWRACE==1, 1, 0)) %>%
  mutate(BLACKMALE = ifelse(MONSEX==0 & NEWRACE==2, 1, 0)) %>%
  mutate(HISPMALE = ifelse(MONSEX==0 & NEWRACE==3, 1, 0)) %>%
  mutate(OTHERMALE = ifelse(MONSEX==0 & NEWRACE==6, 1, 0)) %>%
  mutate(WHITEFEMALE = ifelse(MONSEX==1 & NEWRACE==1, 1, 0)) %>%
  mutate(BLACKFEMALE = ifelse(MONSEX==1 & NEWRACE==2, 1, 0)) %>%
  mutate(HISPFEMALE = ifelse(MONSEX==1 & NEWRACE==3, 1, 0)) %>%
  mutate(OTHERFEMALE = ifelse(MONSEX==1 & NEWRACE==6, 1, 0)) %>%
  
  
  mutate(PART = substr(GDLINEHI, 1, 2)) %>%
  
  mutate(VIOLENT = case_when(PART %in% c("2K", "2A") ~ TRUE,
                             GDLINEHI %in% c('2E1.3','2E1.4','2E2.1','2B3.1','2B3.2','2B3.3') ~ TRUE,
                             TRUE ~ FALSE)) %>%
  mutate(VIOLENT = case_when(GDLINEHI %in% c('2A3.1','2A3.2','2A3.3','2A3.4') ~ FALSE,
                             TRUE ~ VIOLENT)) %>% 
  
  
  mutate(SEXUAL = case_when(PART %in% c("2G") ~ TRUE,
                            GDLINEHI %in% c('2A3.1','2A3.2','2A3.3','2A3.4') ~ TRUE,
                            TRUE ~ FALSE)) %>%
  mutate(SEXUAL = case_when(GDLINEHI %in% c('2G3.1', '2G3.2') ~ FALSE,
                            TRUE ~ SEXUAL)) %>% 
  
  
  mutate(WHITECOLL = case_when(PART %in% c("2T", "2S") ~ TRUE,
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
  
  mutate(IMMIGRATION = case_when(PART %in% c("2L") ~ TRUE,
                                 TRUE ~ FALSE)) %>%
  
  mutate(DRUGTRAFF = case_when(PART %in% c("2D") ~ TRUE,
                               TRUE ~ FALSE)) %>%
  mutate(DRUGTRAFF = case_when(GDLINEHI %in% c('2D2.1', 
                                               '2D2.2', 
                                               '2D2.3',
                                               '2D3.1',
                                               '2D3.2',
                                               '2D3.3',
                                               '2D3.4',
                                               '2D3.5') ~ FALSE,
                               TRUE ~ DRUGTRAFF)) %>% 
  
  mutate(DRUGPOSS =  case_when(GDLINEHI %in% c('2D2.1', '2D2.2') ~ TRUE,
                               TRUE ~ FALSE)) %>%
  
  mutate(PART1_3 = substr(GDLINEHI, 1, 3)) %>%
  
  mutate(SEXUAL2 = SEXUAL) %>%
  mutate(SEXUAL2 = case_when(PART1_3 %in% c("2G2") ~ FALSE,
                             TRUE ~ SEXUAL2)) %>%
  
  mutate(PORN = case_when(PART1_3 %in% c("2G2") ~ TRUE,
                          TRUE ~ FALSE)) %>%
  
  mutate(OTHTYPE = !(WHITECOLL|DRUGTRAFF|VIOLENT|IMMIGRATION|DRUGPOSS|SEXUAL)) %>%
  
  mutate(WITHIN = case_when(BOOKERCD %in% c(0) ~ TRUE,
                          !BOOKERCD %in% c(0) & !is.na(BOOKERCD) ~ FALSE,
                          TRUE ~ NA)) %>%
  
  mutate(STATMIN = ifelse(STATMIN >470, 470, STATMIN)) %>%
  
  mutate(STATMAX = ifelse(STATMAX >470, 470, STATMAX)) %>%
  
  mutate(GLMAX = ifelse(GLMAX == 999, NA,
                        ifelse(GLMAX == 996, 470,
                               ifelse(GLMAX == 995, STATMAX,
                                      ifelse(GLMAX > 470, 470, GLMAX))))) 