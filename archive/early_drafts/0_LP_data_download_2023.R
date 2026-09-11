library(bigrquery)
library(DBI)
library(tidyverse)
library(flextable)
library(janitor)
library(scales)
library(glue)
library(lubridate)
library(here)
source(here::here("utils.R"))

#CML, Sept 27 2024 - I think the only difference between this file and the other labeled 0_data_download_2023 is that Linh
#commented out the na_if lines here, which weren't working for her (maybe a package issue?)
#I saved the 0_..._2023 version later, so I think it's the one to use--might not matter though

# auth to bigquery

#connection to google bigquery 
con <- dbConnect(
  bigrquery::bigquery(),
  project = "balmy-coral-330818",
  dataset = "bqtest"
)

#This line allows you to see all of the tables currently 
dbListTables(con)

# ----------------------2017 -----------------------------------
query2017_1 = dbSendQuery(con, "select `id`,`SENTMON`,`SENTYR`,`SENSPLT0`,
                          `GLMIN`,`TOTCHPTS`,`IS924C`,`WEAPSOC`, `STATMIN`, 
                          `CAROFFAP`,`ACCAP`, `SAFE`, `NEWCNVTN`, `PRESENT`, 
                          `MITROLHI`, `AGGROLHI`, `NEWRACE`, `MONSEX`, `AGE`, 
                          `EDUCATN`, `NEWCIT`, `BOOKERCD`,`REAS1`, `REAS2`, 
                          `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`,
                          `REAS9`, `REAS10`, `REAS11`, `REAS12`, `REAS13`, 
                          `REAS14`, `REAS15`, `REAS16`, `REAS17`,`REAS18`,
                          `REAS19`,`REAS20`,`REAS21`, `REAS22`, `REAS23`, 
                          `REAS24`, `REAS25`, `SOURCES`,`DISTRICT`,`CIRCDIST`, 
                          `NEWEDUC`, `ACCCAT`, `WEAPON`, `XFOLSOR`, `VIOL1PTS`,
                          `MAND1`, `MAND2`, `MAND3`, `MAND4`, `MAND5`, `MAND6`, 
                          `SAFE`, `SENTIMP`, `USSCIDN`
                          from fy17_1")

data2017_1 <-  dbFetch(query2017_1) %>% 
  aggregate_reasons() %>% 
  mutate(across(everything(), as.character))

query2017_2 <- dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy17_2 ")
data2017_2 <-  dbFetch(query2017_2) %>% 
  mutate(across(everything(), as.character))

crimhist17fyquery = dbSendQuery(con, "select * from opafy17nid_down")
crimhist17_opafy = dbFetch(crimhist17fyquery) %>% 
  mutate(across(everything(), as.character))

crimhist17 = bq_table_download("balmy-coral-330818.bqtest.crimhist17nid_down") %>% 
  mutate(across(everything(), as.character)) %>% 
  aggregate_choff()
crimhist17_all <- full_join(crimhist17, crimhist17_opafy, by = 'USSCIDN') 


data2017 <- full_join(data2017_1, data2017_2, by = 'id') %>% 
  mutate(opafy = 2017,
         SENTRNGE = NA,
         #across(everything(), ~na_if(., "NA")) Jul9.2024, LP can't run with this
        
         ) %>% 
  select(-id) %>% 
  full_join(crimhist17_all)

#write.csv(here::here(data2017), "data2017_choff.csv")

# ----------------------2018-----------------------------------
query2018_1 <- dbSendQuery(con, "select `id`,`sentmon`, `sentyr`,`sensplt0`, 
                           `glmin`,`totchpts`,`is924c`,`weapsoc`, `statmin`, 
                           `caroffap`,`accap`, `safe`, `newcnvtn`, `present`, 
                           `mitrolhi`, `aggrolhi`, `newrace`, `monsex`, `age`, 
                           `educatn`, `newcit`, `sentrnge`,`REAS1`, `REAS2`, 
                           `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`,
                           `REAS9`, `REAS10`, `REAS11`, `REAS12`, `REAS13`,
                           `REAS14`, `REAS15`, `REAS16`, `REAS17`,`REAS18`,
                           `REAS19`,`REAS20`,`REAS21`, `REAS22`, `REAS23`, 
                           `REAS24`, `REAS25`,`REAS26`,`REAS27`,`REAS28`,
                           `REAS29`,`REAS30`,`REAS31`,`REAS32`,`REAS33`,
                           `REAS34`,`REAS35`, `SOURCES`,`DISTRICT`,`CIRCDIST`,
                           `NEWEDUC`, `ACCCAT`, `WEAPON`, `XFOLSOR`, `VIOL1PTS`,
                           `MAND1`,`MAND2`,`MAND3`,`MAND4`,`MAND5`,`MAND6`,
                           `SAFE`, `SENTIMP`, `USSCIDN`
                           from fy18_1")

#has mand 1-6 
data2018_1 <-  dbFetch(query2018_1) %>% aggregate_reasons() %>% 
  mutate(across(everything(), as.character))

query2018_2 <-  dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy18_3")
data2018_2 <- dbFetch(query2018_2) %>% 
  mutate(across(everything(), as.character))
data2018 <- full_join(data2018_1, data2018_2, by = 'id') %>% 
  mutate(opafy=2018,
         #across(everything(), ~na_if(., "NA")) #LP see note on line61
         ) %>% 
  select(-id) %>% 
  rename_at(vars(sentmon:sentrnge), str_to_upper) 

crimhist18fyquery = dbSendQuery(con, "select * from opafy18nid_down")
crimhist18_opafy = dbFetch(crimhist18fyquery) %>% 
  mutate(across(everything(), as.character))

crimhist18 = bq_table_download("balmy-coral-330818.bqtest.crimhist18nid_down") %>% 
  mutate(across(everything(), as.character)) %>%
  aggregate_choff()
crimhist18_all <- full_join(crimhist18, crimhist18_opafy)

data2018 <- full_join(data2018, crimhist18_all)
#write.csv(here::here(data2018),"data2018_choff.csv")
# ----------------------2019-----------------------------------
query2019_1 <-  dbSendQuery(con, "select `id`,`SENTMON`, `SENTYR`,`SENSPLT0`, 
                            `GLMIN`,`TOTCHPTS`,`IS924C`,`WEAPSOC`, `STATMIN`, 
                            `CAROFFAP`,`ACCAP`, `SAFE`, `NEWCNVTN`, `PRESENT`,
                            `MITROLHI`, `AGGROLHI`, `NEWRACE`, `MONSEX`, `AGE`, 
                            `EDUCATN`, `NEWCIT`, `SENTRNGE`,`REAS1`, `REAS2`,
                            `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, 
                            `REAS8`, `REAS9`, `REAS10`, `REAS11`, `REAS12`, 
                            `REAS13`, `REAS14`, `REAS15`, `REAS16`, `REAS17`,
                            `REAS18`,`REAS19`,`REAS20`,`REAS21`, `REAS22`, 
                            `REAS23`, `REAS24`, `REAS25`,`REAS26`,`REAS27`,
                            `REAS28`, `SOURCES`,`DISTRICT`,`CIRCDIST`, `NEWEDUC`,
                            `ACCCAT`,`WEAPON`,`XFOLSOR`,`VIOL1PTS`,`MAND1`,
                            `MAND2`,`MAND3`,`MAND4`,`MAND5`,`MAND6`, 
                            `SAFE`, `SENTIMP`, `USSCIDN`
                            from fy19_1")

#has mand 1-6 
data2019_1 <-  dbFetch(query2019_1) %>% aggregate_reasons() %>% 
  mutate(across(everything(), as.character))

query2019_2 <-  dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy19_2 ")
data2019_2 <- dbFetch(query2019_2) %>% 
  mutate(across(everything(), as.character))

data2019 <- full_join(data2019_1, data2019_2, by = 'id') %>% 
  mutate(opafy=2019,
         #across(everything(), ~na_if(., "NA"))#LP see note on line61
         ) %>% 
  select(-id)

crimhist19fyquery = dbSendQuery(con, "select * from opafy19nid_down")
crimhist19_opafy = dbFetch(crimhist19fyquery) %>% 
  mutate(across(everything(), as.character))

crimhist19 = bq_table_download("balmy-coral-330818.bqtest.crimhist19nid_down") %>% 
  mutate(across(everything(), as.character)) %>%
  aggregate_choff()
crimhist19_all <- full_join(crimhist19, crimhist19_opafy)

data2019 <- full_join(data2019, crimhist19_all)
#write.csv(here::here(data2019),"data2019_choff.csv")
# ----------------------2020-----------------------------------
query2020_1 <-  dbSendQuery(con, "select `id`,`SENTMON`, `SENTYR`,`SENSPLT0`, 
                            `GLMIN`,`TOTCHPTS`,`IS924C`,`WEAPSOC`, `STATMIN`, 
                            `CAROFFAP`,`ACCAP`, `SAFE`, `NEWCNVTN`, `PRESENT`, 
                            `MITROLHI`, `AGGROLHI`, `NEWRACE`, `MONSEX`, `AGE`, 
                            `EDUCATN`, `NEWCIT`, `SENTRNGE`,`REAS1`, `REAS2`, 
                            `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, 
                            `REAS8`, `REAS9`, `REAS10`, `REAS11`, `REAS12`,
                            `REAS13`, `REAS14`, `REAS15`, `REAS16`, `REAS17`,
                            `REAS18`,`REAS19`,`REAS20`,`REAS21`, `REAS22`, 
                            `REAS23`, `REAS24`, `REAS25`,`REAS26`, `SOURCES`,
                            `DISTRICT`,`CIRCDIST`,`NEWEDUC`,`ACCCAT`,`WEAPON`,
                            `XFOLSOR`,`VIOL1PTS`,`MAND1`,`MAND2`,`MAND3`,`MAND4`,
                            `MAND5`,`MAND6`, `SAFE`, `SENTIMP`, `USSCIDN`
                            from fy20_1")

#has mand 1-6 
data2020_1 <-  dbFetch(query2020_1) %>% aggregate_reasons() %>% 
  mutate(across(everything(), as.character))

query2020_2 <-  dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy20_2 ")
data2020_2 <- dbFetch(query2020_2) %>% 
  mutate(across(everything(), as.character))

data2020 <- full_join(data2020_1, data2020_2, by = 'id') %>% 
  mutate(opafy = 2020,
         #across(everything(), ~na_if(., "NA"))#LP see note on line61
         ) %>% 
  select(-1)


crimhist20fyquery = dbSendQuery(con, "select * from opafy20nid_down")
crimhist20_opafy = dbFetch(crimhist20fyquery) %>% 
  mutate(across(everything(), as.character))

crimhist20 = bq_table_download("balmy-coral-330818.bqtest.crimhist20nid_down") %>% 
  mutate(across(everything(), as.character)) %>% 
  aggregate_choff()

crimhist20_all <- full_join(crimhist20, crimhist20_opafy, by = 'USSCIDN')

data2020 <- full_join(data2020, crimhist20_all)
#write.csv(data2020, "data2020_choff.csv")
# ----------------------2021-----------------------------------

query2021_1 = dbSendQuery(con, "select `id`,`SENTMON`, `SENTYR`,`SENSPLT0`, 
                          `GLMIN`,`TOTCHPTS`,`IS924C`,`WEAPSOC`, `STATMIN`, 
                          `CAROFFAP`,`ACCAP`, `SAFE`, `NEWCNVTN`, `PRESENT`, 
                          `MITROLHI`, `AGGROLHI`, `NEWRACE`, `MONSEX`, `AGE`, 
                          `EDUCATN`, `NEWCIT`, `SENTRNGE`,`REAS1`, `REAS2`,
                          `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`, 
                          `REAS9`, `REAS10`, `REAS11`, `REAS12`, `REAS13`, 
                          `REAS14`, `REAS15`, `REAS16`, `REAS17`,`REAS18`,
                          `REAS19`,`REAS20`,`REAS21`, `REAS22`, `REAS23`, 
                          `REAS24`, `REAS25`,`REAS26`,`REAS27`,`REAS28`,
                          `REAS29`,`REAS30`,`SOURCES`,`DISTRICT`,`CIRCDIST`, 
                          `NEWEDUC`, `ACCCAT`, `WEAPON`, `XFOLSOR`, `VIOL1PTS`,
                          `MAND1`,`MAND2`,`MAND3`,`MAND4`,`MAND5`,`MAND6`, 
                          `SAFE`, `SENTIMP`, `USSCIDN`
                          from fy21_1")

#has mand 1-6 
data2021_1 <-  dbFetch(query2021_1) %>% aggregate_reasons() %>% 
  mutate(across(everything(), as.character))

query2021_2 = dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy21_3")
data2021_2 <- dbFetch(query2021_2) %>% 
  mutate(across(everything(), as.character))

data2021 <- full_join(data2021_1, data2021_2, by = 'id') %>% 
  mutate(opafy=2021,
         #across(everything(), ~na_if(., "NA"))#LP see note on line61
         ) %>% 
  select(-1) 


crimhist21fyquery = dbSendQuery(con, "select * from opafy21nid_down")
crimhist21_opafy = dbFetch(crimhist21fyquery) %>% 
  mutate(across(everything(), as.character))

crimhist21 = bq_table_download("balmy-coral-330818.bqtest.crimhist21nid_down")%>% 
  mutate(across(everything(), as.character)) %>% 
  aggregate_choff()
crimhist21_all <- full_join(crimhist21, crimhist21_opafy,by = "USSCIDN") %>% 
  mutate(across(everything(), ~na_if(., "NA")))

data2021 <- full_join(data2021, crimhist21_all)

write.csv(here::here(data2021), "data2021_choff.csv")
# ------------------------MERGE ALL YEARS TOGETHER--------------------------

data2017 <- read_csv("data2017_choff.csv")
data2018 <- read_csv("data2018_choff.csv")
data2019 <- read_csv("data2019_choff.csv")
data2020 <- read_csv("data2020_choff.csv")
data2021 <- read_csv("data2021_choff.csv")

io_2017_2021 <- bind_rows(data2017, data2018, data2019, data2020, data2021) 
# %>%
#   mutate(across(c(SENTMON, SENTYR, SENSPLT0, GLMIN, TOTCHPTS, IS924C,
#                   WEAPSOC,STATMIN, CAROFFAP, ACCAP, SAFE,NEWCNVTN, PRESENT,
#                   MITROLHI,AGGROLHI,NEWRACE,MONSEX,AGE, EDUCATN,NEWCIT,
#                   BOOKERCD,SENTRNGE, DISTRICT, CIRCDIST, SOURCES), as.numeric))

#write_csv(io_2017_2021, here::here("data/io_2017_2021.csv"))


#violent crimes 




#construct indicator variable - start identifying violent crimes 
#contruct indicator variable - history of violent crimes 
# STAMIN mandatory minimum variable 


