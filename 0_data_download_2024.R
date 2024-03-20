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
  aggregate_reasons()

query2017_2 <- dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy17_2 ")
data2017_2 <-  dbFetch(query2017_2)
data2017 <- full_join(data2017_1, data2017_2, by = 'id') %>% 
  mutate(opafy = 2017,
         SENTRNGE = NA) %>% 
  select(-id)


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
data2018_1 <-  dbFetch(query2018_1) %>% aggregate_reasons()

query2018_2 <-  dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy18_3")
data2018_2 <- dbFetch(query2018_2)
data2018 <- full_join(data2018_1, data2018_2, by = 'id') %>% 
  mutate(opafy=2018) %>% 
  select(-id) %>% 
  rename_at(vars(sentmon:sentrnge), str_to_upper)

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
data2019_1 <-  dbFetch(query2019_1) %>% aggregate_reasons()
query2019_2 <-  dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy19_2 ")
data2019_2 <- dbFetch(query2019_2)
data2019 <- full_join(data2019_1, data2019_2, by = 'id') %>% 
  mutate(opafy=2019) %>% 
  select(-id)


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
data2020_1 <-  dbFetch(query2020_1) %>% aggregate_reasons()

query2020_2 <-  dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy20_2 ")
data2020_2 <- dbFetch(query2020_2)
data2020 <- full_join(data2020_1, data2020_2, by = 'id') %>% 
  mutate(opafy = 2020) %>% 
  select(-1)


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
data2021_1 <-  dbFetch(query2021_1) %>% aggregate_reasons()
query2021_2 = dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy21_3")
data2021_2 <- dbFetch(query2021_2)
data2021 <- full_join(data2021_1, data2021_2, by = 'id') %>% 
  mutate(opafy=2021) %>% 
  select(-1)

# ------------------------MERGE ALL YEARS TOGETHER--------------------------

io_2017_2021 <- bind_rows(data2017, data2018, data2019, data2020, data2021) 
# %>%
#   mutate(across(c(SENTMON, SENTYR, SENSPLT0, GLMIN, TOTCHPTS, IS924C,
#                   WEAPSOC,STATMIN, CAROFFAP, ACCAP, SAFE,NEWCNVTN, PRESENT,
#                   MITROLHI,AGGROLHI,NEWRACE,MONSEX,AGE, EDUCATN,NEWCIT,
#                   BOOKERCD,SENTRNGE, DISTRICT, CIRCDIST, SOURCES), as.numeric))

write_csv(io_2017_2021, here::here("data/io_2017_2021.csv"))




