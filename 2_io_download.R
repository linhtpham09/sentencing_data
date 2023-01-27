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
# # ----------------------2002-----------------------------------
# 
# query2002 <- dbSendQuery(con, "select `SENTDATE`, `SENSPLT0`, `GLMIN`, `GDLINEHI`, 
# `TOTCHPTS`, `IS924C`, `WEAPSOC`, `STATMIN`, `CAROFFAP`, `ACCAP`, `DEPART`,
# `SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`,`AGGROLHI`, `NEWRACE`, `MONSEX`, `AGE`, 
#                          `EDUCATN`, `NEWCIT`from opafy02nid")
# data2002 <- dbFetch(query2002)
# data2002 <- data2002 %>% mutate(opafy = 2002)
# 
# #write_csv(data2002, here::here("data/io_truncated/data2002.csv"))
# 
# 
# # ----------------------2003-----------------------------------
# query2003 <- dbSendQuery(con, "select `sentdate`, `sensplt0`, `glmin`,`gdlinehi`, `totchpts`, `is924c`, `weapsoc`, `statmin`, `caroffap`, 
# `accap`,`depart`, `safe`, `newcnvtn`,`present`, `mitrolhi`, `aggrolhi`, `newrace`, `monsex`, 
# `age`, `educatn`,`newcit` from opafy03nid")
# data2003 <- dbFetch(query2003)
# data2003 <- data2003 %>% mutate(opafy = 2003)
# #write_csv(data2003, here::here("data/io_truncated/data2003.csv"))
# 
# # ----------------------2004-----------------------------------
# query2004_1 <- dbSendQuery(con ,"select  `sensplt0`, `glmin`,  `totchpts`, `is924c`, `weapsoc`, 
# `statmin`, `caroffap`, `accap`,`DEPART_A`,  `safe`,  `present`, `mitrolhi`, `aggrolhi`, 
# `newrace`, `monsex`, `age`, `educatn`,`newcit` from fy04_1")
# 
# query2004_2 <- dbSendQuery("select `SENTMON`,`SENTYR`,`gdlinehi`,`newcnvtn` from fy04_2")
# 
# 
# # ----------------------2005-----------------------------------
# "`SENTMON`, `SENTYR`, `sensplt0`, `glmin`, `gdlinehi`, `totchpts`, `is924c`, `weapsoc`, `statmin`, `caroffap`, 
# `accap`, `DEPART_A`, `BookerCD`,
#          `safe`, `newcnvtn`, `present`, `mitrolhi`, `aggrolhi`, `newrace`,`monsex`, 
#          `age`, `educatn`, `newcit`, `BOOKPOST`, `REAS1`, `REAS2`, `REAS3`,
#          `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`, `REAS9`, `REAS10`, `REAS11`, `REAS12`"
# 
# 
# # ----------------------2006-----------------------------------
# "`SENTMON`, `SENTYR`, `SENSPLT0`,`GLMIN`, `GDLINEHI`, `TOTCHPTS`, `IS924C`, `WEAPSOC`, `STATMIN`, `CAROFFAP`, `ACCAP`, `BOOKERCD`,
#          `SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`, `AGGROLHI`, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT from opafy06nid"
# 
# # ----------------------2007-----------------------------------
# "SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD,
#          SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT"
# 
# # ----------------------2008-----------------------------------
# "SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD,
#          SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT)"
# 
# # ----------------------2009-----------------------------------
# "SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD,
#          SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT"
# # ----------------------2010-----------------------------------
# 
# # ----------------------2011-----------------------------------
# "SENTMON, SENTYR, SENSPLT0, GLMIN, GDLINEHI, TOTCHPTS, IS924C, WEAPSOC, STATMIN, CAROFFAP, ACCAP, BOOKERCD,
#          SAFE, NEWCNVTN, PRESENT, MITROLHI, AGGROLHI, NEWRACE, MONSEX, AGE, EDUCATN, NEWCIT)"
# ----------------------2012-----------------------------------

# ----------------------2013-----------------------------------

# ----------------------2014-----------------------------------

# ----------------------2015-----------------------------------

# ----------------------2016-----------------------------------



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
                          `REAS24`, `REAS25`from fy17_1")
data2017_1 <-  dbFetch(query2017_1) %>% 
  aggregate_reasons()

query2017_2 <- dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy17_2 ")
data2017_2 <-  dbFetch(query2017_2)
data2017 <- full_join(data2017_1, data2017_2, by = 'id') %>% 
  mutate(opafy = 2017,
         SENTRNGE = NA) %>% 
  select(-1)

#write_csv(data2017, here::here("data/io_truncated/data2017.csv"))

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
                           `REAS34`,`REAS35` from fy18_1")

data2018_1 <-  dbFetch(query2018_1) %>% aggregate_reasons()

query2018_2 <-  dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy18_3")
data2018_2 <- dbFetch(query2018_2)
data2018 <- full_join(data2018_1, data2018_2, by = 'id') %>% 
  mutate(opafy=2018) %>% 
  select(-1) %>% 
  rename_at(vars(sentmon:sentrnge), str_to_upper)

head(data2018)

#write_csv(data2018, here::here("data/io_truncated/data2018.csv"))

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
                            `REAS28` from fy19_1")
data2019_1 <-  dbFetch(query2019_1) %>% aggregate_reasons()
query2019_2 <-  dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy19_2 ")
data2019_2 <- dbFetch(query2019_2)
data2019 <- full_join(data2019_1, data2019_2, by = 'id') %>% 
  mutate(opafy=2019) %>% 
  select(-1)

head(data2019)

#write_csv(data2019, here::here("data/io_truncated/data2019.csv"))


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
                            `REAS23`, `REAS24`, `REAS25`,`REAS26` from fy20_1")
data2020_1 <-  dbFetch(query2020_1) %>% aggregate_reasons()

query2020_2 <-  dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy20_2 ")
data2020_2 <- dbFetch(query2020_2)
data2020 <- full_join(data2020_1, data2020_2, by = 'id') %>% 
  mutate(opafy = 2020) %>% 
  select(-1)

head(data2020)

#write_csv(data2020, here::here("data/io_truncated/data2020.csv"))

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
                          `REAS29`,`REAS30` from fy21_1")

data2021_1 <-  dbFetch(query2021_1) %>% aggregate_reasons()
query2021_2 = dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy21_3")
data2021_2 <- dbFetch(query2021_2)
data2021 <- full_join(data2021_1, data2021_2, by = 'id') %>% 
  mutate(opafy=2021) %>% 
  select(-1)

head(data2021)

#write_csv(data2021, here::here("data/io_truncated/data2021.csv"))

# ----------------------MERGE TOGETHER-----------------------------------
io_raw_2017_2021 <- bind_rows(data2017, data2018, data2019, data2020, data2021) %>% 
  mutate(across(c(SENTMON, SENTYR, SENSPLT0, GLMIN, TOTCHPTS, IS924C,
                  WEAPSOC,STATMIN, CAROFFAP, ACCAP, SAFE,NEWCNVTN, PRESENT,
                  MITROLHI,AGGROLHI,NEWRACE,MONSEX,AGE, EDUCATN,NEWCIT,
                  BOOKERCD,SENTRNGE), as.numeric)) 

#write_csv(io_raw_2017_2021,here::here("data/io_raw_2017_2021.csv"))

str(io_raw_2017_2021)

