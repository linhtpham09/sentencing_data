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

query2002 <- dbSendQuery(con, "select `SENTDATE`, `SENSPLT0`, `GLMIN`, `GDLINEHI`,
`TOTCHPTS`, `IS924C`, `WEAPSOC`, `STATMIN`, `CAROFFAP`, `ACCAP`, `DEPART`,
`SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`,`AGGROLHI`, `NEWRACE`, `MONSEX`, `AGE`,
                         `EDUCATN`, `NEWCIT`, `REASON1`, `REASON2`, `REASTXT1`, 
                         `REASTXT2`, `REASTXT3`, `SOURCES` from opafy02nid")
data2002 <- dbFetch(query2002) %>% aggregate_reasons()
data2002 <- data2002 %>% mutate(opafy = 2002)

#write_csv(data2002, here::here("data/io_truncated/data2002.csv"))


# # ----------------------2003-----------------------------------
query2003 <- dbSendQuery(con, "select `sentdate`, `sensplt0`, `glmin`,`gdlinehi`, `totchpts`, `is924c`, `weapsoc`, `statmin`, `caroffap`,
`accap`,`depart`, `safe`, `newcnvtn`,`present`, `mitrolhi`, `aggrolhi`, `newrace`, `monsex`,
`age`, `educatn`,`newcit`, `REASTXT1`, `REASTXT2`, `REASTXT4`, `REASTXT6`, `REASON4`, `REASON5`,
                         `REASON6`, `SOURCES`from opafy03nid")

data2003 <- dbFetch(query2003)%>% aggregate_reasons()
data2003 <- data2003 %>% mutate(opafy = 2003)
#write_csv(data2003, here::here("data/io_truncated/data2003.csv"))

# # ----------------------2004-----------------------------------
query2004_1 <- dbSendQuery(con ,"select  `id`,`sensplt0`, `glmin`,  `totchpts`, `is924c`, `weapsoc`,
`statmin`, `caroffap`, `accap`,  `safe`,  `present`, `mitrolhi`, `aggrolhi`,
`newrace`, `monsex`, `age`, `educatn`,`newcit`, `sources`from fy04_1")
data2004_1 <- dbFetch(query2004_1)

query2004_2 <- dbSendQuery(con, "select `id`,`DEPART_A`,`SENTMON`,`SENTYR`,`gdlinehi`,`newcnvtn`,`REAS1`, 
`REAS2`, `REAS3`, `REAS4`, `REAS5`,`REAS6` from fy04_2")
data2004_2 <- dbFetch(query2004_2)%>% aggregate_reasons()
data2004 <- full_join(data2004_1, data2004_2, by = 'id') %>% 
  mutate(opafy = 2004) %>% 
  # gets rid of id columns since we don't need after joining 
  select(-id) %>% 
  rename_at(vars(sensplt0:newcnvtn), str_to_upper) # want only opafy and reason to be lowercase

#write_csv(data2004, here::here("data/io_truncated/data2004.csv"))
# # ----------------------2005-----------------------------------

query2005_1 <- dbSendQuery(con, "select `id`,`sensplt0`, `glmin`, `totchpts`, `is924c`, `weapsoc`, `statmin`, `caroffap`,
`accap`,`safe`, `newcnvtn`, `present`, `mitrolhi`, `aggrolhi`, `newrace`,`monsex`,
         `age`, `educatn`, `newcit`, `sources`  from fy05_1")
data2005_1 <- dbFetch(query2005_1)
query2005_2 <- dbSendQuery(con,"select `id`, `gdlinehi`,`SENTMON`, 
`SENTYR`,`BOOKPOST`, `REAS1`, `REAS2`, `REAS3`,
`DEPART_A`, `BookerCD`,
         `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`, `REAS9`, `REAS10`,
                           `REAS11`, `REAS12` from fy05_2")
data2005_2 <- dbFetch(query2005_2) %>% aggregate_reasons()
data2005 <- full_join(data2005_1, data2005_2, by = 'id') %>% 
  mutate(opafy = 2005) %>% 
  select(-id) %>% 
  rename_at(vars(sensplt0:BookerCD), str_to_upper)

#write_csv(data2005, here::here("data/io_truncated/data2005.csv"))
# # ----------------------2006-----------------------------------
query2006 <- dbSendQuery(con, "select `SENTMON`, `SENTYR`, `SENSPLT0`,`GLMIN`, `GDLINEHI`, `TOTCHPTS`, `IS924C`, 
`WEAPSOC`, `STATMIN`, `CAROFFAP`, `ACCAP`, `BOOKERCD`,`SAFE`, `NEWCNVTN`, 
`PRESENT`, `MITROLHI`, `AGGROLHI`, `NEWRACE`, 
`MONSEX`, `AGE`, `EDUCATN`, `NEWCIT`,`REAS1`, `REAS2`, `REAS3`, `REAS4`, `REAS5`,
`REAS6`, `REAS7`, `REAS8`, `REAS9`, `REAS10`,
`REAS11`, `REAS12`, `SOURCES` from opafy06nid")
data2006 <- dbFetch(query2006) %>% aggregate_reasons() %>% mutate(opafy=2006)

#write_csv(data2006, here::here("data/io_truncated/data2006.csv"))
# # ----------------------2007-----------------------------------
query2007_1<- dbSendQuery(con,"select `id`,`SENSPLT0`, `GLMIN`,  `TOTCHPTS`, `IS924C`, `WEAPSOC`, 
`STATMIN`, `CAROFFAP`, `ACCAP`, 
         `SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`, `AGGROLHI`, `NEWRACE`, `MONSEX`, 
`AGE`, `EDUCATN`, `NEWCIT`, `SOURCES` from fy07_1")

query2007_2 <- dbSendQuery(con, "select `id`, `SENTMON`, `SENTYR`, `GDLINEHI`,`BOOKERCD`,`REAS1`, `REAS2`, `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`, `REAS9`, `REAS10`,
`REAS11`, `REAS12` from fy07_3")

data2007_1 <- dbFetch(query2007_1)
data2007_2 <- dbFetch(query2007_2) %>% aggregate_reasons()
data2007 <- full_join(data2007_1, data2007_2, by = 'id') %>% 
  mutate(opafy=2007) %>% 
  select(-id)

#write_csv(data2007, here::here("data/io_truncated/data2007.csv"))
# # ----------------------2008-----------------------------------
query2008_1 <- dbSendQuery(con, "select `id`, `SENSPLT0`, `GLMIN`, `TOTCHPTS`, `IS924C`, `WEAPSOC`, `STATMIN`,
`CAROFFAP`, `ACCAP`, `SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`, `AGGROLHI`, `NEWRACE`, `MONSEX`, `AGE`, 
`EDUCATN`, `NEWCIT`, `SOURCES` from fy08_1")

query2008_2 <- dbSendQuery(con, "select `id`, `SENTMON`, `SENTYR`,`GDLINEHI`,`BOOKERCD`, `REAS1`, `REAS2`, `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`, `REAS9`, `REAS10`,
`REAS11`, `REAS12` from fy08_2")

data2008_1 <- dbFetch(query2008_1)
data2008_2 <- dbFetch(query2008_2) %>% aggregate_reasons()
data2008 <- full_join(data2008_1, data2008_2, by = 'id') %>% 
  mutate(opafy=2008) %>% 
  select(-id)

#write_csv(data2008, here::here("data/io_truncated/data2008.csv"))
# # ----------------------2009-----------------------------------
query2009_1 <- dbSendQuery(con, "select `id`, `SENTMON`, `SENTYR`, `SENSPLT0`, `GLMIN`, `TOTCHPTS`, `IS924C`, `WEAPSOC`,
`STATMIN`, `CAROFFAP`, `ACCAP`, `BOOKERCD`,
         `SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`, 
`AGGROLHI`, `NEWRACE`, `MONSEX`, `AGE`, `EDUCATN`, `NEWCIT`,
`REAS1`, `REAS2`, `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`, `REAS9`, `REAS10`,
`REAS11`, `REAS12`, `SOURCES` from fy09_1")

query2009_2 <- dbSendQuery(con, "select `id`,`GDLINEHI` from fy09_2")

data2009_1 <- dbFetch(query2009_1) %>% aggregate_reasons()
data2009_2 <- dbFetch(query2009_2) 
data2009 <- full_join(data2009_1, data2009_2, by = 'id') %>% 
  mutate(opafy=2009) %>% 
  select(-id)

#write_csv(data2009, here::here("data/io_truncated/data2009.csv"))
# # ----------------------2010-----------------------------------

query2010_1 <- dbSendQuery(con,"select `id`, `sentmon`, `sentyr`, `sensplt0`, `glmin`,  `totchpts`, `is924c`, 
`weapsoc`, `statmin`,
`caroffap`, `accap`, `bookercd`, `safe`, `newcnvtn`, `present`, `mitrolhi`, `aggrolhi`, `newrace`,
`monsex`, `age`, `educatn`,`newcit`,
`reas1`, `reas2`, `reas3`, `reas4`, `reas5`, `reas6`, `reas7`, `reas8`, `reas9`, `reas10`, 
`reas11`, `reas12`, `reas13`, 
`reas14`, `reas15`, `reas16`, `reas17`, `reas18`, `reas19`, `reas20`, `reas21`, `reas22`, 
`reas23`, `reas24`, `sources` from fy10_1" )
query2010_2 <- dbSendQuery(con, "select `id`, `gdlinehi` from fy10_3")
data2010_1 <- dbFetch(query2010_1) %>% aggregate_reasons()
data2010_2 <- dbFetch(query2010_2) 
data2010 <- full_join(data2010_1, data2010_2, by = 'id') %>% 
  mutate(opafy=2010) %>% 
  select(-id)

#write_csv(data2010, here::here("data/io_truncated/data2010.csv"))
# # ----------------------2011-----------------------------------
query2011_1 <- dbSendQuery(con, "select `id`,`SENTMON`, `SENTYR`, `SENSPLT0`, `GLMIN`,  `TOTCHPTS`, `IS924C`, 
`WEAPSOC`, `STATMIN`, `CAROFFAP`, `ACCAP`, `BOOKERCD`,
         `SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`, `AGGROLHI`, `NEWRACE`, 
`MONSEX`, `AGE`, `EDUCATN`, `NEWCIT`,`REAS1`, `REAS2`, `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`, `REAS9`, `REAS10`,
`REAS11`, `REAS12`, `REAS13`, `REAS14`, `REAS15`, `REAS16`, `REAS17`, `REAS18`, 
`REAS19`, `REAS20`, `REAS21`, `REAS22`, `REAS23`, `REAS24`, `SOURCES`from fy11_1")

query2011_2 <- dbSendQuery(con, "select `id`,`GDLINEHI` from fy11_2")


data2011_1 <- dbFetch(query2011_1) %>% aggregate_reasons()
data2011_2 <- dbFetch(query2011_2) 
data2011 <- full_join(data2011_1, data2011_2, by = 'id') %>% 
  mutate(opafy=2011) %>% 
  select(-id)

#write_csv(data2011, here::here("data/io_truncated/data2011.csv"))
# ----------------------2012-----------------------------------
query2012_1 <- dbSendQuery(con, "select `id`, `SENTMON`, `SENTYR`, `SENSPLT0`, `GLMIN`,  `TOTCHPTS`, `IS924C`, 
`WEAPSOC`, `STATMIN`, `CAROFFAP`, 
`ACCAP`, `BOOKERCD`, `SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`, 
`AGGROLHI`, `NEWRACE`, `MONSEX`, `AGE`, `EDUCATN`,
`NEWCIT`, `REAS1`, `REAS2`, `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`, `REAS9`, `REAS10`,
`REAS11`, `REAS12`, `REAS13`, `REAS14`, `REAS15`, `REAS16`, `REAS17`, `REAS18`,
`REAS19`, `REAS20`, `REAS21`, `REAS22`, `REAS23`, `REAS24`,`REAS25`, `REAS26`,
`REAS27`, `REAS28`, `REAS29`, `REAS30`, `REAS31`, `REAS32`, `SOURCES` from fy12_1")

query2012_2 <- dbSendQuery(con, "select `id`,`GDLINEHI` from fy12_2")


data2012_1 <- dbFetch(query2012_1) %>% aggregate_reasons()
data2012_2 <- dbFetch(query2012_2) 
data2012 <- full_join(data2012_1, data2012_2, by = 'id') %>% 
  mutate(opafy=2012) %>% 
  select(-id)
#write_csv(data2012, here::here("data/io_truncated/data2012.csv"))
# # ----------------------2013-----------------------------------
query2013_1 <- dbSendQuery(con, " select `id`,`SENTMON`, `SENTYR`, `SENSPLT0`, `GLMIN`,  `TOTCHPTS`, `IS924C`, 
`WEAPSOC`, `STATMIN`, `CAROFFAP`, 
`ACCAP`, `BOOKERCD`, `SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`, 
`AGGROLHI`, `NEWRACE`, `MONSEX`, `AGE`, `EDUCATN`,
`NEWCIT`,`REAS1`, `REAS2`, `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`, `REAS9`, `REAS10`,
`REAS11`, `REAS12`, `REAS13`, `REAS14`, `REAS15`, `REAS16`, `REAS17`, `REAS18`,
`REAS19`, `REAS20`, `REAS21`, `REAS22`, `REAS23`, `REAS24`,`REAS25`, `REAS26`,
`REAS27`, `REAS28`, `REAS29`, `REAS30`, `SOURCES` from fy13_1")

query2013_2 <- dbSendQuery(con, "select `id`, `GDLINEHI` from fy13_2")
data2013_1 <- dbFetch(query2013_1) %>% aggregate_reasons()
data2013_2 <- dbFetch(query2013_2) 
data2013 <- full_join(data2013_1, data2013_2, by = 'id') %>% 
  mutate(opafy=2013) %>% 
  select(-id)
#write_csv(data2013, here::here("data/io_truncated/data2013.csv"))

# # ----------------------2014-----------------------------------
query2014_1 <- dbSendQuery(con, "select `id`,`SENTMON`, `SENTYR`, `SENSPLT0`, 
`GLMIN`, `TOTCHPTS`, `IS924C`, `WEAPSOC`, `STATMIN`, `CAROFFAP`, 
`ACCAP`, `BOOKERCD`, `SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`, `AGGROLHI`, 
`NEWRACE`, `MONSEX`, `AGE`, `EDUCATN`,`NEWCIT` ,
`REAS1`, `REAS2`, `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`,
`REAS9`, `REAS10`,
`REAS11`, `REAS12`, `REAS13`, `REAS14`, `REAS15`, `REAS16`, `REAS17`, `REAS18`,
`REAS19`, `REAS20`, `REAS21`, `SOURCES` from fy14_1")

query2014_2 <- dbSendQuery(con, "select `id`, `GDLINEHI` from fy14_3")
data2014_1 <- dbFetch(query2014_1) %>% aggregate_reasons()
data2014_2 <- dbFetch(query2014_2) 
data2014 <- full_join(data2014_1, data2014_2, by = 'id') %>% 
  mutate(opafy=2014) %>% 
  select(-id)
#write_csv(data2014, here::here("data/io_truncated/data2014.csv"))

# # ----------------------2015-----------------------------------
query2015_1 <- dbSendQuery(con, "select `id`, `SENTMON`, `SENTYR`, `SENSPLT0`, `GLMIN`, `TOTCHPTS`, 
`IS924C`, `WEAPSOC`, `STATMIN`, `CAROFFAP`, 
`ACCAP`, `BOOKERCD`, `SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`, `AGGROLHI`, `NEWRACE`,
`MONSEX`, `AGE`, `EDUCATN`,`NEWCIT`, `REAS1`, `REAS2`, `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`, `REAS9`, `REAS10`,
`REAS11`, `REAS12`, `REAS13`, `REAS14`, `REAS15`, `REAS16`, `REAS17`, `REAS18`,
`REAS19`, `REAS20`, `SOURCES` from fy15_1")
query2015_2 <- dbSendQuery(con, "select `id`, `GDLINEHI` from fy15_2")
data2015_1 <- dbFetch(query2015_1) %>% aggregate_reasons()
data2015_2 <- dbFetch(query2015_2) 
data2015 <- full_join(data2015_1, data2015_2, by = 'id') %>% 
  mutate(opafy=2015) %>% 
  select(-id)
#write_csv(data2015, here::here("data/io_truncated/data2015.csv"))

# # ----------------------2016-----------------------------------

query2016_1 <- dbSendQuery(con, "select `id`,`SENTMON`, `SENTYR`, `SENSPLT0`, `GLMIN`,  `TOTCHPTS`,
`IS924C`, `WEAPSOC`, `STATMIN`, `CAROFFAP`, 
`ACCAP`, `BOOKERCD`, `SAFE`, `NEWCNVTN`, `PRESENT`, `MITROLHI`, 
`AGGROLHI`, `NEWRACE`, `MONSEX`, `AGE`, `EDUCATN`,
`NEWCIT`, `REAS1`, `REAS2`, `REAS3`, `REAS4`, `REAS5`, `REAS6`, `REAS7`, `REAS8`, `REAS9`, `REAS10`,
`REAS11`, `REAS12`, `REAS13`, `REAS14`, `REAS15`, `REAS16`, `REAS17`, `REAS18`,
`REAS19`, `REAS20`, `REAS21`, `REAS22`, `REAS23`, `REAS24`,`REAS25`, `REAS26`,
`REAS27`, `REAS28`, `REAS29`, `SOURCES` from fy16_1")
query2016_2 <- dbSendQuery(con, "select `id`,`GDLINEHI` from fy16_3")

data2016_1 <- dbFetch(query2016_1) %>% aggregate_reasons()
data2016_2 <- dbFetch(query2016_2) 
data2016 <- full_join(data2016_1, data2016_2, by = 'id') %>% 
  mutate(opafy=2016) %>% 
  select(-id)
#write_csv(data2016, here::here("data/io_truncated/data2016.csv"))


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
                          `REAS24`, `REAS25`, `SOURCES` from fy17_1")
data2017_1 <-  dbFetch(query2017_1) %>% 
  aggregate_reasons()

query2017_2 <- dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy17_2 ")
data2017_2 <-  dbFetch(query2017_2)
data2017 <- full_join(data2017_1, data2017_2, by = 'id') %>% 
  mutate(opafy = 2017,
         SENTRNGE = NA) %>% 
  select(-id)

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
                           `REAS34`,`REAS35`, `SOURCES` from fy18_1")

data2018_1 <-  dbFetch(query2018_1) %>% aggregate_reasons()

query2018_2 <-  dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy18_3")
data2018_2 <- dbFetch(query2018_2)
data2018 <- full_join(data2018_1, data2018_2, by = 'id') %>% 
  mutate(opafy=2018) %>% 
  select(-id) %>% 
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
                            `REAS28`, `SOURCES` from fy19_1")
data2019_1 <-  dbFetch(query2019_1) %>% aggregate_reasons()
query2019_2 <-  dbSendQuery(con, "select `id`, `GDLINEHI` FROM fy19_2 ")
data2019_2 <- dbFetch(query2019_2)
data2019 <- full_join(data2019_1, data2019_2, by = 'id') %>% 
  mutate(opafy=2019) %>% 
  select(-id)

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
                            `REAS23`, `REAS24`, `REAS25`,`REAS26`, `SOURCES` from fy20_1")
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
                          `REAS29`,`REAS30`,`SOURCES` from fy21_1")

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

