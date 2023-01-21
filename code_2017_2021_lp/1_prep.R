library(tidyverse)
library(here)

#Description: 
# This file loads the raw individual offenders files from the USSC's website
# into R and splits them into dataframes of no more than 10,000 columns each. 
# The split files are subsequently loaded into Google Bucket Storage, 
# and from there transferred to BigQuery.

# *Last edited by Charlotte on Jan 19, 2023.* 
#   
# *Regarding 2002, 2003, and 2006: 
# 2002, 2003, and 2006 had less than 10,000 columns. 
# Thus, they did not need any modification 

#----------------------2004----------------------------

read_csv(here::here("data/full_io_files/opafy04nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy04_1.csv"))

read_csv(here::here("data/full_io_files/opafy04nid.csv"),guess_max = 50000) %>% 
  select(9999:17646) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy04_2.csv"))


#----------------------2005----------------------------
read_csv(here::here("data/full_io_files/opafy05nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy05_1.csv"))

read_csv(here::here("data/full_io_files/opafy05nid.csv"),guess_max = 50000) %>% 
  select(9999:14907) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy05_2.csv"))

#----------------------2007----------------------------
read_csv(here::here("data/full_io_files/opafy07nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy07_1.csv"))

read_csv(here::here("data/full_io_files/opafy07nid.csv"),guess_max = 50000) %>% 
  select(9999:19996) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy07_2.csv"))

read_csv(here::here("data/full_io_files/opafy07nid.csv"),guess_max = 50000) %>% 
  select(19997:24528) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy07_2.csv"))

#----------------------2008----------------------------
read_csv(here::here("data/full_io_files/opafy08nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy08_1.csv"))

read_csv(here::here("data/full_io_files/opafy08nid.csv"),guess_max = 50000) %>% 
  select(9999:15341) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy08_2.csv"))
#----------------------2009----------------------------
read_csv(here::here("data/full_io_files/opafy09nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy09_1.csv"))

read_csv(here::here("data/full_io_files/opafy09nid.csv"),guess_max = 50000) %>% 
  select(9999:15423) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy09_2.csv"))

#----------------------2010----------------------------
read_csv(here::here("data/full_io_files/opafy10nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy10_1.csv"))

read_csv(here::here("data/full_io_files/opafy10nid.csv"),guess_max = 50000) %>% 
  select(9999:19996) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy10_2.csv"))

read_csv(here::here("data/full_io_files/opafy10nid.csv"),guess_max = 50000) %>% 
  select(19997:29994) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy10_3.csv"))

read_csv(here::here("data/full_io_files/opafy10nid.csv"),guess_max = 50000) %>% 
  select(29995:30306) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy10_4.csv"))

#----------------------2011----------------------------
read_csv(here::here("data/full_io_files/opafy11nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy11_1.csv"))

read_csv(here::here("data/full_io_files/opafy1nid.csv"),guess_max = 50000) %>% 
  select(9999:11467) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy11_2.csv"))
#----------------------2012----------------------------
read_csv(here::here("data/full_io_files/opafy12nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy12_1.csv"))

read_csv(here::here("data/full_io_files/opafy12nid.csv"),guess_max = 50000) %>% 
  select(9999:18724) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy12_2.csv"))

#----------------------2013----------------------------
read_csv(here::here("data/full_io_files/opafy13nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy13_1.csv"))

read_csv(here::here("data/full_io_files/opafy13nid.csv"),guess_max = 50000) %>% 
  select(9999:18267) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy13_2.csv"))

#----------------------2014----------------------------
read_csv(here::here("data/full_io_files/opafy14nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy14_1.csv"))

read_csv(here::here("data/full_io_files/opafy14nid.csv"),guess_max = 50000) %>% 
  select(9999:19996) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy14_2.csv"))

read_csv(here::here("data/full_io_files/opafy14nid.csv"),guess_max = 50000) %>% 
  select(19997:25001) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy14_3.csv"))

#----------------------2015----------------------------
read_csv(here::here("data/full_io_files/opafy15nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy15_1.csv"))

read_csv(here::here("data/full_io_files/opafy15nid.csv"),guess_max = 50000) %>% 
  select(9999:12819) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy15_2.csv"))

#----------------------2016----------------------------
read_csv(here::here("data/full_io_files/opafy16nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy16_1.csv"))

read_csv(here::here("data/full_io_files/opafy16nid.csv"),guess_max = 50000) %>% 
  select(9999:19996) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy16_2.csv"))

read_csv(here::here("data/full_io_files/opafy16nid.csv"),guess_max = 50000) %>% 
  select(19997:22408) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy16_3.csv"))

#----------------------2017----------------------------
read_csv(here::here("data/full_io_files/opafy17nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy17_1.csv"))

read_csv(here::here("data/full_io_files/opafy17nid.csv"),guess_max = 50000) %>% 
  select(9999:19996) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy17_2.csv"))

read_csv(here::here("data/full_io_files/opafy17nid.csv"),guess_max = 50000) %>% 
  select(19997:20902) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy17_3.csv"))

#----------------------2018----------------------------
read_csv(here::here("data/full_io_files/opafy18nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy18_1.csv"))

read_csv(here::here("data/full_io_files/opafy18nid.csv"),guess_max = 50000) %>% 
  select(9999:19996) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy18_2.csv"))

read_csv(here::here("data/full_io_files/opafy18nid.csv"),guess_max = 50000) %>% 
  select(19997:27693) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy18_3.csv"))

#----------------------2019----------------------------
read_csv(here::here("data/full_io_files/opafy19nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy19_1.csv"))

read_csv(here::here("data/full_io_files/opafy19nid.csv"),guess_max = 50000) %>% 
  select(9999:17732) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy19_2.csv"))

#----------------------2020----------------------------
read_csv(here::here("data/full_io_files/opafy20nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy20_1.csv"))

read_csv(here::here("data/full_io_files/opafy20nid.csv"),guess_max = 50000) %>% 
  select(9999:16855) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy20_2.csv"))

#----------------------2021----------------------------
read_csv(here::here("data/full_io_files/opafy21nid.csv"),guess_max = 50000) %>% 
  select(1:9998) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy21_1.csv"))

read_csv(here::here("data/full_io_files/opafy21nid.csv"),guess_max = 50000) %>% 
  select(9999:19996) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy21_2.csv"))

read_csv(here::here("data/full_io_files/opafy21nid.csv"),guess_max = 50000) %>% 
  select(19997:20801) %>%
  mutate(id = row_number()) %>% 
  write_csv(here::here("data/new_io_data_r/fy21_3.csv"))

#-----------Creating Schema Code for BigQuery--------------------
#----------------------------------------------------------------
# In order to manually upload the data into BigQuery 
# We had to load in all the columns as strings. We also did this in order 
# to preserve data. In order to do so, we had to list out the "schema" 
#of the data to upload the files. Note, we use read.csv instead of read_csv
# because we cannot use nrows with read_csv. 
# Schema is the type breakdown of every column 

# df <- read.csv(here::here("data/new_io_data_r/fy09_2.csv"), nrows = 2)
#  sql <- colnames(df) 
#  sql <- paste(sql,":STRING")
#  sql <- paste(sql, collapse= ', ')
#  write.table(sql, file = 'bq09_2.txt')

#In this example, I used file fy09_2, but I repeatedly interchanged
# the file names to create many files with the sql code to upload the data.
# For every file, I also had to delete the quotation marks (" ") around the
# text and delete the first few lines of text in order to upload the data. 
# It would look as follows: X:STRING , id: STRING, etc... 
# I then copied and pasted this text into BigQuery's Schema option for 
#edit as text. 


