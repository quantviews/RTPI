## example code for analysis of RTPI price data 
## Author: Marcel Salikhov (marcel.salikhov@gmail.com)

# This example is based on Python notebook provided by the authors of RTPI - https://colab.research.google.com/drive/1qqTe8uUwWY4j7-C67UdqwDDDVYPXdUWg?usp=sharing

rm(list = ls())

source('RTPI/functions.R', encoding = 'UTF-8')

date_start <- '2021-01-01'
date_end   <- Sys.Date()


weights <- rtpi_rosstat_weight()
#all_prods <- get_all_prods() # extract all prods & their ids in DB
#data.table::fwrite(all_prods, file = 'csv/all-goods.csv', row.names = FALSE) # save it for later use 

all_prods <- data.table::fread(file = 'csv/all-goods.csv')

rosstat_id = 105

df <- rtpi_price_page_rosstat(rosstat_id)

web_price_ids <- df$web_price_id

prices <- rtpi_price_history(web_price_ids) # price history of items 


ww <- df$web_price_id
pg <- rtpi_price_page_web(ww)

