## example code for analysis of RTPI price data 
## Author: Marcel Salikhov (marcel.salikhov@gmail.com)

# This example is based on Python notebook provided by the authors of RTPI - https://colab.research.google.com/drive/1qqTe8uUwWY4j7-C67UdqwDDDVYPXdUWg?usp=sharing


source('functions.R', encoding = 'UTF-8')

date_start <- '2021-01-01'
date_end   <- Sys.Date()

rosstat_id = 7411

df <- rtpi_price_page_rosstat(rosstat_id)

web_price_ids <- df$web_price_id

prices <- rtpi_price_history(web_price_ids) # price history of items 

