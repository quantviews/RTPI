## R wrapper for RTPI data from VTB/RANEPA (real time prices data for Russia)
## Author: Marcel Salikhov (marcel.salikhov@gmail.com)

library(assertive)
library(httr)
library(data.table)
library(magrittr)
library(dplyr)
library(glue)
library(assert)
library(parallel)



# -------------------------------------------------------------------------

# server url and port for DB access 
# you also need to have a token


URL <- 'http://164.90.194.12:8080'
#token <- ''


# -------------------------------------------------------------------------



#' technical function for general request  
#'
#' @param url general url link
#'
#' @return df converted from json 
#' @export
#'
#' @examples
sent_request <- function(url){
   df <- httr::GET(url, 
                   add_headers("Authorization" = paste0('Bearer ', token), 
                               "Content-Type" = "application/json",
                               "Range-Unit" = "items"))
   
   df <- content(df) 
   df  <- as.data.frame(data.table::rbindlist(df))
   return(df)
}

# 
#' Table with Rosstat weights & rosstat_id
#'
#' @return 
#' @export
#'
#' @examples
rtpi_rosstat_weight <- function(){
   url <- glue('{URL}/rtpi_rosstat_weight')
   df <- sent_request(url)
   return(df)
}

# 
#' get all products in the DB
#'
#' @return
#' @export
#'
#' @examples
get_all_prods <- function()
{
   url <- glue('{URL}/rtpi_product_name?select=*')
   df <- sent_request(url)
   return(df)
}

# 
#' Title get web_price_id by rosstad_id
#'
#' @param rosstat_id 
#'
#' @return
#' @export
#'
#' @examples
rtpi_price_page_rosstat <- function(rosstat_id){
   assert(is.numeric(rosstat_id))
   if(length(rosstat_id) == 1) url <- glue('{URL}/rtpi_price_page?select=*web_price_id&rosstat_id=eq.{rosstat_id}')
   
   if(length(rosstat_id) > 1) {
      prods <- paste0('(', paste(rosstat_id, collapse = ','), ')')
      url <- glue('{URL}/rtpi_price_page?select=*web_price_id&rosstat_id=in.{prods}')
   }
   df <- sent_request(url)
   return(df)
}


#' Title helper for rtpi_price_page_web
#'
#' @param k 
#' @param web_price_id 
#'
#' @return
#' @export
#'
#' @examples
rtpi_price_page_web_ <- function(k, web_price_id){
   assert(is.numeric(web_price_id),
          is.numeric(k))
   
   web_price_id_ <- web_price_id[unlist(k)]
   cat(head(web_price_id_), ' ... \n')
   prods <- paste0('(', paste(web_price_id_, collapse = ','), ')')
   url <- glue('{URL}/rtpi_price_page?select=*&web_price_id=in.{prods}')
   df <- sent_request(url)
   cat('wait 3 seconds ... \n') # a  delay for lower server load 
   Sys.sleep(3)
   return(df)
}


#' Title получить web_price_id по web_price_id
#'
#' @param web_price_id 
#'
#' @return
#' @export
#'
#' @examples
rtpi_price_page_web <- function(web_price_id){
   assert_is_numeric(as.numeric(web_price_id))
   if(length(web_price_id) == 1){
      url <- paste0(glue('{URL}/rtpi_price_page?select=*&web_price_id=in.'),
                    web_price_id)
      df <- sent_request(url)
   } 
   # you need to divide request because of the limit on data lenght (<1000 items)
   if(length(web_price_id) > 1) {
      if(length(web_price_id) < 1000){
         prods <- paste0('(', paste(web_price_id, collapse = ','), ')')
         url <- glue('{URL}/rtpi_price_page?select=*&web_price_id=in.{prods}')
         df <- sent_request(url)
      }
      
      if(length(web_price_id) >= 1000){
         
         K <- parallel::splitIndices(length(web_price_id), ncl = ceiling(length(web_price_id)/1000))
         ll <- lapply(K, "rtpi_price_page_web_", web_price_id = web_price_id)
         df <- rlist::list.stack(ll)
      }
   }
   #df <- sent_request(url)
   return(df)
}

# 
#' Title price history by set of ids (web_price_id)
#'
#' @param web_price_id 
#'
#' @return
#' @export
#'
#' @examples
rtpi_price_history  <- function(web_price_id){
   assert(is.numeric(web_price_id))
   
   if(length(web_price_id) == 1) url <- glue('{URL}/rtpi_price?select=date_observe,current_price,web_price_id&web_price_id=eq.{web_price_id}')
   if(length(web_price_id) > 1) {
          prods <- paste0('(', paste(web_price_id, collapse = ','), ')')
          url <- glue('{URL}/rtpi_price?select=date_observe,current_price,web_price_id&web_price_id=in.{prods}')
       }
   
   if(length(web_price_id) == 0) stop
   df <- sent_request(url)
   df <- df[!is.na(df$current_price),]
   df$date_observe <-  as.Date(df$date_observe)
   df <- unique(df)
   
   df <- df %>% 
      group_by(web_price_id, date_observe) %>% 
      summarise(current_price = mean(current_price))
   df <- tidyr::pivot_wider(df, values_from  = current_price, names_from = web_price_id, values_fill = NA )
   #as.character(all$web_price_id) %in% names(df)[-1]
   return(df)
}

# get_id_all <- function(){
#    url <- paste0('http://164.90.194.12:8080/rtpi_product_name?select=*')
#    df <- sent_request(url)
#    return(df)
# }

# 
#' Title get data on all stores is DB
#'
#' @return
#' @export
#'
#' @examples
get_rtpi_store_id <- function(){
   url <- glue('{URL}/rtpi_store_id?select=*')
   df <- sent_request(url)
   return(df)
}

# 
#' Title получить данные по товарам отдельного url-магазинов
#'
#' @param url_ 
#'
#' @return
#' @export
#'
#' @examples
get_like_store <- function(url_){
   url <- glue('{URL}/rtpi_price_page?select=*&price_url=like.*{url_}/*')
   df <- sent_request(url)
   return(df)
}

# 
#' Title заполнить данные по ценам между датами 
#'
#' @param h 
#'
#' @return
#' @export
#'
#' @examples
widen_price_history <- function(h){
 start_date <- h$date_observe[1]
 end_date <- Sys.Date()
 dt <- seq.Date(start_date, end_date, by = 1)
 hh <- data.frame("date_observe" = dt)
 hh <- merge(hh, h, by = 'date_observe', all.x = TRUE)
 hh[,-1] <- apply(hh[,-1],2, zoo::na.locf)
 return(hh)
}
   
