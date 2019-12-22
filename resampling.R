library(tidyverse)
library(data.table)
library(readxl)

DJPM <- fread('commodities/DJPM.csv')[, Date := lubridate::dmy(Date)]

DJPM[, week := week(Date)]
DJPM[, year := year(Date)]
DJPM[, week_year := paste0(week,"-", year)]
DJPM_weekly <- DJPM[, .(Date = data.table::last(Date), DJPM = data.table::last(`Dow Jones Precious Metals Index`)), by = week_year]

DJPM_weekly_tibble <- as_tibble(DJPM_weekly) %>%
  mutate(timediff = Date-lag(Date))

earliest_date <- as.Date("2008-01-01")
last_date <- as.Date("2019-12-12")

find_date <- function(){
  for (file in c(list.files('commodities'), list.files('esg-indices'), list.files('nonesg-indices'))){
  if (str_detect(file, ".csv")){
    if (file.exists(paste0('commodities/', file))){
      file_df <- read_csv(paste0('commodities/', file))
    } else if (file.exists(paste0('esg-indices/', file))){
      file_df <- read_csv(paste0('esg-indices/', file))
    } else if (file.exists(paste0('nonesg-indices/', file))){
      file_df <- read_csv(paste0('nonesg-indices/', file))
    }
  } else if (str_detect(file, ".xls")){
    if (file.exists(paste0('commodities/', file))){
      file_df <- read_excel(paste0('commodities/', file))
    } else if (file.exists(paste0('esg-indices/', file))){
      file_df <- read_excel(paste0('esg-indices/', file))
    } else if (file.exists(paste0('nonesg-indices/', file))){
      file_df <- read_excel(paste0('nonesg-indices/', file))
    }
  }
  file_name <- stringr::str_replace(file, ".csv", "")
  file_name <- stringr::str_replace(file_name, ".xlsx", "")
  
  file_df %>%
    write_csv(paste0('data/raw/', colnames(file_df)[2], ".csv"))
  
  file_df <- file_df %>%
    arrange(Date)
  
  earliest_file <- head(file_df$Date, 1)
  last_file <- tail(file_df$Date, 1)

  if (earliest_file >= earliest_date){
    earliest_index <- colnames(file_df)[2]
    earliest_date <- earliest_file
  }
  
  if (last_file <= last_date){
    last_index <- colnames(file_df)[2]
    last_date <- last_file
  }
  print(paste0("Early date - ", earliest_index))
  print(paste0("Late date - ", last_index))
  }
}

resample_properly <- function(){
  dttm_list <- c("DJS EU", "GSLI", "STOXX Europe Industry Neutral ESG EUR", "STOXX Global ESG Impact USD", "DJS US")
  dmy_list <- c("Dow Jones Commodity Index Grains", "Dow Jones Commodity Index Industrial Metals", 
                "Dow Jones Precious Metals Index", "S&P500 ESG", "STOXX Europe Leaders NE", "STOXX Europe Leaders")
  
  for (file in list.files("data/raw")){
    file_df <- read_csv(paste0("data/raw/", file))
    file_name <- str_replace_all(file , ".csv", "")
    if (file_name %in% dttm_list){
      file_df <- file_df %>%
        mutate(Date = lubridate::date(Date))
    } else if (file_name %in% dmy_list){
      file_df <- file_df %>%
        mutate(Date = lubridate::dmy(Date))
    }
    
    file_name2 <- stringr::str_replace_all(file_name, " ", "_")
    print(file_name)
    colnames(file_df) <- c("Date", "Price")
    
    file_df <- file_df %>%
      arrange(Date) %>%
      dplyr::filter(Date >= "2012-10-01" & Date <= "2019-09-30") %>%
      mutate(week = lubridate::week(Date), year = lubridate::year(Date)) %>%
      mutate(week = ifelse(length(week) == 1, paste0(0, week), week)) %>%
      mutate(week_year = paste0(sprintf("%02d", lubridate::week(Date)), "-", year)) %>%
      select(-week, -year)
    
    
    file_dt <- data.table(file_df)
    file_dt_weekly <- file_dt[, .(Price = data.table::last(Price)),
                              by = week_year]
    file_df_weekly <- as_tibble(file_dt_weekly)
    
    colnames(file_df_weekly) <- c("week_year", file_name)
    
    file_df_weekly %>%
      write_csv(paste0('data/resampled-properly-W/', file))
    
    
  }
  
}