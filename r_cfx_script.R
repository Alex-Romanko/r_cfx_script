#!/usr/bin/env Rscript

if (!require(data.table)) install.packages('data.table', repos = "http://cran.us.r-project.org")
if (!require(filesstrings)) install.packages('filesstrings', repos = "http://cran.us.r-project.org")
if (!require(stringr)) install.packages('stringr', repos = "http://cran.us.r-project.org")
if (!require(googlesheets4)) install.packages('googlesheets4', repos = "http://cran.us.r-project.org")
if (!require(gargle)) install.packages('gargle', repos = "http://cran.us.r-project.org")

library(gargle)
library(googlesheets4)
library(data.table)
library(stringr)
library(filesstrings)
library(httr)

# ------------------------------------------------------------------------------

perform_initial_setup <- function(start_dir) {
  setwd(start_dir)
  working_dir_content <- list.files()
  if (!("entry.txt" %in% working_dir_content)){
    stop("wrong start directory or no 'entry.txt' file")
  }
  if (!("archive" %in% working_dir_content)){
    dir.create("archive")
  }
  if (!("CFX_files" %in% working_dir_content)){
    dir.create("CFX_files")
  }
  list_archive <- list.files(path = "archive", pattern = "*.csv" )
  list_CFX_files <- list.files(path = 'CFX_files', pattern = "*.csv" )
  files_to_process <- list_CFX_files[!list_CFX_files %in% list_archive]
  if (length(files_to_process) == 0){
    stop("There is nothing to do!")
  }
  return(files_to_process)
}

read_CFX_files <- function(list_for_read, start_dir) {
  setwd(start_dir)
  setwd('CFX_files')
  all_CSV_files <- lapply(list_for_read, function(x) 
    fread(x, header = F, sep = ",", dec = ".", encoding = "UTF-8"))
  setwd(start_dir)
  return(all_CSV_files)
}

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

get_single_dt <- function(nested_elem){
  CFX_file_name <- nested_elem$V2[1]
  first_significant_row <- c('Well')
  index_row <- which(nested_elem$V1 == first_significant_row)
  single_table <- header.true(nested_elem[index_row:nrow(nested_elem),])
  setDT(single_table)
  single_table[, CFX_name := CFX_file_name]
  return(single_table)
}

get_wide_table <- function(data_table) {
  long_data <- data_table[, c('Sample', 'Well', 'Fluor', 'Cq', 'CFX_name')]
  sort_long_data <- long_data[order(Sample, Well, Fluor),]
  sort_long_data[, index := as.numeric(paste(c(1:.N))), by = Sample]
  sort_long_data[, Cq := as.character(round(as.numeric(Cq), 1))]
  wide_table <- dcast(data = sort_long_data, formula = Sample  ~ index, value.var = 'Cq')
  instrument_id_row <- as.data.frame(long_data$CFX_name[1])
  colnames(instrument_id_row) <- c('Sample')
  wide_table <- rbind(wide_table, instrument_id_row, fill=TRUE)
  return(wide_table)
}

move_files_to_archive <- function(files_to_process) {
  setwd(start_dir)
  setwd('CFX_files')
  cfx_file_folder <- getwd()
  setwd(start_dir)
  setwd('archive')
  archive_folder <- getwd()
  setwd(cfx_file_folder)
  sapply(files_to_process, function(x) file.move(x, archive_folder))
  setwd(start_dir)
}

write_to_gs <- function (table_to_append) {
  # blank of json for inserting rows
  ins_range <- jsonlite::fromJSON('{
    "requests": [
      {
        "insertRange": {
          "range": {
            "startRowIndex": 2,
            "endRowIndex": 10,
            "sheetId": 123
          },
          "shiftDimension": "ROWS"
        }
      }
    ]
    }')
  # spreadsheet to write
  google_spreadsheet <- 'https://docs.google.com/spreadsheets/d/16Ox7jX0ljQ5r6Zc7492uaW583-3wDS9HlREXVRNCY9Q/edit#gid=1884007052'
  # list of the spreadsheet 
  sheet_to_append <- "R_CFX_raw"
  # insert sheetId must be id of list named after "R_CFX_raw"
  ins_range$requests$insertRange$range$sheetId <- 1884007052
  # insert empty rows 
  ins_range$requests$insertRange$range$endRowIndex <- nrow(ready_to_append)+2
  
  body_ins_range <- jsonlite::toJSON(ins_range)
  
  # permissions to request
  scope <-  c('https://www.googleapis.com/auth/spreadsheets', 
              'https://www.googleapis.com/auth/drive')
  
  # get authentication 
  token <- token_fetch(scopes = scope)
  gs4_auth(
    email = gargle::gargle_oauth_email(),
    path = NULL,
    scopes = scope,
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token = token
  )
  # get request for inserting blank rows
  res <- POST("https://sheets.googleapis.com/v4/spreadsheets/16Ox7jX0ljQ5r6Zc7492uaW583-3wDS9HlREXVRNCY9Q:batchUpdate", 
              body = body_ins_range, encode = "json",  config(token = token))
  # write data frame in the beginning of the table
  range_write(data = ready_to_append, ss = google_spreadsheet, sheet = sheet_to_append, 
              range = ('A3'),col_names = F, reformat = F)
}

# ------------------------------------------------------------------------------

# start script
# getwd() # must be the folder where placed the script, CFX_files and entry.txt

start_dir <- getwd()

files_to_process <- perform_initial_setup(start_dir)

# get list with nested data tables
all_CSV_files <- read_CFX_files(files_to_process, start_dir)

# deleting first few technical rows 
pre_processed_tables <- lapply(all_CSV_files, function(x) get_single_dt(x))

# reshaping table 
ready_to_append <- rbindlist(lapply(pre_processed_tables, function(x) 
                                    get_wide_table(x)), fill=TRUE)

# write to local backup 
fwrite(ready_to_append, 'ready_to_append.csv', 
       append = T, col.names = F, row.names = F)

write_to_gs(ready_to_append)

# clean CFX_files folder
move_files_to_archive(files_to_process)

print("Congratulation! Files successfully uploaded!")
