suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(stringr)
  library(royale)
  library(gt)
})
clan_tag <- '99R2PQVR'

time_short <- function(times) {
  times <- as.duration(floor_date(as_datetime(times), 'minutes') %--% floor_date(now(), 'minutes')) / dminutes(1)
  out <- as.character(times)
  out <- ifelse(times < 60, paste0(times, 'm'), out)
  out <- ifelse(times >= 60 & times <= 60 * 24, paste0(floor(times / 60), 'h ', times %% 60, 'm'), out)
  out <- ifelse(times > 60 * 24, paste0(floor(times / (60 * 24)), 'd ', floor((times - (floor(times / (60 * 24)) * 60 * 24)) / 60), 'h'), out)
  out
}

time_min <- function(times) {
  times <- as.duration(floor_date(as_datetime(times), 'minutes') %--% floor_date(now(), 'minutes')) / dminutes(1)
  times
}

walk(fs::dir_ls('R/utils'), source)
