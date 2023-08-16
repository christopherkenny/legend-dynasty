suppressPackageStartupMessages({
  library(tidyverse)
  library(royale)
  library(gt)
  library(googlesheets4)
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

# prep data ----

## clan ----
clan <- cr_get_clan(clan = clan_tag) 

## player ----
players <- lapply(clan$player_tag, function(tag){
  out <- NULL
  try(out <- cr_get_player(tag = tag))
  out
}) |> 
  list_rbind()

players <- players |> 
  mutate(
    bronze = map_int(cards, \(x) sum(x$max_level - x$level <= 4)),
    silver = map_int(cards, \(x) sum(x$max_level - x$level <= 3)),
    gold = map_int(cards, \(x) sum(x$max_level - x$level <= 2)),
    legendary = map_int(cards, \(x) sum(x$max_level - x$level <= 1)),
    maxed = map_int(cards, \(x) sum(x$max_level - x$level <= 0)),
    elite = map_int(cards, \(x) sum(x$max_level - x$level < 0)),
  )

## war ----
war <- cr_get_riverrace_current(clan = clan_tag)
past_war <- cr_get_riverrace_log(clan = clan_tag)

clan_war <- war$clan[[1]] 

