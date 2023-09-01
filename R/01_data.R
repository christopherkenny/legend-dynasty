source(here::here('R/00_setup.R'))
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
  mutate( # cards at least this level
    bronze = map_int(cards, \(x) sum(x$max_level - x$level <= 4)),
    silver = map_int(cards, \(x) sum(x$max_level - x$level <= 3)),
    gold = map_int(cards, \(x) sum(x$max_level - x$level <= 2)),
    legendary = map_int(cards, \(x) sum(x$max_level - x$level <= 1)),
    maxed = map_int(cards, \(x) sum(x$max_level - x$level <= 0)),
    elite = map_int(cards, \(x) sum(x$max_level - x$level < 0)),
  )

players |> 
  write_csv('data/players.csv')

## war ----
war <- cr_get_riverrace_current(clan = clan_tag)
past_war <- cr_get_riverrace_log(clan = clan_tag)

clan_war <- war$clan[[1]] 

war_sheet <- full_join(
  past_war |> 
    mutate(week = row_number())  |> 
    unnest_longer(standings) |> 
    unnest_wider(standings) |> 
    unnest(clan) |> unnest(clan) |> 
    filter(.data$clan_tag == paste0('#', .env$clan_tag)) |> 
    rename(tag = clan_participants_tag) |> 
    select(season_id, week, tag, fame = clan_participants_fame) |> 
    arrange(season_id, week) |> 
    pivot_wider(id_cols = tag, values_from = fame, names_from = c(season_id, week), 
                names_glue = 'fame_{season_id}-{str_pad(week, 2, pad = 0)}'),
  war$clan[[1]] |> 
    select(name = clan_participants_name, tag = clan_participants_tag, 
           fame_11 = clan_participants_fame),

  by = 'tag'
) |> 
  relocate(fame_11, .after = everything()) #|> # maybe 
#mutate(across(starts_with('fame'), replace_na, 0))
war_sheet
# get seasons can be used to back out column titles
cr_get_global_seasons() |> 
  tail(11) |> 
  pull(id)

# TODO: write war to csv

## cards ---
cards <- players |> 
  select(name, tag, cards) |> 
  unnest(cards, names_sep = '_') |> 
  left_join(card_amounts, by = c('cards_level', 'cards_max_level'))

cards <- cards |> 
  mutate(
    lv = cards_max_level - cards_level,
    value = case_when(
      lv == 0 ~ 'MAX',
      lv < 0 ~ 'ELITE',
      TRUE ~ paste0(pmin(cards_count, cards_to_upgrade), '/', cards_to_upgrade)
    )
  ) |> 
  select(name, tag, cards_name, value) |> 
  pivot_wider(id_cols = c(name, tag), names_from = cards_name, values_from = value) |> 
  select(name, tag, card_labels$name) |> 
  mutate(across(everything(), .fns = \(x) replace_na(x, '---')))


# insert urls for images
imgs <- make_master_card_list() |> 
  select(name, image) |> 
  pivot_wider(names_from = name, values_from = image) 

cards |> 
  add_row(imgs, .before = 1) |> 
  write_csv('data/cards.csv')
 
## boat
boat <- players |> 
  select(name, tag, cards) |> 
  unnest(cards, names_sep = '_') |> 
  left_join(card_amounts, by = c('cards_level', 'cards_max_level'))  |> 
  mutate(cards_level = (14 - cards_max_level) + cards_level) |> 
  select(name, tag, cards_name, cards_level) |> 
  pivot_wider(id_cols = c(name, tag), names_from = cards_name, values_from = cards_level) |> 
  select(name, tag, all_of(c('P.E.K.K.A', 'Mega Knight', 'Giant Skeleton',
                             'Royal Recruits', 'Bowler', 'Sparky', 'Minion Horde',
                             'Dark Prince', 'Prince', 'Executioner', 'Mini P.E.K.K.A',
                             'Elite Barbarians', 'Inferno Dragon', 'Barbarians', 
                             'Witch', 'Rascals', 'Golem', 'Wizard', 'Three Musketeers', 
                             'Electro Giant'))) |> 
  rowwise() |> 
  mutate(
    boat = sum(c_across(`P.E.K.K.A`:`Electro Giant`), na.rm = TRUE)
  ) |> 
  ungroup() |> 
  arrange(desc(boat)) |> 
  mutate(across(everything(), as.character))
boat |> 
  add_row(imgs |> select(all_of(intersect(names(imgs), names(boat)))), .before = 1) |> 
  write_csv('data/boat.csv')
