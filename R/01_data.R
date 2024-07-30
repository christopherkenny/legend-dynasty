source(here::here('R/00_setup.R'))
if (Sys.getenv('username') != 'chris') {
  options(royale.use_proxy = TRUE)
}

# prep data ----

## clan ----
clan <- cr_get_clan(clan = clan_tag) 

clan |> 
  write_csv('data/clan.csv')

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

clan_war <- war |> unnest(clan)

war_sheet <- full_join(
  past_war |> 
    group_by(season_id) |> 
    mutate(week = section_index + 1)  |> 
    ungroup() |> 
    unnest_longer(standings) |> 
    unnest_wider(standings) |> 
    unnest(clan) |> unnest(clan) |> 
    filter(.data$clan_tag == paste0('#', .env$clan_tag)) |> 
    rename(
      tag = clan_participants_tag,
      name = clan_participants_name
    ) |> 
    select(season_id, week, name, tag, fame = clan_participants_fame) |> 
    arrange(season_id, week) |> 
    pivot_wider(id_cols = c(name, tag), values_from = fame, names_from = c(season_id, week), 
                names_glue = 'fame_{season_id}-{str_pad(week, 2, pad = 0)}'),
  war |> 
    unnest(clan) |> 
    select(name = clan_participants_name, tag = clan_participants_tag, 
           fame_11 = clan_participants_fame),

  by = c('tag', 'name')
) |> 
  relocate(fame_11, .after = everything())

# back out current war and week
curr_week <- {clan_war |> 
  pull(section_index) |> 
  pluck(1)} + 1
curr_war <- str_pad(ifelse(curr_week == 1, as.integer(str_extract(names(war_sheet), '\\d+')[12]) + 1, str_extract(names(war_sheet), '\\d+')[12]), 2, pad = 0)

# rename fame_11
names(war_sheet)[which(names(war_sheet) == 'fame_11')] <- str_glue('fame_{curr_war}-{curr_week}')

# write war to csv
war_sheet |> 
  write_csv('data/war.csv')


# Decks Used ----

decks_used <- full_join(
  past_war |> 
    group_by(season_id) |> 
    mutate(week = section_index + 1)  |> 
    ungroup() |> 
    unnest_longer(standings) |> 
    unnest_wider(standings) |> 
    unnest(clan) |> unnest(clan) |> 
    filter(.data$clan_tag == paste0('#', .env$clan_tag)) |> 
    rename(
      tag = clan_participants_tag,
      name = clan_participants_name
    ) |> 
    select(season_id, week, name, tag, decks_used = clan_participants_decks_used) |> 
    arrange(season_id, week) |> 
    pivot_wider(id_cols = c(name, tag), values_from = decks_used, names_from = c(season_id, week), 
                names_glue = 'decks_used_{season_id}-{str_pad(week, 2, pad = 0)}'),
  war |> 
    unnest(clan) |>
    select(name = clan_participants_name, tag = clan_participants_tag, 
           decks_used_11 = clan_participants_fame),
  by = c('name', 'tag')
) |> 
  relocate(decks_used_11, .after = everything())

# rename decks_used_11
names(decks_used)[which(names(decks_used) == 'decks_used_11')] <- str_glue('decks_used_{curr_war}-{curr_week}')

# write decks used to csv
decks_used |> 
  write_csv('data/decks_used.csv')

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
  select(name, tag, dplyr::any_of(card_labels$name)) |> 
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
