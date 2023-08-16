clan <- cr_get_clan(clan = clan_tag) 
war <- cr_get_riverrace_current(clan = clan_tag)
past_war <- cr_get_riverrace_log(clan = clan_tag)
players <- lapply(clan$player_tag, function(tag){
  out <- NULL
  try(out <- cr_get_player(tag = tag))
  out
}) %>% 
  list_rbind()

clan_war <- war$clan[[1]] 

players <- players %>% 
  mutate(
    bronze = map_int(cards, \(x) sum(x$max_level - x$level <= 4)),
    silver = map_int(cards, \(x) sum(x$max_level - x$level <= 3)),
    gold = map_int(cards, \(x) sum(x$max_level - x$level <= 2)),
    legendary = map_int(cards, \(x) sum(x$max_level - x$level <= 1)),
    maxed = map_int(cards, \(x) sum(x$max_level - x$level <= 0)),
    elite = map_int(cards, \(x) sum(x$max_level - x$level < 0)),
  )

clan %>% 
  select(starts_with('player')) %>%
  rename_with(.fn = \(x) str_sub(x, start = 8)) %>% 
  select(-exp_level, -clan_rank, -previous_clan_rank, -starts_with('arena'), -clan_chest_points) %>% 
  mutate(
    last_seen_flag = time_min(last_seen) > (48 * 60),
    last_seen = time_short(last_seen)
  ) %>% 
  relocate(name, tag, last_seen, role, trophies, starts_with('donations')) %>% 
  left_join(
    players %>% select(tag, war_day_wins, elite, maxed, legendary, gold, silver, bronze),
    by = 'tag'
  ) %>% 
  gt() %>% 
  tab_spanner(label = 'Player', columns = c(name, tag)) %>% 
  tab_spanner(label = 'Donations', columns = starts_with('donations')) %>% 
  tab_spanner(label = 'Card Levels', columns = c(elite, maxed, legendary, gold, silver, bronze)) %>% 
  cols_label(
    name = 'Name',
    tag = 'Tag',
    last_seen = 'Last Seen',
    role = 'Role',
    trophies = 'Trophies',
    donations = 'Don.',
    donations_received = 'Rec.',
    war_day_wins = 'CW1 Wins',
    elite = 'E',
    maxed = 'M',
    legendary = 'L',
    gold = 'G',
    silver = 'S',
    bronze = 'B'
  ) %>% 
  cols_hide(last_seen_flag) %>% 
  data_color(
    columns = trophies,
    fn = scales::col_numeric(
      palette = c('white', '#b7e1cd'),
      na.color = 'white',
      domain = c(5000, 9000)
    )
  ) %>% 
  data_color(
    columns = donations,
    fn = scales::col_numeric(
      palette = c('#FFFFFFFF', '#b7e1cd'),
      domain = c(0, 200)
    )
  ) %>% 
  data_color(
    columns = last_seen_flag,
    target_columns = last_seen,
    method = 'factor',
    palette = c('#FFFFFFFF', '#982649')
  ) %>% 
  opt_table_font(
    font = google_font('Martel Sans')
  )

war_sheet <- left_join(
  war$clan[[1]] %>% 
    select(name = clan_participants_name, tag = clan_participants_tag, fame_11 = clan_participants_fame),
  bind_rows(past_war$standings, .id = 'week') %>% 
    tidyr::unnest(clan) %>% 
    filter(.data$clan_tag == paste0('#', .env$clan_tag)) %>% 
    rename(tag = clan_participants_tag) %>% 
    select(week, tag, fame = clan_participants_fame) %>% 
    pivot_wider(id_cols = tag, values_from = fame, names_from = week, 
                names_glue = '{str_pad(.value, 2)}_{str_pad(week, 2, pad = 0)}'),
  by = 'tag'
) %>% 
  relocate(fame_11, .after = everything()) #%>% # maybe 
  #mutate(across(starts_with('fame'), replace_na, 0))
war_sheet

# testing
me <- players %>% slice(1)
me$current_deck[[1]] %>% 
  mutate(level = 14 - (max_level - level)) %>% 
  select(-max_level) %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(columns = icon_urls), 
    fn = function(x) web_image(x)
  ) %>% 
  cols_hide(c(id, count, star_level)) %>% 
  cols_label(
    name = 'card',
    icon_urls = ''
  )
