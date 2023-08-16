players <- lapply(clan$player_tag, function(tag){
  out <- NULL
  try(out <- cr_get_player(tag))
  out
}) %>% 
  bind_rows()

x <- players %>% 
  select(name, tag, cards) %>% 
  unnest(cards, names_sep = '_') %>% 
  left_join(card_amounts, by = c('cards_level', 'cards_max_level'))

x %>% 
  mutate(
    lv = cards_max_level - cards_level,
    value = case_when(
      lv == 0 ~ 'MAX',
      TRUE ~ paste0(min(cards_level, cards_to_upgrade), '/', cards_to_upgrade)
    )
  ) %>% 
  select(name, tag, cards_name, value) %>% 
  pivot_wider(id_cols = c(name, tag), names_from = cards_name, values_from = value) %>% 
  select(name, tag, card_labels$name) %>% 
  mutate(across(everything(), .fns = \(x) replace_na(x, '---'))) %>% 
  add_row(.before = 1) %>% 
  sheet_write(ss = sheet_url, sheet = 'test')
  

# insert urls for images
make_master_card_list() %>% select(name, image) %>% pivot_wider(names_from = name, values_from = image) 