# players <- lapply(clan$player_tag, function(tag){
#   out <- NULL
#   try(out <- cr_get_player(tag))
#   out
# }) |> 
#   bind_rows()

x <- players |> 
  select(name, tag, cards) |> 
  unnest(cards, names_sep = '_') |> 
  left_join(card_amounts, by = c('cards_level', 'cards_max_level'))

x <- x |> 
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

x |> 
  add_row(imgs, .before = 1) |> 
  write_csv('data/cards.csv')
