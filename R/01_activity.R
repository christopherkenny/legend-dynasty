
# testing
me <- players |> slice(1)
me$current_deck[[1]] |> 
  mutate(level = 14 - (max_level - level)) |> 
  select(-max_level) |> 
  gt() |> 
  text_transform(
    locations = cells_body(columns = icon_urls), 
    fn = function(x) web_image(x)
  ) |> 
  cols_hide(c(id, count, star_level)) |> 
  cols_label(
    name = 'card',
    icon_urls = ''
  )
