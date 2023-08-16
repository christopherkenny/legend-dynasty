sheet_url <- 'https://docs.google.com/spreadsheets/d/16lx1oL3S9i6laOAMjAl4E9PbW5Djwz6-8BMS5aSTwG0/edit#gid=871948399'

get_card_sheet <- function() {
  googlesheets4::read_sheet(sheet_url, sheet = 'Cards')
}

