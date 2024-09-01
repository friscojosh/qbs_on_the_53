# a simple script, to be run daily, to track the roster composition of NFL teams
# throughout the 2024 season, especially at QB.

library(rvest)
library(tidyverse)
library(gt)
library(gtExtras)
library(DBI)
library(RSQLite)

date <- Sys.Date()

team_short_names <- c("BUF", "MIA", "NE", "NYJ", "BAL", "CIN", "CLE", "PIT", "HOU",
                      "IND", "JAX", "TEN", "DEN", "KC", "LV", "LAC", "DAL", "NYG",
                      "PHI", "WAS", "CHI", "DET", "GB", "MIN", "ATL", "CAR", "NO",
                      "TB", "ARZ", "LAR", "SF", "SEA")

get_team_roster <- function(team_short_name) {
  # .table
  # //*[@id="ctl00_phContent_dcTBody"]
  url <- glue::glue("https://www.ourlads.com/nfldepthcharts/roster/{team_short_name}")
  
  print(glue::glue("Getting {team_short_name}"))
  
  roster <- read_html(url)
  
  roster_table <- roster  |> 
    html_nodes(xpath='//*[@id="ctl00_phContent_dcTBody"]')  |> 
    html_table()
  
  roster_table <- roster_table[[1]]
  colnames(roster_table) <- c("jersey", "name", "positon", "dob", "age", "height", 
                              "weight", "school", "orginal_team", "draft", "experience")
  roster_table <- roster_table |> mutate(team = team_short_name)
  # select just the 53 man roster
  the_53 <- roster_table[2:54,] |>
    # some teams are still only at 52 men so we need to remove this header row
    filter(jersey != "Practice Squad")
}

rosters <- map(team_short_names, get_team_roster) |> map_dfr(bind_rows)

rosters_by_position <- rosters |> 
  group_by(team, positon) |>
  summarize(count = n())

qb_number_by_team <- rosters |>
  filter(positon == "QB") |>
  group_by(team) |>
  summarize(qb_count = n()) |>
  mutate(date = date)

# connect to the DB
db <- dbConnect(SQLite(), "data/sqlite/qbs_on_the_53.sqlite")
# write
dbWriteTable(db, append = TRUE, "qbs_on_53", qb_number_by_team)
# disconnect from db  
dbDisconnect(db)

# pretty tables

# potentially sharp teams
# qb_number_by_team |>
#   filter(qb_count == 3) |> 
#   # fix team short names
#   mutate(team = ifelse(team == "ARZ", "ARI", team)) |> 
#   mutate(team = ifelse(team == "LAR", "LA", team)) |> 
#   left_join(
#     nflreadr::load_teams() |> 
#       filter(!team_abbr %in% c("LAR", "OAK", "SD", "STL")) |> 
#       select(team_abbr, team_name),
#     by = c("team" = "team_abbr")) |> 
#   select(team, team_name, qb_count) |> 
#   arrange(qb_count, team_name) |> 
#   gt() |>
#   gt_theme_538() |> 
#   cols_label(
#     team = "",
#     team_name = "TEAM",
#     qb_count = "QBs"
#   ) |> 
#   gt::opt_table_font(
#     font = list(
#       gt::google_font("Hanken Grotesk"),
#       gt::default_fonts()
#     ),
#     weight = 300
#   ) |> 
#   gt::tab_style(
#     locations = gt::cells_body(columns = team_name),
#     style = gt::cell_text(weight = "bold")
#   ) |> 
#   gt::tab_style(
#     style = gt::cell_borders(
#       sides = "bottom", color = "black", weight = gt::px(1)
#     ),
#     locations = gt::cells_row_groups()
#   ) |>
#   gt::tab_style(
#     locations = gt::cells_body(columns = qb_count),
#     style = gt::cell_text(weight = "Medium 500")
#   ) |> 
#   gt::text_transform(
#     locations = gt::cells_body(gt::ends_with("team")),
#     fn = function(x){
#       url <- data.frame(team_abbr = x) %>%
#         left_join(
#           nflreadr::load_teams() |> 
#             filter(!team_abbr %in% c("LAR", "OAK", "SD", "STL")) %>%
#             select(team_abbr, team_logo_espn),
#           by = "team_abbr"
#         ) |> 
#         pull(team_logo_espn)
#       gt::web_image(url = url, height = 30)
#     }) |> 
#   gt::cols_width(
#     qb_count ~     gt::px(60)
#   ) |> 
#   gt::tab_style(
#     locations = gt::cells_column_labels(),
#     style = list(
#       gt::cell_text(align = "center", weight = "bold", size = pct(75))
#     )
#   )
# 
# # teams that still dont get it
# qb_number_by_team |>
#   filter(qb_count == 2) |> 
#   # fix team short names
#   mutate(team = ifelse(team == "ARZ", "ARI", team)) |> 
#   mutate(team = ifelse(team == "LAR", "LA", team)) |> 
#   left_join(
#     nflreadr::load_teams() |> 
#       filter(!team_abbr %in% c("LAR", "OAK", "SD", "STL")) |> 
#       select(team_abbr, team_name),
#     by = c("team" = "team_abbr")) |> 
#   select(team, team_name, qb_count) |> 
#   arrange(qb_count, team_name) |> 
#   gt() |>
#   gt_theme_538() |> 
#   cols_label(
#     team = "",
#     team_name = "TEAM",
#     qb_count = "QBs"
#   ) |> 
#   gt::opt_table_font(
#     font = list(
#       gt::google_font("Hanken Grotesk"),
#       gt::default_fonts()
#     ),
#     weight = 300
#   ) |> 
#   gt::tab_style(
#     locations = gt::cells_body(columns = team_name),
#     style = gt::cell_text(weight = "bold")
#   ) |> 
#   gt::tab_style(
#     style = gt::cell_borders(
#       sides = "bottom", color = "black", weight = gt::px(1)
#     ),
#     locations = gt::cells_row_groups()
#   ) |>
#   gt::tab_style(
#     locations = gt::cells_body(columns = qb_count),
#     style = gt::cell_text(weight = "Medium 500")
#   ) |> 
#   gt::text_transform(
#     locations = gt::cells_body(gt::ends_with("team")),
#     fn = function(x){
#       url <- data.frame(team_abbr = x) %>%
#         left_join(
#           nflreadr::load_teams() |> 
#             filter(!team_abbr %in% c("LAR", "OAK", "SD", "STL")) %>%
#             select(team_abbr, team_logo_espn),
#           by = "team_abbr"
#         ) |> 
#         pull(team_logo_espn)
#       gt::web_image(url = url, height = 30)
#     }) |> 
#   gt::cols_width(
#     qb_count ~     gt::px(60)
#   ) |> 
#   gt::tab_style(
#     locations = gt::cells_column_labels(),
#     style = list(
#       gt::cell_text(align = "center", weight = "bold", size = pct(75))
#     )
#   )
