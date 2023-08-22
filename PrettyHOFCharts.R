## Build Helper Functions ##

## Color Scales ##
CareerWAR_Col_Func <- function(x) {
  ifelse(x < -100, "#ff0000",
         ifelse(x < -90 & x >= -100, "#ff1a1a",
                ifelse(x < -80 & x >= -90, "#ff3333",
                       ifelse(x < -70 & x >= -80, "#ff4d4d",
                              ifelse(x < -60 & x >= -70, "#ff6666",
                                     ifelse(x < -50 & x >= -60, "#ff8080",
                                            ifelse(x < -40 & x >= -50, "#ff9999",
                                                   ifelse(x < -30 & x >= -40, "#ffb3b3",
                                                          ifelse(x < -20 & x >= -30, "#ffcccc",
                                                                 ifelse(x < -10 & x >= -20, "#ffe6e6",
                                                                        ifelse(x >= -10 & x <= 10, "#ffffff",
                                                                               ifelse(x < 20 & x > 10, "#eafaf1",
                                                                                      ifelse(x < 30 & x >= 20, "#98E6BB",
                                                                                             ifelse(x < 40 & x >= 30, "#c1f0d6",
                                                                                                    ifelse(x < 50 & x >= 40, "#98e6bb",
                                                                                                           ifelse(x < 60 & x >= 50, "#6fdca0",
                                                                                                                  ifelse(x < 70 & x >= 60, "#46d285",
                                                                                                                         ifelse(x < 80 & x >= 70, "#2db96c",
                                                                                                                                ifelse(x < 90 & x >= 80, "#28a460",
                                                                                                                                       ifelse(x < 100 & x >= 90, "#239054",
                                                                                                                                              ifelse(x >= 100, "#1e7b48", NA)))))))))))))))))))))
}
WARBest_Col_Func <- function(x) {
  ifelse(x < 0, "#ff6666",
         ifelse(x < 2 & x >= 0, "#ffcccc",
                ifelse(x < 3 & x >= 2, "#ffffb3",
                       ifelse(x < 4 & x >= 3, "#DeF7E9",
                              ifelse(x < 5 & x >= 4, "#c1f0d6",
                                     ifelse(x < 6 & x >= 5, "#98e6bb",
                                            ifelse(x < 6.5 & x >= 6, "#98e6bb",
                                                   ifelse(x < 7 & x >= 6.5, "#6fdca0",
                                                          ifelse(x < 7.5 & x >= 7, "#46d285",
                                                                 ifelse(x < 8 & x >= 7.5, "#2db96c",
                                                                        ifelse(x < 8.5 & x >= 8, "#28a460",
                                                                               ifelse(x < 9 & x >= 8.5, "#239054",
                                                                                      ifelse(x >= 9 , "#1e7b48",
                                                                                             NA)))))))))))))
}
WAR_Col_Func <- function(x) {
  ifelse(x < 0, "#ff6666",
         ifelse(x < 1 & x >= 0, "#ffcccc",
                ifelse(x < 2 & x >= 1, "#ffffb3",
                       ifelse(x < 3 & x >= 2, "#DeF7E9",
                              ifelse(x < 4 & x >= 3, "#98e6bb",
                                     ifelse(x < 5 & x >= 4, "#37B96F",
                                            ifelse(x < 6 & x >= 5, "#28a460",
                                                   ifelse(x >= 6, "#1e7b48",
                                                          NA))))))))
}

# Case_when logic for team abbreviation replacement
team_abbreviation_mapping <- function(x) {
  case_when(
    x == 'NYA' ~ 'NYY',
    x == "SLN" ~ 'STL',
    x == 'KCA' ~ 'KC',
    x == 'CHN' ~ 'CHC',
    x == 'ARI' ~ 'AZ',
    x == 'SDN' ~ 'SD',
    x == 'TBA' ~ 'TB',
    x == 'ANA' ~ 'LAA',
    x == 'FLO' ~ 'MIA',
    x == 'CHA' ~ 'CWS',
    x == 'NYN' ~ 'NYM',
    x == 'SFN' ~ 'SF',
    x == 'WAS' ~ 'WSH',
    x == 'LAN' ~ 'LAD',
    TRUE ~ x
  )
}

# Custom function to replace NA values
replace_na_rows <- function(row) {
  non_na_values <- na.omit(row)
  if (length(non_na_values) == 0) {
    return(row)
  }
  na_positions <- is.na(row)
  row[na_positions] <- non_na_values
  return(row)
}


## Build Player Team Rosters ##

# Pitchers #
PITCH_TM <- Lahman::Pitching %>%
  select(playerID, teamID, G) %>%
  mutate(across(teamID, ~ team_abbreviation_mapping(.))) %>%
  group_by(playerID, teamID) %>%
  summarise(
    Total_Games = sum(G)
  ) %>%
  ungroup() %>%
  arrange(desc(Total_Games)) %>%
  group_by(playerID) %>%
  mutate(
    team_count = n_distinct(teamID),
    team_order = row_number()
    ) %>%
  group_by(playerID, Total_Games) %>%
  mutate(duplicate_id = row_number()) %>%
  pivot_wider(
    id_cols = c(playerID, team_count, duplicate_id),
    names_from = team_order,
    values_from = teamID,
    names_prefix = "Team_"
  ) %>%
  select(playerID, starts_with("Team_")) %>%
  rename(tm_cnt = team_count) %>%
  group_by(playerID, tm_cnt) %>%
  reframe(across(starts_with("Team_"), replace_na_rows)) %>%
  distinct(playerID, tm_cnt, .keep_all = TRUE) %>%
  mutate(
    Primary_Team = Team_1
  ) %>%
  select(playerID, Primary_Team, tm_cnt, starts_with("Team_"))

# Batters #
BAT_TM <- Lahman::Batting %>%
  select(playerID, teamID, G) %>%
  mutate(across(teamID, ~ team_abbreviation_mapping(.))) %>%
  group_by(playerID, teamID) %>%
  summarise(
    Total_Games = sum(G)
  ) %>%
  ungroup() %>%
  arrange(desc(Total_Games)) %>%
  group_by(playerID) %>%
  mutate(
    team_count = n_distinct(teamID),
    team_order = row_number()
  ) %>%
  group_by(playerID, Total_Games) %>%
  mutate(duplicate_id = row_number()) %>%
  pivot_wider(
    id_cols = c(playerID, team_count, duplicate_id),
    names_from = team_order,
    values_from = teamID,
    names_prefix = "Team_"
  ) %>%
  select(playerID, starts_with("Team_")) %>%
  rename(tm_cnt = team_count) %>%
  group_by(playerID, tm_cnt) %>%
  reframe(across(starts_with("Team_"), replace_na_rows)) %>%
  distinct(playerID, tm_cnt, .keep_all = TRUE) %>%
  mutate(
    Primary_Team = Team_1
  ) %>%
  select(playerID, Primary_Team, tm_cnt, starts_with("Team_"))
#### END SET UP ####

#results_df_test %>% filter(playerID == 'arenano01') %>% select(starts_with("WAR"))
#colnames(results_df_test)
#valid_team_names(remove_league_info = FALSE)


## Plot Eligible Players and Probability ##
eligible_retired <- function(type = 'BAT',
                             Roids = c(1,0),
                             RecentRetire = c(1,0),
                             active_ply = c('Y', 'N'),
                             lim = 20,
                             sub_type = c("1B", "2B", "3B", "SS", "C", "OF", "P"),
                             age = 100,
                             elig = c('Y', 'N'),
                             tm = valid_team_names()){
  
  #sub_type <- if_else(type == 'PIT', c('P'), sub_type)
  
  TeamDF <- if(type == 'PIT'){
    PITCH_TM
  } else if (type == 'BAT'){
    BAT_TM
  }
  
  elig <- if(length(active_ply) == 1 & active_ply == 'Y'){
    'N'
  } else {
    elig
  }
  
  df <- if(type == 'PIT'){
    p_results_df_test %>%
      mutate(
        WAR = rowSums(select(., starts_with("WAR_")))
      )
  } else if (type == 'BAT'){
    results_df_test %>%
      select(-starts_with(c("WAR_off_", "WAR_def_"))) %>%
      mutate(
        WAR = rowSums(select(., starts_with("WAR_")))
      )
  }
  #print(df)
  
  df<- df %>%
    merge(Lahman::People %>% select(playerID, nameLast, nameFirst, debut, finalGame, birthYear), by="playerID") %>%
    left_join(TeamDF, by="playerID") %>%
    mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N'),
           Deb_Age=as.numeric(substr(debut, 1, 4))-birthYear,
           Ret_Age=as.numeric(substr(finalGame, 1, 4))-birthYear,
           Seasons = Ret_Age - Deb_Age,
           Age=2022-birthYear,
           WARPerYR=Mean_WAR/Seasons,
           Active = case_when(
             nameFirst == 'Cole' & nameLast == 'Hamels' ~ 'N',
             nameFirst == 'Anibal' & nameLast == 'Sanchez' ~ 'N',
             nameFirst == 'Matt' & nameLast == 'Harvey' ~ 'N',
             nameFirst == 'Sergio' & nameLast == 'Romo' ~ 'N',
             nameFirst == 'Steve' & nameLast == 'Cishek' ~ 'N',
             nameFirst == 'Robinson' & nameLast == 'Chirinos' ~ 'N',
             nameFirst == 'Travis' & nameLast == 'Shaw' ~ 'N',
             nameFirst == 'Dustin' & nameLast == 'Garneau' ~ 'N',
             nameFirst == 'Jason' & nameLast == 'Castro' ~ 'N',
             nameFirst == 'Kurt' & nameLast == 'Suzuki' ~ 'N',
             nameFirst == 'Jed' & nameLast == 'Lowrie' ~ 'N',
             nameFirst == 'Stephen' & nameLast == 'Vogt' ~ 'N',
             nameFirst == 'Steven' & nameLast == 'Souza' ~ 'N',
             nameFirst == 'David' & nameLast == 'Phelps' ~ 'N',
             nameFirst == 'Albert' & nameLast == 'Pujols' ~ 'N',
             nameFirst == 'Lorenzo' & nameLast == 'Cain' ~ 'N',
             nameFirst == 'Craig' & nameLast == 'Stammen' ~ 'N',
             nameFirst == 'Yadier' & nameLast == 'Molina' ~ 'N',
             nameFirst == 'Jake' & nameLast == 'McGee' ~ 'N',
             TRUE ~ Active
           ),
           Retired_23 = case_when(
             nameFirst == 'Cole' & nameLast == 'Hamels' ~ 1,
             nameFirst == 'Anibal' & nameLast == 'Sanchez' ~ 1,
             nameFirst == 'Matt' & nameLast == 'Harvey' ~ 1,
             nameFirst == 'Sergio' & nameLast == 'Romo' ~ 1,
             nameFirst == 'Steve' & nameLast == 'Cishek' ~ 1,
             nameFirst == 'Robinson' & nameLast == 'Chirinos' ~ 1,
             nameFirst == 'Travis' & nameLast == 'Shaw' ~ 1,
             nameFirst == 'Dustin' & nameLast == 'Garneau' ~ 1,
             nameFirst == 'Jason' & nameLast == 'Castro' ~ 1,
             nameFirst == 'Kurt' & nameLast == 'Suzuki' ~ 1,
             nameFirst == 'Jed' & nameLast == 'Lowrie' ~ 1,
             nameFirst == 'Stephen' & nameLast == 'Vogt' ~ 1,
             nameFirst == 'Steven' & nameLast == 'Souza' ~ 1,
             nameFirst == 'David' & nameLast == 'Phelps' ~ 1,
             nameFirst == 'Albert' & nameLast == 'Pujols' ~ 1,
             nameFirst == 'Lorenzo' & nameLast == 'Cain' ~ 1,
             nameFirst == 'Craig' & nameLast == 'Stammen' ~ 1,
             nameFirst == 'Yadier' & nameLast == 'Molina' ~ 1,
             nameFirst == 'Jake' & nameLast == 'McGee' ~ 1,
             TRUE ~ 0
           ),
           Eligible = if_else(finalGame <= "2018-01-01" & Seasons >= 10 & Active == 'N', 'Y', 'N'),
           Inducted = inducted,
           Name = paste0(nameFirst, " ", nameLast),
           HOF_Prob = round(HOF_Prob, digits = 4)
    ) %>%
    arrange(desc(predict), desc(HOF_Prob), desc(WARPerYR), -WAR_1) %>%
    filter(TookSteroids %in% Roids) %>%
    filter(Retired_23 %in% RecentRetire) %>%
    filter(Active %in% active_ply) %>%
    filter(POS %in% sub_type) %>%
    filter(Age < age) %>%
    filter(WAR > 10) %>%
    filter(Eligible %in% elig)
  
  df <- if(length(tm) == 33){
    df %>%
      select(Name, Age, POS, Primary_Team, Active, WAR, WARPerYR, WAR_1, predict, HOF_Prob, Eligible, Inducted) %>%
      head(lim)
  } else if(length(tm) == 1) {
    df %>%
      filter(if_any(starts_with("Team_"), ~ . == tm)) %>%
      mutate(Primary_Team = tm) %>%
      select(Name, Age, POS, Primary_Team, Active, WAR, WARPerYR, WAR_1, predict, HOF_Prob, Eligible, Inducted) %>%
      head(lim)
  } else {
    df %>%
      select(Name, Age, POS, Primary_Team, Active, WAR, WARPerYR, WAR_1, predict, HOF_Prob, Eligible, Inducted) %>%
      head(lim)
  }

  # Create Subtitle Label #
  act_lab <- if(length(active_ply) == 2){
    "All"
  } else if(length(active_ply == 1) & active_ply == 'Y') {
    'All Active'
  } else if(length(active_ply == 1) & active_ply == 'N' & length(elig) == 2) {
    'All Retired'
  } else if(length(active_ply == 1) & elig == 'Y'){
    'All Eligible'
  } else {
    print("Empty DataFrame - Try Again")
  }
  
  pos_lab <- if_else(type == 'PIT', 'Pitchers', if_else(type == 'BAT' & length(sub_type) == 7, 'Batters', paste(sub_type, collapse = ", ")))
  roid_lab <- if_else(Roids == 1, "(Including PED Users)", '')
  age_lab <- if_else(age == 100, '', paste0('Under ',age))
  final_lab <- paste("Liklihood of Entering the MLB Hall of Fame Among", act_lab, pos_lab, age_lab, roid_lab, "As Of 1/1/2023", sep = ' ')

  # Create a gt object with specified formatting
  gt_obj <- df %>%
    gt() %>%
    cols_label(
      Name = md("Name"),
      Age = md("Age"),
      POS = md("Pos."),
      Primary_Team = md("Team"),
      Active = md("Active"),
      WAR = md("Career"),
      WARPerYR = md("Per Year"),
      WAR_1 = md("Best Season"),
      predict = md("Make HOF"),
      HOF_Prob = md("HOF %"),
      Eligible = md("Eligible '23"),
      Inducted = md("Inducted")
    ) %>%
    tab_spanner(
      label = "Wins Above Replacement",
      columns = c(WAR, WARPerYR, WAR_1),
      id = 'WARP'
    ) %>%
    tab_spanner(
      label = "Player Info",
      columns = c(Name, Age, POS, Primary_Team, Active),
      id = 'PLY'
    ) %>%
    tab_spanner(
      label = "Prediction",
      columns = c(predict, HOF_Prob),
      id = 'PRED'
    ) %>%
    tab_spanner(
      label = "Result",
      columns = c(Eligible, Inducted),
      id = 'RESULT'
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("right", "left", "bottom"),
          color = "#BBBBBB",
          weight = px(2)
        ),
        cell_text(
          weight = "bold",
          style = "italic",
          color = "white"
        ),
        cell_fill(
          color = "#2F4985"
        )
      ),
      locations = cells_column_spanners(spanners = c("WARP", "PLY", "PRED", "RESULT"))
    ) %>%
    tab_style(
      locations = cells_column_labels(),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black"
        )
      )
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c(Active, WAR_1, HOF_Prob)),
      style = list(
        css(
          color = "black",
          text_align = "center",
          font_weight = "bold",
          border_bottom = "2px solid black",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = Name),
      style = list(
        css(
          text_align = "left",
          font_weight = "bold"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(Age, POS, Primary_Team, WARPerYR, WAR_1)),
      style = list(
        css(
          text_align = "center"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(Active, WAR_1)),
      style = list(
        css(
          text_align = "center",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(WAR)),
      style = list(
        css(
          text_align = "center",
          font_weight = "bold"
        )
      )
    ) %>%
    tab_style(
      locations = cells_body(columns = c(HOF_Prob)),
      style = list(
        css(
          text_align = "center",
          font_weight = "bold",
          border_right = "2px solid #BBBBBB"
        )
      )
    ) %>%
    gt_fmt_mlb_scoreboard_logo((columns = "Primary_Team")) %>%
    tab_style(
      locations = cells_body(columns = c(predict), rows = df$predict == 'Y'),
      style = list(css(background_color = "#E7C548", font_weight = "bold", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(Inducted), rows = df$Inducted == 'Y'),
      style = list(css(background_color = "#E7C548", font_weight = "bold", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(Eligible), rows = df$Eligible == 'Y'),
      style = list(css(background_color = "#A6E2B3", font_weight = "bold", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(predict), rows = df$predict == 'N'),
      style = list(css(background_color = "#FFFFFF", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(Inducted), rows = df$Inducted == 'N'),
      style = list(css(background_color = "#FFFFFF", text_align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(Eligible), rows = df$Eligible == 'N'),
      style = list(css(background_color = "#FFFFFF", text_align = "center"))
    ) %>%
    data_color(
      columns = `WAR`,
      fn = CareerWAR_Col_Func
    ) %>%
    data_color(
      columns = `WARPerYR`,
      fn = WAR_Col_Func
    ) %>%
    data_color(
      columns = `WAR_1`,
      fn = WARBest_Col_Func
    ) %>%
    data_color(
      columns = `HOF_Prob`,
      colors = scales::col_numeric(palette = c("#FFFFFF", "#71CA97"),
                                   domain = c(0, 1))
    ) %>%
    fmt_percent(
      columns = HOF_Prob,
      decimals = 2
    ) %>%
    fmt_number(
      columns = c(WAR),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(WARPerYR, WAR_1),
      decimals = 2
    ) %>%
    tab_header(
      title = md(
        paste0("<span style='color:white'>**Cooperstown Liklihood**</style>")
      ),
      subtitle = md(
        paste0("<span style='color:white'>*", final_lab,"*</style>")
      )
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = "3px",
      table.border.top.color = "#BBBBBB",
      table.border.right.style = "solid",
      table.border.right.width = "3px",
      table.border.right.color = "#BBBBBB",
      table.border.bottom.style = "solid",
      table.border.bottom.width = "3px",
      table.border.bottom.color = "#BBBBBB",
      table.border.left.style = "solid",
      table.border.left.width = "3px",
      table.border.left.color = "#BBBBBB",
      heading.background.color = "#001C5D"
      
    )
return(gt_obj)  

}
eligible_retired(type = 'PIT',
                 Roids = 0,
                 active_ply = 'N',
                 #sub_type = c('3B'),
                 #tm = 'HOU',
                 elig = 'Y',
                 age = 100,
                 lim = 10
                 )


