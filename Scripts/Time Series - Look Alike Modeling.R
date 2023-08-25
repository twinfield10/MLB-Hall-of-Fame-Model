## Analyzing Active Players Over Time Series ##
build_player_df <- function(pID) {
  
  # Filter the data for the player and arrange by year
  df <- tmp %>%
    select(-C_HOFM_Points) %>%
    filter(playerID == pID) %>%
    arrange(yearID)
  
  # Define a function to build the DataFrame for a given year
  build_player_df <- function(year) {
    yr_df <- df %>%
      filter(yearID <= year) %>%
      select(-jaws, -d_jaws)
    
    yr_df <- append_jaws(yr_df)
    yr_df <- append_C_HOFM(yr_df)
    
    yr_df <- yr_df %>%
      get_fit_data(final_game_min = '2000-01-01', final_game_max = '2023-01-01', ORDER_KEY = "WAA") %>%
      inner_join(get_aggregated_fit_df(.), by = 'playerID') %>%
      select(-starts_with(c("WSWin_", "MVPWin_", "GG_Win_", "SS_Win_", "TC_Win_", "AllStar_", "C_HOFM_Points")))
    
    return(yr_df)
  }
  
  # Use purrr::map to build DataFrames for each year
  listofdfs <- purrr::map(unique(df$yearID), build_player_df)
  
  # Combine all DataFrames into one and arrange by 'Seasons'
  final_df <- bind_rows(listofdfs) %>%
    arrange(Seasons)
  
  # Add columns with zeros for missing cols
  col_add <- setdiff(names(fit_df_train), names(final_df))
  final_df[, col_add] <- 0
  final_df[is.na(final_df)] <- 0
  final_df <- final_df %>% select(colnames(fit_df_train)) %>% mutate(playerID = paste0(playerID,"-", yearID))
  
  # jaws adjustment #
  return(final_df)
}
build_full_player_year <- function(p_list){
  len_player <- length(p_list)
  
  listofplayerdfs <- list()
  for(i in 1:len_player){
    print(p_list[i])
    d <- build_player_df(pID = p_list[i])
    listofplayerdfs[[i]] <- d
  }
  final_df <- bind_rows(listofplayerdfs)
  return(final_df)
}
multi_player_test <- build_full_player_year(p_list = players_to_pivot)

altuve_test <- build_player_df(pID = 'altuvjo01')

year_candidates <- tmp %>%
  filter(finalGame >= "2022-01-01" & debut >= "2000-01-01") %>%
  group_by(playerID) %>%
  mutate(t_PA = sum(PA)) %>%
  filter(t_PA > 3000) %>%
  ungroup() %>%
  select(playerID) %>%
  unique() %>%
  pull()

players_to_pivot <- results_df_test %>%
  filter(HOF_Prob > 0.001 & playerID %in% year_candidates) %>%
  select(playerID) %>%
  filter(playerID %notin% c("bogaexa01"))
  unique() %>%
  pull()
  

## Create "Train Data" Based on YearID rather than WAA ##

#fit_df_train_year <- get_fit_data(tmp, final_game_min='1901-01-01', final_game_max='2023-01-01', ORDER_KEY="yearID") %>%
#  filter(PA_1>=250 & playerID %notin% VetCom_IDs)
#fit_df_train_year <- fit_df_train_year %>%
#  inner_join(get_aggregated_fit_df(fit_df_train_year), by='playerID') %>%
#  select(-starts_with(c("WSWin_", "MVPWin_", "GG_Win_", "SS_Win_", "TC_Win_", "AllStar_", "C_HOFM_Points")))

## PLOT ##
  install.packages("ggrepel")
  library(ggrepel)
  
  
  plot_data <- results_df_test %>% 
    merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame, debut, birthYear), by="playerID") %>%
    select(-starts_with(c("WAR_off_", "WAR_def_"))) %>%
    mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N'),
           Deb_Age=as.numeric(substr(debut, 1, 4))-birthYear,
           Age=Deb_Age+Seasons,
           PA = rowSums(select(., starts_with("PA"))),
           WAR = rowSums(select(., starts_with("WAR_"))),
           WARPer100PA = ((WAR/PA)*100),
           WARPerYR=WAR/(Seasons),
           Full_Name = paste(nameFirst, nameLast)
    ) %>%
    filter(Active == 'Y', PA >= 1500 & playerID %in% players_to_pivot & Full_Name != 'Albert Pujols') %>%
    arrange(desc(predict), desc(HOF_Prob), desc(WARPerYR), -WAA_1) %>% 
    select(Full_Name, Seasons, predict, HOF_Prob, Age)
  
  plot_data %>% select(Full_Name) %>% unique() %>% pull()
  
  plot_plyrs <- c('Jose Altuve', 'Aaron Judge', 'Bryce Harper', "Freddie Freeman", "Matt Olson")
  
  plot_data %>%
    group_by(Full_Name) %>%
    mutate(
      max_season = max(Seasons)
    ) %>%
    ungroup() %>%
    mutate(
      Label = if_else(Seasons == max_season, paste0(Full_Name, "-", percent(HOF_Prob, scale = 100, accuracy = 0.1)), if_else(HOF_Prob < 0.01,'', percent(HOF_Prob, scale = 100, accuracy = 0.1)))
    ) %>%
    filter(Full_Name %in% plot_plyrs) %>%
    ggplot(aes(x = Seasons, y = HOF_Prob, group = Full_Name)) +
    geom_line(aes(color = Full_Name), size = 1.2) +
    geom_label(data = . %>% filter(Label != ''), aes(label = Label), nudge_x = -0.35, size = 4) +
    labs(x = "Seasons", y = "Make HOF %") +
    ggtitle("Hall of Fame Probability Over Seasons", subtitle = "Among Active Batters (as of 2022)") +
    theme_bw() +
    theme(legend.position = "none") +
    scale_color_discrete() +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_log10(labels = percent_format(scale = 100)) +
    theme(legend.position = "none")


colnames(results_df_test)