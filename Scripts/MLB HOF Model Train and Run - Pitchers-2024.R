#### BRING IN MODEL DATA ####
br_war<- pull_br_data()

P_bb <- get_lahman_pitching() %>% mutate(IPouts = round(IPouts/3, digits = 2))
P_bb <- append_age(P_bb)
P_bb <- p_append_br_war(P_bb, war=br_war$PitchingWAR)
P_bb <- append_pos(P_bb)
P_bb <- append_hof(P_bb)
P_bb <- append_post_pitch(P_bb)
P_bb <- append_mvps(P_bb)
P_bb <- append_all_star(P_bb)
P_bb <- append_cys(P_bb)
P_bb <- append_ROY(P_bb)
P_bb <- append_PTC(P_bb)
P_bb <- append_GG(P_bb)
P_bb <- append_steroids(P_bb)
P_bb <- append_p_jaws(P_bb)
P_bb <- p_append_HOFM(P_bb)
P_bb <- p_append_C_HOFM(P_bb)

#Preview Data
P_bb %>% filter(playerID == 'shielja02') %>% arrange(desc(HOFM_Points))  %>% head(3) %>% print.data.frame()
#### Feature Generation ####
p_tmp <- P_bb %>% filter(POS %in% c('SP', 'RP'))

p_fit_df_train <- p_get_fit_data(p_tmp, final_game_min='1901-01-01', final_game_max='2012-01-01')%>%
  filter(IPouts_1>=10 & playerID %notin% VetCom_IDs & playerID %notin% c('wagnebi02'))

p_fit_df_test <- rbind(
  p_get_fit_data(p_tmp, final_game_min='2012-01-01', final_game_max='2022-12-31') %>%
    filter(IPouts_1>=10),
  p_get_fit_data(p_tmp, final_game_min='1901-01-01', final_game_max='2011-12-31') %>%
    filter(IPouts_1>=10 & playerID %in% VetCom_IDs),
  p_get_fit_data(p_tmp, final_game_min='1901-01-01', final_game_max='2021-12-31') %>%
    filter(IPouts_1>=10 & playerID %in% c('wagnebi02'))
) 

p_fit_df_train %>% filter(playerID %in% HOF_Ballot_IDs) %>% select(playerID) %>% pull()



## Add Aggregate Features ##
p_fit_df_train <- p_fit_df_train %>%
  inner_join(p_get_aggregated_fit_df(p_fit_df_train), by='playerID') %>%
  select(-starts_with(c("MVPWin_", "GG_Win_", "CYWin_", "AllStar_", "C_HOFM_Points")))
p_fit_df_test <- p_fit_df_test %>%
  inner_join(p_get_aggregated_fit_df(p_fit_df_test), by='playerID') %>%
  select(-starts_with(c("MVPWin_", "GG_Win_", "CYWin_", "AllStar_", "C_HOFM_Points")))


# Check #
head(p_fit_df_test %>% filter(playerID == 'wagnebi02'), 1) %>% print.data.frame()

#### Defining The Model ####
p_frm <- as.formula(inducted ~ . - playerID)

p_xx = model.matrix(p_frm, data=p_fit_df_train)[,-1]
p_yy = p_fit_df_train$inducted

p_cl <- makeCluster(detectCores())
registerDoParallel(p_cl)

set.seed(420)
p_cv_train <- trainControl(method = "cv", number = 10)


p_xgbFit1 <- train(p_xx, p_yy,
                   method = "xgbLinear",
                   trControl = p_cv_train,
                   verbose = FALSE
                   )
stopCluster(p_cl)

## Training Results ##
table(predict(p_xgbFit1), p_fit_df_train$inducted)

## Test Results ##
p_xx_test <- model.matrix(p_frm,data=p_fit_df_test)[,-1]
p_yy_test <- p_fit_df_test$inducted

p_results_df_test <- p_fit_df_test
p_results_df_test$predict <- 'N'
p_cc = which(predict(p_xgbFit1, newdata = p_xx_test) == 'Y')
p_results_df_test[p_cc,]$predict <- 'Y'
p_results_df_test$HOF_Prob <- predict(p_xgbFit1, newdata = p_xx_test, type = "prob")[,"Y"]

##### PREDICTIONS #####

## All Players - Active/Inactive ##
p_results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame), by="playerID") %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N')) %>%
  filter(TookSteroids == 0 & playerID %notin% VetCom_IDs ) %>%
  arrange(desc(predict), desc(HOF_Prob), desc(HOFM_Points), -WAA_1) %>% 
  head(25) %>% 
  select(nameLast, nameFirst, predict, HOF_Prob, inducted, jaws, HOFM_Points, WAA_1, POS, Active) %>% 
  print.data.frame()

## Eligible Players ##
p_results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame), by="playerID") %>%
  mutate(Eligible = if_else(finalGame <= "2017-01-01", 'Y', 'N')) %>%
  filter(TookSteroids == 0  & Eligible == 'Y' & playerID %notin% VetCom_IDs) %>%
  arrange(desc(predict), desc(HOF_Prob), desc(HOFM_Points), -WAA_1) %>% 
  head(25) %>% 
  select(nameLast, nameFirst, predict, HOF_Prob, inducted, jaws, HOFM_Points, WAA_1, POS, Eligible) %>% 
  print.data.frame()

## PED Players - Active/Inactive ##
p_results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame), by="playerID") %>%
  filter(TookSteroids == 1) %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N')) %>%
  arrange(desc(predict), desc(HOF_Prob), -WAA_1) %>%
  #head(25) %>% 
  select(nameLast, nameFirst, predict, HOF_Prob, inducted, jaws, HOFM_Points, WAA_1, POS, Active, TookSteroids) %>%
  print.data.frame()

## Veterans Committee Potential Election Players - Active/Inactive ##
p_results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame), by="playerID") %>%
  filter(playerID %in% VetCom_IDs) %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N')) %>%
  arrange(desc(predict), desc(HOF_Prob), -WAA_1) %>%
  #head(25) %>% 
  select(nameLast, nameFirst, predict, HOF_Prob, inducted, jaws, HOFM_Points, WAA_1, POS, Active, TookSteroids) %>%
  print.data.frame()

## Active Players ##
p_results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, debut, finalGame, birthYear), by="playerID") %>%
  #Filter To Look at Active Players
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N'),
         Deb_Age=as.numeric(substr(debut, 1, 4))-birthYear,
         Age=2023-birthYear,
         IP = rowSums(select(., starts_with("IPouts"))),
         WAR = rowSums(select(., starts_with("WAR"))),
         WARPer100IP = ((WAR/IP)*100),
         WARPerYR=WAR/(Age-Deb_Age)
         ) %>%
  filter(Active == 'Y', IP >= 500, playerID %notin% VetCom_IDs) %>%
  arrange(desc(predict), desc(HOF_Prob), -WAA_1) %>%
  head(25) %>% 
  select(nameLast, nameFirst, predict, HOF_Prob, inducted, jaws, HOFM_Points, WAA_1, POS, Active, TookSteroids) %>%
  print.data.frame()

## Importance Scores ##

# Top 20 #
varImp(p_xgbFit1)$importance  %>% head(20)

# Check Table #
p_imp_scores <- varImp(p_xgbFit1)$importance
p_imp_scores["POS", "Overall"]

## Save Results Table As A CSV ##
write.csv(p_results_df_test, file = "C:\\Users\\TWinfield\\Desktop\\MLB HOF Model\\PitchersResults_HOFModel_2023.csv")