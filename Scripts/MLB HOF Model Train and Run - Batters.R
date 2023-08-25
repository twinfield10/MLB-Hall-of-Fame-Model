#### BRING IN MODEL DATA ####
br_war<- pull_br_data()

bb <- get_lahman_batting()
bb <- append_age(bb)
bb <- append_br_war(bb, war=br_war$BattingWAR) 
bb <- append_pos(bb)
bb <- append_hof(bb)
bb <- append_ws_wins(bb)
bb <- append_pen_wins(bb)
bb <- append_div_wins(bb)
bb <- append_mvps(bb)
bb <- append_all_star(bb)
bb <- append_ROY(bb)
bb <- append_SS(bb)
bb <- append_GG(bb)
bb <- append_TC(bb)
bb <- append_steroids(bb)
bb <- append_jaws(bb)
bb <- append_HOFM(bb)
bb <- append_C_HOFM(bb)
bb %>% head(2) %>% print.data.frame()

#### Feature Generation ####
tmp <- bb %>% filter(POS %notin% c('SP', 'RP'))
fit_df_train <- get_fit_data(tmp, final_game_min='1901-01-01', final_game_max='2012-01-01') %>%
  filter(PA_1>=250 & playerID %notin% VetCom_IDs)

fit_df_test <- rbind(
  get_fit_data(tmp, final_game_min='2012-01-01', final_game_max='2022-12-31') %>%
  filter(PA_1>=250),
  get_fit_data(tmp, final_game_min='1901-01-01', final_game_max='2011-12-31') %>%
    filter(PA_1>=250 & playerID %in% VetCom_IDs)
) 

## Add Aggregate Features ##
fit_df_train <- fit_df_train %>%
  inner_join(get_aggregated_fit_df(fit_df_train), by='playerID') %>%
  select(-starts_with(c("WSWin_", "MVPWin_", "GG_Win_", "SS_Win_", "TC_Win_", "AllStar_", "C_HOFM_Points")))

fit_df_test <- rbind(
  fit_df_test %>%
  inner_join(get_aggregated_fit_df(fit_df_test), by='playerID') %>%
  select(-starts_with(c("WSWin_", "MVPWin_", "GG_Win_", "SS_Win_", "TC_Win_", "AllStar_", "C_HOFM_Points"))) %>%
  filter(playerID != 'altuvjo01'),
  multi_player_test
)


# Check #
head(fit_df_test %>% filter(playerID == 'mcgwima01'), 1) %>% print.data.frame()


#### Defining The Model ####
frm <- as.formula(inducted ~ . - playerID)

xx = model.matrix(frm, data=fit_df_train)[,-1]
yy = fit_df_train$inducted

cl <- makeCluster(detectCores())
registerDoParallel(cl)

set.seed(1123)
cv_train <- trainControl(method = "cv", number = 10)

xgbFit1 <- train(xx, yy,
                 method = "xgbLinear",
                 trControl = cv_train,
                 verbose = FALSE)
stopCluster(cl)

## Training Results ##
table(predict(xgbFit1), fit_df_train$inducted)

## Test Results ##
xx_test <- model.matrix(frm,data=fit_df_test)[,-1]
yy_test <- fit_df_test$inducted

results_df_test <- fit_df_test
results_df_test$predict <- 'N'
cc = which(predict(xgbFit1, newdata = xx_test) == 'Y')
results_df_test[cc,]$predict <- 'Y'
results_df_test$HOF_Prob <- predict(xgbFit1, newdata = xx_test, type = "prob")[,"Y"]

 ##### PREDICTIONS #####

## All Players - Active/Inactive ##
results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame), by="playerID") %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N')) %>%
  filter(TookSteroids == 0 & playerID %notin% VetCom_IDs ) %>%
  arrange(desc(predict), desc(HOF_Prob), desc(HOFM_Points), -WAA_1) %>%
  head(25) %>%
  select(nameLast, nameFirst, predict, HOF_Prob, inducted, jaws, HOFM_Points, WAA_1, POS, Active) %>% 
  print.data.frame()

## Eligible Players ##
results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame), by="playerID") %>%
  mutate(Eligible = if_else(finalGame <= "2017-01-01", 'Y', 'N')) %>%
  filter(TookSteroids == 0  & Eligible == 'Y' & playerID %notin% VetCom_IDs) %>%
  arrange(desc(predict), desc(HOF_Prob), desc(HOFM_Points), -WAA_1) %>% 
  head(25) %>% 
  select(nameLast, nameFirst, predict, HOF_Prob, inducted, jaws, HOFM_Points, WAA_1, POS, Eligible) %>% 
  print.data.frame()

## PED Players - Active/Inactive ##
results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame), by="playerID") %>%
  filter(TookSteroids == 1) %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N')) %>%
  arrange(desc(predict), desc(HOF_Prob), -WAA_1) %>%
  #head(25) %>% 
  select(nameLast, nameFirst, predict, HOF_Prob, inducted, jaws, HOFM_Points, WAA_1, POS, Active, TookSteroids) %>%
  print.data.frame()

## Potential VetCom Additions - Inactive ##
results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame), by="playerID") %>%
  filter(playerID %in% VetCom_IDs) %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N')) %>%
  arrange(desc(predict), desc(HOF_Prob), -WAA_1) %>%
  #head(25) %>% 
  select(nameLast, nameFirst, predict, HOF_Prob, inducted, jaws, HOFM_Points, WAA_1, POS, Active, TookSteroids) %>%
  print.data.frame()

## Active Players - Active/Inactive ##
results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame, debut, birthYear), by="playerID") %>%
  #Filter To Look at Active Players
  select(-starts_with(c("WAR_off_", "WAR_def_"))) %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N'),
         Deb_Age=as.numeric(substr(debut, 1, 4))-birthYear,
         Age=2023-birthYear,
         PA = rowSums(select(., starts_with("PA"))),
         WAR = rowSums(select(., starts_with("WAR_"))),
         WARPer100PA = ((WAR/PA)*100),
         WARPerYR=WAR/(Age-Deb_Age)
  ) %>%
  filter(Active == 'Y', PA >= 1500) %>%
  arrange(desc(predict), desc(HOF_Prob), desc(WARPerYR), -WAA_1) %>% 
  head(25) %>% 
  select(nameLast, nameFirst, WAR, WARPerYR, predict, HOF_Prob, jaws, HOFM_Points, WAA_1, Age) %>% 
  print.data.frame()





## Importance Scores ##

# Top 20 #
varImp(xgbFit1)$importance  %>% head(20)

# Check Table #
b_imp_scores <- varImp(xgbFit1)$importance
b_imp_scores["HOFM_Points", "Overall"]

colnames(results_df_test)

## Save Results Table As A CSV ##
# Results
write.csv(results_df_test, file = "C:\\Users\\TWinfield\\Desktop\\MLB HOF Model\\BattersResults_HOFModel.csv")