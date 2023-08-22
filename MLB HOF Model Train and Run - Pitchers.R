#Pull In Data
br_war<- pull_br_data()

P_bb <- get_lahman_pitching()
P_bb <- append_age(P_bb)
P_bb <- p_append_br_war(P_bb, war=br_war$PitchingWAR)
P_bb <- append_pos(P_bb)
P_bb <- append_hof(P_bb)
P_bb <- append_ws_wins(P_bb)
P_bb <- append_mvps(P_bb)
P_bb <- append_all_star(P_bb)
P_bb <- append_cys(P_bb)
P_bb <- append_Rel_OY(P_bb)
P_bb <- append_PTC(P_bb)
P_bb <- append_GG(P_bb)
P_bb <- append_steroids(P_bb)
#Preview Data
P_bb %>% head(2) %>% print.data.frame()


##Feature Generation
#Pitchers
p_tmp <- P_bb %>% filter(POS=='P')
p_fit_df_train <- p_get_fit_data(p_tmp, final_game_min='1901-01-01', final_game_max='2012-01-01')%>%
  filter(IPouts_1>=10 & playerID %notin% ROID_IDS)
p_fit_df_test <- rbind(
  p_get_fit_data(p_tmp, final_game_min='2012-01-01', final_game_max='2022-31-12') %>%
  filter(IPouts_1>=10),
  p_get_fit_data(p_tmp, final_game_min='1901-01-01', final_game_max='2012-01-01')%>%
    filter(IPouts_1>=10 & playerID %in% ROID_IDS)
)

head(p_fit_df_train, 1) %>% print.data.frame()


## Weight Model For Steroids ##

# Create a sample weight vector for the "TookSteroids" variable
#sample_weights <- ifelse(p_fit_df_train$TookSteroids == 1, 99, 1)

## Defining The Model
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

##Training Results
table(predict(p_xgbFit1), p_fit_df_train$inducted)

##Test Results
p_xx_test <- model.matrix(p_frm,data=p_fit_df_test)[,-1]
p_yy_test <- p_fit_df_test$inducted

p_results_df_test <- p_fit_df_test
p_results_df_test$predict <- 'N'
p_cc = which(predict(p_xgbFit1, newdata = p_xx_test) == 'Y')
p_results_df_test[p_cc,]$predict <- 'Y'
p_results_df_test$HOF_Prob <- predict(p_xgbFit1, newdata = p_xx_test, type = "prob")[,"Y"]

##Prediction Iime!
p_results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame), by="playerID") %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N')) %>%
  arrange(desc(predict), desc(HOF_Prob), -WAA_1) %>%
  head(25) %>% 
  select(nameLast, nameFirst, predict, HOF_Prob, inducted, WAA_1, POS, Active) %>% 
  print.data.frame()

####ACTIVE PLAYERS
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
  filter(Active == 'Y', IP >= 500) %>%
  arrange(desc(predict), desc(HOF_Prob), desc(WARPerYR), -WAA_1) %>%
  head(25) %>% 
  select(nameLast, nameFirst, WAR, WARPerYR, predict, HOF_Prob, WAA_1, Age) %>%
  print.data.frame()

p_imp_scores <- varImp(p_xgbFit1)$importance

p_imp_scores["Median_WAR", "Overall"]
