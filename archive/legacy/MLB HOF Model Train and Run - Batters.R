#Pull In Data
br_war<- pull_br_data()

bb <- get_lahman_batting()
bb <- append_age(bb)
bb <- append_br_war(bb, war=br_war$BattingWAR)
bb <- append_pos(bb)
bb <- append_hof(bb)
bb <- append_ws_wins(bb)
bb <- append_mvps(bb)
bb <- append_all_star(bb)
bb <- append_SS(bb)
bb <- append_GG(bb)
bb <- append_TC(bb)
bb <- append_steroids(bb)
bb %>% head(2) %>% print.data.frame()

##Feature Generation
#Batters
tmp <- bb %>% filter(POS!='P')
fit_df_train <- get_fit_data(tmp, final_game_min='1901-01-01', final_game_max='2012-01-01') %>%
  filter(PA_1>=30 & playerID %notin% ROID_IDS)
fit_df_test <- rbind(
  get_fit_data(tmp, final_game_min='2012-01-01', final_game_max='2022-12-31') %>%
  filter(PA_1>=30),
  get_fit_data(tmp, final_game_min='1901-01-01', final_game_max='2012-01-01')%>%
    filter(PA_1>=10 & playerID %in% ROID_IDS)
)

head(fit_df_train, 1) %>% print.data.frame()


## Defining The Model
frm <- as.formula(inducted ~ . - playerID)

xx = model.matrix(frm, data=fit_df_train)[,-1]
yy = fit_df_train$inducted

cl <- makeCluster(detectCores())
registerDoParallel(cl)

set.seed(1123)
cv_train <- trainControl(method = "cv", number = 10)

xgbFit1 <- train(xx, yy, method = "xgbLinear", trControl = cv_train, verbose = FALSE)
stopCluster(cl)

##Training Results
table(predict(xgbFit1), fit_df_train$inducted)

##Test Results
xx_test <- model.matrix(frm,data=fit_df_test)[,-1]
yy_test <- fit_df_test$inducted

results_df_test <- fit_df_test
results_df_test$predict <- 'N'
cc = which(predict(xgbFit1, newdata = xx_test) == 'Y')
results_df_test[cc,]$predict <- 'Y'
results_df_test$HOF_Prob <- predict(xgbFit1, newdata = xx_test, type = "prob")[,"Y"]

##Prediction Iime!
results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame), by="playerID") %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N')) %>%
  arrange(desc(predict), desc(HOF_Prob), -WAA_1) %>% 
  head(25) %>% 
  select(nameLast, nameFirst, predict, HOF_Prob, inducted, WAA_1, POS, Active) %>% 
  print.data.frame()

#ACTIVE PLAYERS
results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame, debut, birthYear), by="playerID") %>%
  #Filter To Look at Active Players
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N'),
         Deb_Age=as.numeric(substr(debut, 1, 4))-birthYear,
         Age=2023-birthYear,
         PA = rowSums(select(., starts_with("PA"))),
         WAR = rowSums(select(., starts_with("WAR"))),
         WARPer100PA = ((WAR/PA)*100),
         WARPerYR=WAR/(Age-Deb_Age)
  ) %>%
  filter(Active == 'Y', PA >= 1500) %>%
  arrange(desc(predict), desc(HOF_Prob), desc(WARPerYR), -WAA_1) %>% 
  head(25) %>% 
  select(nameLast, nameFirst, WAR, WARPerYR, predict, HOF_Prob, WAA_1, Age) %>% 
  print.data.frame()

varImp(xgbFit1)$importance  %>% head(20)

b_imp_scores <- varImp(xgbFit1)$importance

b_imp_scores["Median_WAA", "Overall"]

