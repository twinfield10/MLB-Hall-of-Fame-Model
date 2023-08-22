## Check Steroid Users Propensity ##

## Roid IDs ##
get_ROID_ID <- function() {
  names <- c(
    'Barry Bonds',
    'Bret Boone',
    'Kevin Brown',
    'Ken Caminiti',
    'Jose Canseco',
    'Roger Clemens',
    'Lenny Dykstra',
    'Chuck Finley',
    'Eric Gagne',
    'Jason Giambi',
    'Troy Glaus',
    'Wally Joyner',
    'David Justice',
    'Chuck Knoblauch',
    'Mark McGwire',
    'Magglio Ordonez',
    'Rafael Palmeiro',
    'Andy Pettitte',
    'Manny Ramirez',
    'Brian Roberts',
    'Ivan Rodriguez',
    'Benito Santiago',
    'Gary Sheffield',
    'Sammy Sosa',
    'Miguel Tejada',
    'Mo Vaughn',
    'Matt Williams'
  )
  
  ROID <- Lahman::People %>%
    mutate(
      full_name = paste0(nameFirst, ' ', nameLast),
      birthdate = paste0(birthMonth,'/',birthDay,'/',birthYear),
      filt_name_bday = paste0(full_name,birthdate)
    ) %>%
    filter(full_name %in% names ) %>%
    filter(filt_name_bday != "Kevin Brown3/14/1965") %>%
    filter(filt_name_bday != "Kevin Brown4/21/1973") %>%
    filter(filt_name_bday != "Matt Williams7/25/1959") %>%
    filter(filt_name_bday != "Matt Williams4/12/1971") %>%
    mutate(
      TookSteroids = 'Y'
    ) %>%
    select(playerID) %>%
    pull()
  return(ROID)
}
ROID_IDS <- get_ROID_ID()



## Batters ##

##Feature Generation
#Batters
ROID_tmp <- bb %>% filter(POS!='P')

#Pitchers
#ROID_tmp <- P_bb %>% filter(POS=='P')

ROID_fit_df_train <- get_fit_data(ROID_tmp, final_game_min='1901-01-01', final_game_max='2012-01-01') %>%
  filter(PA_1>=30 & playerID %notin% ROID_IDS)
  #filter(IPouts_1>=10 & playerID %notin% ROID_IDS)
  #filter()
ROID_fit_df_test <- get_fit_data(ROID_tmp, final_game_min='1901-01-01', final_game_max='2022-12-31') %>%
  filter(PA_1>=30)
  #filter(IPouts_1>=10)

head(ROID_fit_df_train, 1) %>% print.data.frame()


## Defining The Model
ROID_frm <- as.formula(inducted ~ . - playerID)

ROID_xx = model.matrix(ROID_frm, data=ROID_fit_df_train)[,-1]
ROID_yy = ROID_fit_df_train$inducted

ROID_cl <- makeCluster(detectCores())
registerDoParallel(ROID_cl)

set.seed(69)
cv_train <- trainControl(method = "cv", number = 10)

ROID_xgbFit1 <- train(ROID_xx, ROID_yy, method = "xgbLinear", trControl = cv_train, verbose = FALSE)
stopCluster(ROID_cl)

##Training Results
table(predict(ROID_xgbFit1), ROID_fit_df_train$inducted)

##Test Results
ROID_xx_test <- model.matrix(ROID_frm,data=ROID_fit_df_test)[,-1]
ROID_yy_test <- ROID_fit_df_test$inducted

ROID_results_df_test <- ROID_fit_df_test
ROID_results_df_test$predict <- 'N'
ROID_cc = which(predict(ROID_xgbFit1, newdata = ROID_xx_test) == 'Y')
ROID_results_df_test[ROID_cc,]$predict <- 'Y'
ROID_results_df_test$HOF_Prob <- predict(ROID_xgbFit1, newdata = ROID_xx_test, type = "prob")[,"Y"]

##Prediction Iime!
ROID_results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame), by="playerID") %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N')) %>%
  arrange(desc(predict), desc(HOF_Prob), -WAA_1) %>% 
  select(playerID, nameLast, nameFirst, predict, HOF_Prob, inducted, WAA_1, POS, Active) %>%
  mutate(
    HOF_Prob = round(HOF_Prob, digits = 3)
  ) %>%
  filter(predict == 'Y' & inducted == 'N' & Active == 'N') %>%
  print.data.frame()