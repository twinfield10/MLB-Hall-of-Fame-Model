library(formattable)
library(scales)
library(baseballr)
library(mlbplotr)
library(gt)

### Steroid Test ###

check_roids <- function() {
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
    select(playerID, TookSteroids)
  print(ROID)
  
  bat_df <- results_df_test %>%
    select(playerID, predict, HOF_Prob)
  
  pitch_df <- p_results_df_test %>%
    select(playerID, predict, HOF_Prob)

  
  full_df <- rbind(bat_df, pitch_df) %>%
  inner_join(ROID, by = 'playerID')
  
  print(full_df)
}
post_roid_tbl <- check_roids()
pre_roid_tbl

###Build Reference Table for Career Stats#######
B_CareerRef <- bb %>%
  group_by(playerID) %>%
  summarise(
    G = sum(G),
    PA = sum(PA),
    AB = sum(AB),
    H = sum(H),
    BB = sum(BB),
    IBB = sum(IBB),
    HBP = sum(HBP),
    HR = sum(HR),
    RBI = sum(RBI),
    SB = sum(SB),
    x2B = sum(X2B),
    X3B = sum(X3B),
    BA = H/AB,
    OBP = (H+BB+IBB+HBP)/PA,
    SLG = (((H - x2B - X3B - HR) + ((x2B)*2) + ((x3B)*3) + (HR*4))/AB),
    OPS = OBP + SLG,
    WAR = sum(WAR),
    WAA = sum(WAA),
    WAR_Per162 = ((WAR/G)*162),
    WAA_Per162 = ((WAA/G)*162),
    GoldGlove = sum(Win_GG)
  )


#######PITCHERS###########
##Prediction Iime!
p_all <- p_results_df_test %>% 
  #merge(B_CareerRef %>% select(playerID, WAR, WAA, WAR_Per162, WAA_Per162), by="playerID") %>%
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, debut, finalGame, birthYear), by="playerID") %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N'),
         Deb_Age=as.numeric(substr(debut, 1, 4))-birthYear,
         Age=2023-birthYear,
         WAR = rowSums(select(., starts_with("WAR"))),
         WARPerYR=WAR/(Age-Deb_Age)) %>%
  mutate(
    Name = paste0(nameFirst, " ", nameLast)
  ) %>%
  arrange(desc(predict), desc(HOF_Prob), desc(WARPerYR), -WAA_1) %>%
  mutate(
    HOF_Pred = percent(HOF_Prob, accuracy = .01)
  ) %>%
  head(25) %>% 
  select(Name, WAR, WARPerYR, predict, HOF_Pred, WAA_1) %>% 
  print.data.frame()

####ACTIVE PLAYERS
p_active <- p_results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, debut, finalGame, birthYear), by="playerID") %>%
  #Filter To Look at Active Players
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N'),
         Deb_Age=as.numeric(substr(debut, 1, 4))-birthYear,
         Age=2023-birthYear,
         IP = rowSums(select(., starts_with("IPouts"))),
         WAR = rowSums(select(., starts_with("WAR"))),
         WARPer100IP = ((WAR/IP)*100),
         WARPerYR=WAR/(Age-Deb_Age),
         Name = paste0(nameFirst, " ", nameLast)
  ) %>%
  filter(Active == 'Y', IP >= 500) %>%
  arrange(desc(predict), desc(HOF_Prob), desc(WARPerYR), -WAA_1) %>% 
  head(25) %>%
  mutate(
    HOF_Pred = percent(HOF_Prob, accuracy = .01)
  ) %>%
  select(Name, WAR, WARPerYR, predict, HOF_Prob, WAA_1) %>% 
  print.data.frame()


#######BATTERS#########

b_all<-results_df_test %>%
  merge(B_CareerRef %>% select(playerID, G, WAR, WAA, WAR_Per162, WAA_Per162), by="playerID") %>%
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, debut, finalGame, birthYear), by="playerID") %>%
  filter(G >= (162*3)) %>%
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N'),
         Name = paste0(nameFirst, " ", nameLast)) %>%
  arrange(desc(predict), desc(HOF_Prob), desc(WAR_Per162), -WAA_1) %>% 
  head(25) %>% 
  mutate(
    HOF_Pred = percent(HOF_Prob, accuracy = .01)
  ) %>%
  select(Name, WAR, WAR_Per162, predict, HOF_Prob, WAA_1) %>% 
  print.data.frame()

#ACTIVE PLAYERS
b_active <- results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame, debut, birthYear), by="playerID") %>%
  #Filter To Look at Active Players
  mutate(Active = if_else(finalGame >= "2022-01-01", 'Y', 'N'),
         Deb_Age=as.numeric(substr(debut, 1, 4))-birthYear,
         Age=2022-birthYear,
         PA = rowSums(select(., starts_with("PA"))),
         WAR = rowSums(select(., starts_with("WAR"))),
         WARPer100PA = ((WAR/PA)*100),
         WARPerYR=WAR/(Age-Deb_Age),
         Name = paste0(nameFirst, " ", nameLast)
  ) %>%
  filter(Active == 'Y', PA >= 1500) %>%
  arrange(desc(predict), desc(HOF_Prob), desc(WARPerYR), -WAA_1) %>% 
  head(25) %>% 
  select(Name, WAR, WARPerYR, predict, HOF_Prob, WAA_1) %>% 
  print.data.frame()

## END TABLE SET UP
##Format Names
nms <- c("Full Name","Career WAR", "WAR Per 162", "Make HOF (Yes/No)", "Make HOF (%)", "Best WAA Season")
names(p_all) <- nms
names(p_active) <- nms
names(b_all) <- nms
names(b_active) <- nms

######################################################

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"
CustomGold = '#E6C34C'
CustomWhite = "FFFFFF"

HOF_Func <- formatter("span",
                      style = x ~ style(
                      font.weight = "bold",
                      color = ifelse(x == 'Y', CustomGold, CustomWhite)))


formattable(p_all, align = c("r", "c", "c", "c", "c", "c", "c"),
            list(
              `Career WAR`= color_tile(customGreen0, customGreen),
              `WAR Per 162`= color_tile(customGreen0, customGreen),
              `Make HOF (Yes/No)`= HOF_Func,
              `Best WAA Season`= color_tile(customGreen0, customGreen)
            )) 