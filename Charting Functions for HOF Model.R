library(formattable)

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
    WAR = sum(WAR),
    WAA = sum(WAA),
    WAR_Per162 = ((WAR/G)*162),
    WAA_Per162 = ((WAA/G)*162)
  )


#######PITCHERS###########
##Prediction Iime!
p_all <- p_results_df_test %>% 
  #merge(B_CareerRef %>% select(playerID, WAR, WAA, WAR_Per162, WAA_Per162), by="playerID") %>%
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, debut, finalGame, birthYear), by="playerID") %>%
  mutate(Active = if_else(finalGame >= "2021-01-01", 'Y', 'N'),
         Deb_Age=as.numeric(substr(debut, 1, 4))-birthYear,
         Age=2022-birthYear,
         WAR = rowSums(select(., starts_with("WAR"))),
         WARPerYR=WAR/(Age-Deb_Age)) %>%
  mutate(Active = case_when(
    nameLast == 'Verlander' & nameFirst == 'Justin' ~ 'Y',
    TRUE ~ Active
  )) %>%
  mutate(
    Name = paste0(nameFirst, " ", nameLast)
  ) %>%
  arrange(desc(predict), desc(WARPerYR), -WAA_1) %>% 
  head(25) %>% 
  select(Name, WAR, WARPerYR, predict, WAA_1, Age) %>% 
  print.data.frame()

####ACTIVE PLAYERS
p_active <- p_results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, debut, finalGame, birthYear), by="playerID") %>%
  #Filter To Look at Active Players
  mutate(Active = if_else(finalGame >= "2021-01-01", 'Y', 'N'),
         Deb_Age=as.numeric(substr(debut, 1, 4))-birthYear,
         Age=2022-birthYear,
         IP = rowSums(select(., starts_with("IPouts"))),
         WAR = rowSums(select(., starts_with("WAR"))),
         WARPer100IP = ((WAR/IP)*100),
         WARPerYR=WAR/(Age-Deb_Age),
         Name = paste0(nameFirst, " ", nameLast)
  ) %>%
  mutate(Active = case_when(
    Name == 'Justin Verlander' ~ 'Y',
    TRUE ~ Active
  )) %>%
  filter(Active == 'Y', IP >= 500) %>%
  arrange(desc(predict), desc(WARPerYR), -WAA_1) %>% 
  head(25) %>% 
  select(Name, WAR, WARPerYR, predict, WAA_1, Age) %>% 
  print.data.frame()
## END TABLE SET UP

#######BATTERS#########

b_all<-results_df_test %>%
  merge(B_CareerRef %>% select(playerID, G, WAR, WAA, WAR_Per162, WAA_Per162), by="playerID") %>%
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, debut, finalGame, birthYear), by="playerID") %>%
  filter(G >= (162*3)) %>%
  mutate(Active = if_else(finalGame >= "2021-01-01", 'Y', 'N'),
         Name = paste0(nameFirst, " ", nameLast)) %>%
  arrange(desc(predict), desc(WAR_Per162), -WAA_1) %>% 
  head(20) %>% 
  select(Name, G, WAR, WAR_Per162, predict, WAA_1) %>% 
  print.data.frame()

#ACTIVE PLAYERS
b_active <- results_df_test %>% 
  merge(Lahman::People %>% select(playerID, nameLast, nameFirst, finalGame, debut, birthYear), by="playerID") %>%
  #Filter To Look at Active Players
  mutate(Active = if_else(finalGame >= "2021-01-01", 'Y', 'N'),
         Deb_Age=as.numeric(substr(debut, 1, 4))-birthYear,
         Age=2022-birthYear,
         PA = rowSums(select(., starts_with("PA"))),
         WAR = rowSums(select(., starts_with("WAR"))),
         WARPer100PA = ((WAR/PA)*100),
         WARPerYR=WAR/(Age-Deb_Age),
         Name = paste0(nameFirst, " ", nameLast)
  ) %>%
  filter(Active == 'Y', PA >= 1500) %>%
  arrange(desc(predict), desc(WARPerYR), -WAA_1) %>% 
  head(25) %>% 
  select(Name, WAR, WARPerYR, predict, WAA_1, Age) %>% 
  print.data.frame()

##Format Names
nms <- c("Full Name", "Games Played", "Career WAR", "WAR Per 162", "Predict Make HOF", "Best WAA Season")

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

formattable(b_all, align = c("r", "c", "c", "c", "c", "c"),
            list(
              `Career WAR`= color_tile(customGreen0, customGreen),
              `WAR Per 162`= color_tile(customGreen0, customGreen),
              `Predict Make HOF`= HOF_Func,
              `Best WAA Season`= color_tile(customGreen0, customGreen)
            )) 