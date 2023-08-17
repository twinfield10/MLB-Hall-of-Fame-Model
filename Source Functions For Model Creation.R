library(dplyr)
library(tidyr)
library(Lahman)
library(randomForest)
library(xgboost)
library(glmnet)
library(caret)
library(caretEnsemble)
library(magrittr)
library(doParallel)

##Create Awards Ref Table
awards <- Lahman::AwardsPlayers %>%
  group_by(awardID) %>%
  summarise(cnt=n_distinct(awardID))

## Set Up Function To Combine Career Numbers

combine_lahman_pitching_stints <- function(pitching) {
pitching %>%
  tidyr::gather(key, value, -playerID, -yearID, -stint, -teamID, -lgID) %>%
  dplyr::group_by(playerID, yearID, key) %>%
  dplyr::summarise(value=sum(value, na.rm=TRUE)) %>%
  tidyr::spread(key, value) %>%
  dplyr::mutate(RA9=27*R/IPouts) %>%
  ungroup()
}

combine_lahman_batting_stints <- function(batting) {
  batting %>%
    dplyr::mutate(BASIC_WOBA =
                    0.7*BB +
                    0.9*(H-X2B-X3B-HR) +
                    1.25*X2B +
                    1.6*X3B +
                    2.0*HR) %>%
    dplyr::mutate(wwoba=BASIC_WOBA*PA) %>%
    tidyr::gather(key, value, -playerID, -yearID, -stint, -teamID, -lgID) %>%
    dplyr::group_by(playerID, yearID, key) %>%
    dplyr::summarise(value=sum(value, na.rm=TRUE)) %>%
    tidyr::spread(key, value) %>%
    dplyr::mutate(SlugPct=TB/AB,
                  OBP=(H+BB+HBP)/PA,
                  BA=H/AB, BASIC_WOBA=wwoba/PA,
                  OPS=OBP+SlugPct) %>%
    ungroup()
}

#' returns a data frame with batting stats, aggregating over stints
get_lahman_batting <- function() {
Lahman::battingStats() %>% combine_lahman_batting_stints() }

#' returns a data frame with pitching stats, aggregating over stints
get_lahman_pitching <- function() {
Lahman::Pitching %>% combine_lahman_pitching_stints() }

##Filter Rosters by Retirement Year
filter_by_retirement_year <- function(.data) {
  Lahman::People %>%
    dplyr::filter(finalGame >= last_game_min, finalGame <= last_game_max) %>%
    dplyr::select(playerID, bbrefID, birthYear) }

#' combine baseball reference war data
#' applies to batting or pitching
combine_war_stints <- function(.data) {
  .data %>%
    tidyr::gather(key, value, -player_ID, -year_ID, -stint_ID) %>%
    dplyr::group_by(player_ID, year_ID, key) %>%
    dplyr::mutate(value=as.numeric(value)) %>%
    dplyr::summarise(value=sum(value, na.rm=TRUE)) %>%
    tidyr::spread(key, value) %>%
    ungroup()
}

get_primary_pos <- function() {
  Lahman::Fielding %>%
    dplyr::group_by(playerID, POS) %>%
    dplyr::summarise(ngame=sum(G, na.rm=TRUE)) %>%
    dplyr::arrange(playerID, -ngame) %>%
    dplyr::mutate(pos_rank=row_number()) %>%
    dplyr::filter(pos_rank==1) %>%
    dplyr::select(playerID, POS) %>%
    ungroup()
}

append_pos <- function(.data) {
  pp = get_primary_pos()
  .data %>% merge(pp) %>% mutate(POS=as.factor(POS)) }


#' Append age to a data frame based on Lahman::Master playerID
append_age <- function(.data) {
.data %>%
  merge(Lahman::People, by="playerID") %>%
  dplyr::mutate(Age=yearID-birthYear)
}

#' append HOF status: inducted = (TRUE, FALSE)
#' TODO: specify as 1 of 4 categories; 1st ballot, BBWAA, Veterns, No
append_hof <- function(.data) {
hofers <- Lahman::HallOfFame %>%
  dplyr::filter(inducted=='Y',
                category=='Player') %>%
  dplyr::select(playerID, inducted, votedBy)

.data %>% merge(hofers, by="playerID", all.x=TRUE) %>%
  tidyr::replace_na(list(inducted='N')) %>%
  tidyr::replace_na(list(votedBy='N')) %>%
  mutate(votedBy=ifelse(votedBy %in% c("BBWAA", "Special Election"), "BBWAA", votedBy)) %>%
  mutate(votedBy=ifelse(votedBy %in% c("BBWAA", "N"), votedBy, "VetCom")) %>%
  mutate(inducted=as.factor(inducted), votedBy=as.factor(votedBy)) }

#' append batting war
append_br_war <- function(.data, war) {
  tmp <- war %>%
    dplyr::select(player_ID, WAA, WAR, WAR_off, WAR_def, year_ID, stint_ID)
  
  tmp <- tmp %>%
    combine_war_stints() %>%
    dplyr::rename(bbrefID=player_ID, yearID=year_ID)
  
  tmp <- tmp %>%
    group_by(yearID) %>%
    arrange(-WAA) %>%
    mutate(WAA_rank=row_number()) %>%
    ungroup()
  
  .data %>% merge(tmp)
}


#' append MVPs
append_mvps <- function(.data) {
  
  mvps <- Lahman::AwardsPlayers %>%
    dplyr::filter(awardID=='Most Valuable Player') %>%
    dplyr::mutate(MVPWin='Y') %>%
    dplyr::select(playerID, yearID, MVPWin)
  
  mvp_shares <- Lahman::AwardsSharePlayers %>%
    dplyr::filter(awardID=='MVP') %>%
    dplyr::mutate(MVPShare=pointsWon/pointsMax) %>%
    dplyr::select(playerID, yearID, MVPShare)
  
  kk <- .data %>%
    merge(mvps, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(MVPWin='N')) %>%
    merge(mvp_shares, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(MVPShare=0.0)) %>%
    dplyr::mutate(MVPWin = ifelse(MVPWin=='Y', 1, 0), MPVWin=as.numeric(MVPWin)) }

#' append CY Youngs
append_cys <- function(.data) {
  
  cys <- Lahman::AwardsPlayers %>%
    dplyr::filter(awardID=='Cy Young Award') %>%
    dplyr::mutate(CYWin='Y') %>%
    dplyr::select(playerID, yearID, CYWin)
  
  cys_shares <- Lahman::AwardsSharePlayers %>%
    dplyr::filter(awardID=='Cy Young') %>%
    dplyr::mutate(CYShare=pointsWon/pointsMax) %>%
    dplyr::select(playerID, yearID, CYShare)
  
  kk <- .data %>%
    merge(cys, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(CYWin='N')) %>%
    merge(cys_shares, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(CYShare=0.0)) %>%
    dplyr::mutate(CYWin = ifelse(CYWin=='Y', 1, 0), CYWin=as.numeric(CYWin))
  }

# Append Reliever Of The Year
append_Rel_OY <- function(.data) {
  
  Rel_oy <- Lahman::AwardsPlayers %>%
    dplyr::filter(awardID=='Reliever of the Year Award') %>%
    dplyr::mutate(RelOYWin='Y') %>%
    dplyr::select(playerID, yearID, RelOYWin)
  
  kk <- .data %>%
    merge(Rel_oy, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(RelOYWin='N')) %>%
    dplyr::mutate(RelOYWin = ifelse(RelOYWin=='Y', 1, 0), RelOYWin=as.numeric(RelOYWin))
}

# Append Gold Glove Winners
append_GG <- function(.data) {
  
  GG <- Lahman::AwardsPlayers %>%
    dplyr::filter(awardID=='Gold Glove') %>%
    dplyr::mutate(GG_Win='Y') %>%
    dplyr::select(playerID, yearID, GG_Win)
  
  kk <- .data %>%
    merge(GG, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(GG_Win='N')) %>%
    dplyr::mutate(GG_Win = ifelse(GG_Win=='Y', 1, 0), GG_Win=as.numeric(GG_Win))
}

# Append Triple Crown Winners
append_TC <- function(.data) {
  
  TC <- Lahman::AwardsPlayers %>%
    dplyr::filter(awardID=='Triple Crown') %>%
    dplyr::mutate(TC_Win='Y') %>%
    dplyr::select(playerID, yearID, TC_Win)
  
  kk <- .data %>%
    merge(TC, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(TC_Win='N')) %>%
    dplyr::mutate(TC_Win = ifelse(TC_Win=='Y', 1, 0), TC_Win=as.numeric(TC_Win))
}

append_PTC <- function(.data) {
  
  TC <- Lahman::AwardsPlayers %>%
    dplyr::filter(awardID=='Pitching Triple Crown') %>%
    dplyr::mutate(PTC_Win='Y') %>%
    dplyr::select(playerID, yearID, PTC_Win)
  
  kk <- .data %>%
    merge(TC, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(PTC_Win='N')) %>%
    dplyr::mutate(PTC_Win = ifelse(PTC_Win=='Y', 1, 0), PTC_Win=as.numeric(PTC_Win))
}

# Append Silver Sluger Winners
append_SS <- function(.data) {
  
  SS <- Lahman::AwardsPlayers %>%
    dplyr::filter(awardID=='Silver Slugger' | awardID == 'SIlver Slugger') %>%
    dplyr::mutate(SS_Win='Y') %>%
    dplyr::select(playerID, yearID, SS_Win)
  
  kk <- .data %>%
    merge(SS, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(SS_Win='N')) %>%
    dplyr::mutate(SS_Win = ifelse(SS_Win=='Y', 1, 0), SS_Win=as.numeric(SS_Win))
}




#' append WS wins
append_all_star <- function(.data) {
  
  all_stars <- Lahman::AllstarFull %>%
    group_by(playerID, yearID) %>%
    summarise(startingPos=max(startingPos)) %>%
    mutate(AllStar='Y',
           AllStarStart=ifelse(is.na(startingPos), 'N', 'Y')) %>%
    select(playerID, yearID, AllStar, AllStarStart)
  
  .data %>% merge(all_stars, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(AllStar='N', AllStarStart='N')) %>%
    mutate(AllStar=as.numeric(ifelse(AllStar=='Y', 1, 0)),
           AllStarStart=as.numeric(ifelse(AllStarStart=='Y', 1, 0))
    )
}


#' append WS wins
append_ws_wins <- function(.data) {
  
  all_players <- Lahman::Appearances %>%
    group_by(playerID, yearID) %>%
    mutate(rr=row_number()) %>%
    filter(rr==max(rr)) %>%
    select(playerID, yearID, teamID)
  
  ws_winners <- Lahman::Teams %>%
    filter(WSWin == 'Y') %>%
    select(yearID, teamID, WSWin)
  
  ws_players <- merge(all_players, ws_winners, by=c("yearID", "teamID")) %>%
    select(playerID, yearID, WSWin)
  
  ee = merge(.data, ws_players, by=c("yearID", "playerID"), all.x=TRUE) %>%
    tidyr::replace_na(list(WSWin='N')) %>%
    mutate(WSWin=as.numeric(ifelse(WSWin=='Y', 1, 0))) }

#' append WS wins
append_war_rank <- function(war_df) {
  war_df %>%
    group_by(playerID) %>%
    mutate(WAR=as.numeric(WAR)) %>%
    arrange(-WAR) %>%
    dplyr::mutate(war_rank=row_number()) %>%
    ungroup() %>% arrange(playerID, yearID) }

filter_by_years <- function(war_df, min_year=10) {
  war_df %>%
    group_by(playerID) %>%
    mutate(nyear=max(war_rank)) %>%
    filter(nyear >= min_year) %>%
    ungroup() %>%
    dplyr::select(-nyear)
}

#' fit data
#' takes the top MAXYEAR seasons, ordered by ORDER_KEY and #' returns the value of LIST_OF_STATS. If less than MAXYEAR has been played, filled in with 0
get_fit_data <- function(.data,
MAXYEAR=20,
LIST_OF_STATS=NULL,
ORDER_KEY="WAA",
final_game_min='1901-01-01',
final_game_max='2022-01-01') {
  
  kitchen_sink <- c("AB", "BB", "CS", "G", "GIDP",
                    "HBP", "HR", "IBB", "BR",
                    "PA", "R", "RBI", "SB", "SF", "SH", "SO", "TB",
                    "wwoba", "X1B", "X2B", "X3B", "Age", "WAA", "WAR", "WAR_off", "WAR_def")
  war_and_fip <- c("PA", "BB", "SO", "WAA", "WAA_rank", "WAR", "WAR_off", "WAR_def")
  grit <- c("AllStar", "AllStarStart", "WSWin", "MVPWin", "MVPShare")
  
  if (is.null(LIST_OF_STATS)) {
    LIST_OF_STATS <- c(war_and_fip, grit)
  }
  
  kk <- .data %>%
    dplyr::filter(finalGame>=final_game_min, finalGame<=final_game_max)
  
  kk$ORDER_KEY <- kk[[ORDER_KEY]]
  
  kk <- kk %>%
    dplyr::arrange(playerID, -ORDER_KEY) %>%
    dplyr::select(-ORDER_KEY) %>%
    dplyr::group_by(playerID) %>%
    dplyr::mutate(nyear=row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(nyear>=1, nyear <= MAXYEAR)
  kk <- kk[,c("playerID", "yearID", "nyear", LIST_OF_STATS, "POS", "inducted")]
  
  col_numerics <- names(kk)[sapply(kk, is.numeric)]
  col_factors <- names(kk)[sapply(kk, is.factor)]
  
  kkg <- kk %>%
    tidyr::gather(key, value,
                  -playerID,
                  -yearID,
                  -nyear,
                  -POS,
                  -inducted) %>%
    mutate(new_key=paste(key, nyear, sep='_'))
  
  kkg %<>%
    select(-key, -nyear, -yearID) %>%
    tidyr::spread(new_key, value, fill=0)
  
  #TODO: find a better way to accomplish this
  # specifically, because I'm gathering both numeric and factor columns,
  # everything gets cast as character, and need to convert back
  for (s in col_numerics) {
    kkg %<>% mutate_each(funs(as.numeric), starts_with(s))
  }
  
  for (s in col_factors) {
    kkg %<>% mutate_each(funs(as.factor), starts_with(s))
  }
  
  col_factors <- names(kkg)[sapply(kkg, is.factor)]
  for (s in col_factors) {
    print(s)
    print(levels(kkg[[s]]))
    
    if ("0" %in% levels(kkg[[s]])){
      kkg[kkg[[s]] == "0",][[s]] = "N"
      kkg[[s]] <- droplevels(kkg)[[s]]
    }
  }
  
  # ineligible
  kkg %>% filter(playerID!='rosepe01', playerID!='jacksjo01')
  
}

get_aggregated_fit_df <-
  function(fit_df,
           keys_to_aggregate=c('^AllStar_',
                               '^AllStarStart_',
                               '^WSWin_', '^MVPWin_',
                               '^MVPShare_'),
           keys_to_keep=c("^playerID$",
                          "^inducted$",
                          "^POS$",
                          "^WAR_[0-9]+$")) {
    
    nn <- names(fit_df)
    i = 0
    for (k in keys_to_keep) {
      nx <- nrow((fit_df[,grepl(k, nn)]))
      if (i==0) {
        dfX <- fit_df[,grepl(k, nn)]
      } else {
        dfX <- cbind.data.frame(dfX, fit_df[,grepl(k, nn)])
      }
      i <- i + 1
    }
    
    for (k in keys_to_aggregate) {
      nk <- stringr::str_replace_all(k, '\\^|_', '')
      cc <- grep(k, names(fit_df))
      agg1 <- rowSums(fit_df[,cc])
      dfX[[nk]] <- agg1
    }
    
    k = '^WAR_'
    cc = grep(k, names(dfX))
    dfX$CWAR <- rowSums(dfX[,cc])
    
    dfX
  }

convert_to_factors <- function(dfX,
                                 factors_list=c("^AllStar", "^MVPWin", "^WSWin")) {
  
  cc <- unlist(lapply(factors_list, grep, names(dfX)))
  for (i in seq_along(cc)) {
    c1 <- cc[i]
    dfX[,c1] <- as.factor(unlist(dfX[,c1]))
  }
  
  dfX[,!(sapply(dfX, nlevels) == 1)]
  
}
###########################PITCHER SPECIFIC DATA############################
#' fit Pitchers data
#' takes the top MAXYEAR seasons, ordered by ORDER_KEY and #' returns the value of LIST_OF_STATS. If less than MAXYEAR has been played, filled in with 0
p_get_fit_data <- function(.data,
                         MAXYEAR=20,
                         P_LIST_OF_STATS=NULL,
                         ORDER_KEY="WAA",
                         final_game_min='1901-01-01',
                         final_game_max='2022-01-01') {
  
  kitchen_sink <- c("IPouts", "GS", "G", "W", "L", "CG", "SHO", "SV",
                    "H", "ER", "HR", "BB", "SO", "BAOpp", "ERA", "BFP","GF","R",
                    "RA9", "Age", "WAA", "WAR"
                    )
  war_and_fip <- c("IPouts", "BB", "SO", "W","BAOpp", "L", "WAA", "WAA_rank", "WAR")
  grit <- c("AllStar", "AllStarStart", "WSWin", "MVPWin", "MVPShare", "CYWin", "CYShare", "RelOYWin")
  
  if (is.null(P_LIST_OF_STATS)) {
    P_LIST_OF_STATS <- c(war_and_fip,  grit)
  }
  
  kk <- .data %>%
    dplyr::filter(finalGame>=final_game_min, finalGame<=final_game_max)
  
  kk$ORDER_KEY <- kk[[ORDER_KEY]]
  
  kk <- kk %>%
    dplyr::arrange(playerID, -ORDER_KEY) %>%
    dplyr::select(-ORDER_KEY) %>%
    dplyr::group_by(playerID) %>%
    dplyr::mutate(nyear=row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(nyear>=1, nyear <= MAXYEAR)
  kk <- kk[,c("playerID", "yearID", "nyear", P_LIST_OF_STATS, "POS", "inducted")]
  
  col_numerics <- names(kk)[sapply(kk, is.numeric)]
  col_factors <- names(kk)[sapply(kk, is.factor)]
  
  kkg <- kk %>%
    tidyr::gather(key, value,
                  -playerID,
                  -yearID,
                  -nyear,
                  -POS,
                  -inducted) %>%
    mutate(new_key=paste(key, nyear, sep='_'))
  
  kkg %<>%
    select(-key, -nyear, -yearID) %>%
    tidyr::spread(new_key, value, fill=0)
  
  #TODO: find a better way to accomplish this
  # specifically, because I'm gathering both numeric and factor columns,
  # everything gets cast as character, and need to convert back
  for (s in col_numerics) {
    kkg %<>% mutate_each(funs(as.numeric), starts_with(s))
  }
  
  for (s in col_factors) {
    kkg %<>% mutate_each(funs(as.factor), starts_with(s))
  }
  
  col_factors <- names(kkg)[sapply(kkg, is.factor)]
  for (s in col_factors) {
    print(s)
    print(levels(kkg[[s]]))
    
    if ("0" %in% levels(kkg[[s]])){
      kkg[kkg[[s]] == "0",][[s]] = "N"
      kkg[[s]] <- droplevels(kkg)[[s]]
    }
  }
  
  # ineligible
  kkg %>% filter(playerID!='rosepe01', playerID!='jacksjo01')
  
}

p_get_aggregated_fit_df <-
  function(fit_df,
           keys_to_aggregate=c('^AllStar_',
                               '^AllStarStart_',
                               '^WSWin_', '^MVPWin_','^MVPShare_',
                               '^CYWin_', '^CYShare_', '^RelOYWin_'),
           keys_to_keep=c("^playerID$",
                          "^inducted$",
                          "^POS$",
                          "^WAR_[0-9]+$")) {
    
    nn <- names(fit_df)
    i = 0
    for (k in keys_to_keep) {
      nx <- nrow((fit_df[,grepl(k, nn)]))
      if (i==0) {
        dfX <- fit_df[,grepl(k, nn)]
      } else {
        dfX <- cbind.data.frame(dfX, fit_df[,grepl(k, nn)])
      }
      i <- i + 1
    }
    
    for (k in keys_to_aggregate) {
      nk <- stringr::str_replace_all(k, '\\^|_', '')
      cc <- grep(k, names(fit_df))
      agg1 <- rowSums(fit_df[,cc])
      dfX[[nk]] <- agg1
    }
    
    k = '^WAR_'
    cc = grep(k, names(dfX))
    dfX$CWAR <- rowSums(dfX[,cc])
    
    dfX
  }

p_append_br_war <- function(.data, war) {
  tmp <- war %>%
    dplyr::select(player_ID, WAA, WAR, year_ID, stint_ID)
  
  tmp <- tmp %>%
    combine_war_stints() %>%
    dplyr::rename(bbrefID=player_ID, yearID=year_ID)
  
  tmp <- tmp %>%
    group_by(yearID) %>%
    arrange(-WAA) %>%
    mutate(WAA_rank=row_number()) %>%
    ungroup()
  
  .data %>% merge(tmp)
}

p_convert_to_factors <- function(dfX,
                               factors_list=c("^AllStar", "^MVPWin", "^WSWin", "^CYWin", "^RelOYWin")) {
  
  cc <- unlist(lapply(factors_list, grep, names(dfX)))
  for (i in seq_along(cc)) {
    c1 <- cc[i]
    dfX[,c1] <- as.factor(unlist(dfX[,c1]))
  }
  
  dfX[,!(sapply(dfX, nlevels) == 1)]
  
}

compute_jaws <- function(war_df) {
  jaws_df <- war_df %>%
    group_by(playerID) %>%
    mutate(WAR=as.numeric(WAR)) %>%
    arrange(-WAR) %>%
    dplyr::mutate(war_rank=row_number(),
                  ipeak=as.integer(war_rank<=7),
                  nyear=max(war_rank)) %>%
    group_by(playerID, votedBy, POS) %>%
    dplyr::summarise(last_year=max(yearID),
                     nyear=mean(nyear),
                     cwar=sum(WAR),
                     pwar=sum(ipeak*WAR),
                     jaws=0.5*(cwar+pwar)) %>%
    ungroup()
}

normalize_war <- function(dfX) {
  cc <- which(grepl('^WAR_', names(dfX)))
  tmp <- dfX %>% filter(WAR_1>0)
  tmp <- na.omit(tmp)
  tmp[,cc] <- tmp[,cc] / tmp$WAR_1
  tmp
  
}

## FIT DATA FUNCTIONS
library(lme4)

glmnet_fit <- function(fit_df) {
  xx <- model.matrix(inducted ~ . - playerID, data=fit_df)[,-1]
  yy <- fit_df$inducted
  glmnet_mod <- cv.glmnet(xx, yy , family='binomial')
  
}

glmer_fit <- function(war_df) {
  bb10 <- filter_by_years(war_df)
  tt <- bb10 %>% mutate(ipeak=as.integer(war_rank<=7),
                        WAR_PEAK=ipeak*WAR) %>%
    group_by(playerID, inducted, POS) %>%
    summarise(cwar=sum(WAR),
              pwar=sum(WAR_PEAK),
              mvps=sum(MVPWin),
              allstars=sum(AllStar),
              allstarstarts=sum(AllStarStart),
              wswins=sum(WSWin)) %>%
    ungroup() %>%
    filter(POS!='P')
  
  glmer_mod <- glmer(inducted ~ 1 + mvps + (cwar+pwar|POS),
                     data=tt, family='binomial')
  
}


