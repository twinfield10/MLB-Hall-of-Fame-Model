# =============================================================================
# Feature-engineering function library (ported from the -2024 source file).
# Data is read from the snapshot via tbl() (see R/tables.R); there is no longer
# a dependency on the installed `Lahman` package for data. Model libraries
# (caret/xgboost/...) live in the model module, not here.
# =============================================================================
suppressMessages({
  library(dplyr); library(tidyr); library(magrittr)
  library(purrr); library(readr); library(stringr)
})
if (!exists("tbl"))             source(file.path("R", "tables.R"))
if (!exists("load_supplement")) source(file.path("R", "data_supplement.R"))
if (!exists("build_fit_matrix")) source(file.path("R", "feature_matrix.R"))
`%notin%` <- Negate(`%in%`)
options(dplyr.summarise.inform = FALSE)

# ================================
# ========== Data Loads ==========
# ================================
# Snapshot reads now go through tbl() (R/tables.R); Baseball-Reference WAR
# through load_war()/pull_br_war() (R/data_live.R). The old load_lahman(),
# pull_br_data() and (unused) pull_fg_data() have been removed.


# =========================================
# ========== Lahman Manipulation ==========
# =========================================

# Lahman Batting
combine_lahman_batting_stints <- function(batting) {
  batting %>%
    dplyr::mutate(
      BASIC_WOBA = 0.7*BB + 0.9*(H-X2B-X3B-HR) + 1.25*X2B + 1.6*X3B + 2.0*HR,
      TB = (H-X2B-X3B-HR) + (X2B*2) + (X3B*3) + (HR*4) 
      ) %>%
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
get_lahman_batting <- function() {
  # tbl("Batting") is schema-normalised (2B/3B -> X2B/X3B) by the snapshot layer.
  tbl("Batting") %>%
    select(-any_of(c("G_batting", "G_old"))) %>%
    mutate(PA = AB + BB + IBB + HBP + SH + SF) %>%
    combine_lahman_batting_stints()
}


## Returns a data frame with pitching stats, aggregating over stints
combine_lahman_pitching_stints <- function(pitching) {
  pitching %>%
    tidyr::gather(key, value, -playerID, -yearID, -stint, -teamID, -lgID) %>%
    dplyr::group_by(playerID, yearID, key) %>%
    dplyr::summarise(value=sum(value, na.rm=TRUE)) %>%
    tidyr::spread(key, value) %>%
    dplyr::mutate(RA9=27*R/IPouts) %>%
    ungroup()
}
get_lahman_pitching <- function() {
  tbl("Pitching") %>% combine_lahman_pitching_stints()
}

##Create Awards Ref Table
#awards <- tbl("AwardsPlayers") %>%
#  group_by(awardID) %>%
#  summarise(cnt=n_distinct(awardID))

## Filter Rosters by Retirement Year
filter_by_retirement_year <- function(.data) {
  tbl("People") %>%
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
  tbl("Fielding") %>%
    dplyr::group_by(playerID, POS) %>%
    dplyr::summarise(
      ngame=sum(G, na.rm=TRUE),
      ngame_start=sum(GS, na.rm=TRUE)) %>%
    dplyr::mutate(
      POS = case_when(
        POS == 'P' & ngame_start/ngame > 0.5 ~ 'SP',
        POS == 'P' & ngame_start/ngame <= 0.5 ~ 'RP',
        TRUE ~ POS
      )
    ) %>%
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
    merge(tbl("People"), by = "playerID") %>%
    dplyr::group_by(playerID) %>%
    dplyr::mutate(.last_season = max(yearID)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Active players have finalGame = NA in the snapshot. Fall back to the last
      # season actually played so they stay inside the eligibility windows and get
      # scored (the old code only scored them because the Lahman *package* happened
      # to give them a finalGame). See docs/PARITY.md.
      finalGame = dplyr::if_else(is.na(finalGame),
                                 as.Date(paste0(.last_season, "-12-31")),
                                 as.Date(finalGame)),
      Age = yearID - birthYear
    ) %>%
    dplyr::select(-.last_season)
}

#' append HOF status: inducted = (TRUE, FALSE)
#' TODO: specify as 1 of 4 categories; 1st ballot, BBWAA, Veterns, No
append_hof <- function(.data, label_max_year = NULL) {
hof <- tbl("HallOfFame")
# Never label on HOF results from the season we're predicting (or later): the
# 2025 snapshot's HallOfFame already contains the 2026 class, which would leak.
if (!is.null(label_max_year)) hof <- dplyr::filter(hof, yearID <= label_max_year)
hofers <- hof %>%
  dplyr::filter(inducted=='Y',
                category=='Player') %>%
  dplyr::select(playerID, inducted, votedBy)


.data %>% merge(hofers, by="playerID", all.x=TRUE) %>%
  tidyr::replace_na(list(inducted='N')) %>%
  tidyr::replace_na(list(votedBy='N')) %>%
  mutate(votedBy=ifelse(votedBy %in% c("BBWAA", "Special Election"), "BBWAA", votedBy)) %>%
  mutate(votedBy=ifelse(votedBy %in% c("BBWAA", "N"), votedBy, "VetCom")) %>%
  mutate(inducted=as.factor(inducted), votedBy=as.factor(votedBy)) }

pull_hof_vet_eligible <- function(vetcom_years = seq(1997, 2022), ballot_max_years = 10) {
  hofers <- tbl("HallOfFame") %>%
    filter(yearID %in% vetcom_years) %>%
    mutate(inducted = if_else(inducted == 'Y', 1, 0)) %>%
    group_by(playerID) %>%
    mutate(
      inducted_alltime = max(inducted),
      yrs_on_ballot = sum(votedBy == 'BBWAA')
    ) %>%
    ungroup() %>%
    dplyr::filter(inducted_alltime < 1,
                  yrs_on_ballot >= ballot_max_years,
                  category == 'Player') %>%
    select(playerID) %>%
    unique() %>%
    pull()
  return(hofers)
}
# (VetCom_IDs is built in R/cohorts.R, not at source-time.)

#' append batting war
#' Batter WAR (thin wrapper over append_war; see R/feature_matrix.R).
append_br_war <- function(.data, war) {
  append_war(.data, war, extra_cols = c("WAR_off", "WAR_def", "OPS_plus"))
}

### AWARDS ###
#' append MVPs
append_mvps <- function(.data) {
  
  # Winners come from the snapshot (2024 MVPs are native); the old add_row() patches
  # for judge/ohtani are gone. A lagging snapshot can inject via the supplement.
  mvps <- tbl("AwardsPlayers") %>%
    dplyr::filter(awardID=='Most Valuable Player') %>%
    dplyr::mutate(MVPWin='Y') %>%
    dplyr::select(playerID, yearID, MVPWin) %>%
    bind_rows(sup_win(sup(), "MVP_win", "MVPWin"))

  mvp_shares <- tbl("AwardsSharePlayers") %>%
    dplyr::filter(awardID=='Most Valuable Player') %>%
    dplyr::mutate(MVPShare=pointsWon/pointsMax) %>%
    dplyr::select(playerID, yearID, MVPShare) %>% arrange(-yearID, -MVPShare) %>%
    bind_rows(sup_share(sup(), "MVP_share", "MVPShare"))

  kk <- .data %>%
    merge(mvps, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(MVPWin='N')) %>%
    merge(mvp_shares, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(MVPShare=0.0)) %>%
    dplyr::mutate(MVPWin = as.numeric(ifelse(MVPWin=='Y', 1, 0))) }  # was: + typo'd MPVWin duplicate

#' append CY Youngs
append_cys <- function(.data) {
  
  cys <- tbl("AwardsPlayers") %>%
    dplyr::filter(awardID=='Cy Young Award') %>%
    dplyr::mutate(CYWin='Y') %>%
    dplyr::select(playerID, yearID, CYWin) %>%
    bind_rows(sup_win(sup(), "CY_win", "CYWin"))

  cys_shares <- tbl("AwardsSharePlayers") %>%
    dplyr::filter(awardID=='Cy Young Award') %>%
    dplyr::mutate(CYShare=pointsWon/pointsMax) %>%
    dplyr::select(playerID, yearID, CYShare) %>%
    bind_rows(sup_share(sup(), "CY_share", "CYShare"))

  kk <- .data %>%
    merge(cys, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(CYWin='N')) %>%
    merge(cys_shares, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(CYShare=0.0)) %>%
    dplyr::mutate(CYWin = ifelse(CYWin=='Y', 1, 0), CYWin=as.numeric(CYWin))
  }

# Append Reliever Of The Year
append_ROY <- function(.data) {
  
  Rel_oy <- tbl("AwardsPlayers") %>%
    dplyr::filter(awardID=='Rookie of the Year') %>%
    dplyr::mutate(ROYWin='Y') %>%
    dplyr::select(playerID, yearID, ROYWin) %>%
    bind_rows(sup_win(sup(), "ROY_win", "ROYWin"))

  kk <- .data %>%
    merge(Rel_oy, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(ROYWin='N')) %>%
    dplyr::mutate(ROYWin = ifelse(ROYWin=='Y', 1, 0), ROYWin=as.numeric(ROYWin))
}

# Append Gold Glove Winners
append_GG <- function(.data) {
  
  GG <- tbl("AwardsPlayers") %>%
    dplyr::filter(awardID=='Gold Glove') %>%
    dplyr::mutate(GG_Win='Y') %>%
    dplyr::select(playerID, yearID, GG_Win) %>%
    bind_rows(sup_win(sup(), "GG_win", "GG_Win"))

  kk <- .data %>%
    merge(GG, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(GG_Win='N')) %>%
    dplyr::mutate(GG_Win = ifelse(GG_Win=='Y', 1, 0), GG_Win=as.numeric(GG_Win))
}

# Append Triple Crown Winners
append_TC <- function(.data) {
  
  TC <- tbl("AwardsPlayers") %>%
    dplyr::filter(awardID=='Triple Crown') %>%
    dplyr::mutate(TC_Win='Y') %>%
    dplyr::select(playerID, yearID, TC_Win)
  
  kk <- .data %>%
    merge(TC, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(TC_Win='N')) %>%
    dplyr::mutate(TC_Win = ifelse(TC_Win=='Y', 1, 0), TC_Win=as.numeric(TC_Win))
}

# Append Pitching Triple Crown Winners
append_PTC <- function(.data) {
  
  TC <- tbl("AwardsPlayers") %>%
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
  
  SS <- tbl("AwardsPlayers") %>%
    dplyr::filter(awardID=='Silver Slugger' | awardID == 'SIlver Slugger') %>%
    dplyr::mutate(SS_Win='Y') %>%
    dplyr::select(playerID, yearID, SS_Win) %>%
    bind_rows(sup_win(sup(), "SS_win", "SS_Win"))

  kk <- .data %>%
    merge(SS, by=c("playerID", "yearID"), all.x=TRUE) %>%
    tidyr::replace_na(list(SS_Win='N')) %>%
    dplyr::mutate(SS_Win = ifelse(SS_Win=='Y', 1, 0), SS_Win=as.numeric(SS_Win))
}

### OTHER FEATURES ###

# append ASG Nominations + Starts
append_all_star <- function(.data) {
  
  all_stars <- tbl("AllstarFull") %>%
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

# Append WS wins
append_ws_wins <- function(.data) {
  
  all_players <- tbl("Appearances") %>%
    group_by(playerID, yearID) %>%
    mutate(rr=row_number()) %>%
    filter(rr==max(rr)) %>%
    select(playerID, yearID, teamID)
  
  ws_winners <- tbl("Teams") %>%
    filter(WSWin == 'Y') %>%
    select(yearID, teamID, WSWin)
  
  ws_players <- merge(all_players, ws_winners, by=c("yearID", "teamID")) %>%
    select(playerID, yearID, WSWin)
  
  ee = merge(.data, ws_players, by=c("yearID", "playerID"), all.x=TRUE) %>%
    tidyr::replace_na(list(WSWin='N')) %>%
    mutate(WSWin=as.numeric(ifelse(WSWin=='Y', 1, 0))) }

# Append Pennant Wins
append_pen_wins <- function(.data) {
  
  all_players <- tbl("Appearances") %>%
    group_by(playerID, yearID) %>%
    mutate(rr=row_number()) %>%
    filter(rr==max(rr)) %>%
    select(playerID, yearID, teamID)
  
  pen_winners <- tbl("Teams") %>%
    filter(LgWin == 'Y') %>%
    select(yearID, teamID, LgWin)
  
  pen_players <- merge(all_players, pen_winners, by=c("yearID", "teamID")) %>%
    select(playerID, yearID, LgWin)
  
  ee = merge(.data, pen_players, by=c("yearID", "playerID"), all.x=TRUE) %>%
    tidyr::replace_na(list(LgWin='N')) %>%
    mutate(PenWin=as.numeric(ifelse(LgWin=='Y', 1, 0))) %>%
    select(-LgWin)}

# Append Division Wins
append_div_wins <- function(.data) {
  
  all_players <- tbl("Appearances") %>%
    group_by(playerID, yearID) %>%
    mutate(rr=row_number()) %>%
    filter(rr==max(rr)) %>%
    select(playerID, yearID, teamID)
  
  div_winners <- tbl("Teams") %>%
    filter(DivWin == 'Y') %>%
    select(yearID, teamID, DivWin)
  
  div_players <- merge(all_players, div_winners, by=c("yearID", "teamID")) %>%
    select(playerID, yearID, DivWin)
  
  ee = merge(.data, div_players, by=c("yearID", "playerID"), all.x=TRUE) %>%
    tidyr::replace_na(list(DivWin='N')) %>%
    mutate(DivWin=as.numeric(ifelse(DivWin=='Y', 1, 0))) }

# Append Postseason Pitching
append_post_pitch <- function(.data){
  CSDS_Rounds <- c('ALCS', 'NLCS',
                   'NLDS2', 'NLDS1', 'ALDS2', 'ALDS1')
  
  df <- tbl("PitchingPost") %>%
    filter(yearID >= 1903) %>%
    group_by(playerID, yearID) %>%
    summarise(
      WS_GS = sum(GS[round == 'WS']),
      WS_GR = sum(G[round == 'WS']) - WS_GS,
      WS_W  = sum(W[round == 'WS']),
      LCS_W = sum(W[round %in% CSDS_Rounds])
    ) %>%
    ungroup() %>%
    select(playerID, yearID, WS_GS, WS_GR, WS_W, LCS_W)
  
  ee = .data %>%
    left_join(df, by=c("yearID", "playerID")) %>%
    tidyr::replace_na(list(WS_GS=0)) %>%
    tidyr::replace_na(list(WS_GR=0)) %>%
    tidyr::replace_na(list(WS_W =0)) %>%
    tidyr::replace_na(list(LCS_W=0))
}

# Append WAR Rank wins
append_war_rank <- function(war_df) {
  war_df %>%
    group_by(playerID) %>%
    mutate(WAR=as.numeric(WAR)) %>%
    arrange(-WAR) %>%
    dplyr::mutate(war_rank=row_number()) %>%
    ungroup() %>% arrange(playerID, yearID) }

# Append Steroids
append_steroids <- function(.data) {
  
  names <- c(
    'Barry Bonds',
    'Bret Boone',
    'Tim Beckham',
    'Ervin Santana',
    'Dee Gordon',
    'Starling Marte',
    'Fernando Tatis',
    'Ryan Braun',
    'Michael Pineda',
    'Melky Cabrera',
    'Bartolo Colon',
    'Nelson Cruz',
    'Paul Lo Duca',
    'Robinson Cano',
    'Kevin Brown',
    'Ken Caminiti',
    'Jose Canseco',
    'Roger Clemens',
    'Lenny Dykstra',
    'Chuck Finley',
    'Eric Gagne',
    'Jason Giambi',
    'Troy Glaus',
    'Jose Guillen',
    'Wally Joyner',
    'David Justice',
    'Chuck Knoblauch',
    'Mark McGwire',
    'Magglio Ordonez',
    'Rafael Palmeiro',
    'Andy Pettitte',
    'Manny Ramirez',
    'Brian Roberts',
    'Alex Rodriguez',
    'Benito Santiago',
    'Gary Sheffield',
    'Sammy Sosa',
    'Miguel Tejada',
    'Mo Vaughn',
    'Matt Williams',
    'Gregg Zaun'
  )
  
  false_names <- c("Kevin Brown3/14/1965", "Kevin Brown4/21/1973",
                   "Matt Williams7/25/1959", "Matt Williams4/12/1971",
                   "Nelson Cruz9/13/1972", "Ryan Braun7/29/1980")
  
  ROID_RAW <- tbl("People") %>%
    mutate(
      full_name = paste0(nameFirst, ' ', nameLast),
      birthdate = paste0(birthMonth,'/',birthDay,'/',birthYear),
      filt_name_bday = paste0(full_name,birthdate)
    ) %>%
    filter(full_name %in% names & filt_name_bday %notin% false_names) %>%
    mutate(
      TookSteroids = 1
    ) %>%
    select(playerID, TookSteroids)
  
  ROID <- .data %>%
    merge(ROID_RAW, by=c("playerID"), all.x=TRUE) %>%
    tidyr::replace_na(list(TookSteroids=0)) %>%
    dplyr::mutate(TookSteroids = ifelse(TookSteroids==1, 1, 0), TookSteroids=as.numeric(TookSteroids))
  
}

# Filter By Year
filter_by_years <- function(war_df, min_year=10) {
  war_df %>%
    group_by(playerID) %>%
    mutate(nyear=max(war_rank)) %>%
    filter(nyear >= min_year) %>%
    ungroup() %>%
    dplyr::select(-nyear)
}

# Create Train and Test Data Sets -- BATTERS
# takes the top MAXYEAR seasons, ordered by ORDER_KEY and #' returns the value of LIST_OF_STATS. If less than MAXYEAR has been played, filled in with 0
### Default Key is WAA, but can change to YearID for potential time-series model?
#' Batter feature matrix (thin wrapper over build_fit_matrix; see R/feature_matrix.R).
get_fit_data <- function(.data, MAXYEAR = 20, LIST_OF_STATS = NULL, ORDER_KEY = "WAA",
                         final_game_min = "1901-01-01", final_game_max = "2025-01-01") {
  if (is.null(LIST_OF_STATS)) {
    LIST_OF_STATS <- c(
      "PA", "SO", "BB", "WAA", "WAA_rank", "WAR", "WAR_off", "WAR_def",
      "jaws", "d_jaws", "HOFM_Points", "C_HOFM_Points",
      "AllStar", "AllStarStart", "WSWin", "MVPWin", "MVPShare",
      "SS_Win", "GG_Win", "TC_Win", "TookSteroids")
  }
  build_fit_matrix(.data, stats = LIST_OF_STATS, season_prefix = "PA",
                   has_djaws = TRUE, med_prefilter_positive = TRUE,
                   MAXYEAR = MAXYEAR, ORDER_KEY = ORDER_KEY,
                   final_game_min = final_game_min, final_game_max = final_game_max)
}

## Append Aggregated Data - Think I want to keep this just to Awards - No career WAA or WAR
#' Batter career aggregates (thin wrapper over aggregate_career).
get_aggregated_fit_df <- function(fit_df,
    keys_to_aggregate = c("^AllStar_", "^AllStarStart_", "^WSWin_", "^MVPWin_",
                          "^ROYWin", "^SS_Win_", "^GG_Win_", "^TC_Win_", "^HOFM_Points_"),
    keys_to_keep = c("^playerID$", "^C_HOFM_Points$")) {
  aggregate_career(fit_df, keys_to_aggregate, keys_to_keep)
}

###########################PITCHER SPECIFIC DATA############################
#' fit Pitchers data
#' takes the top MAXYEAR seasons, ordered by ORDER_KEY and #' returns the value of LIST_OF_STATS. If less than MAXYEAR has been played, filled in with 0
#' Pitcher feature matrix (thin wrapper over build_fit_matrix; see R/feature_matrix.R).
p_get_fit_data <- function(.data, MAXYEAR = 20, P_LIST_OF_STATS = NULL, ORDER_KEY = "WAA",
                           final_game_min = "1901-01-01", final_game_max = "2025-01-01") {
  if (is.null(P_LIST_OF_STATS)) {
    P_LIST_OF_STATS <- c(
      "IPouts", "BB", "SO", "W", "BAOpp", "L", "SV", "WAA", "WAA_rank", "WAR",
      "ERA_plus", "jaws", "HOFM_Points", "C_HOFM_Points",
      "AllStar", "AllStarStart", "MVPWin", "MVPShare", "CYWin", "CYShare",
      "GG_Win", "TookSteroids")
  }
  build_fit_matrix(.data, stats = P_LIST_OF_STATS, season_prefix = "IPouts",
                   has_djaws = FALSE, med_prefilter_positive = FALSE,
                   MAXYEAR = MAXYEAR, ORDER_KEY = ORDER_KEY,
                   final_game_min = final_game_min, final_game_max = final_game_max)
}

#' Pitcher career aggregates (thin wrapper over aggregate_career).
p_get_aggregated_fit_df <- function(fit_df,
    keys_to_aggregate = c("^AllStar_", "^AllStarStart_", "^MVPWin_", "^MVPShare_",
                          "^ROYWin", "^CYWin_", "^CYShare_", "^GG_Win_", "^HOFM_Points_"),
    keys_to_keep = c("^playerID$", "^C_HOFM_Points$")) {
  aggregate_career(fit_df, keys_to_aggregate, keys_to_keep)
}

#' Pitcher WAR (thin wrapper over append_war; see R/feature_matrix.R).
p_append_br_war <- function(.data, war) {
  append_war(.data, war, extra_cols = c("ERA_plus"))
}


## Manipulate WAR Features ##

# JAWS (Jaffe)
append_jaws <- function(.data) {
  
  jaws_df <- .data %>%
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
    ungroup() %>%
    select(playerID, jaws)
  
  jaws_df <- .data %>%
    group_by(playerID) %>%
    mutate(WAR=as.numeric(WAR_def)) %>%
    arrange(-WAR) %>%
    dplyr::mutate(war_rank=row_number(),
                  ipeak=as.integer(war_rank<=7),
                  nyear=max(war_rank)) %>%
    group_by(playerID, votedBy, POS) %>%
    dplyr::summarise(last_year=max(yearID),
                     nyear=mean(nyear),
                     cwar=sum(WAR),
                     pwar=sum(ipeak*WAR),
                     d_jaws=0.5*(cwar+pwar)) %>%
    ungroup() %>%
    select(playerID, d_jaws) %>%
    left_join(jaws_df, by = 'playerID')

  
  .data %>% left_join(jaws_df, by = 'playerID')
}

append_p_jaws <- function(.data) {
  
  jaws_df <- .data %>%
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
    ungroup() %>%
    select(playerID, jaws)
  
  
  .data %>% left_join(jaws_df, by = 'playerID')
}

# Hall of Fame Monitor (James)
append_HOFM <- function(.data){
  
  BALead_df <- .data %>%
    mutate(
      Qual_PA = case_when(
        yearID < 1961 ~ 477.4,
        yearID >= 1961 ~ 502.2
      )
    ) %>%
    filter(PA > Qual_PA) %>%
    group_by(yearID) %>%
    summarise(
      Qual_PA = max(Qual_PA),
      max_BA = max(BA)
      )
  
  MLBLead_df <- .data %>%
    mutate(XB = X2B + X3B) %>%
    group_by(yearID) %>%
    summarise(
      max_HR = max(HR),
      max_RBI = max(RBI),
      max_R = max(R),
      max_H = max(H),
      max_SB = max(SB),
      max_XB = max(XB)
    )
  
  
  HOFM_df <- .data %>%
    left_join(BALead_df, by = 'yearID') %>%
    left_join(MLBLead_df, by = 'yearID') %>%
    mutate(
      BA_pts = case_when(
        BA >= 0.4 & G > 100 ~ 15.0,
        BA >= 0.35 & BA < 0.4 & G >= 100 ~ 5.0,
        BA >= 0.3 & BA < 0.35 & G >= 100 ~ 2.5,
        TRUE ~ 0
      ),
      H_pts = case_when(
        H >= 200 ~ 5,
        TRUE ~ 0
      ),
      RBI_pts = case_when(
        RBI >= 100 ~ 3,
        TRUE ~ 0
      ),
      R_pts = case_when(
        R >= 100 ~ 3,
        TRUE ~ 0
      ),
      HR_pts = case_when(
        HR >= 50 ~ 10,
        HR >= 40 & HR < 50 ~ 4,
        HR >= 30 & HR < 40 ~ 2,
        TRUE ~ 0
      ),
      X2B_pts = case_when(
        X2B >= 45 ~ 2,
        X2B >= 35 & X2B < 45 ~ 1,
        TRUE ~ 0
      ),
      MVP_pts = case_when(
        MVPWin == 1 ~ 8,
        TRUE ~ 0
      ),
      ASG_pts = case_when(
        AllStar == 1 ~ 3,
        TRUE ~ 0
      ),
      GG_pts = case_when(
        GG_Win == 1 & POS %in% c('C', 'SS', '2B') ~ 2,
        GG_Win == 1 & POS %notin% c('C', 'SS', '2B') ~ 1,
        TRUE ~ 0
      ),
      WS_pts = case_when(
        G > 82 & WSWin == 1 & POS %in% c('C', 'SS') ~ 6,
        G > 82 & WSWin == 1 & POS %in% c('2B') ~ 5,
        G > 82 & WSWin == 1 & POS %in% c('3B','OF') ~ 3,
        G > 82 & WSWin == 1 & POS %in% c('1B') ~ 1,
        TRUE ~ 0
      ),
      Pen_pts = case_when(
        G > 82 & PenWin == 1 & WSWin == 0 & POS %in% c('C', 'SS') ~ 5,
        G > 82 & PenWin == 1 & WSWin == 0 & POS %in% c('2B') ~ 3,
        G > 82 & PenWin == 1 & WSWin == 0 & POS %in% c('3B','OF') ~ 1,
        TRUE ~ 0
      ),
      Div_pts = case_when(
        G > 82 & DivWin == 1 & PenWin == 0 & WSWin == 0 & POS %in% c('C', 'SS') ~ 2,
        G > 82 & DivWin == 1 & PenWin == 0 & WSWin == 0 & POS %in% c('2B', '3B','OF') ~ 1,
        TRUE ~ 0
      ),
      LeadBA_pts = case_when(
        PA > Qual_PA & BA == max_BA ~ 6,
        TRUE ~ 0
      ),
      LeadHR_pts = case_when(
        HR == max_HR ~ 4,
        TRUE ~ 0
      ),
      LeadRBI_pts = case_when(
        RBI == max_RBI ~ 4,
        TRUE ~ 0
      ),
      LeadR_pts = case_when(
        R == max_R ~ 3,
        TRUE ~ 0
      ),
      LeadH_pts = case_when(
        H == max_H ~ 2,
        TRUE ~ 0
      ),
      LeadSB_pts = case_when(
        SB == max_SB ~ 2,
        TRUE ~ 0
      ),
      LeadXB_pts = case_when(
        (X2B+X3B) == max_XB ~ 2,
        TRUE ~ 0
      )
      ) %>%
    rowwise() %>%
    mutate(
      HOFM_Points = sum(c_across(ends_with("_pts")))
    ) %>%
    ungroup() %>%
    select(-ends_with("_pts"), -starts_with("max_"), -Qual_PA)
  return(HOFM_df)
}

append_C_HOFM <- function(.data){
  
  HOFM_df <- .data %>%
    group_by(playerID, POS) %>%
    mutate(
      C_H_Pts = case_when(
        sum(H) >= 3500 ~ 50,
        sum(H) >= 3000 & sum(H) < 3500 ~ 40,
        sum(H) >= 2500 & sum(H) < 3000 ~ 15,
        sum(H) >= 2000 & sum(H) < 2500 ~ 4,
        TRUE ~ 0
      ),
      C_HR_Pts = case_when(
        sum(HR) >= 600 ~ 30,
        sum(HR) >= 500 & sum(HR) < 600 ~ 20,
        sum(HR) >= 400 & sum(HR) < 500 ~ 10,
        sum(HR) >= 300 & sum(HR) < 400 ~ 3,
        TRUE ~ 0
      ),
      C_BA_Pts = case_when(
        (sum(H) / sum(AB)) >= .330 ~ 24,
        (sum(H) / sum(AB)) >= .315 ~ 16,
        (sum(H) / sum(AB)) >= .300 ~ 8,
        TRUE ~ 0
      ),
      C_G_Pts = case_when(
        POS == 'C' & sum(G) >= 1800 ~ 60,
        POS == 'C' & sum(G) >= 1600 & sum(G) < 1800 ~ 45,
        POS == 'C' & sum(G) >= 1400 & sum(G) < 1600 ~ 30,
        POS == 'C' & sum(G) >= 1200 & sum(G) < 1400 ~ 15,
        POS %in% c('2B', 'SS') & sum(G) >= 2100 ~ 30,
        POS %in% c('2B', 'SS') & sum(G) >= 1800 & sum(G) < 2100  ~ 15,
        POS == '3B' & sum(G) > 2000 ~ 15,
        TRUE ~ 0
      ),
      C_GAdd_Pts = case_when(
        POS %in% c('2B', 'SS', '3B') & sum(G) >= 2500 ~ 15,
        TRUE ~ 0
      ),
      C_GBA_Add_Pts = case_when(
        POS %in% c('2B', 'SS', 'C') & sum(G) >= 1500 & (sum(H) / sum(AB)) >= .275 ~ 15,
        TRUE ~ 0
      ),
      C_HOFM_Points = C_H_Pts + C_HR_Pts + C_BA_Pts + C_G_Pts + C_GAdd_Pts + C_GBA_Add_Pts
    ) %>%
    ungroup() %>%
    select(-ends_with("_Pts"), -starts_with("max_"))
  
  return(HOFM_df)
}

p_append_HOFM <- function(.data){
  
  BALead_df <- .data %>%
    mutate(
      Qual_IP = case_when(
        yearID < 1961 ~ 154,
        yearID >= 1961 ~ 162
      ),
      WP = W / (W+L)
    ) %>%
    filter(IPouts > Qual_IP) %>%
    group_by(yearID) %>%
    summarise(
      Qual_IP = max(Qual_IP),
      min_ERA = min(ERA),
      max_WP = max(WP)
    )
  
  MLBLead_df <- .data %>%
    group_by(yearID) %>%
    summarise(
      max_G = max(G),
      max_W = max(W),
      max_IPouts = max(IPouts),
      max_SO = max(SO),
      max_SV = max(SV),
      max_SHO = max(SHO),
      max_CG = max(CG)
    )
  
  
  HOFM_df <- .data %>%
    left_join(BALead_df, by = 'yearID') %>%
    left_join(MLBLead_df, by = 'yearID') %>%
    mutate(
      W_pts = case_when(
        W >= 30 ~ 15,
        W >= 25 & W < 30 ~ 10,
        W >= 23 & W < 25 ~ 8,
        W >= 20 & W < 23 ~ 6,
        W >= 18 & W < 20 ~ 4,
        W >= 15 & W < 18 ~ 2,
        TRUE ~ 0
      ),
      SO_pts = case_when(
        SO >= 300 ~ 6,
        SO >= 250 & SO < 300 ~ 3,
        SO >= 200 & W < 250 ~ 2,
        TRUE ~ 0
      ),
      WP_pts = case_when(
        W >= 14 & W/(W+L) >= 0.7 ~ 2,
        TRUE ~ 0
      ),
      ERA_pts = case_when(
        IPouts >= 150 & ERA < 2.00 ~ 4,
        IPouts >= 150 & ERA < 3.00 & ERA >=2.00 ~ 1,
        TRUE ~ 0
      ),
      SV_pts = case_when(
        SV >= 40 ~ 7,
        SV >= 30 & SV < 40 ~ 4,
        SV >= 20 & SV < 30 ~ 1,
        TRUE ~ 0
      ),
      MVP_pts = case_when(
        MVPWin == 1 ~ 8,
        TRUE ~ 0
      ),
      CY_pts = case_when(
        CYWin == 1 ~ 5,
        TRUE ~ 0
      ),
      ASG_pts = case_when(
        AllStar == 1 ~ 3,
        TRUE ~ 0
      ),
      GG_pts = case_when(
        GG_Win == 1 ~ 1,
        TRUE ~ 0
      ),
      WS_GS_pts = WS_GS*2,
      WS_GR_pts = WS_GR,
      WS_W_pts = WS_W*2,
      LCS_W_pts = LCS_W, 
      LeadERA_pts = case_when(
        IPouts > Qual_IP & ERA == min_ERA ~ 2,
        TRUE ~ 0
      ),
      LeadG_pts = case_when(
        G == max_G ~ 1,
        TRUE ~ 0
      ),
      LeadW_pts = case_when(
        W == max_W ~ 1,
        TRUE ~ 0
      ),
      LeadIP_pts = case_when(
        IPouts == max_IPouts ~ 1,
        TRUE ~ 0
      ),
      LeadWP_pts = case_when(
        (W/(W+L)) == max_WP & IPouts > Qual_IP ~ 1,
        TRUE ~ 0
      ),
      LeadSO_pts = case_when(
        SO == max_SO ~ 1,
        TRUE ~ 0
      ),
      LeadSV_pts = case_when(
        SV == max_SV ~ 1,
        TRUE ~ 0
      ),
      LeadSHO_pts = case_when(
        SHO == max_SHO ~ 1,
        TRUE ~ 0
      ),
      LeadCG_pts = case_when(
        CG == max_CG ~ 0.5,
        TRUE ~ 0
      )
    ) %>%
    rowwise() %>%
    mutate(
      HOFM_Points = sum(c_across(ends_with("_pts")))
    ) %>%
    ungroup() %>%
    select(-ends_with("_pts"), -starts_with("max_"), -Qual_IP, -min_ERA)
  return(HOFM_df)
}

p_append_C_HOFM <- function(.data){
  
  HOFM_df <- .data %>%
    group_by(playerID) %>%
    mutate(
      C_W_Pts = case_when(
        sum(W) >= 300 ~ 35,
        sum(W) >= 275 & sum(W) < 300 ~ 25,
        sum(W) >= 250 & sum(W) < 275 ~ 20,
        sum(W) >= 225 & sum(W) < 250 ~ 15,
        sum(W) >= 200 & sum(W) < 225 ~ 10,
        sum(W) >= 175 & sum(W) < 200 ~ 8,
        sum(W) >= 150 & sum(W) < 175 ~ 5,
        TRUE ~ 0
      ),
      C_WP_Pts = case_when(
        sum(W) / (sum(W) + sum(L)) >= .625 & sum(GS) >= 190 ~ 8,
        sum(W) / (sum(W) + sum(L)) >= .600 & sum(W) / (sum(W) + sum(L)) < .625 & sum(GS) >= 190 ~ 5,
        sum(W) / (sum(W) + sum(L)) >= .575 & sum(W) / (sum(W) + sum(L)) < .600 & sum(GS) >= 190 ~ 3,
        sum(W) / (sum(W) + sum(L)) >= .550 & sum(W) / (sum(W) + sum(L)) < .575 & sum(GS) >= 190 ~ 1,
        TRUE ~ 0
      ),
      C_ERA_Pts = case_when(
        ((sum(ER) / sum(IPouts)))*9 < 3.00 & sum(GS) >= 190 ~ 10,
        TRUE ~ 0
      ),
      C_SV_Pts = case_when(
        sum(SV) >= 300 ~ 20,
        sum(SV) >= 200 & sum(SV) < 300 ~ 10,
        TRUE ~ 0
      ),
      C_G_Pts = case_when(
        sum(G) >= 1000 ~ 30,
        sum(G) >= 850 & sum(G) < 1000 ~ 20,
        sum(G) >= 700 & sum(G) < 850 ~ 10,
        TRUE ~ 0
      ),
      C_SO_Pts = case_when(
        sum(SO) >= 4000 ~ 20,
        sum(SO) >= 3000 & sum(SO) < 4000 ~ 10,
        TRUE ~ 0
      ),
      C_3CY_Pts = case_when(
        sum(CYWin) >= 3 ~ 15,
        TRUE ~ 0
      ),
      C_HOFM_Points = C_SO_Pts + C_G_Pts + C_SV_Pts + C_ERA_Pts + C_WP_Pts + C_W_Pts + C_3CY_Pts
    ) %>%
    ungroup() %>%
    select(-ends_with("_Pts"), -starts_with("max_"))
  
  return(HOFM_df)
}

normalize_war <- function(dfX) {
  cc <- which(grepl('^WAR_', names(dfX)))
  tmp <- dfX %>% filter(WAR_1>0)
  tmp <- na.omit(tmp)
  tmp[,cc] <- tmp[,cc] / tmp$WAR_1
  tmp
  
}
