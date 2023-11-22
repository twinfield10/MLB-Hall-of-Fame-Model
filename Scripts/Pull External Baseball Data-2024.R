library(readr)

pull_br_data <- function() {
  bref_bat_url = 'https://www.baseball-reference.com/data/war_daily_bat.txt'
  bref_pitch_url = 'https://www.baseball-reference.com/data/war_daily_pitch.txt'
 
  dtype <- "ciiciciccicdddddddddddddddddddddddccddddddddddddd"
  br_bat_war <- read_csv(bref_bat_url, col_types = dtype)
  
  dtype <- "ciiciciciiiddidddiddiddddddddddddddddddddd"
  br_pitch_war <- read_csv(bref_pitch_url, col_types = dtype)
  list(BattingWAR=br_bat_war, PitchingWAR=br_pitch_war)
}

pull_fg_data <- function () {
  #BATTING: https://www.fangraphs.com/leaders-legacy.aspx?pos=all&stats=bat&lg=all&qual=y&type=c,4,6,11,12,13,21,-1,40,41,-1,23,37,38,50,61,111,-1,203,199,58,7,8,9,10,14,15,16,17,18,19,20,21,22,50,51,52&season=2022&month=0&season1=1871&ind=1&team=&rost=&age=&filter=&players=0&startdate=&enddate=
  #PITCHING: https://www.fangraphs.com/leaders-legacy.aspx?pos=all&stats=pit&lg=all&qual=y&type=c,4,5,11,7,8,13,-1,36,37,40,43,44,48,51,-1,6,45,62,-1,59,6,14,15,16,17,18,19,20,21,22,23,24&season=2022&month=0&season1=1871&ind=1&team=0&rost=0&age=0&filter=&players=0
  
  #GIT Read
  bat_path <- 'https://raw.githubusercontent.com/twinfield10/MLB-Hall-of-Fame-Model/main/Data/FG_WAR_BAT_2023.csv'
  pit_path <- 'https://raw.githubusercontent.com/twinfield10/MLB-Hall-of-Fame-Model/main/Data/FG_WAR_PIT_2023.csv'
  
  fg_bat_war <- read_csv(bat_path)
  fg_pitch_war <- read_csv(pit_path)
  list(BattingWAR=fg_bat_war, PitchingWAR=fg_pitch_war) }

combine_bat_pitch <- function(.data) {
  db = .data$BattingWAR
  dp = .data$PitchingWAR
  db %>%
    select(player_ID, year_ID, WAR) %>%
    group_by(player_ID, year_ID) %>%
    summarise(WAR=sum(as.numeric(WAR)))
  
}

## Roid IDs ##
get_ROID_ID <- function() {
  names <- c(
    'Barry Bonds','Bret Boone','Tim Beckham','Ervin Santana','Dee Gordon',
    'Starling Marte','Fernando Tatis','Ryan Braun','Michael Pineda','Melky Cabrera',
    'Bartolo Colon','Nelson Cruz','Paul Lo Duca','Robinson Cano','Kevin Brown',
    'Ken Caminiti','Jose Canseco','Roger Clemens','Lenny Dykstra','Chuck Finley',
    'Eric Gagne','Jason Giambi','Troy Glaus','Jose Guillen','Wally Joyner',
    'David Justice','Chuck Knoblauch','Mark McGwire','Magglio Ordonez',
    'Rafael Palmeiro','Andy Pettitte','Manny Ramirez','Brian Roberts','Alex Rodriguez',
    'Benito Santiago','Gary Sheffield','Sammy Sosa','Miguel Tejada','Mo Vaughn',
    'Matt Williams','Gregg Zaun'
  )
  
  false_names <- c("Kevin Brown3/14/1965", "Kevin Brown4/21/1973",
                   "Matt Williams7/25/1959", "Matt Williams4/12/1971",
                   "Nelson Cruz9/13/1972", "Ryan Braun7/29/1980")
  
  ROID_RAW <- Lahman::People %>%
    mutate(
      full_name = paste0(nameFirst, ' ', nameLast),
      birthdate = paste0(birthMonth,'/',birthDay,'/',birthYear),
      filt_name_bday = paste0(full_name,birthdate)
    ) %>%
    filter(full_name %in% names & filt_name_bday %notin% false_names) %>%
    mutate(
      TookSteroids = 'Y'
    )
  
  ROID <- ROID_RAW %>%
    select(playerID) %>%
    pull()
  
  return(ROID)
}
ROID_IDS <- get_ROID_ID()