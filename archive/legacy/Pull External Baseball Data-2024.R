combine_bat_pitch <- function(.data) {
  db = .data$BattingWAR
  dp = .data$PitchingWAR
  db %>%
    select(player_ID, year_ID, WAR) %>%
    group_by(player_ID, year_ID) %>%
    reframe(WAR=sum(as.numeric(WAR)))
  
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

# Get Ballot
ballot_ids_26 <- c(
  # First-time candidates
  "braunry",    # Ryan Braun
  "hamelco",    # Cole Hamels
  "porceri",    # Rick Porcello
  "choosh0",     # Shin-Soo Choo
  "encared",    # Edwin Encarnación
  "gordoal",    # Alex Gordon
  "gonzagi",    # Gio González
  "kempma0",     # Matt Kemp
  "markani",    # Nick Markakis
  "kendrho",    # Howie Kendrick
  "murphda",    # Daniel Murphy
  "pencehu",    # Hunter Pence
  
  # Returning candidates
  "beltrca",    # Carlos Beltrán
  "jonean0",     # Andruw Jones
  "rodrial",    # Alex Rodríguez
  "ramirma",    # Manny Ramírez
  "utleych",    # Chase Utley
  "pettian",    # Andy Pettitte
  "hernafe",    # Félix Hernández
  "abreubo",    # Bobby Abreu
  "pedrodu",    # Dustin Pedroia
  "rolliji",    # Jimmy Rollins
  "vizquom",    # Omar Vizquel
  "buehrma",    # Mark Buehrle
  "rodrifr",    # Francisco Rodríguez
  "wrighda",    # David Wright
  "hunteto"     # Torii Hunter
)
find_hof_ballot_ids <- function(l = ballot_ids_26, last_year = 2020, rem_ids = c(), add_ids = c()){
  bat_df <- get_lahman_batting()  %>% filter(playerID %notin% c()) %>% mutate(short_playerID = substr(playerID, 1, 7)) %>% filter(short_playerID %in% l) %>% group_by(playerID, short_playerID) %>% summarise(maxYear = max(yearID), .groups = "drop") %>% filter(maxYear <= last_year) %>% group_by(short_playerID) %>% filter(maxYear == max(maxYear)) %>% ungroup() %>% arrange(playerID)
  pit_df <- get_lahman_pitching() %>% filter(playerID %notin% c()) %>% mutate(short_playerID = substr(playerID, 1, 7)) %>% filter(short_playerID %in% l) %>% group_by(playerID, short_playerID) %>% summarise(maxYear = max(yearID), .groups = "drop") %>% filter(maxYear <= last_year) %>% group_by(short_playerID) %>% filter(maxYear == max(maxYear)) %>% ungroup() %>% arrange(playerID)
  
  all_ids <- unique(c(bat_df$playerID, pit_df$playerID))
  
  # Drop
  all_ids <- setdiff(all_ids, rem_ids)
  
  # Add
  all_ids <- c(all_ids, add_ids)
  
  return(all_ids)
}

HOF_Ballot_IDs <- find_hof_ballot_ids(rem_ids = c("hunteto03", "braunry01", "wrighda04", "murphda07"), add_ids = c("jonesan01"))
