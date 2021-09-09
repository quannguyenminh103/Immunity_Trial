# if(LatestDate<today  & HourNow> ExpectedUpdateBefore){
  ## DATA FOR COVID-19 CASES
  dataurl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
  his_data <- read.csv(dataurl, stringsAsFactors = FALSE) %>% mutate(date = as.Date(date)) %>% select(date,state,cases,deaths)
  cur_date <- his_data$date[length(his_data$date)]
  
  cur_data <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/live/us-states.csv", stringsAsFactors = FALSE) %>% mutate(date = as.Date(date)) %>%
    select(date,state,cases,deaths)
  ## combine the historical data and today data together
  if (cur_date != cur_data$date[1]){
    data <- rbind(his_data, cur_data)
  }
  
  # Population
  pop <- read.csv("./population.csv")
  combined_data <- data %>% inner_join(pop, by = 'state')
  
  ### this table includes the data for vaccination 
  vacc_data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv",stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    select(date_vac = date, state = location, people_vaccinated, people_fully_vaccinated)
  vacc_data$state[vacc_data$state == 'New York State'] <- 'New York'

  ### this table combines all information we need for ONLY CASES:
  cases_data <- combined_data %>% group_by(date) %>% summarise(date = date[1],
                                                 state = "United States",
                                                 cases = sum(cases),
                                                 population = 328239523,
                                                 Code = 'US') %>%
    bind_rows(combined_data) %>% as.data.frame() %>% arrange(desc(date)) %>% distinct(.keep_all = TRUE)
  colorSelected <- c("#7F96E4", "#EB996F", "#D7B2CE", "#EEE6C5", "#B7E4EC", "#ADEE87", "#5FBDDA", "#AEEECC", "#D45959", 
                     "#CE5E86", "#63B952", "#EECE9B", "#62ED8E", "#E253D7", "#B3C359", "#B2C5EC", "#CDE63C", "#DD419F",
                     "#A35FD9", "#5BEACF", "#7FBDA9", "#6BE7A9", "#C235E3", "#E99493", "#F0B3BA", "#EB9C3F", "#DCE8A9",
                     "#6E5AA2", "#74E4E7", "#6576EE", "#B16C4A", "#E578CB", "#E2EEE5", "#EE3A63", "#B4A369", "#673CDA",
                     "#C3B5A0", "#EAEA85", "#E7D6E8", "#58A670", "#69EF38", "#9E7983", "#65EA61", "#4E6F97", "#EAC872",
                     "#5DA6D8", "#EEE34A", "#52837A", "#D9ACEA", "#D1B43D", "#A6EB45", "#B888E1", "#E993C2", "#9DABB0",
                     "#E96339", "#A4D08B")
  stateList <- sort(unique(cases_data$state))
  all_states <- stateList[c(which(stateList == "United States"),1:which(stateList == "United States")-1,(which(stateList == "United States") + 1):length(stateList))]
  if (max(cases_data$date) <= max(vacc_data$date_vac)) {
      updated_date <- max(cases_data$date) 
  } else {
    updated_date <- max(vacc_data$date_vac)
  }
  DATA <- data.frame(Date = as.character(), State = as.character(), Code = as.character(), ABias = as.numeric(),
                      VaccType = as.character(), CaseFrac = as.numeric(), VaccFrac = as.numeric(), ImmuLevel = as.numeric())
  # at least one dose and ascertainment bias of 3
  OneDoseThreeAB <- LoadDatabyDate(all_states, 3, "At Least One Dose")
  # at least one dose and ascertainment bias of 4
  OneDoseFourAB <- LoadDatabyDate(all_states, 4, "At Least One Dose")
  # at least one dose and ascertainment bias of 5
  OneDoseFiveAB <- LoadDatabyDate(all_states, 5, "At Least One Dose")
  # fully vaccinated and ascertainment bias of 3
  FullThreeAB <- LoadDatabyDate(all_states, 3, "Fully Vaccinated")
  # fully vaccinated and ascertainment bias of 3
  FullFourAB <- LoadDatabyDate(all_states, 4, "Fully Vaccinated")
  # fully vaccinated and ascertainment bias of 5
  FullFiveAB <- LoadDatabyDate(all_states, 5, "Fully Vaccinated")
  DATA <- rbind(DATA, OneDoseThreeAB, OneDoseFourAB, OneDoseFiveAB, FullThreeAB, FullFourAB, FullFiveAB)
  STATELINE <- as.data.frame(st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/tl_2017_us_state.geojson"))
  source('LoadUS.R')
  GETCOUNTYDATA = LoadUS()
  US_DATA = as.data.frame(GETCOUNTYDATA[1])
  missingCounties = unlist(GETCOUNTYDATA[2])
  today = unlist(GETCOUNTYDATA[3])
  ## there are some counties that aren't UNKNOWN, but, they reported 0 for vaccine
  missingCounties = c(missingCounties, US_DATA[which(is.na(US_DATA["VaccOneFrac"])),"fips"])
  missingCounties = na.omit(missingCounties)
  # for (code in missingCounties){
  #   state_abbreviation = US_DATA[US_DATA$fips == code,"stname"]
  #   US_DATA[US_DATA$fips == code, "VaccOneFrac"] <-  OneDoseThreeAB[which(OneDoseThreeAB$Code == state_abbreviation & OneDoseThreeAB$Date == today),"VaccFrac"]
  #   US_DATA[US_DATA$fips == code, "VaccFullFrac"] <- FullThreeAB[which(FullThreeAB$Code == state_abbreviation & FullThreeAB$Date == today),"VaccFrac"]
  # }
  # 
  # stateNeeded <- c('GA', 'VA', 'WV', 'TX', "HI", 'CO', 'NM', "NE", 'SD')
  # for (stateSelected in stateNeeded) {
  #   US_DATA[US_DATA$stname == stateSelected, "VaccOneFrac"] <-  OneDoseThreeAB[which(OneDoseThreeAB$Code == stateSelected & OneDoseThreeAB$Date == today),"VaccFrac"]
  #   US_DATA[US_DATA$stname == stateSelected, "VaccFullFrac"] <- FullThreeAB[which(FullThreeAB$Code == stateSelected & FullThreeAB$Date == today),"VaccFrac"]
  # }
  for (code in missingCounties){
    state_abbreviation = US_DATA[US_DATA$fips == code,"stname"]
    US_DATA[US_DATA$fips == code, "VaccOneFrac"] <-  OneDoseThreeAB[which(OneDoseThreeAB$Code == state_abbreviation & OneDoseThreeAB$Date == today),"VaccFrac"]
    US_DATA[US_DATA$fips == code, "VaccFullFrac"] <- FullThreeAB[which(FullThreeAB$Code == state_abbreviation & FullThreeAB$Date == today),"VaccFrac"]
    US_DATA[US_DATA$fips == code, "OneDose3AB"] <- OneDoseThreeAB[which(OneDoseThreeAB$Code == state_abbreviation & OneDoseThreeAB$Date == today),"ImmuLevel"]
    US_DATA[US_DATA$fips == code, "OneDose4AB"] <- OneDoseFourAB[which(OneDoseFourAB$Code == state_abbreviation & OneDoseFourAB$Date == today),"ImmuLevel"]
    US_DATA[US_DATA$fips == code, "OneDose5AB"] <- OneDoseFiveAB[which(OneDoseFiveAB$Code == state_abbreviation & OneDoseFiveAB$Date == today),"ImmuLevel"]
    US_DATA[US_DATA$fips == code, "FullDose3AB"] <- FullThreeAB[which(FullThreeAB$Code == state_abbreviation & FullThreeAB$Date == today),"ImmuLevel"]
    US_DATA[US_DATA$fips == code, "FullDose4AB"] <- FullFourAB[which(FullFourAB$Code == state_abbreviation & FullFourAB$Date == today),"ImmuLevel"]
    US_DATA[US_DATA$fips == code, "FullDose5AB"] <- FullFiveAB[which(FullFiveAB$Code == state_abbreviation & FullFiveAB$Date == today),"ImmuLevel"]
  }

  # stateNeeded <- c('GA', 'VA', 'WV', 'TX', "HI", 'CO', 'NM', "NE", 'SD')
  # for (stateSelected in stateNeeded) {
  #   US_DATA[US_DATA$stname == stateSelected, "VaccOneFrac"] <-  OneDoseThreeAB[which(OneDoseThreeAB$Code == stateSelected & OneDoseThreeAB$Date == today),"VaccFrac"]
  #   US_DATA[US_DATA$stname == stateSelected, "VaccFullFrac"] <- FullThreeAB[which(FullThreeAB$Code == stateSelected & FullThreeAB$Date == today),"VaccFrac"]
  #   US_DATA[US_DATA$stname == stateSelected, "OneDose3AB"] <- OneDoseThreeAB[which(OneDoseThreeAB$Code == stateSelected & OneDoseThreeAB$Date == today),"ImmuLevel"]
  #   US_DATA[US_DATA$stname == stateSelected, "OneDose4AB"] <- OneDoseFourAB[which(OneDoseFourAB$Code == stateSelected & OneDoseFourAB$Date == today),"ImmuLevel"]
  #   US_DATA[US_DATA$stname == stateSelected, "OneDose5AB"] <- OneDoseFiveAB[which(OneDoseFiveAB$Code == stateSelected & OneDoseFiveAB$Date == today),"ImmuLevel"]
  #   US_DATA[US_DATA$stname == stateSelected, "FullDose3AB"] <- FullThreeAB[which(FullThreeAB$Code == stateSelected & FullThreeAB$Date == today),"ImmuLevel"]
  #   US_DATA[US_DATA$stname == stateSelected, "FullDose4AB"] <- FullFourAB[which(FullFourAB$Code == stateSelected & FullFourAB$Date == today),"ImmuLevel"]
  #   US_DATA[US_DATA$stname == stateSelected, "FullDose5AB"] <-FullFiveAB[which(FullFiveAB$Code == stateSelected & FullFiveAB$Date == today),"ImmuLevel"]
  # }
  
  US_DATA$OneDose3AB <- round((1 - (1-US_DATA$pInf*3)*(1-US_DATA$VaccOneFrac))*100)
  US_DATA$OneDose4AB <- round((1 - (1-US_DATA$pInf*4)*(1-US_DATA$VaccOneFrac))*100)
  US_DATA$OneDose5AB <- round((1 - (1-US_DATA$pInf*5)*(1-US_DATA$VaccOneFrac))*100)
  US_DATA$FullDose3AB <- round((1 - (1-US_DATA$pInf*3)*(1-US_DATA$VaccFullFrac))*100)
  US_DATA$FullDose4AB <- round((1 - (1-US_DATA$pInf*4)*(1-US_DATA$VaccFullFrac))*100)
  US_DATA$FullDose5AB <- round((1 - (1-US_DATA$pInf*5)*(1-US_DATA$VaccFullFrac))*100)
  US_DATA$pInf <- round(US_DATA$pInf*100)
  US_DATA$VaccOneFrac <- round(US_DATA$VaccOneFrac*100)
  US_DATA$VaccFullFrac <- round(US_DATA$VaccFullFrac*100)
  
  #}