LoadUS <- function() {
#cases from NYT
  dataurl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  data <- as.data.frame(fread(dataurl, stringsAsFactors = FALSE)) %>% mutate(date = as_date(date))
  data[data$state == "Guam", "fips"] <- 66010
  #geography
  county <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/tl_2017_us_county.geojson")
  #merge counties that are reported together by the NYT
  HMM<- st_union(county[c(2782,1612),])%>% st_cast("MULTIPOLYGON")
  county$geometry[2782] = HMM
  county$GEOID[2782] = 2997
  county$NAME[2782] = "Bristol Bay plus Lake Peninsula"
  HMM<- st_union(county[c(30,3049),])%>% st_cast("MULTIPOLYGON")
  county$geometry[30] = HMM
  county$GEOID[30] = 2998
  county$NAME[30] = "Yakutat plus Hoonah-Angoon"
    
  #county population level data
  pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/county-population.csv", stringsAsFactors = FALSE) %>%
    select(-X)
  pop <- pop %>% rbind(c(66010,170179), c(78010, 50601), c(78020,4170),c(78030,51634)) # guam, st. croix, st. john, st.thomas (virgin islands)
  #merge population for counties reported together by the NYT  
  #bristol bay and Lake Peninsula
  IND = which(pop$fips == 2164)
  IND2 = which(pop$fips == 2060)
  pop$fips[IND] = 2997
  pop$pop[IND] = pop$pop[IND]+ pop$pop[IND2]
  #Yakutat and Hoonah-Angoon
  IND = which(pop$fips == 2282)
  IND2 = which(pop$fips == 2105)
  pop$fips[IND] = 2998
  pop$pop[IND] = pop$pop[IND]+ pop$pop[IND2]

#calculate total cases 14 days ago
  #cur_date <- data$date[length(data$date)]
  cur_date <- "2021-08-04"
  past_date <- ymd(cur_date) - 14
  
  data_past <- data %>%
    filter(date == past_date) %>%
    mutate(fips = case_when(
      county == "New York City" ~ 99999,
      TRUE ~ as.numeric(fips)
    )) %>%
    select(date_past = date, fips = fips, cases_past = cases) %>% mutate(date = date_past + 14)
  data_join <<- data_past %>%
    #inner_join(data_past, by = "fips") %>%
    inner_join(pop, by = "fips") 
  #data_join$CaseDiff <- (data_join$cases-data_join$cases_past)*10/data_join$n
  data_join$date <- as.character(data_join$date)
  today <- max(data_join$date)
  ## Vaccination
  vacc_county <- as.data.frame(fread("https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD")) %>%
    filter(Date >= as.character(format(as.Date(today) - 14, format = "%m/%d/%Y")), Date <= as.character(format(as.Date(today), format = "%m/%d/%Y"))) %>%
    select(date = Date, fips = FIPS, VaccOne = Administered_Dose1_Recip, VaccFull = Series_Complete_Yes)
  
  vacc_county$fips <- as.numeric(vacc_county$fips)
  #vacc_county <- na.omit(vacc_county)
  vacc_county <- vacc_county %>% drop_na(date)
  dateList <- unique(vacc_county$date)
  exceptionCount <- function(stateAbb, county_fips_list, county_fips) {
    for (i in 1:length(dateList)){
      newrow <- data.frame(date = as.character(), fips = as.numeric(), VaccOne = as.numeric(), VaccFull = as.numeric())
      newrow[1,"date"] <- dateList[i]
      newrow[1,"fips"] <- county_fips
      newrow[1,"VaccOne"] <- 0
      newrow[1,"VaccFull"] <- 0
      for (j in county_fips_list) {
        newrow[1,"VaccOne"] <- newrow[1,"VaccOne"] + vacc_county %>% filter(date == dateList[i], fips == j) %>% select(VaccOne)
        newrow[1,"VaccFull"] <- newrow[1,"VaccFull"] + vacc_county %>% filter(date == dateList[i], fips == j) %>% select(VaccFull)
      }
      vacc_county <- vacc_county %>% rbind(newrow)
    }
    return(vacc_county)
  }
  ## New York City
  ny_fips_list = c(36061, 36005, 36047, 36081, 36085) # New York, Bronx, Kings, Queens, Richmon
  vacc_county <- exceptionCount("NY", ny_fips_list, 99999)
  ## Kansas City, MO: Cass, Clay, Jackson and Platte -> no need to change
  ## Joplin, MO: no need to change
  ## Alaska:
  ak_fips_list1 = c(2282,2105)
  ak_fips_list2 = c(2060,2164)
  vacc_county <- exceptionCount("AK", ak_fips_list1, 2998)
  vacc_county <- exceptionCount("AK", ak_fips_list2, 2997)
  ## Alameda County, Cali: no need to change
  ## Douglas County, Neb: no need to change
  ## Chicago: no need to change
  ## delete some unnecessary counties and states
  county <- county[which(county$GEOID != 2060),]
  county <- county[which(county$GEOID != 2105),]
  county <- county[which(county$stname != "AS"),]
  county <- county[which(county$stname != "MP"),]
  # get vaccine data based on the most updated case dataset
  removedFips <-  setdiff(unique(vacc_county$fips), unique(county$GEOID))
  vacc_county <- vacc_county[! vacc_county$fips %in% removedFips,]
  # vacc_county_today <- vacc_county %>% filter(date == as.character(format(as.Date(today) - 14, format = "%m/%d/%Y"))) %>% 
  #   select(-date)
  vacc_county_today <- vacc_county %>% filter(date == as.character(format(as.Date(today) - 14, format = "%m/%d/%Y"))) %>% 
    select(-date)
  
  # ######integrate texas datasets
  # texas_fips <- county %>% filter(stname == "TX") %>% as.data.frame() %>% select(fips = GEOID, NAME)
  # texasData <- read.csv("https://raw.githubusercontent.com/shiruken/covid-texas/master/docs/vaccine/data.csv") %>%
  #   filter(date == (as.Date(today) - 14)) %>% select(NAME = county, VaccOne = one_dose, VaccFull = vaccinated) %>%
  #   inner_join(texas_fips, by = 'NAME') %>% select(-NAME, fips, VaccOne, VaccFull)
  # for (i in 1:length(texasData$VaccOne)){
  #   vacc_county_today[which(vacc_county_today$fips == texasData$fips[i]),"VaccOne"] <- texasData[i, "VaccOne"]
  #   vacc_county_today[which(vacc_county_today$fips == texasData$fips[i]),"VaccFull"] <- texasData[i, "VaccFull"]
  # }
  ########
  missingCounties <- setdiff(unique(data_join$fips), unique(vacc_county_today$fips))
  ## INTEGRATE ALL DATA TOGETHER!!!
  CountyMap <- data_join %>% inner_join(county, by = c('fips' = 'GEOID')) %>%
    merge(vacc_county_today, by = 'fips', all = TRUE) 
  CountyMap[which(CountyMap$VaccOne == 0),"VaccOne"] <- NA
  CountyMap[which(CountyMap$VaccFull == 0),"VaccFull"] <- NA
  CountyMap$pInf = CountyMap$cases_past/CountyMap$pop
  CountyMap$VaccOneFrac <- CountyMap$VaccOne/CountyMap$pop
  CountyMap$VaccFullFrac <- CountyMap$VaccFull/CountyMap$pop
  
  
  US_DATA = subset(CountyMap,select=c("date","fips","NAME","stname","pInf","VaccOneFrac","VaccFullFrac","geometry"))
  # US_DATA$OneDose3AB <- round((1 - (1-US_DATA$pInf*3)*(1-US_DATA$VaccOneFrac))*100)
  # US_DATA$OneDose4AB <- round((1 - (1-US_DATA$pInf*4)*(1-US_DATA$VaccOneFrac))*100)
  # US_DATA$OneDose5AB <- round((1 - (1-US_DATA$pInf*5)*(1-US_DATA$VaccOneFrac))*100)
  # US_DATA$FullDose3AB <- round((1 - (1-US_DATA$pInf*3)*(1-US_DATA$VaccFullFrac))*100)
  # US_DATA$FullDose4AB <- round((1 - (1-US_DATA$pInf*4)*(1-US_DATA$VaccFullFrac))*100)
  # US_DATA$FullDose5AB <- round((1 - (1-US_DATA$pInf*5)*(1-US_DATA$VaccFullFrac))*100)
  # US_DATA$pInf <- round(US_DATA$pInf*100)
  # US_DATA$VaccOneFrac <- round(US_DATA$VaccOneFrac*100)
  # US_DATA$VaccFullFrac <- round(US_DATA$VaccFullFrac*100)
  return(list(US_DATA, missingCounties, today))
}
  