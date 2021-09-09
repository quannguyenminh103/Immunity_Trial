#LoadUS <- function() {
  dataurl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  data <- as.data.frame(fread(dataurl, stringsAsFactors = FALSE)) %>% mutate(date = as_date(date)) %>%
    filter(date >= '2021-02-01')
  county <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/tl_2017_us_county.geojson")
  stateline <<- as.data.frame(st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/tl_2017_us_state.geojson"))
  stateline <- stateline[,c('STUSPS','NAME')] 
  names(stateline) <- c('stname','name')
  pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/county-population.csv", stringsAsFactors = FALSE)
  cur_date <- data$date[length(data$date)]
  past_date <- ymd(cur_date) - 14
  
  data_cur <- data %>%
    filter(date <= cur_date) %>%
    mutate(fips = case_when(
      county == "New York City" ~ 99999,
      TRUE ~ as.numeric(fips)
    )) %>%
    select(c(date,fips, cases, deaths,))
  data_past <- data %>%
    filter(date <= past_date) %>%
    mutate(fips = case_when(
      county == "New York City" ~ 99999,
      TRUE ~ as.numeric(fips)
    )) %>%
    select(date_past = date, fips = fips, cases_past = cases) %>% mutate(date = date_past + 14)
  data_join <- data_cur %>% inner_join(data_past, by = c('fips','date'))
  data_join <<- data_join %>% inner_join(pop, by = "fips") %>%  mutate(n = date - date_past) %>%
  select(-c('X','date_past'))
  data_join$n <- as.numeric(data_join$n)
  data_join$CaseDiff <- (data_join$cases-data_join$cases_past)*10/data_join$n
  data_join$pInf <- data_join$CaseDiff/data_join$pop
  data_join$CaseDiff <- data_join$cases - data_join$cases_past
  US_DATA <- subset(data_join, select=c("date",'fips','CaseDiff','pInf'))
  write_csv(US_DATA, "RiskData.csv")
  # USMap <- data_join %>% inner_join(county, by = c('fips' = 'GEOID')) %>%
  # inner_join(stateline, by = 'stname')
  # 
  # USMap$RegionName = paste0(USMap$NAME,', ',USMap$name)
  # USMap$DateReport = as.character(USMap$date) 
  # US_DATA = subset(USMap,select=c("DateReport","RegionName","pInf","geometry"))
  # US_DATA <- st_as_sf(US_DATA)
  return(US_DATA)
#}
  