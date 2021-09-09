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
# vacc_data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv",stringsAsFactors = FALSE) %>%
#   mutate(date = as.Date(date)) %>%
#   select(date_vac = date, state = location, people_vaccinated, people_fully_vaccinated)
# vacc_data$state[vacc_data$state == 'New York State'] <- 'New York'

### this table combines all information we need for ONLY CASES:
cases_data <- combined_data %>% group_by(date) %>% summarise(date = date[1],
                                                             state = "United States",
                                                             cases = sum(cases),
                                                             deaths = sum(deaths),
                                                             population = 328239523,
                                                             Code = 'US') %>%
  bind_rows(combined_data) %>% as.data.frame() %>% arrange(desc(date)) %>% distinct(.keep_all = TRUE)

cases_data$deathRate <- cases_data$deaths/cases_data$population*100
deathRateData <- subset(cases_data,select=c("date","state","deathRate"))
colorSelected <- c("#7F96E4", "#EB996F", "#D7B2CE", "#EEE6C5", "#B7E4EC", "#ADEE87", "#5FBDDA", "#AEEECC", "#D45959", 
                   "#CE5E86", "#63B952", "#EECE9B", "#62ED8E", "#E253D7", "#B3C359", "#B2C5EC", "#CDE63C", "#DD419F",
                   "#A35FD9", "#5BEACF", "#7FBDA9", "#6BE7A9", "#C235E3", "#E99493", "#F0B3BA", "#EB9C3F", "#DCE8A9",
                   "#6E5AA2", "#74E4E7", "#6576EE", "#B16C4A", "#E578CB", "#E2EEE5", "#EE3A63", "#B4A369", "#673CDA",
                   "#C3B5A0", "#EAEA85", "#E7D6E8", "#58A670", "#69EF38", "#9E7983", "#65EA61", "#4E6F97", "#EAC872",
                   "#5DA6D8", "#EEE34A", "#52837A", "#D9ACEA", "#D1B43D", "#A6EB45", "#B888E1", "#E993C2", "#9DABB0",
                   "#E96339", "#A4D08B")

stateList <- sort(unique(cases_data$state))
all_states <- stateList[c(which(stateList == "United States"),1:which(stateList == "United States")-1,(which(stateList == "United States") + 1):length(stateList))]
cur_date <- cases_data$date[1]
past_date <- ymd(cur_date) - 14

data_past <- cases_data %>%
  filter(date <= past_date) %>%
  select(date_past = date, state, deathRate, cases_past = cases, population) %>% 
  mutate(date = date_past + 14, infectedRate = cases_past/population*100)

# if (max(cases_data$date) <= max(vacc_data$date_vac)) {
#   updated_date <- max(cases_data$date) 
# } else {
#   updated_date <- max(vacc_data$date_vac)
# }
# DATA <- data.frame(Date = as.character(), State = as.character(), Code = as.character(), ABias = as.numeric(),
#                    VaccType = as.character(), CaseFrac = as.numeric(), VaccFrac = as.numeric(), ImmuLevel = as.numeric())
# # at least one dose and ascertainment bias of 3
# OneDoseThreeAB <- LoadDatabyDate(all_states, 3, "At Least One Dose")
# # at least one dose and ascertainment bias of 4
# OneDoseFourAB <- LoadDatabyDate(all_states, 4, "At Least One Dose")
# # at least one dose and ascertainment bias of 5
# OneDoseFiveAB <- LoadDatabyDate(all_states, 5, "At Least One Dose")
# # fully vaccinated and ascertainment bias of 3
# FullThreeAB <- LoadDatabyDate(all_states, 3, "Fully Vaccinated")
# # fully vaccinated and ascertainment bias of 3
# FullFourAB <- LoadDatabyDate(all_states, 4, "Fully Vaccinated")
# # fully vaccinated and ascertainment bias of 5
# FullFiveAB <- LoadDatabyDate(all_states, 5, "Fully Vaccinated")
# DATA <- rbind(DATA, OneDoseThreeAB, OneDoseFourAB, OneDoseFiveAB, FullThreeAB, FullFourAB, FullFiveAB)
# 
# final_df <- inner_join(DATA,deathRateData, by = c("Date"="date","State"="state"))

# At Least One Dose for 3 AB:
# d_partial <- final_df %>% filter(ABias == 3, VaccType == "At Least One Dose") %>% arrange(ImmuLevel)
# d_full <- final_df %>% filter(ABias == 3, VaccType == "Fully Vaccinated") %>% arrange(ImmuLevel)
# stateOrderedList <- unique(d_partial$State)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(grDevices)
stateOrderedList <- all_states
#replaces data_past with d_partial
for (i in 1:10){
  title <- paste("Plot for", paste(stateOrderedList[(5*(i-1)+1):(5*i)],collapse = ', '))
  print(title)
  p <- ggplot(data_past[data_past$state %in% stateOrderedList[(5*(i-1)+1):(5*i)] ,], aes(x=infectedRate,y=deathRate, group = state, color = state)) + geom_line()+
    geom_point() +
    #scale_color_viridis(discrete = TRUE) +
    ggtitle("Plot1") +
    theme_ipsum() +
    labs(title,
         y = "Cumulative Deaths/Population (%)", x = "Infected Rate (%)") +
    theme(plot.title = element_text(size=15))
  ggsave(paste0(title,".png"))
}
title <- paste("Plot for", paste(stateOrderedList[50:56],collapse = ', '))
p <- ggplot(data_past[data_past$state %in% stateOrderedList[50:56] ,], aes(x=infectedRate,y=deathRate, group = state, color = state)) + geom_line()+
  geom_point() +
  #scale_color_viridis(discrete = TRUE) +
  ggtitle("Plot1") +
  theme_ipsum() +
  labs(title,
       y = "Cumulative Deaths/Population (%)", x = "Infected Rate (%)") +
  theme(plot.title = element_text(size=15))
p
ggsave(paste0(title,".png"))
#dev.off()
