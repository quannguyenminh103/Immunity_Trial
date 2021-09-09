MakeCountyMap <- function(VaccType,aBias) {
  US_MAP = US_DATA
  if (VaccType == "At Least One Dose") {
      type <- "VaccOneFrac"
      if (aBias == 3) {
        US_MAP$ImmuLevel = US_MAP$OneDose3AB
      } else if (aBias == 4) {
        US_MAP$ImmuLevel = US_MAP$OneDose4AB
      } else {
        US_MAP$ImmuLevel = US_MAP$OneDose5AB
      }
  } else {
      type <- "VaccFullFrac"
      if (aBias == 3) {
        US_MAP$ImmuLevel = US_MAP$FullDose3AB
      } else if (aBias == 4) {
        US_MAP$ImmuLevel = US_MAP$FullDose4AB
      } else {
        US_MAP$ImmuLevel = US_MAP$FullDose5AB
      }
  }
  #US_MAP <- US_MAP[order(US_MAP[,"ImmuLevel"], na.last=F),] %>% mutate(ranking = as.numeric(rownames(US_MAP)))
  #create risk-maps
  # US_MAP$ImmuLabels <- US_MAP$ImmuLevel
  # US_MAP <- US_MAP %>%
  #   mutate(ImmuLabels = case_when(
  #     ImmuLabels > 99 ~ '> 99',
  #     ImmuLabels < 1 ~ '< 1',
  #     ImmuLabels < 0 ~ 'NA',
  #     is.na(ImmuLabels) ~ 'No Data',
  #     TRUE ~ as.character(ImmuLabels)
  #   ))
  # bins <- c(0,1,25,50,75,99,100)
  # legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99")
  # pal <- colorBin("viridis", domain = US_MAP$ImmuLevel, bins = bins, na.color = 'grey')
  # continous color
  legendlabs <<- c("< 50", "50-55", "55-60", "60-65", "> 70")
  pal <- colorNumeric("viridis", domain = c(0,100), na.color = 'grey')
  legendlabs = c("0%", "20%", "40%", "60%", "80%", "100%")
  JAM = leafletOptions(worldCopyJump=TRUE)
  labels <- sprintf(
    "<strong>County: %s, %s</strong><br/>Population-level Immunity: %.0f%%<br/>Infected: %.0f%%<br/>Vaccinated (%s): %.0f%%<br/>Updated Date: %s<br/>",
    US_MAP$NAME, US_MAP$stname, US_MAP$ImmuLevel, US_MAP$pInf*as.numeric(aBias), VaccType, US_MAP[,type],US_MAP$date
  ) %>% lapply(htmltools::HTML)
  US_MAP <- st_as_sf(US_MAP)
  map <- leaflet(US_MAP,options=JAM) %>% addTiles() %>% #setView(-120, 50, zoom = 3) %>% #including Alaska & Hawaii
    setView(-98.35, 39.7, zoom = 4) %>% 
    #addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = US_MAP,
                fillColor = ~pal(ImmuLevel),
                weight = 0.1,
                opacity = 0.7,
                color = "black",
                dashArray = "2",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.6,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addLegend(pal = pal, opacity = 1, title = 'Immunity Level (%)', na.label = 'NA', values = c(0,100), 
              #labels = legendlabs,
              position = "topright",
              labFormat = function(type, cuts, p) {
                paste0(legendlabs)
              })
  return(map)
}
  