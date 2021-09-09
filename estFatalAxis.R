estFatalAxis <- function() {
  cur_combined_data <- cur_data %>% inner_join(pop, by = 'state')
  final_data <- cur_combined_data %>% summarise(date = date[1],
                                                state = "United States",
                                                cases = sum(cases),
                                                deaths = sum(deaths),
                                                population = 331002651,
                                                Code = 'US') %>%
    bind_rows(combined_data) %>% as.data.frame() %>% arrange(desc(date)) %>% distinct(.keep_all = TRUE)
  ifr_table <- final_data %>% filter(state == 'United States')
  IFR <- ifr_table$cases*4/ifr_table$deaths
  
  # choose the infected rate ~ 0.3, 0.4, 0.6, 0.75
  infectedRate <- c(0.3,0.4,0.6,0.75)
  fatalities <- infectedRate/IFR*331002651 # frac_deaths * US Population
  convert <- function(x) {
    if (x >= 1e6) {
      label <- paste0("~",round(x/1e6,1),"M")
    } else if (x < 1e6 && x >= 1e3) {
      label <- paste0("~",round(x/1e3,0),"K")
    } else {
      label <- paste0("~",x)
    }
    return(label)
  }
  fatal_labels <- lapply(fatalities,convert)
  x = y = 0
  opacity = 0
  axis <- plot(0,0,ylim = c(-0.1,1),xlim = c(0,1),bty = 'n',xaxt='n', yaxt='n',ann=F,lwd=0,cex=0)
  for (i in 1:1000) {
    x = opacity = 0.001*i
    y = 0.2 * x
    lines(c(x,x),c(0,y),type='l',lwd=5, col = rgb(red = 0, green = 0, blue = 0, alpha = opacity/5),
            ylim = c(-0.1,1),xlim = c(0,1))
  }
  polygon(c(0,1,1),c(0,0,0.2),lwd = 3)
  points(infectedRate,c(0,0,0,0),xlim=c(0,1),bty = 'n',xaxt='n', yaxt='n',ann=F,ylim=c(-0.1,1),
         cex = 2.5,pch = 21, bg=c('gray','green','yellow','red'), col = 'black',lwd = 2)
  text(infectedRate,c(-0.1,-0.1,-0.1,-0.1),labels = fatal_labels)
  text(0,-0.1,labels = 'Estimated Fatalities',cex = 1,pos = 4)
  return(axis)
}
