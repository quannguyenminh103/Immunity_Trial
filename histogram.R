library(ggplot2)
# set up cut-off values 
breaks <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
# specify interval/bin labels
tags <- c("[0-5)","[5-10)", "[10-15)", "[15-20)", "[20-25)", "[25-30)","[30-35)", "[35-40)","[40-45)", "[45-50)",
          "[50-55)","[55-60)", "[60-65)", "[65-70)", "[70-75)", "[75-80)","[80-85)", "[85-90)","[90-95)", "[95-100)")
# bucketing values into bins
group_tags <- cut(US_DATA$OneDose3AB, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) + 
  geom_bar(fill="bisque",color="white",alpha=0.7) + 
  stat_count(geom="text", aes(label=sprintf("%.0f",..count..)), vjust=-0.5) +
  labs(x='Population-level Immunity (%)',title='Immunity Level Histogram Plot across US counties WITH state-level average vaccine immunity assumption',
       y = 'Number of Counties') +
  theme_minimal() 
