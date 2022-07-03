library(tidyverse)

paygap <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')

nhs <- paygap %>% 
  filter(grepl("nhs", employer_name, ignore.case = TRUE)) 

nhs_year <- nhs %>% 
  mutate(year = lubridate::year(date_submitted)) %>% 
  group_by(year) %>% 
  summarise(hourly_med = median(diff_median_hourly_percent, na.rm = TRUE),
            bonus_med = median(diff_median_bonus_percent, na.rm = TRUE)) %>% 
  ungroup()

employer <- nhs %>% 
  group_by(employer_name) %>%
  count()
  
nhs_year %>% 
  ggplot(aes(year, hourly_med))+
  geom_line(color = "blue", size = 1)+
  theme_classic()+
  ggtitle("Median % difference in hourly pay in the NHS (n = 242 employers) in favour of men")+
  xlab("")+
  ylab("")+
  ylim(0, 13)+
  labs(
    caption = "#TidyTuesday W-26 | UK Gov. Gender Pay Gap Service | Maria Ten"
    )


