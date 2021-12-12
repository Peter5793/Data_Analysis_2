#Exploratory Data Analysis with Tidyverse
#load the libraries
# loading the tidyverse and we shall be using 
# ggplot
install.packages("tidyverse")
library(tidyverse)
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

#download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

untar("lax_to_jfk.tar.gz", tar = "internal")
# read_csv only
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(),
                                         'DivArrDelay' = col_number()))
# analyzing the individual feature patterns using visualization
ggplot(data = sub_airline, mapping = aes(x = Reporting_Airline, y = ArrDelay)) +
      geom_boxplot(fill = "bisque", color = "black", alpha = 0.3) + 
      geom_jitter(aes(color = "blue"), alpha=0.2)+
      labs(x = "Airline") + 
      ggtitle("Arrival Delays by Airline") + 
      guides(color = FALSE)+
      theme_minimal() +
      coord_cartesian(ylim = quantile(sub_airline$ArrDelay, c(0, 0.99)))
#loading the Alaska data 
alaska_flights <- sub_airline %>%
  filter(Reporting_Airline == "AS") %>%
  filter(!is.na(DepDelay) & !is.na(ArrDelay)) %>%
  filter(DepDelay < 40)

ggplot(data = alaska_flights, mapping = aes(x = DepDelay, y = ArrDelay)) +
  geom_point() +
  ggtitle("Alaska Flight Departure Delays vs Arrival Delays")

#list the data types for each column
str(sub_airline)
str(sub_airline$ArrDelayMinutes)
#correlation between Depature delay and the arrival delay
cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)
#positive linear relationship
ggplot(data = sub_airline, mapping = aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
    geom_point() +
    geom_smooth(method = "lm", na.rm = TRUE)

cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)
#weak linear relationship
ggplot(data = sub_airline, mapping = aes(x = WeatherDelay, y = ArrDelayMinutes)) +
  geom_point()+
  geom_smooth(method = "lm", na.rm = TRUE)
#correaltion between weather and arrival delay
cor(sub_airline$WeatherDelay, sub_airline$ArrDelayMinutes, use = "complete.obs")
#correaltion between carrierDelay and arrival delay
cor(sub_airline$CarrierDelay, sub_airline$ArrDelayMinutes, use = "complete.obs")
ggplot(data = sub_airline, mapping = aes(x = CarrierDelay, y = ArrDelayMinutes)) +
  geom_point()+
  geom_smooth(method = "lm", na.rm = TRUE)
#there is a correaltion between the carrierDelay and the arrival delay minutes
#descriptive Statistical Analysis
summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(count = n(),
            mean = mean(ArrDelayMinutes, na.rm = TRUE),
            std_dev = sd(ArrDelayMinutes, na.rm = TRUE),
            min = min(ArrDelayMinutes, na.rm = TRUE),
            median = median(ArrDelayMinutes, na.rm = TRUE),
            iqr = IQR(ArrDelayMinutes, na.rm = TRUE),
            max = max(ArrDelayMinutes, na.rm = TRUE))
summary_airline_delays
sapply(sub_airline, typeof)

#value count
sub_airline %>%
  count(Reporting_Airline)

avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), groups = 'keep')
head(avg_delays)
avg_delays
# sorting the data frame
sorted <-avg_delays %>%
  arrange(desc(mean_delays))

head(sorted)

avg_delays %>%
  ggplot(aes(x = Reporting_Airline,
             y = DayOfWeek,
             fill = mean_delays)) +
  # set the tile's borders to be white with size 0.2
  geom_tile(color = "white", size = 0.2) + 
  #define the gradient color scale
  scale_fill_gradient(low = "yellow", high = "red")

# This visualization will use lubridate package
library(lubridate)
# Let's take a simple average across Reporting_Airline and DayOfWeek
avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep') %>%
  # create a new variable "bins" from mean_delays
  # make the first range -0.1 to 0.1 to include zero values
  mutate(bins = cut(mean_delays,breaks = c(-0.1,0.1,10,20,30,50, max(mean_delays)),
                    labels = c("0","0-10","10-20","20-30","30-50",">50"))) %>%
  mutate(bins = factor(as.character(bins),levels = rev(levels(bins))))


ggplot(avg_delays, aes(x = Reporting_Airline, 
                       y = lubridate::wday(DayOfWeek, label = TRUE), 
                       fill = bins)) +
  geom_tile(colour = "white", size = 0.2) +
  geom_text(aes(label = round(mean_delays, 3))) +
  guides(fill = guide_legend(title = "Delays Time Scale"))+
  labs(x = "Reporting Airline",y = "Day of Week",title = "Average Arrival Delays")+
  # Define color palette for the scale
  scale_fill_manual(values = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4"))
