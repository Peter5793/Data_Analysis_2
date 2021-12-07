#load tidyverse
library(tidyverse)
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

# download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# untar the file so we can get the csv only
# if you run this on your local machine, then can remove tar = "internal" 
untar("lax_to_jfk.tar.gz", tar = "internal")
# read_csv only 
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(), 
                                         'DivArrDelay' = col_number()))
head(sub_airline)

is.na(c(1,NA))
#counting missing values
sub_airline %>%
  summarize(count = sum(is.na(CarrierDelay)))
map(sub_airline, ~sum(is.na(.)))
# check the dimensions of the data set
dim(sub_airline)
# dropping the colums DivDistance and DivArrDelay
drop_na_cols <- sub_airline %>% select(-DivDistance, -DivArrDelay)
dim(drop_na_cols)
head(drop_na_cols)
#drop the missing values
drop_na_rows <- drop_na_cols %>% drop_na(CarrierDelay)
dim(drop_na_rows)
head(drop_na_rows)
#Convert NA to 0
replace_na <- drop_na_rows %>% replace_na(list(CarrierDelay = 0,
                                               WeatherDelay = 0,
                                               NASDelay = 0,
                                               SecurityDelay = 0,
                                               LateAircraftDelay = 0))
head(replace_na)
#calculate the mean value for CarrierDelay column
carrier_mean <- mean(drop_na_rows$CarrierDelay)
#replace NA by mean value in "CarrierDelay" column
sub_airline %>%
  replace_na(list(CarrierDelay = carrier_mean))
is.na(sub_airline)
sub_airline %>%
  summarize(count = sum(is.na(CarrierDelay)))
sub_airline %>%
  summarize_all(class) %>%
  gather(variable, class)

date_airline <- replace_na %>%
  separate(FlightDate, sep = "-", into = c("year", "month", "day"))
head(date_airline)

date_airline %>%
  select(year, month, day) %>%
  mutate_all(type.convert) %>%
  mutate_if(is.character, as.numeric)

#Data Normalization
#makes the comparison between two variables fair
simple_scale <- sub_airline$ArrDelay/ max(sub_airline$ArrDelay)
head(simple_scale)
#Simple scaling for departure delay
simple_scale_dep <- sub_airline$DepDelay/max(sub_airline$DepDelay)
head(simple_scale_dep)

#min_max
minmax_scale <- (sub_airline$ArrDelay - min(sub_airline$ArrDelay))/
                ( max(sub_airline$ArrDelay) - min(sub_airline$ArrDelay))
head(minmax_scale)
#Normalization the departure delay using min-max
minmax_scale2 <- (sub_airline$DepDelay - min(sub_airline$DepDelay))/
                 (max(sub_airline$DepDelay) - min(sub_airline$DepDelay))
head(minmax_scale2)
#data standardization(z-score)
z_scale <- (sub_airline$ArrDelay - mean(sub_airline$ArrDelay))/
           (sd(sub_airline$ArrDelay))
head(z_scale)
Z_scale2 <- (sub_airline$DepDelay - mean(sub_airline$DepDelay))/
            (sd(sub_airline$DepDelay))
head(Z_scale2)

ggplot(data = sub_airline, mapping = aes(x = ArrDelay)) +
  geom_histogram(bins = 100, color = "white", fill = "red") +
  coord_cartesian(xlim = c(-73, 682))

binning <- sub_airline %>%
           mutate(quantile_rank = ntile(sub_airline$ArrDelay, 4))
head(binning)

ggplot(data = binning, mapping = aes(x = quantile_rank)) +
      geom_histogram(bins = 4, color = "white", fill = "red")

sub_airline %>%
  mutate(dummy = 1) %>% # column with single value
  spread(
    key = Reporting_Airline, # column to spread
    value = dummy, 
    fill = 0) %>%
  slice(1:5)
  
sub_airline %>%
  spread(Reporting_Airline, ArrDelay) %>%
  slice(1:5)
#Visulaize Airline Category
sub_airline %>% # start with data
  mutate(Reporting_Airline = factor(Reporting_Airline,
                                    labels = c("AA", "AS", "DL", "UA", "B6", "PA (1)", "HP", "TW", "VX")))%>%
  ggplot(aes(Reporting_Airline)) +
  stat_count(width = 0.5) +
  labs(x = "Number of data points in each airline")

#create a new indicator variable to the column
sub_airline %>%
  mutate(dummy = 1) %>% # column with single value
  spread(
    key = Month, #column to spread
    value = dummy,
    fill = 0) %>%
  slice(1:5)
#create indicator variable to the column of "Month" by applying departure delay values
sub_airline %>%
  spread(Month, DepDelay) %>%
  slice(1:5) #show the first 5 rows
    

  