library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)


train_crime <- read_csv("data/train.csv")

train_crime$Dates <- as.POSIXct(train_crime$Dates, format = "%m/%d/%Y %H:%M", tz = "UTC")

dayofweek <- 1:7
names(dayofweek) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
train_crime$DayOfWeekNum <- dayofweek[train_crime$DayOfWeek]

#
# looking at counts of top 15 crime categories
#

cat_names_15 <- table(train_crime$Category) %>% sort(decreasing = T) %>% head(n = 15) %>% names

train_crime %>%
        filter(Category %in% cat_names_15) %>%
          mutate(Year_crime = as.factor(year(Dates))) %>%
          ggplot(aes(Category, fill = Year_crime)) +
              geom_bar(stat = "bin") +
              coord_flip() +
              labs(title = "Count of Crime Categories Occuring in\nSan Francisco from 01/06/2003 - 05/13/2015")


#
# looking at percentages of top 10 categories Over the years
#

cat_names_10 <- table(train_crime$Category) %>% sort(decreasing = T) %>% head(n = 10) %>% names

train_crime %>%
        mutate(Year_crime = as.factor(year(Dates))) %>%
          group_by(Year_crime, Category) %>%
            tally() %>%
              group_by(Year_crime) %>%
                mutate(perc = (round(n/sum(n), 3)*100)) %>%
                filter(Category %in% cat_names_10) %>%
        ggplot(aes(Year_crime, perc, colour = Category, group = Category)) +
            geom_line() + geom_point() +
              labs(x = "Year",
                   y = "Percentage of All Crime Categories",
                   title = "Percentage of Crime in San Francisco\nfor Top 10 Categories from 01/06/2003 - 05/13/2015")

#
# looking at histogram of districts and hours
#

train_crime %>%
        mutate(hours = as.factor(hour(Dates))) %>%
          ggplot(aes(hours, colour = hours)) + 
            geom_bar(stat = 'bin') +
              coord_flip() +
                facet_grid(. ~ PdDistrict) +
            guides(colour = FALSE) +
              labs(x = "Hour",
                   y = "Number of Crimes",
                   title = "Histogram of Crimes in San Francisco\nSeparated by District and Hour (From 01/06/03 - 05/13/15)") +
              theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1))


#
# looking at histogram of district and top 10 crime categories
#
cat_names_10 <- table(train_crime$Category) %>% sort(decreasing = T) %>% head(n = 10) %>% names

train_crime %>%
  filter(Category %in% cat_names_10) %>%
  ggplot(aes(PdDistrict, colour = PdDistrict)) + 
  geom_bar(stat = 'bin') +
  coord_flip() +
  facet_grid(. ~ Category) +
  guides(colour = FALSE) +
  labs(x = "District",
       y = "Number of Crimes",
       title = "Histogram of Crimes in San Francisco\nSeparated by District and Category (From 01/06/03 - 05/13/15)") +
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1))


add_count <- table(train_crime$Address) %>% sort(decreasing = T)
