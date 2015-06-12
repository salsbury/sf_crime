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
# function for top n categories (based on counts)
#

top_n_crime <- function(df, num, cat){
  table(df[, cat]) %>% sort(decreasing = TRUE) %>%
        head(n = num) %>% names
  
}

#
# SF Crime EDA
#


#
# looking at counts of top 15 crime categories
#

cat_names_15 <- top_n_crime(train_crime, 15, "Category")

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

cat_names_10 <- top_n_crime(train_crime, 10, "Category")

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
cat_names_10 <- top_n_crime(train_crime, 10, "Category")

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

#
# looking at addresses with the most crime
#

cat_names_20 <- top_n_crime(train_crime, 20, "Category")

add_names_15 <- top_n_crime(train_crime, 15, "Address")
crime_filt <- train_crime %>%
                filter(Y < 40, 
                       Address %in% add_names_15,
                       Category %in% cat_names_20)

crime_filt %>%
  mutate(Add_fact = factor(Address, levels = rev(add_names_15))) %>%
  group_by(Add_fact, Category) %>%
  summarize(Total = n()) %>%
  ggplot(aes(Add_fact, Total, fill = Category)) +
  guides(fill = guide_legend(ncol = 2)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Street Block of Place Crime was Reported",
       y = "Number of Crimes Reported",
       title = "Addresses in San Francisco with the Most Crime Reported\n(Top 20 Crime Categories From 01/06/03 - 05/13/15)")

#
# looking at more Addresses (Removing 800 Bryant Street Where the Main Police Station is)
#

cat_names_20 <- top_n_crime(train_crime, 20, "Category")

add_names_20 <- top_n_crime(train_crime, 21, "Address")[-1]
crime_filt <- train_crime %>%
  filter(Y < 40, 
         Address %in% add_names_20,
         Category %in% cat_names_20)

crime_filt %>%
  mutate(Add_fact = factor(Address, levels = rev(add_names_20))) %>%
    group_by(Add_fact, Category) %>%
      summarize(Total = n()) %>%
  ggplot(aes(Add_fact, Total, fill = Category)) +
  guides(fill = guide_legend(ncol = 2)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Street Block of Place Crime was Reported",
      y = "Number of Crimes Reported",
       title = "Addresses in San Francisco with the Most Crime Reported\n(Top 20 Crime Categories From 01/06/03 - 05/13/15)\nRemoving 800 Bryant Police Station")


#
# Looking at crimes in assault separated by district
#

crime_filt <- train_crime %>%
    filter(Category == "ASSAULT")
desc_names_10 <- top_n_crime(crime_filt, 10, "Descript")

crime_filt %>%
      filter(Descript %in% desc_names_10) %>%
        ggplot(aes(Descript, colour = Descript)) +
          geom_bar(stat = "bin") +
            coord_flip() +
              facet_grid(. ~ PdDistrict) +
                guides(colour = FALSE) +
        labs(x = "Description",
            y = "Number of Crimes Reported",
            title = "Histogram of Assault Category in San Francisco\nSeparated by Crime and District (From 01/06/03 - 05/13/15)") +
              theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1))


#
# looking at Resolutions for theft
#

crime_filt <- train_crime %>%
                filter(Category == "LARCENY/THEFT")

res_names_10 <- top_n_crime(crime_filt, 10, "Resolution")


crime_filt %>%
          filter(Resolution %in% res_names_10) %>%
            ggplot(aes(Resolution)) +
              geom_bar(stat = "bin") +
                coord_flip() +
                  labs(y = "Number of Resolutions",
                       title = "Top 10 Resolutions for LARCENY/THEFT Category")

#
# looking at Percentage of Resolutions for Descriptions in Assaults
#

crime_filt <- train_crime %>%
  filter(Category == "ASSAULT")

des_names_10 <- top_n_crime(crime_filt, 10, "Descript")
res_names_10 <- top_n_crime(crime_filt, 10, "Resolution")


crime_filt %>%
  filter(Descript %in% des_names_10) %>%
    group_by(Descript, Resolution) %>%
      tally() %>%
        group_by(Descript) %>%
          mutate(Percentage = (n/sum(n))*100) %>%
            filter(Resolution %in% res_names_10) %>%
  ggplot(aes(Descript, Percentage, fill = Resolution)) +
      geom_bar(stat = "identity") +
        coord_flip() +
        labs(y = "Percentage of Resolutions for Crime Descriptions",
          title = "Percentage of Resolutions for\nTop Crime Descriptions for ASSAULT Category")


#
# Looking at resolutions for all crimes
#
cat_names_20 <- top_n_crime(train_crime, 20, "Category")

res_names_15 <- top_n_crime(train_crime, 15, "Resolution")

crime_filt <- train_crime %>%
  filter(
         Resolution %in% res_names_15,
         Category %in% cat_names_20)

crime_filt %>%
  mutate(Res_fact = factor(Resolution, levels = rev(res_names_15))) %>%
  group_by(Res_fact, Category) %>%
  summarize(Total = n()) %>%
  ggplot(aes(Res_fact, Total, fill = Category)) +
  guides(fill = guide_legend(ncol = 2)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Resolution",
       y = "Number of Resolutions",
       title = "Resolutions of Crime in San Francisco\n(Top 20 Crime Categories From 01/06/03 - 05/13/15)")

#
# looking at the count of crimes for each hour in the days for the year 2014
#

crime_2014 <- train_crime %>%
  filter(year(Dates) == 2014)

minute(crime_2014$Dates) <- 0

crime_2014 %>%
  group_by(Dates) %>%
  tally() %>%
  mutate(date_only = as.Date(Dates),
         hour_only = hour(Dates)) %>%
  ggplot(aes(hour_only, n, group = date_only, colour = date_only)) +
  geom_line() +
  labs(x = "Hour of Day",
       y = "Counts of Crimes Reported",
       title = "Count of San Francisco Crimes for Each Hour of the Days in 2014")
