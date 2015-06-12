library(ggmap)
library(ggplot2)
library(rgdal)

load("data/train_crime.Rdata")

#code from http://www.markhneedham.com/blog/2014/11/17/r-ggmap-overlay-shapefile-with-filled-polygon-of-regions/
sfn = readOGR("./data/sfzipcodes", "sfzipcodes") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))

top_n_crime <- function(df, num, cat){
  table(df[, cat]) %>% sort(decreasing = TRUE) %>%
    head(n = num) %>% names
  
}

#looking at crimes in zip codes of San Francisco

zip_df <- sfn[sfn$ZIP_CODE == 94112,]
zip_bb <- slot(zip_df, name = "bbox")

cat_names_10 <- top_n_crime(train_crime, 10, "Category")
clean_tc_df<- train_crime %>%
                filter(Y < 40,
                       X >= zip_bb[1],
                       X <= zip_bb[3],
                       Y >= zip_bb[2],
                       Y <= zip_bb[4],
                       Category %in% cat_names_10,
                       Dates >= "2015-01-01")

ggplot() + 
        geom_polygon(data = zip_df, 
                     aes(x = long, y = lat, group = group),
                     fill="#3D3D4C") +
          geom_point(data = clean_tc_df,
                     aes(X, Y, colour = Category), alpha = 0.075) +
              geom_path(data = zip_df,
                        aes(x = long, y = lat, group = group), colour = "black") +
                guides(colour = guide_legend(override.aes = list(alpha=1.0, size=3.0),
                                             title = "Police District")) +
                  labs(title = "Crime in San Francisco Separated by Districts\nFrom (01/06/03 - 05/13/15)") +
                    theme(axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.ticks = element_blank(),
                          axis.line = element_blank())

#                     
# Plotting the 3000 addresses with the most crime and their top category
# after removing 800 Bryant street(police station is) and 800 Market St 
# (Outliers that make the size of the rest of the addresses harder to see)
#
top_add <- top_n_crime(train_crime, 3002, "Address")[-c(1,2)]

add_cat_top <- train_crime %>%
  filter(Address %in% top_add) %>%
  group_by(Address, Category) %>%
  tally() %>%
  group_by(Address) %>%
  top_n(n = 1)


top_cat_add <- top_n_crime(add_cat_top, 14, "Category")

add_loc <- train_crime %>%
  filter(Address %in% top_add) %>%
  group_by(Address) %>%
  summarise(x_mid = mean(X), y_mid = mean(Y))

top_crime_df <- inner_join(add_cat_top, add_loc, by = c("Address")) %>%
  filter(Category %in% top_cat_add)

ggplot() + 
  geom_polygon(data = sfn, 
               aes(x = long, y = lat, group = group),
               fill="white") +
  geom_point(data = top_crime_df,
             aes(x_mid, y_mid, colour = Category, size = n), 
             alpha = 0.60) +
  geom_path(data = sfn,
            aes(x = long, y = lat, group = group), colour = "black") +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=3.0)
  ),
  size = guide_legend("# of Crimes from Top Category")) +
  labs(title = "Top 3000 Addresses with Most Overall Crime in San Francisco\nShowing its Category with the Most Crimes\nRemoving 800 Bryant St & 800 Market St\nFrom (01/06/03 - 05/13/15)") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())


#                     
# Plotting the 3000 addresses with the most crime and their second top category
# after removing 800 Bryant street(police station is) and 800 Market St 
# (Outliers that make the size of the rest of the addresses harder to see)
#
top_add <- top_n_crime(train_crime, 3002, "Address")[-c(1,2)]

add_cat_top_2 <- train_crime %>%
  filter(Address %in% top_add) %>%
  group_by(Address, Category) %>%
  tally() %>%
  group_by(Address) %>%
  top_n(n = 2)

add_cat_top <- train_crime %>%
  filter(Address %in% top_add) %>%
  group_by(Address, Category) %>%
  tally() %>%
  group_by(Address) %>%
  top_n(n = 1)

add_cat_second <- anti_join(add_cat_top_2, add_cat_top)

top_cat_add <- top_n_crime(add_cat_second, 14, "Category")
top_cat_add <- top_cat_add[!(top_cat_add == "OTHER OFFENSES")]

add_loc <- train_crime %>%
  filter(Address %in% top_add) %>%
  group_by(Address) %>%
  summarise(x_mid = mean(X), y_mid = mean(Y))

top_crime_df <- inner_join(add_cat_second, add_loc, by = c("Address")) %>%
  filter(Category %in% top_cat_add)

ggplot() + 
  geom_polygon(data = sfn, 
               aes(x = long, y = lat, group = group),
               fill="white") +
  geom_point(data = top_crime_df,
             aes(x_mid, y_mid, colour = Category, size = n), 
             alpha = 0.60) +
  geom_path(data = sfn,
            aes(x = long, y = lat, group = group), colour = "black") +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=3.0)
  ),
  size = guide_legend("# of Crimes from Second Top Category")) +
  labs(title = "Top 3000 Addresses with Most Overall Crime in San Francisco\nShowing its Category with the Second Most Crimes\nRemoving the Other Offenses Category\nRemoving 800 Bryant St & 800 Market St\nFrom (01/06/03 - 05/13/15)") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())



