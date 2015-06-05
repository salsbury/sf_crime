library(ggmap)
library(ggplot2)
library(rgdal)

load("data/train_crime.Rdata")

#code from http://www.markhneedham.com/blog/2014/11/17/r-ggmap-overlay-shapefile-with-filled-polygon-of-regions/
sfn = readOGR("./data/sfzipcodes", "sfzipcodes") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))

clean_tc_df<- train_crime %>%
                filter(Y < 40)

ggplot() + 
        geom_polygon(data = sfn, 
                     aes(x = long, y = lat, group = group),
                     fill="#3D3D4C") +
          geom_point(data = clean_tc_df,
                     aes(X, Y, colour = PdDistrict), alpha = 0.075) +
              geom_path(data = sfn,
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
                                          

al1 = get_map(location = c(-122.4367, 37.7533), 
              zoom = 12, maptype = 'roadmap')
al1MAP = ggmap(al1)
al1MAP

