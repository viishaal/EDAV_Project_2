library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
}

coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
}

df = read.csv("Cleaned_main_cause.csv", strip.white = TRUE)
df$Began = as.Date(df$Began, format = "%Y-%m-%d")

# clean countries column
df_no_na = df[complete.cases(data.frame(lon=df$Centroid.X, lat=df$Centroid.Y)),]
df_no_na$clean_countries = coords2country(data.frame(lon=df_no_na$Centroid.X, lat=df_no_na$Centroid.Y))

## group by country
df_no_na %>% select(Country, clean_countries) %>% head()
df_g_country = df_no_na %>% group_by(clean_countries) %>% summarise(total_dead = sum(Dead))
df_g_country = df_g_country[complete.cases(df_g_country),]
df_g_country = df_g_country[order(-df_g_country$total_dead), ]
df_sub = df_g_country[1:70,]


g1 = ggplot(data=df_sub) 
g1 = g1 + geom_bar(aes(x=reorder(clean_countries, total_dead), y=total_dead), fill="blue", stat="identity", alpha=0.7) 
g1 = g1 + labs(title="Casualities by Country", x="country", y="casualities")
g1 = g1 + theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)))
g1 = g1 + theme(axis.title.x=element_text(size=15, vjust=1.0))
g1 = g1 + theme(axis.text.x=element_text(size=14, vjust=1.0))
g1 = g1 + theme(axis.title.y=element_text(size=15))
g1 = g1 + coord_flip()
g1 = g1 + scale_fill_brewer()
g1


## get continent mapping
df_no_na$continent = coords2continent(data.frame(lon=df_no_na$Centroid.X, lat=df_no_na$Centroid.Y))

## group by continent
df_no_na %>% select(Country, continent) %>% head()
df_cont = df_no_na %>% group_by(continent) %>% summarise(total_dead = sum(Dead), total_displaced=sum(Displaced))
df_cont = df_cont[complete.cases(df_cont),]
df_cont = df_cont[order(-df_cont$total_dead), ]
df_cont

g2 = ggplot(data=df_cont) 
g2 = g2 + geom_bar(aes(x=reorder(continent, total_dead), y=total_dead), fill="blue", stat="identity", alpha=0.7) 
g2 = g2 + labs(title="Casualities by Continent", x="continent", y="casualities")
g2 = g2 + theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)))
g2 = g2 + theme(axis.title.x=element_text(size=15, vjust=1.0))
g2 = g2 + theme(axis.text.x=element_text(size=14, vjust=1.0))
g2 = g2 + theme(axis.title.y=element_text(size=15))
g2 = g2 + coord_flip()
g2 = g2 + scale_fill_brewer()
g2



## has death rate been changing with years?

library(reshape2)
df = df_no_na
df$year = substr(as.character(df$Began), 0, 4)
dd = df %>% group_by(year, continent) %>% summarise(dead=sum(Dead), displaced=sum(Displaced))
#m = melt(dd, id=c("year"))

g1 = ggplot(data=dd, aes(x=year, y=log(dead), group=continent, colour=continent))
#g1 = g1 + geom_line(size=0.5)
g1 = g1 + geom_smooth(method = lm)
#g1 = g1 + geom_point(size=0.25, shape=22, fill="white")
g1 = g1 + scale_colour_brewer(palette="Set1")
g1 = g1 + labs(title="Timeline: Continent wise Casualities", y="Dead (log scale)", x="")
g1 = g1 + theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)))
g1 = g1 + theme(axis.text.x = element_text(size=14, vjust=1.0))
g1 = g1 + theme(axis.text.y = element_text(size=14))
g1 = g1 + theme(axis.title.x = element_text(size=15, vjust=-0.5))
g1 = g1 + theme(axis.title.y = element_text(size=15))
g1 = g1 + theme(legend.title = element_text(size=14))
g1 = g1 + theme(legend.text = element_text(size=13))
g1

## plot displacement of people
g1 = ggplot(data=dd, aes(x=year, y=log(displaced), group=continent, colour=continent))
g1 = g1 + geom_smooth(method = lm)
g1 = g1 + scale_colour_brewer(palette="Set1")
g1 = g1 + labs(title="Timeline: Continent wise Displaced", y="Displaced (log scale)", x="")
g1 = g1 + theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)))
g1 = g1 + theme(axis.text.x = element_text(size=14, vjust=1.0))
g1 = g1 + theme(axis.text.y = element_text(size=14))
g1 = g1 + theme(axis.title.x = element_text(size=15, vjust=-0.5))
g1 = g1 + theme(axis.title.y = element_text(size=15))
g1 = g1 + theme(legend.title = element_text(size=14))
g1 = g1 + theme(legend.text = element_text(size=13))
g1



