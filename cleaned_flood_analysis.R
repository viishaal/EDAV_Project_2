setwd("/Users/vishaljuneja/Dropbox/EDAV/Repos/EDAV_Project_2")

install.packages("plotly")
library(plotly)

df = read.csv("Cleaned_main_cause.csv", strip.white = TRUE)
df$Began = as.Date(df$Began, format = "%Y-%m-%d")

# clean countries column

trim_whitespace <- function (x) gsub("^\\s+|\\s+$", "", x)
df$Country = trim_whitespace(df$Country)

trim_commas <- function (x) gsub(",+$", "", x)
df$Country = trim_commas(df$Country)

## country is very dirty (must clean again) and also attach continent


## plotly box plot
#df_small = df[,c("Country", "duration", "Dead", "Displaced", "Severity..", )]
plot_ly(df, y=log(Dead), x=Severity.., type="box") %>%
layout(title = "Severity vs Casuality",
       xaxis = list(title = "Severity"),
         yaxis = list(title = "Casuality in log scale"))


##correlating displaced and dead

df_non_zero_dead = df[df$Dead > 0 & df$Displaced > 0, ]
g1 = ggplot() + geom_point(aes(x=log(df_non_zero_dead$Dead), y=log(df_non_zero_dead$Displaced)))
g1 = g1 + labs(title="Dead Vs Displaced (log scale)", x="Dead", y="Displaced")
g1 = g1 + geom_smooth(aes(x=log(df_non_zero_dead$Dead), y=log(df_non_zero_dead$Displaced)), method = loess)
g1 = g1 + theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 10, 10, 10)))
g1 = g1 + theme(axis.text.x=element_text(size=12, vjust=0.5))
g1 = g1 + theme(axis.title.x=element_text(size=15))
g1 = g1 + theme(axis.title.y=element_text(size=15))
g1


## Severity density plots (trellis)

subset = c("Displaced", "Severity..")
df_cat = df[,subset]
df_cat = df_cat[complete.cases(df_cat),]
df_cat$Displaced = log(df_cat$Displaced)
df_cat$Severity.. = as.character(df_cat$Severity..)
names(df_cat) = c("Displaced", "Severity")
densityplot(~df_cat$Displaced | df_cat$Severity, 	
            main=list(label="Displaced Vs Severity (density plot)", cex=1.25),
            ylab = list(cex=1.25),
            xlab=list(label="Displaced (log scale)", cex=1.25),
            scales=list(cex=1))


subset = c("Dead", "Severity..")
df_cat = df[,subset]
df_cat = df_cat[complete.cases(df_cat),]
df_cat$Dead = log(df_cat$Dead)
df_cat$Severity.. = as.character(df_cat$Severity..)
names(df_cat) = c("Dead", "Severity")
densityplot(~df_cat$Dead | df_cat$Severity, 	
            main=list(label="Dead Vs Severity (density plot)", cex=1.25),
            ylab = list(cex=1.25),
            xlab=list(label="Dead (log scale)", cex=1.25),
            scales=list(cex=1))


## dead vs displaced over the yearslibrary(reshape2)
dd = df %>% group_by(year) %>% summarise(dead=sum(Dead), displaced=sum(Displaced))
dd$year = substr(as.character(dd_full$year), 0, 4)
m = melt(dd, id=c("year"))
g1 = ggplot(data=m, aes(x=year, y=log(value), group=variable, colour=variable)) + geom_line(size=1.5)
g1 = g1 + geom_point(size=4, shape=23, fill="white")
g1 = g1 + scale_colour_brewer(palette="Set2")
g1

#g2 = ggplot(data=m, aes(x=year, y=log(value))) + geom_area(fill="blue", alpha=.2) + geom_line()
#g2

g3 = ggplot(data=dd)
g3 = g3 + geom_bar(aes(x=year, y=log(displaced), colour="Displaced"), stat="identity", fill="blue", alpha=0.5)
g3 = g3 + geom_bar(data=dd, aes(x=year, y=log(dead), colour="Dead"), stat="identity", fill="pink", alpha=0.7)
g3 = g3 + labs(title="Timeline: Displaced | Dead", x="Year", y="Displaced | Dead")
g3 = g3 + theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)))
g3 = g3 + theme(axis.text.x=element_text(size=14, vjust=1.0))
g3 = g3 + theme(axis.title.x=element_text(size=17, vjust=-0.5))
g3 = g3 + theme(axis.title.y=element_text(size=15))
g3 = g3 + scale_colour_manual(name="Legend",
                              values=c(Displaced="blue", Dead="red"))
#g3 = g3 + geom_point(aes(color="My points"))
g3



df %>% group_by(Severity..) %>% 
  summarise(avg = mean(Dead), med = median(Dead), total = sum(Dead),freq = n())
