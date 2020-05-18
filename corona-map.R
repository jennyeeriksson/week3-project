library(plotly)

if(!file.exists("corona.csv"))
{
    download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/05-17-2020.csv", "corona.csv")
}
data<-read.csv("corona.csv")

#Creating table
library(dplyr)
names(data)[6]<-"lat"
names(data)[7]<-"lng"
deaths<-data %>% group_by(Country_Region) %>% summarize(tot_death=sum(na.omit(Deaths)), tot_cases=sum(na.omit(Confirmed)), lat=na.omit(lat)[which.max(Confirmed)], lng=na.omit(lng)[which.max(Confirmed)], Country=Country_Region[1])
deaths<-deaths[complete.cases(deaths), ]

deaths$hover <- with(deaths, paste(Country, "Deaths: ", tot_death, "Disease: ", tot_cases))

#Plot on map
borders <- list(color = toRGB("red"))

map_options <- list(
    projection = list(type = 'Mercator'),
    showlakes=T,
    lakecolor=toRGB('white'),
    showcountries = TRUE
)
par(mfrow=c(2,2))
plot_geo(deaths, lat=~lat, lon=~lng, color=~tot_death*30) %>% add_markers(text=deaths$hover, size=deaths$tot_death, sep = "<br />") %>%layout(title='Corona map', geo=map_options)


# Plot on pie chart
plot_ly(deaths, type='pie', labels = ~Country, values = ~tot_death, textposition = 'inside')


