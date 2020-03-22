library("ggplot2")


deaths.data <- read.csv(file="../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")


death_fr = subset(deaths.data, Country.Region == "France" & Province.State == "France", select=c(5:ncol(deaths.data)))
death_fr <- as.data.frame(t(death_fr))

colnames(death_fr) <- c("Deaths")

death_fr$Date <- sub("X", "", rownames(death_fr))
death_fr$Date <- as.Date(death_fr$Date, "%m.%d.%y")


png("Death_FR_test.png")
p <- ggplot(data=death_fr, aes(y=Deaths, x=Date))
#p1 <- p + geom_line()
p1 <- p + geom_bar(stat="identity")
print(p1)
dev.off()