
#Q1 - a
social_robots <- c(63,20,8,15)
pie(social_robots, main = "Social Robots", col = rainbow(length(social_robots)), labels = c("Legs","Wheels","Both", "None"))

#Q1 - b
barplot(social_robots, main = "Social Robots", xlab = "Types", ylab = "Frequency", names.arg = c("Legs","Wheels","Both", "None"), col = "pink", horiz = FALSE)


#Q2 - a
library(datasets)
?mtcars
cars <- table(mtcars$cyl)
barplot(cars)
plot(cars)
barplot(cars, main = "Motor Trend Car Road Tests", ylab = "Fuel Consumption", col = "magenta", horiz = FALSE)

#Q2 - b
library(ggplot2)
summary(diamonds)


#Q3 - a
Q3 <- rnorm(40, mean = 2, sd = 5)
bins <- seq(-10, 15, by = 2)
Xm <- seq(-10 + 1, 15 - 1, by = 2)
table(bins)
scores <- cut(Q3, bins)
transform(table(scores), midpnt = Xm, Rel_Freq = prop.table(Freq), Cum_Freq = cumsum(Freq))

#Q3 - b
t = transform(table(scores), midpnt = Xm, Rel_Freq = prop.table(Freq), Cum_Freq = cumsum(Freq))
h <- hist(Q3, breaks = bins, main = "Histogram of Targeted Values", xlab = "Class Width", col = "green")
lines(x=c(1, h$mids, tail(h$mids, 1) + el(diff(h$mids))), y=c(0, h$counts,0), lwd=1)

#Q4 
setwd("C:/Users/Downloads")
data <- read.csv("covid_19_data.csv")

#Q4 - a
length(unique(data$Country.Region))

#Q4 - b
unique(data$Province.State[data$Country.Region == "Mainland China"][data$Confirmed != 0])

#Q4 - c
summary (data)

#Q4 - d
par(mfrow=c(2,2))
hist(data$Confirmed[data$Province.State == "South Australia"], col = "green", main = "Histogram of Confirmed Cases in S.Australia")
hist(data$Deaths[data$Province.State == "South Australia"], col = "blue", main = "Histogram of Deaths in S.Australia")
hist(data$Recovered[data$Province.State == "South Australia"], col = "yellow", main = "Histogram of Recovered Cases in S.Australia")

#Q4 - e
library("anytime")
retval <- subset(data, anydate(ObservationDate) > as.Date("2020-12-31"))
write.csv(retval,"out.csv")