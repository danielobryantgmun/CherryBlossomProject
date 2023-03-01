kyotodata <- read.csv(file.choose())
liestaldata <- read.csv(file.choose())
washingtondata <- read.csv(file.choose())
vancouverdata <- read.csv(file.choose())
japandata <- read.csv(file.choose())
koreadata <- read.csv(file.choose())
meteodata <- read.csv(file.choose())

combineddata <- rbind2(kyotodata[1:824,], liestaldata[1:119,])
combineddata <- rbind2(combineddata, washingtondata[1:92,])
combineddata <- rbind2(combineddata, vancouverdata[1:1,])
combineddata <- rbind2(combineddata, japandata)
combineddata <- rbind2(combineddata, koreadata)
combineddata <- rbind2(combineddata, meteodata)

# var are location lat long alt year bloom_doy

library(earth)
library(dplyr)



attach(combineddata)
library(earth)
marsfit <- earth(bloom_doy ~ lat + long + alt + year, degree = 10)
summary(marsfit)
hist(marsfit$residuals)
plot(marsfit)
detach(combineddata)

attach(kyotodata)
df <- data.frame(lat, long, alt, year, bloom_doy)
train <- df[1:824,]
test <- df[825:834,]
new_data <- test[,c("lat","long","alt","year")]
predictionskyoto <- predict(marsfit, newdata = new_data)
predictionskyoto <- round(predictionskyoto, digits = 0)
detach(kyotodata)

attach(liestaldata)
df <- data.frame(lat, long, alt, year, bloom_doy)
train <- df[1:119,]
test <- df[120:129,]
new_data <- test[,c("lat","long","alt","year")]
predictionsliestal <- predict(marsfit, newdata = new_data)
predictionsliestal <- round(predictionsliestal, digits = 0)
detach(liestaldata)


attach(washingtondata)
df <- data.frame(lat, long, alt, year, bloom_doy)
train <- df[1:92,]
test <- df[93:102,]
new_data <- test[,c("lat","long","alt","year")]
predictionswashington <- predict(marsfit, newdata = new_data)
predictionswashington <- round(predictionswashington, digits = 0)
detach(washingtondata)


attach(vancouverdata)

df <- data.frame(lat, long, alt, year, bloom_doy)
df <- rbind(df, df[rep(1,10),])
df$year[2] = 2023
df$year[3] = 2024
df$year[4] = 2025
df$year[5] = 2026
df$year[6] = 2027
df$year[7] = 2028
df$year[8] = 2029
df$year[9] = 2030
df$year[10] = 2031
df$year[11] = 2032
train <- df[1:1,]
test <- df[2:11,]
new_data <- test[,c("lat","long","alt","year")]
predictionsvancouver <- predict(marsfit, newdata = new_data)
predictionsvancouver <- round(predictionsvancouver, digits = 0)
detach(vancouverdata)

Table <- data.frame(predictionskyoto, predictionsliestal, predictionswashington, predictionsvancouver)
Table
