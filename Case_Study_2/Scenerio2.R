#Scenerio 2
data2 <- videoMultiple
data2[data2 == 99] <- NA
sum(is.na(data2))

video.data2 <- na.omit(videoMultiple)
video.data2



videoMultiple[is.na(videoMultiple)] <- 99
print(videoMultiple$action)

data2[3]
mean(data$freq)

data$freq
data$time
hist(data$freq)

plot(data$time, data$freq, main = "Scatterplot freq vs time")
