install.packages("readxl", repos = "http://cran.us.r-project.org")
library("readxl")

tennis <- read_excel("/Users/kaitlynshevlin/menstennisGS.xlsx")
summary(tennis)
View(tennis)


#Create new upset column
tennis$Upset <- ifelse(as.numeric(tennis$WRank) > as.numeric(tennis$LRank), 'Predicted', 'Upset')
count(tennis, Upset)

#Logistic Regression
tennis$Upset[tennis$Upset == "Upset"] <- 0
tennis$Upset[tennis$Upset == "Predicted"] <- 1
fit <- lm(Upset ~ Surface + Round + W1*L1 + WRank*LRank, data = tennis)
summary(fit)

fit1 <- lm(WRank ~ Surface + LRank + W1*L1, data = tennis)
summary(fit1)
par(mfrow = c(2,2))
plot(fit1)

#Outcomes: predicted vs. upsets
tennis$Upset <- ifelse(as.numeric(tennis$WRank) > as.numeric(tennis$LRank), 'Predicted', 'Upset')
count(tennis, Upset)

dataframe <- data.frame(tennis$Winner[tennis$Round == "The Final"], tennis$Tournament[tennis$Round == "The Final"])
    colnames(dataframe)[1] = "Winner"
    colnames(dataframe)[2] = "Tournament"
dataframe

#Plot of upsets by surface
surfaceupset <- ggplot(tennis, aes(fill = Upset, y = Surface, x = Tournament)) + geom_bar(position = "stack", stat = "identity") + 
  ggtitle("Upsets by Surface") +
  xlab("")
surfaceupset

#Scatterplot of WRank and LRank with Upset fill
ggplot(tennis, aes(x=WRank, y=LRank, color=Upset)) + geom_point(size=1)