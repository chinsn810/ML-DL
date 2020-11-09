library(arules)
groceries=read.transactions("groceries.csv",header=FALSE,sep=",")
View(groceries)
summary(groceries)
inspect(groceries[1:5])

itemFrequency(groceries[,1:3])
itemFrequencyPlot(groceries,support=0.1)
itemFrequencyPlot(groceries,topN=10)
image(groceries[1:5])
freq=itemFrequency(groceries)
image(sample(groceries, 100))#each dot represents unique item bought per customer 
arules=apriori(groceries,parameter=list(support=0.006,confidence=0.25,minlen=2))
inspect(arules)
inspect(sort(arules,by='lift'))
inspect(sort(arules, by = "lift")[1:5])

subrules=inspect(sort(arules,by='support'))
subrules

library(arulesViz)
library(RColorBrewer)
plot(arules,control=list(col='purple'))
plot(arules,col=brewer.pal(8,'Dark2'))

berryrules <- subset(arules, items %in% "berries")
inspect(berryrules)
inspect(arules)
write(arules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)
groceryrules_df <- as(arules, "data.frame")
str(groceryrules_df)





