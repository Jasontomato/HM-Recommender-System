Df <- read.csv("BreadBasket_DMS.csv")
#
# Retain only transaction number and item
Df$Date <- NULL
Df$Time <- NULL
Df$Day <- NULL
Df$Month <- NULL
Df$Year <- NULL
Df$Hour <- NULL
Df$Minute <- NULL
#
# Write to a csv file
#
write.csv(Df, file="Df2.csv")
#
# Load the required libraries arules and aruesviz
library(arules)
library(arulesViz)
# 
# The read.transactions command will convert this into sparse binary matrix format
tr <- read.transactions("Df2.csv", format = "single", header = TRUE, sep = ",", cols = c("Transaction","Item"))
#

# Look at the top 15 most frequent items
itemFrequencyPlot(tr,topN=15,type="relative")
# Call the apriori algorithms
rules <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.8))
summary(rules)
# Inspect rules
rules<-sort(rules, decreasing=TRUE,by="lift")
inspect(rules[1:5])
#
# To target Tea on the rhs
#
rules<-apriori(data=tr, parameter=list(supp=0.0003,conf = 0.5), 
               appearance = list(default="lhs",rhs="Tea"),
               control = list(verbose=F))
#
rules<-sort(rules, decreasing=TRUE,by="lift")
inspect(rules[1:5])
plot(rules)
#
# To target Coffee on the rhs
rules<-apriori(data=tr, parameter=list(supp=0,conf = 0), 
               appearance = list(default="rhs",lhs="Coffee"),
               control = list(verbose=F))
#
rules<-sort(rules, decreasing=TRUE,by="lift")
inspect(rules[1:5])
plot(rules)

