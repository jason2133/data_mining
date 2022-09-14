##### Association Rule #####

# C:/Users/jason/¹ÙÅÁ È­¸é/coding1/data_mining/Assignment/Assignment_1_Association Analysis/BNKSERV.csv

## Calling libraries
# install.packages("arules")
# install.packages("arulesViz")

library(arules)
library(arulesViz)

# Reading Data
BNKSERV = read.transactions("C:/Users/jason/¹ÙÅÁ È­¸é/coding1/data_mining/Assignment/assignment1/BNKSERV.csv", format = "single", cols = c(1,2), sep=",", skip=1, rm.duplicate=TRUE)
inspect(BNKSERV)

str(BNKSERV)
as(BNKSERV, "data.frame")[1:10,]
as(BNKSERV, "data.frame")[10:20,]

rules = apriori(BNKSERV, parameter=list(support=0.1, confidence=0.7, minlen=2), control=list(verbose=F))
rules.sorted = sort(rules, by=c("support","lift")) #sorting data
inspect(rules.sorted)

# rules.sub = subset(rules, subset = rhs %in% "heineken" & lift > 1) 
# inspect(rules.sub)

# rules.sub = subset(rules, subset = lhs %in% "heineken" & lift > 1) 
# inspect(rules.sub)

# Plot
plot(rules)
plot(rules, measure = c("support", "lift"), shading = "confidence")
