###################################################
#
# Example: Association Analysis with Grocery Data 
#
###################################################

## Calling libraries
# install.packages("arules")
# install.packages("arulesViz")

library(arules) #install.packages("arules")
library(arulesViz) #install.packages("arulesViz") 


## Reading data

# C:/Users/jason/¹ÙÅÁ È­¸é/coding1/data_mining/ch4

Grocery = read.transactions("C:/Users/jason/¹ÙÅÁ È­¸é/coding1/data_mining/ch4/Grocery.csv", format = "single", cols = c(1,3), sep=",", skip=1, rm.duplicate=TRUE)
inspect(Grocery)

str(Grocery)
as(Grocery, "data.frame")[1:10,]


## Generating rules

rules = apriori(Grocery, parameter=list(support=0.1, confidence=0.7, minlen=2), control=list(verbose=F))
rules.sorted = sort(rules, by=c("support","lift")) #sorting data
inspect(rules.sorted)
rules.sorted
# Support - Confidence - Coverage - Lift - Count
# Coverage : Probability of Incident A


## Generating a subset of rules

# rhs : heineken in B (Right)
rules.sub = subset(rules, subset = rhs %in% "heineken" & lift > 1) 
inspect(rules.sub)

# lhs : heineken in A (Left)
rules.sub = subset(rules, subset = lhs %in% "heineken" & lift > 1) 
inspect(rules.sub)


## Plotting

# Overall things
plot(rules)


plot(rules, measure = c("support", "lift"), shading = "confidence")


#END
