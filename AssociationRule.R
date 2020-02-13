# install.packages("arules", dependencies=T)
# install.packages("arulesViz", dependencies=T)

library(tidyverse)
library(arules)
library(arulesViz)

items <- groceries$`Items in basket`

data("Groceries")
grocery_rules <- apriori(Groceries,
                         parameter = list(support = 0.01, confidence = 0.5))

plot(grocery_rules, method = "graph")
plot(grocery_rules, method = "paracoord")

ruleExplorer(grocery_rules)

tr <- read.transactions(file="groceries.itemset", format = "basket", sep = ',', skip = 0)
rules <- apriori(tr,
                 parameter = list(support = 0.1, confidence = 0.5))

plot(rules, method = "paracoord")
ruleExplorer(rules)
