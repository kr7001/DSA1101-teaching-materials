setwd("~/My Drive (krlee7001@gmail.com)/NUS (Acads Related)/AY2526 Sem 2/DSA1101 TA")

########## OFFSITE Q3 LOAD ##############

#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)

data(Epub)
?Epub

inspect(head(Epub))
summary(Epub)

Epub@itemInfo[1:10,]
Epub@data[1:10,]

########## OFFSITE Q3A ##############

itemsets.1 = apriori(data = Epub, 
                     parameter = 
                       list(minlen = 1, maxlen = 1, # only 1-itemsets
                            support = 0.005, target="frequent itemsets"))

summary(itemsets.1) # 67 1-itemsets that are frequent

inspect(head(sort(itemsets.1, by = "support"), 5))


########## OFFSITE Q3B ##############

itemsets.2 = apriori(data = Epub, 
                     parameter = 
                       list(minlen = 2, maxlen = 2, # only 2-itemsets
                            support = 0.005, target="frequent itemsets"))

summary(itemsets.2) # no 2-itemsets that are frequent


########## OFFSITE Q3C ##############

rules = apriori(Epub, 
                parameter = list(support = 0.001, confidence = 0.3, 
                            target = "rules"))

highLiftRules = head(sort(rules, by="lift"), 5)
inspect(highLiftRules)

plot(highLiftRules, method = "graph", engine = "igraph",
     edgeCol = "blue", alpha = 1) 

"
Key on how to read this graph:
1. Larger circle -> Higher Support 
2. Darker circle -> Higher Lift

Each circle is a rule. Arrows going into the circle is
LHS of rule and arrows going out of circle is RHS of rule
"


