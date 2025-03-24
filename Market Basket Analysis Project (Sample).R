library(arules)
library(arulesViz)
data("Groceries")
str(Groceries)
View(Groceries)
summary(Groceries)
head(Groceries)
# Perform market basket analysis
rules <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.7))

# Inspect the rules
rules_sorted<-sort(rules, by = "lift",decreasing = TRUE)
inspect(rules_sorted[1:10])

#identify and remove dupilcate rules
rules_sorted
redundant_rules<-is.redundant(rules_sorted)
redundant_rules

summary(redundant_rules)
rules_sorted<-rules_sorted[!redundant_rules]
rules_sorted

#keeping bottled beer as constant
rules_beer<- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.05),appearance = list(default="rhs",lhs="bottled beer"))
inspect(rules_beer[1:10])

red_beer_rules <- is.redundant(rules_beer)
red_beer_rules

new_beer<-rules_beer[!red_beer_rules]
new_beer
# Visualize the rules
plot(new_beer, method = "graph")

# Create an interactive HTML table
inspectDT(new_beer)
