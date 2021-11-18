install.packages("arules",dependencies=c("Depends", "Suggests"))
install.packages("arulesViz",dependencies=c("Depends", "Suggests"))
install.packages("plyr", dependencies=c("Depends", "Suggests"))
install.packages('caret', dependencies=c("Depends", "Suggests"))

install.packages("tidyverse", dependencies=c("Depends", "Suggests"))

install.packages('readxl', dependencies=c("Depends", "Suggests"))
install.packages('knitr', dependencies=c("Depends", "Suggests"))
install.packages('ggplot', dependencies=c("Depends", "Suggests"))
install.packages('lubridate',dependencies=c("Depends", "Suggests"))


library(arules)
library(arulesViz)
library(plyr)
library(caret)

library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)



# reading file separated by the coma( this is the right set)
tr <- read.transactions('C:/Users/ypbow/Documents/ElectronidexTransactions2017.csv', format = 'basket',sep=',')



#Step 2

#checking for duplicates

#duplicated(tr)
unique(tr)


sum(is.na(tr))
#na.omit(basket)


length (tr) # Number of transactions.
size (tr) # Number of items per transaction
a=LIST(tr)
a[2]

LIST(tr) # # Lists the transactions by conversion
itemLabels(tr)# To see the item labels
summary(tr)





#Step 3

itemFrequencyPlot(tr, topN=20, type='absolute')

# of Transactions you'd like to plot))

image(sample(tr, 125))




#Step 4,5 Rules
#1

rules<- apriori (tr, parameter = list(supp = 0.001, conf = 0.8))
rules.sorted <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)
inspect(rules.sorted[1:10])
plot(rules.sorted)
#plot(rules[1:10])

ItemRules <- subset(rules, items %in% "Dell Desktop")
ItemRules
inspect(ItemRules[1:5])

is.redundant(rules)
matRules=rules2matrix(rules)
 # find redundant rules
rules.sorted <- sort(rules, by="lift",decreasing=TRUE)
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)


#2

rules <- apriori(tr,parameter = list(minlen=1, supp=0.001, conf=0.8), control = list(verbose=F))
rules.sorted <- sort(rules, by="lift",decreasing=TRUE)
inspect(rules.sorted[1:10])
summary(rules)
plot(rules.sorted)

#3 new rule does work 

rules <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.8))
rules.sorted <- sort(rules, decreasing = TRUE, by = "support")
inspect(rules.sorted[1:10])
summary(rules)
plot(rules.sorted)

#4

tr1 = as(tr, "transactions")
rules = apriori(tr1, parameter=list(support=0.001, confidence=0.8));
summary(rules)
rules.sorted <- sort(rules, decreasing = TRUE, by = "support")
inspect(rules.sorted[1:10])
plot(rules.sorted);
head(quality(rules));
plot(rules.sorted, measure=c("support","lift"), shading="confidence");
plot(rules.sorted, shading="order", control=list(main ="Two-key plot"));


#5

rules = apriori(tr1, parameter=list(support=0.001, confidence=0.8));
plot(rules, measure=c("support","lift"), engine='interactive', interactive=TRUE);
subrules = rules[quality(rules)$confidence > 0.8];
inspect(subrules[1:10])
summary(subrules)


#5
plot(subrules, method="matrix", measure="lift");
#plot(subrules, method="matrix", measure="measure", control=list(reorder=TRUE));
plot(subrules, method="matrix3D", measure="lift");
#plot(subrules, method="matrix3D", measure="lift", control = list(reorder=TRUE));
plot(subrules, method="matrix", measure=c("lift", "confidence"));
#plot(subrules, method="matrix", measure=c("lift","confidence"), control = list(reorder=TRUE));
plot(rules, method="grouped");
#plot(rules, method="grouped", control=list(k=50));
#sel = plot(rules, method="grouped", interactive=TRUE);

#6 

itemFrequencyPlot(tr, support = 0.001, cex.names=0.8);
fsets = eclat(tr, parameter = list(support = 0.05), control = list(verbose=FALSE));
singleItems = fsets[size(items(fsets)) == 1];
singleSupport = quality(singleItems)$support;
names(singleSupport) = unlist(LIST(items(singleItems), decode = FALSE));
head(singleSupport, n = 5);#need to read about results
itemsetList = LIST(items(fsets), decode = FALSE);
allConfidence = quality(fsets)$support / sapply(itemsetList, function(x)
  max(singleSupport[as.character(x)]));
quality(fsets) = cbind(quality(fsets), allConfidence);
summary(fsets);







