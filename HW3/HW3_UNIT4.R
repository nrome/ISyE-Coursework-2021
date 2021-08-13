#   LOAD PACKAGES   #########################################################

#   Load packages with pacman
pacman::p_load(pacman, arules)

#   PROBLEM 1  #########################################################

p1.Db = list(c("M","O","N","K","E","Y"), c("D","O","N","K","E","Y"), 
             c("M", "A", "K", "E"), c("M", "U", "C", "K", "Y"), c("C", "O", "K", "I", "E"))

#   1.a    ------------------------------

#   convert to TID-itemset format (transaction)
p1.Db = as(p1.Db,'transactions')
# display the dataset
inspect(p1.Db)

#   Db1 = as(p1Db,'tidLists') # convert to vertical data layout (item-TID_set)
#   inspect(Db1)
#   as(Db,'matrix') # convert to matrix format

p1.FreqItems = apriori(p1.Db, parameter = list(support = 0.6, target = "frequent itemsets"))
inspect(p1.FreqItems)

#   From what I've read:
#   Apriori — could be slower but may have more reach — 
#             it’s tree based, based on breadth, 
#             and requires multiple data stores to generate a candidate set.
#           - all subset of a frequent item will also be frequent - 
#             no infrequent overhead will be gen/tested
#   FP is apparently a faster array based approach, checks depth first, 
#   and mines patterns of large datastores. 

#   1.b    ------------------------------

#   Min Support as 0.6, confidence as 0.75.
#   No need to adjust Right Hand Side (rhs) and Left Hand Side (lhs)

p1.AssocRules = apriori(p1.Db, parameter = list(support = 0.6, confidence = 0.75, target = "rules"))
#   Get last Left Hand Side with two attributes
inspect(p1.AssocRules[10:12])

#   1.c    ------------------------------

# Closed Frequent Patterns
inspect(p1.FreqItems[is.closed(p1.FreqItems)])
# Maximal Frequent Patterns
inspect(p1.FreqItems[is.maximal(p1.FreqItems)])

#   PROBLEM 2  #########################################################

p2.Db = list(c("Crab","Milk","Cheese","Bread"), c("Cheese","Milk","Apple", "Pie", "Bread"),
             c("Apple", "Milk", "Bread", "Pie"), c("Bread", "Milk", "Cheese"))

#   2.a    ------------------------------

#   convert to TID-itemset format (transaction)
p2.Db = as(p2.Db,'transactions')
# display the dataset
inspect(p2.Db)

p2.FreqItems = apriori(p2.Db, parameter = list(support = 0.6, target = "frequent itemsets"))
inspect(p2.FreqItems)

p2.AssocRules = apriori(p2.Db, parameter = list(support = 0.6, confidence = 0.8, target = "rules"))
#   Get last Left Hand Side with two attributes
inspect(p2.AssocRules[7:8])

#   PROBLEM 3  #########################################################

#   Considering the bread/milk transactions for purchase behavior - what about the hypothetical
#   maybe items not occuring in a transaction some might still be valuable for potential sales
#   (e.g., those that buy fish but don't by red meat, or those that buy coffee and not tea)
#   However, we have to manually suggest absent items if we want to use the Apriori algorithm
#   This was tricky to figure out in R
#   This approach would be problematic for large datasets

#   I've used the Adult data (available in R with the > data(Adult) command)
#   Validate negative correlation within a strong association rule (i.e., where lift less than 1)

data(Adult)
head(Adult)

Adult.FreqItems = apriori(Adult, parameter = list(support = 0.6, target = "frequent itemsets"))
inspect(Adult.FreqItems)

Adult.AssocRules = apriori(Adult, parameter = list(support = 0.6, confidence = 0.75, target = "rules"))
inspect(Adult.AssocRules)

inspect(head(sort(Adult.AssocRules, by=c("supp", "conf"))))

#   PROBLEM 4  #########################################################

#   4.a    ------------------------------

#   4.b    ------------------------------

#   4.c    ------------------------------

#   CLEAN UP  #########################################################

#   Clean environment
rm(list = ls())

#   Uninstall packages
p_unload(all)

#   Remove plots
dev.off()  # do only if a plot exists

#   Clear the console
cat("\014")   # command shortcut ctrl+L

