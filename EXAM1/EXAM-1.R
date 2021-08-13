#   LOAD PACKAGES   #########################################################

#   Load packages with pacman
#   classInt: bin depth or width,
pacman::p_load(pacman, classInt, arules, cluster)

#   PROBLEM 1  #########################################################

#   Create vector - one dimension
p1_testScoreVector <- c(74, 68, 84, 80, 56, 64, 40, 47, 78, 71, 95, 50)

classIntervals(p1_testScoreVector, 4)

#   Split BINs of 3 w/ Smoothing
bin_1 <- c(74, 68, 84)  # 75.33333
bin_2 <- c(80, 56, 64)  # 66.66667
bin_3 <- c(40, 47, 78)  # 55
bin_4 <- c(71, 95, 50)  # 72

sum(bin_4) / 3

#   Equal Frequency binning
p1_equalFrequency <- classIntervals(p1_testScoreVector, 4, style = 'quantile')
print(p1_equalFrequency)

#   Equal Width binning
p1_equalWidth <- classIntervals(p1_testScoreVector, 4, style = 'equal')
print(p1_equalWidth)

#   PROBLEM 2  #########################################################

dataset = matrix(c(250,600,750,8400), ncol = 2)
colnames(dataset) = c('BEER','NO-BEER')
rownames(dataset) = c('NUTS','NO-NUTS')
dataset

#   Calculate Chi Square
chisq.test(dataset, correct=FALSE)

#   PROBLEM 3  #########################################################

dissimilarity_matrix = data.frame(test1 = c('Code-A','Code-A','Code-B','Code-C'),
                        test2 = c('Code-I','Code-I','Code-II','Code-II'),
                        test3 = c('Excellent','Fail','Fail','Pass'), 
                        test4 = c(80, 20, 100, 60))
dissimilarity_matrix$test1 = as.factor(dissimilarity_matrix$test1)
dissimilarity_matrix$test2 = as.factor(dissimilarity_matrix$test2)
dissimilarity_matrix$test3 = ordered(dissimilarity_matrix$test3, levels = c('Fail','Pass','Excellent'))
dissimilarity_matrix$test4 = as.numeric(dissimilarity_matrix$test4)
daisy(dissimilarity_matrix)

#   PROBLEM 4  #########################################################

p4.Db = list(c("C","F","A","E","D","H"), 
             c("H","A","B","C"), 
             c("D","E","C","B"), 
             c("C","A","G","D","B"), 
             c("A","H","G","C"))

#   convert to TID-itemset format (transaction)
p4.Db = as(p4.Db,'transactions')
# display the dataset
inspect(p4.Db)

#   4.1    ------------------------------

p4.FreqItems = apriori(p4.Db, parameter = list(support = 0.6, target = "frequent itemsets"))
inspect(p4.FreqItems)

#   4.2    ------------------------------

# Closed Frequent Patterns
inspect(p4.FreqItems[is.closed(p4.FreqItems)])
# Maximal Frequent Patterns
inspect(p4.FreqItems[is.maximal(p4.FreqItems)])

#   4.3    ------------------------------

#   Min Support as 0.6, Min Confidence as 0.8.
#   No need to adjust Right Hand Side (rhs) and Left Hand Side (lhs)

p4.AssocRules = apriori(p4.Db, parameter = list(support = 0.6, confidence = 0.8, target = "rules"))
inspect(p4.AssocRules)


#   CLEAN UP  #########################################################

#   Clean environment
rm(list = ls())

#   Uninstall packages
p_unload(all)

#   Remove plots
dev.off()  # do only if a plot exists

#   Clear the console
cat("\014")   # command shortcut ctrl+L
