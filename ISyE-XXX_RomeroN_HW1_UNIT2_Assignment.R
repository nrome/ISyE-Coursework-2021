#   Load packages with pacman
pacman::p_load(pacman, dplyr, ggplot2,forecast, scico, rio, tidyverse, tidyr, lsa)

#   PROBLEM 1  #########################################################

#   1.a    ------------------------------

#   Create vector one dimension
ageVector <- c(11, 12, 14, 14, 19, 20, 20, 22, 22, 22, 23, 25, 25, 25, 30, 33, 33,
      35, 35, 35, 36, 36, 40, 45, 46, 55, 72)

#   Compute Mean
result.mean <- mean(ageVector)
print(result.mean) #    29.81481

#   Compute Median
median.result <- median(ageVector)
print(median.result) #    25

#   1.b    ------------------------------

mode <- table(ageVector)
print(mode) #   establish tuple for frequency

names(mode)[which(mode==max(mode))] #   get like names/attributes of the vector (i.e., 22, 25, 35)

sort(ageVector) #   eye test for like attributes of the vector

#   Compute Mode/Confirm
names(table(ageVector))[table(ageVector)==max(table(ageVector))] #   get like names/attributes of the vector (i.e., 22, 25, 35)

#   data distribution is multimodal (i.e., there are like data attributes in sets of two [bimodal] and sets of three [trimodal])
#   the mode displays all the trimodal data attributes

#   1.c    ------------------------------

#   There are 27 data attributes - Q1 is 20 and Q3 is 36

#   1.d    ------------------------------

quantile(ageVector)
#     0%  25%  50%  75% 100% 
#   11.0 21.0 25.0 35.5 72.0 

#   1.e    ------------------------------

boxplot(ageVector)

#   1.f    ------------------------------

#   Quantile plot: displays data as values are measured against an independent variable then graphically plotted against the corresponding quantile
#   Q-Q plot: renders/displays data distribution against the corresponding quantiles of a different univariate distribution


#   PROBLEM 2  #########################################################

#   2.a    ------------------------------

hospitalAgeVector <- c(23, 24, 26, 27, 35, 42, 45, 49, 50, 52, 56, 56, 57, 59, 60, 61, 63, 64)
hospitalFatVector <- c(10.1, 26.4, 7.3, 18.4, 32.6, 24.5, 28.6, 27.2, 31.5, 35.4, 41.9, 27.6, 31.0, 30.7, 35.6, 42.1, 37.5, 36.8)

df <- data.frame(hospitalAgeVector, hospitalFatVector)
print(df)

#   Compute Age Mean
result.mean <- mean(hospitalAgeVector)
print(result.mean) #    47.16667

#   Compute Age Median
median.result <- median(hospitalAgeVector)
print(median.result) #    51

#   Compute Age Standard Deviation
sd.result = sqrt(var(hospitalAgeVector))
print (sd.result) #    14.29212

#   Compute Fat Mean
result.mean <- mean(hospitalFatVector)
print(result.mean) #    29.17778

#   Compute Fat Median
median.result <- median(hospitalFatVector)
print(median.result) #    30.85

#   Compute Fat Standard Deviation
sd.result = sqrt(var(hospitalFatVector))
print (sd.result) #    9.560923

#   2.b    ------------------------------

boxplot(hospitalAgeVector)
boxplot(hospitalFatVector)

#   2.c    ------------------------------

df %>%
    select(hospitalAgeVector, hospitalFatVector) %>%
    plot(
        main = "Scatterplot | hospital tested age and body fat",
        xlab = "Hospital Tested Age",
        ylab = "Hospital Tested Body Fat %"
    )

#   Q-Q plot for Age Vector
with(df, {
  qqnorm(hospitalAgeVector);
  qqline(hospitalAgeVector)
  })

#   Q-Q plot for Fat Vector
with(df, {
  qqnorm(hospitalFatVector);
  qqline(hospitalFatVector)
})

#   PROBLEM 3  #########################################################

dist_tuples <- rbind(c(23, 2, 52, 9), c(22, 4, 46, 8))

#   3.a    ------------------------------

dist(dist_tuples,method = 'euclidean', upper=TRUE)

#   3.b    ------------------------------

dist(dist_tuples,method = 'manhattan')

#   3.c    ------------------------------

dist(dist_tuples,method = 'minkowski', p=3)

#   3.d    ------------------------------

dist(dist_tuples,method = 'maximum')


#   PROBLEM 4  #########################################################

#   4.a    ------------------------------

x1 = rbind(c(1.5, 1.7), c(1.4, 1.6))
dist(x1,method = 'euclidean', upper=TRUE)
dist(x1,method = 'manhattan')
dist(x1,method = 'maximum')
cosine(x1) #   Calculate Cosine Similarity

x2 = rbind(c(2, 1.9), c(1.4, 1.6))
dist(x2,method = 'euclidean', upper=TRUE)
dist(x2,method = 'manhattan')
dist(x2,method = 'maximum')
cosine(x2) #   Calculate Cosine Similarity

x3 = rbind(c(1.6, 1.8), c(1.4, 1.6))
dist(x3,method = 'euclidean', upper=TRUE)
dist(x3,method = 'manhattan')
dist(x3,method = 'maximum')
cosine(x3) #   Calculate Cosine Similarity

x4= rbind(c(1.2, 1.5), c(1.4, 1.6))
dist(x4,method = 'euclidean', upper=TRUE)
dist(x4,method = 'manhattan')
dist(x4,method = 'maximum')
cosine(x4) #   Calculate Cosine Similarity

x5 = rbind(c(1.5, 1.0), c(1.4, 1.6))
dist(x5,method = 'euclidean', upper=TRUE)
dist(x5,method = 'manhattan')
dist(x5,method = 'maximum')
cosine(x5) #   Calculate Cosine Similarity

#   4.b    ------------------------------

x1 = rbind(c(0.6616216, 0.7498379), c(0.6585046, 0.7525767))
dist(x1,method = 'euclidean', upper=TRUE)

x2 = rbind(c(0.7249994, 0.6887495), c(0.6585046, 0.7525767)) #   Normalized data  
dist(x2,method = 'euclidean', upper=TRUE)

x3 = rbind(c(0.6643638, 0.7474093), c(0.6585046, 0.7525767)) #   Normalized data  
dist(x3,method = 'euclidean', upper=TRUE)

x4 = rbind(c(0.624695, 0.7808688), c(0.6585046, 0.7525767)) #   Normalized data  
dist(x4,method = 'euclidean', upper=TRUE)

x5 = rbind(c(0.8320503, 0.5547002), c(0.6585046, 0.7525767)) #   Normalized data  
dist(x5,method = 'euclidean', upper=TRUE)

1.6/sqrt(1.4^2 + 1.6^2) #   Normalization equation

#   PROBLEM 5  #########################################################

dis_matrix = data.frame(test1 = c('Code-A','Code-A','Code-B','Code-C'), test2 = c('Excellent','Pass','Pass','Fail'), test3 = c(50, 100,80, 0))
dis_matrix$test1 = as.factor(dis_matrix$test1)
dis_matrix$test2 = ordered(dis_matrix$test2, levels = c('Fail','Pass','Excellent'))
#   dis_matrix$test3 = as.factor(dis_matrix$test3)
dis_matrix$test3 = as.numeric(dis_matrix$test3)
library(cluster)
daisy(dis_matrix)


#   CLEAN UP  #########################################################

#   Clean environment
rm(list = ls())

#   Uninstall packages
p_unload(all)

#   Remove plots
dev.off()  # do only if a plot exists

#   Clear the console
cat("\014")   # command shortcut ctrl+L
