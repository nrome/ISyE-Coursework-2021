#   Load packages with pacman
pacman::p_load(pacman, dplyr, ggplot2,forecast, scico, rio, tidyverse, tidyr, hrbrthemes, caret)

#   PROBLEM 1  #########################################################

#   Create vector one dimension
p1_ageVector <- c(13, 15, 15, 16, 19, 20, 20, 21, 22, 22, 23, 25, 25,
                  25, 30, 33, 33, 34, 35, 35, 35, 36, 40, 45, 46, 52, 70)

#   Split BINs of 3 w/ Smoothing
bin_1 <- c(13, 15, 15)  # 14.33333
bin_2 <- c(16, 19, 20)  # 18.33333
bin_3 <- c(20, 21, 22)  # 21
bin_4 <- c(22, 23, 25)  # 23.33333
bin_5 <- c(25, 25, 30)  # 26.66667
bin_6 <- c(33, 33, 34)  # 33.33333
bin_7 <- c(35, 35, 35)  # 35
bin_8 <- c(36, 40, 45)  # 40.33333
bin_9 <- c(46, 52, 70)  # 56

#   created bins/vectors for each segment then used the formula (x + x + x) / 3
#   this effect averages out bin values

sum(bin_9) / 3


#   PROBLEM 2  #########################################################

#   2.a    ------------------------------

x = c(200, 600, 300, 800, 1000)

#   Min-max normalize data from 0min, 1max
(x - min(x)) / (max(x) - min(x))    # 0.000 0.500 0.125 0.750 1.000

#   2.b    ------------------------------

#   Z-score normalization
scale(x)    # -1.13546718, 0.05976143, -0.83666003, 0.65737574, 1.25499004, scaled:center": 580, "scaled:scale": 334.664
(x - mean(x)) / sd(x)   # -1.13546718, 0.05976143, -0.83666003,0.65737574, 1.25499004 (manual formula)

#   2.c    ------------------------------

#   Normalize by decimal scaling
formatC(as.numeric(x/1000), format = 'f', flag='0', digits = 3)    # "0.200", "0.600", "0.300", "0.800", "1.000" 

#   PROBLEM 3  #########################################################

p3_ageVector <- c(23, 24, 26, 27, 35, 42, 45, 49, 50,
                  52, 56, 56, 57, 59, 60, 61, 63, 64)

p3_fatVector <- c(10.1, 26.4, 7.3, 18.4, 32.6, 24.5, 28.6, 27.2, 31.5, 
                  35.4, 41.9, 27.6, 31.0, 30.7, 35.6, 42.1, 37.5, 36.8)

#   3.a    ------------------------------

#   Z-score normalization
znorm_age = scale(p3_ageVector)
print(znorm_age)

znorm_fat = scale(p3_fatVector)
print(znorm_fat)

#   3.b    ------------------------------

#   Calculate Correlation Coefficient
cor(znorm_age, znorm_fat)   # 0.8085593
cov(p3_ageVector, p3_fatVector) / (sd(p3_ageVector)*sd(p3_fatVector))   # same as above 0.8085593

#   The result is relatively close to 1, the result indicates a strong positive correlation

#   Compute Covariance
cov(p3_ageVector, p3_fatVector)   # 110.4863

#   PROBLEM 4  #########################################################

#   4.a    ------------------------------

hist(p3_ageVector,
     #  x axis starts w/ 0, view values from 20-70 plot equal-widths of 10
     breaks=c(0, seq(20,70, 10)), 
     main="Age Histogram")

hist(p3_fatVector,
     #  x axis starts w/ 0, view values from 20-70 plot equal-widths of 10
     breaks=c(0, seq(10,50, 10)),
     main="Fat Histogram")

#   4.b    ------------------------------

#   Use problem 3 data
df = data.frame(p3_ageVector)

# Cluster Sampling - use sample of 4

group_1 = p3_ageVector[c(p3_ageVector<=40)]
print(group_1)    # 23 24 26 27 35

group_2 = p3_ageVector[c(p3_ageVector>40 & p3_ageVector<=55)]
print(group_2)    # 42 45 49 50 52

group_3 = p3_ageVector[c(p3_ageVector>55 & p3_ageVector<=60)]
print(group_3)    # 56 56 57 59 60

group_4 = p3_ageVector[c(p3_ageVector>60)]
print(group_4)    # 61 63 64

cluster_set = c(group_1, group_2, group_3, group_4)
print(cluster_set)    # 23 24 26 27 35 42 45 49 50 52 56 56 57 59 60 61 63 64

#   ask for tests from sets 1:4 and limit 2 results
select_sample = sample(1:4, size = 2, replace = FALSE)
print(select_sample)    # sample sets 4, 2

sampled = c(group_4, group_2)
print(sampled)    # 61 63 64 42 45 49 50 52

# Stratified Sampling - use sample of 5

df$strata = NA
df$strata[df$p3_ageVector<30] = 'youth'
df$strata[df$p3_ageVector>=30 & df$p3_ageVector<60] = 'middle-aged'
df$strata[df$p3_ageVector>60] = 'senior'
df$strata = factor(df$strata)

strat_sample_1 = sample(1:nrow(df),size = 5, replace = FALSE)
df[sort(strat_sample_1),]

strat_sample_2 = sample(levels(df$strata),size = 1)
df[df$strata == strat_sample_2,]

sample.prop = function(x, prop) {
    n = round(length(x) * prop)
    sample(x, n)
}

strat_sample_3 = by(1:nrow(df), INDICES = df$strata, FUN = sample.prop, prop = 5/nrow(df))
df[sort(unlist(strat_sample_3)),]

#   CLEAN UP  #########################################################

#   Clean environment
rm(list = ls())

#   Uninstall packages
p_unload(all)

#   Remove plots
dev.off()  # do only if a plot exists

#   Clear the console
cat("\014")   # command shortcut ctrl+L

