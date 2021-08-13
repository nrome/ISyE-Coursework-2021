#   LOAD PACKAGES   ####################################################

#   Load packages with pacman
pacman::p_load(pacman, rpart, rpart.plot, readxl, e1071, animation, cluster)

#   Set working directory
setwd("~/Desktop/UW-MADISON-ENG/ENGINEERING-412/EXAM2")

#   PROBLEM 1  #########################################################

problem.1.dataset = read.csv('binaryclass.csv')
problem.1.dataset
head(problem.1.dataset)
str(problem.1.dataset)
summary(problem.1.dataset)

problem.1.dataset$count = 1

entropy = function(x) {
    # x is a sequence of frequencies such as x = c(5,9)
    pi = x/sum(x)
    info = -sum(pi * log(pi, base = 2))
    return(info)
}

x = c(6,4)
#   Overall Entropy
entropy(x)

Info = function(count, by, status, data){
    Split = aggregate(data[[count]], data[c(by, status)], FUN = sum)
    Dj = aggregate(Split$x, Split[by], FUN = sum)
    InfoDj = aggregate(Split$x, Split[by], FUN = entropy)
    info = sum(Dj$x/sum(Dj$x) * InfoDj$x)
    return(info)
}

Info('count', 'A', 'Class.Label', problem.1.dataset)
Info('count', 'B', 'Class.Label', problem.1.dataset)

Info.gain = function(count_col, by_col, target_col, data){
    info.nosplit = Info(count_col, NULL, target_col, data)
    info.split = Info(count_col, by_col, target_col, data)
    gain = info.nosplit - info.split
    return(gain)
}

Info.gain('count', 'A', 'Class.Label', problem.1.dataset)
Info.gain('count', 'B', 'Class.Label', problem.1.dataset)



#   PROBLEM 2  #########################################################

#   2.a    ------------------------------

#   2.b    ------------------------------

#   PROBLEM 3  #########################################################

problem.3.dataset = as.data.frame(read_excel('Uni6p2.xlsx'))
#   Data set Col -> Class [+] is P, [-] is N)
print(problem.3.dataset)
model = naiveBayes(Class ~ A + B + C, data = problem.3.dataset)
model
#   Use the 10th row as the test set
predict(model, newdata = problem.3.dataset[10, ], type = "raw") #   [-]: 0.998999 > [+]: 0.001000985


#   PROBLEM 4  #########################################################

#   textual and completed

#   PROBLEM 5  #########################################################

D = matrix(c(2, 7, 4, 5, 8, 3, 7, 2, 5, 6, 4, 4, 3, 1, 8, 7), ncol = 2, byrow = TRUE)
#   Represent eight datasets
rownames(D) = c('A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2')
#   Denote x and y coordinates
colnames(D)  = c('x', 'y')

#   Three Clusters A1, B1, C1 centers
center = D[c('A1', 'B1', 'C1'),]
#   Distance function is Euclidean distance
dissim = dist(rbind(D, center), method = 'euclidean')

#   Convert to conventional matrix
dissim = as.matrix(dissim)
#   8 points
npoint = nrow(D)
#   first eight rows and all columns except first eight columns
dissim = dissim[1:npoint, -(1:npoint)]
#   group by min value - returns columns w/ smallest val w/in each row
group = apply(dissim, MARGIN = 1, FUN = which.min)

#   Use the k-means algorithm to show only
#   Assignment step:
kmeans.group = function(D, center) {
    npoint = nrow(D)
    dissim = dist(rbind(D, center), method = 'euclidean')
    dissim = as.matrix(dissim)
    dissim = dissim[1:npoint, -(1:npoint)]
    group = apply(dissim, MARGIN = 1, FUN = which.min)
    return(group)
}

#   Update step:
kmeans.newcenter = function(D, group){
    center.x = aggregate(D[,1], by = list(group), FUN = mean)
    center.y = aggregate(D[,2], by = list(group), FUN = mean)
    center = cbind(center.x$x, center.y$x)
    return(center)
}

#   5.a    ------------------------------
#   three cluster centers after the first round of execution

#   Calculate using center points - first execution 
group = kmeans.group(D, center)
group
plot(D, col = group)

#   5.b    ------------------------------
#   final points in the three clusters after the algorithm stops

#   Calculate new center points - final points where groups no longer change/converge
center = kmeans.newcenter(D, group)
points(center, col = 1:3, pch = 8)
group = kmeans.group(D, center)
group
plot(D, col = group)

#   R kmeans package ####################
D
cl = kmeans(D, centers = D[c('A1', 'B1', 'C1'),], algorithm = "MacQueen")
print(cl)

#   exact same from manual/user defined function - final points
plot(D, col = cl$cluster)
#   three resorting center points - same from manual/user defined function - first round
points(cl$centers, col = 1:3, pch = 8)

#   R animation package ####################

#   Process until results converge 
oopt = ani.options(interval = 2)
kmeans.ani(D, center = 3)

#   PROBLEM 6  #########################################################

#   R cluster package / similarity matrix ####################

sim = diag(.5,5, nrow = 5)
    sim[upper.tri(sim)] = c(0.34, 0.84, 0.36, 0.25, 0.52, 0.71, 0.1, 0.94, 0.81, 0.65)
sim = sim + t(sim)
sim

dist = 1 - sim
dist

#   AGNES
agn = agnes(dist, method = "single")
plot(agn)
#   Hit <Return> to see next plot:
#   Hit <Return> to see next plot:

#   Single Link
agn = agnes(sim, diss = F, method = "single")
plot(agn)
#   Hit <Return> to see next plot:
#   Hit <Return> to see next plot:

#   Complete Link
agn = agnes(sim, diss = F, method = "complete")
plot(agn)
#   Hit <Return> to see next plot:
#   Hit <Return> to see next plot:

#   Diana Algorithm
dv = diana(dist)
plot(dv)
#   Hit <Return> to see next plot:
#   Hit <Return> to see next plot:


#   CLEAN UP   #########################################################

#   Clean environment
rm(list = ls())

#   Uninstall packages
p_unload(all)

#   Remove plots
dev.off()  # do only if a plot exists

#   Clear the console
cat("\014")   # command shortcut ctrl+L

