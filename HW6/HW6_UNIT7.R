#   LOAD PACKAGES   ####################################################

#   Load packages with pacman
pacman::p_load(pacman, animation, cluster)

#   Set working directory
setwd("/Users/nickromero/Desktop/UW-MADISON-ENG/ENGINEERING-412/HW5/Resources")

#   PROBLEM 1  #########################################################

D = matrix(c(2, 9, 3, 5, 7, 4, 5, 7, 7, 5, 5, 5, 1, 2, 4, 8), ncol = 2, byrow = TRUE)

#   Represent eight datasets
rownames(D) = c('A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2')

#   Denote x and y coordinates
colnames(D)  = c('x', 'y')

#   Three Clusters A1, B1, C1 centers
center = D[c('A1', 'B1', 'C1'),]
center
#   Distance function is Euclidean distance
dissim = dist(rbind(D, center), method = 'euclidean')

#   Convert to conventional matrix
dissim = as.matrix(dissim)
dissim
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

#   test group
kmeans.group(D, center) == group

#   Update step:
kmeans.newcenter = function(D, group){
    center.x = aggregate(D[,1], by = list(group), FUN = mean)
    center.y = aggregate(D[,2], by = list(group), FUN = mean)
    center = cbind(center.x$x, center.y$x)
    return(center)
}

#   test center
kmeans.newcenter(D, group) == center

#   1.a    ------------------------------
#   three cluster centers after the first round of execution

#   Calculate using center points
#   first execution 
group = kmeans.group(D, center)
plot(D, col = group)

#   1.b    ------------------------------
#   final points in the three clusters after the algorithm stops

#   Calculate new center points
#   final points where groups no longer change/converge
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
#   three resorting center points
#   exact same from manual/user defined function - first round
points(cl$centers, col = 1:3, pch = 8)

#   R animation package ####################

#   Process until results converge 
oopt = ani.options(interval = 2)
kmeans.ani(D, center = 3)

#   R cluster package / similarity matrix ####################

sim = diag(.5,5, nrow = 5)
sim[upper.tri(sim)] = c(0.1, 0.41, 0.64, 0.55, 0.47, 0.44, 0.35, 0.98, 0.85, 0.76)
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

#   PROBLEM 2  #########################################################

#   2.a

#   2.b 

#   PROBLEM 4  #########################################################


#   PROBLEM 5  #########################################################


#   PROBLEM 6  #########################################################

dataset = matrix(c(8, 10, 24, 28, 30, 40, 46), ncol = 2, byrow = TRUE)
#   dataset = c(8, 10, 24, 28, 30, 40, 46)

#   { 16, 42 }
cl.1  = c(8, 10, 24, 28)
#   { 10, 40 }
cl.2  = c(30, 40, 46)

#   Represent eight datasets
rownames(dataset) = c('cl1', 'cl2')

#   Denote x and y coordinates
colnames(dataset)  = c('x', 'y')

#   Three Clusters A1, B1, C1 centers
center = dataset[c('16', '42'),]
center
#   Distance function is Euclidean distance
dissim = dist(rbind(dataset, center), method = 'euclidean')

#   Convert to conventional matrix
dissim = as.matrix(dissim)
dissim
#   8 points
npoint = nrow(dataset)
#   first eight rows and all columns except first eight columns
dissim = dissim[1:npoint, -(1:npoint)]


#   group by min value - returns columns w/ smallest val w/in each row
group = apply(dissim, MARGIN = 1, FUN = which.min)

#   6.a    ------------------------------

#   Three Clusters A1, B1, C1 centers
ds.center = dataset[c('16', '42'),]
ds.center

#   6.b    ------------------------------

#   Process until results converge 
oopt = ani.options(interval = 2)
kmeans.ani(dataset, center = 2)

#   6.c    ------------------------------

#   Single Link / AGNES
agn = agnes(dataset, method = "single")
plot(agn)
#   Hit <Return> to see next plot:
#   Hit <Return> to see next plot:

#   Single Link / AGNES
agn = agnes(cl.1, method = "single")
plot(agn)
#   Hit <Return> to see next plot:
#   Hit <Return> to see next plot:

#   Single Link / AGNES
agn = agnes(cl.2, method = "single")
plot(agn)
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

