#####
##  Concentration of measure on a hypercube
##  URL: https://github.com/snarles/misc (folder stats319)
##  Reference: "l1, Isoperimetric Inequalities for Graphs and Superconcentrators", Alon and Milman 1985
#####

library(AlgDesign)

###
#  USER PARAMETERS: Try changing these parameters
###

## Set the dimensionality (= diameter) of the hypercube.
rho <- 5

## The volume of the set A.
init_frac <- 0.5

## The set B will be generated as the set of all vertices outside this distance of A.
epsilon <- 0.2
(dist_expand <- floor(epsilon * rho))

###
#  CALCULATIONS
###

## The matrix used to project the hypercube into two dimensions.
## The matrix is 2 x 10 for up to 10 dimensions.

proj_mat <- cbind(
  c(0, 1),
  c(1, 0),
  c(0.6, 0.6),
  c(-0.6, 0.6),
  c(2, 1),
  c(-1, 2),
  c(4, 3),
  c(-3, 4),
  c(9, 6),
  c(-6, 9)
)

## Only the first rho columns of the matrix are actually used. 
proj_mat <- proj_mat[, 1:rho]

## Generating the coordinates of the hypercube is easy.
cube_coords <- t(as.matrix(gen.factorial(rep(2, rho))) + 1)/2
proj_coords <- proj_mat %*% cube_coords

## Generate the adjacency matrix.
dist_mat <- as.matrix(dist(t(cube_coords), method = "manhattan"))
adj_mat <- (dist_mat == 1) + 0
adj_mat[upper.tri(adj_mat)] <- 0 ## optional: to avoid doubling edges
edges <- which(adj_mat == 1, TRUE)

## Generate the set A.
seed_vertex <- 1
n <- ncol(cube_coords)
dists <- dist_mat[seed_vertex, ]
tab <- table(dists)
tab2 <- cumsum(tab)
max_dist_A <- which(tab2 >= n * init_frac)[1] - 1
excess_count <- tab2[max_dist_A + 1] - (init_frac * n)
max_dist_count <- floor(tab[max_dist_A + 1] - excess_count)
setA <- dists < max_dist_A
inds_max_dist <- sample(which(dists == max_dist_A), max_dist_count)
setA[inds_max_dist] <- TRUE
stopifnot(sum(setA) == floor(n * init_frac))
stopifnot(sum(dists[setA]) == sum(sort(dists)[1:sum(setA)]))

## Generate the set B.
min_dist_from_A <- apply(dist_mat[, setA], 1, min)
setB <- min_dist_from_A > dist_expand

####
##  PLOTTING
####

plot(t(proj_coords), pch = ".", axes = FALSE, ann = FALSE)
for (edge_no in 1:nrow(edges)) {
  lines(t(proj_coords[, edges[edge_no, ]]))
}

points(t(proj_coords[, setA]), pch = 20)
points(t(proj_coords[, setB]), pch = 20, col = "white")
points(t(proj_coords[, setB]), pch = 1)
title(mtext(bquote(rho == .(rho)), adj = 0))
title(mtext(bquote(epsilon == .(epsilon)), adj = 1))
title(sub = paste("b = ", sum(setB)/n))
