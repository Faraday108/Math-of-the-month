r1 <- matrix(c(-.5,-.5,.5,.5,-.5,.5,.5,-.5), ncol = 2)
t <- pi/3

branchRect <- function(m, t) {
  rotn <- matrix(c(cos(t), sin(t), -sin(t), cos(t)), ncol = 2)
  rotp <- matrix(c(cos(t-pi/2), sin(t-pi/2), -sin(t-pi/2), cos(t-pi/2)), ncol = 2)
  #center the input rectangle, middle of opposite corners is center
  # subtract center coord from each 
  mcenter <- matrix(rep((m[1,] + m[3,])/2, each = 4), ncol = 2)
  mc <- m - mcenter
  
  r1c <- t(apply(mc, 1, function(x) {sin(t) * rotp %*% matrix(x, ncol = 1)}))
  r1 <- r1c - matrix(rep(r1c[4,] - mc[3,], each = 4),ncol = 2) +  mcenter
  r2c <- t(apply(mc, 1, function(x) {cos(t) * rotn %*% matrix(x, ncol = 1)}))
  r2 <- r2c - matrix(rep(r2c[1,] - mc[2,], each = 4),ncol = 2) + mcenter
  
  list(r1, r2)
}

make_tree <- function(trunk, generations, t) {

  branches <- list(trunk)

  plot(1,type = "n", asp = 1, xlim = c(-2,2), ylim = c(-1,4))
  polygon(trunk, col = "darkgreen")
  n <- 1
  k <- 1
  for (j in 1:generations){
    for (i in k:length(branches)) {
      newBranches <- branchRect(branches[[i]], t)
      invisible(lapply(newBranches, polygon, col = "darkgreen"))
      branches <- c(branches, newBranches)
      n <- n + 1
    }
    k <- n
  }
}

make_tree(r1, generations = 10, t)
