# cross product
cross <- function(x, y, i=1:3) {
  create3D <- function(x) head(c(x, rep(0, 3)), 3)
  x <- create3D(x)
  y <- create3D(y)
  j <- function(i) (i-1) %% 3+1
  return (x[j(i+1)]*y[j(i+2)] - x[j(i+2)]*y[j(i+1)])
}

# Vector norm
norm_vec <- function(x) sqrt(sum(x^2))

# "Up" vector rotation matrix
RU <- function(a) {
  a <- a*pi/180
  matrix(c(cos(a), -sin(a), 0, sin(a), cos(a), 0, 0, 0, 1), nrow = 3)
}
# "Left" vector rotation matrix
RL <- function(a) {
  a <- a*pi/180
  matrix(c(cos(a), 0, sin(a), 0, 1, 0, -sin(a), 0, cos(a)), nrow = 3)
}
# "Heading" vector rotation matrix
RH <- function(a) {
  a <- a*pi/180
  matrix(c(1, 0, 0, 0, cos(a), sin(a), 0, -sin(a), cos(a)), nrow = 3)
}

# Function to draw orientation vectors
draw_T0 <- function(T0, add = F, tropism = NULL, lwd = 1) {
  plot3d(c(0,0,0), 
         xlab = "x", ylab = "y", zlab = "z",
         xlim = c(-1,1), ylim = c(-1,1), zlim = c(-1,1), add = add)
  col = c("red", "green", "blue")
  for (i in 1:3) {
    lines3d(c(c(0,0,0),T0[,i]), col = col[i], lwd = lwd)
  }
  if (!is.null(tropism)) {
    lines3d(c(c(0,0,0), tropism))
  }
}


# Rotate heading toward tropism vector
RT <- function(T0, tropism = c(0,-1,0), e){
  # alpha = rotation amount, e*|HxT|
  a <- e*norm_vec(cross(T0[,1], tropism)) 
  # Should r be around original axis or cross product HxT?
  # r <- tropism
  r <- cross(T0[,1], tropism) #rotation axis
  r <- if(a != 0) { #normalize
    r/norm_vec(r)
  } else {
    r
  }
  
  # rotation matrix - arbitrary axis
  rot_mat <- matrix(c(r[1]^2*(1-cos(a))+cos(a), 
                      r[1]*r[2]*(1-cos(a))+r[3]*sin(a), 
                      r[1]*r[3]*(1-cos(a))-r[2]*sin(a), 
                      r[1]*r[2]*(1-cos(a))-r[3]*sin(a),
                      r[2]^2*(1-cos(a))+cos(a), 
                      r[2]*r[3]*(1-cos(a))+r[1]*sin(a), 
                      r[1]*r[3]*(1-cos(a))+r[2]*sin(a), 
                      r[2]*r[3]*(1-cos(a))-r[1]*sin(a), 
                      r[3]^2*(1-cos(a))+cos(a)),
                    nrow = 3)
  # does order matter? it sure does. This is how it's defined in the source: 
  # https://www.3dgep.com/3d-math-primer-for-game-programmers-matrices/#Rotation_about_an_arbitrary_axis
  rot_mat %*% T0
}
