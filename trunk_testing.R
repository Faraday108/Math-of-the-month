source("vector_operations.R")
coord <- data.frame(x = c(0,1,.5), y = c(0,0,.5))
plot(coord)
extrude3d(coord, thickness = 1, material = list(col = "chocolate")) %>% 
  shade3d(specular = "black")

trunk <- function(r, pts, col) {
  UP <- c(0,0,1)
  p1 <- as.vector(pts[1,], mode = "numeric")
  p2 <- as.vector(pts[2,], mode = "numeric")
  v1 <- p2 - p1
  v2 <- cross(UP,v1)
  nv1 <- norm_vec(v1)
  nv2 <- norm_vec(v2)
  if(nv1*nv2 > 0){
    ang <- acos((UP %*% v1)/(norm_vec(UP)*norm_vec(v1)))
  } else {
    ang <- 0
  }
  
  n <- c(1:round(6*r/1.73))
  x <- r*cos(2*pi*n/max(n))
  y <- r*sin(2*pi*n/max(n))
  #open3d(silent = TRUE)
  extrude3d(x,y,thickness = nv1) %>%
    rotate3d(-ang[1],v2[1],v2[2],v2[3]) %>% 
    translate3d(p1[1],p1[2],p1[3]) %>%
    shade3d(specular = "black", col = col)
}

# I'd like when a branch goes off at an angle for the base to 
# be translated along the face of the prior segment so all the
# branches don't originate from the center
trunk2 <- function(r, pts, col) {
  UP <- c(0,0,1)
  p1 <- as.vector(pts[1,], mode = "numeric")
  p2 <- as.vector(pts[2,], mode = "numeric")
  v1 <- p2 - p1
  v2 <- cross(UP,v1)
  nv1 <- norm_vec(v1)
  nv2 <- norm_vec(v2)
  if(nv1*nv2 > 0){
    ang <- .9*acos((UP %*% v1)/(norm_vec(UP)*norm_vec(v1)))
  } else {
    ang <- 0
  }
  
  n <- c(1:round(6*r/1.73))
  x <- r*cos(2*pi*n/max(n))
  y <- r*sin(2*pi*n/max(n))
  #open3d(silent = TRUE)
  # extrude3d(x,y,thickness = nv1) %>%
  #   rotate3d(-ang[1],v2[1],v2[2],v2[3]) %>% 
  #   translate3d(p1[1],p1[2],p1[3]) %>%
  #   shade3d(specular = "black", col = col)
  
  ## This rotates around the 'top' of the branch
  # Problem happens when two branches meet at a non-tertiary point
  # As these should be straight. Would need to operate on more context
  extrude3d(x,y,thickness = -nv1) %>% 
    rotate3d(-ang[1],v2[1],v2[2],v2[3]) %>% 
    translate3d(p2[1],p2[2],p2[3]) %>%
    shade3d(specular = "black", col = col)
}


polygon <- matrix(c(0, 0, 0, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)
extruded_obj <- extrude3d(polygon, thickness=3)

# Open a new rgl window
open3d()

extruded_obj_rotated <- rotate(extruded_obj, angle = 45, 0, 0, 1)
# Plot the extruded object
shade3d(extruded_obj)

# Rotate the object around the z-axis by 45 degrees
rotate3d("z", 45)
