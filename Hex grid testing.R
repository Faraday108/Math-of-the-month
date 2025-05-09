library(tidyverse)
library(rgl)

####### CREATE BASE UNIT HEXAGON POINTS ##############
# points per side
pps <- 20
# Move initial point along diagonal to center
imove <- 10
# Initial x and y
x0 <- 0 + imove * 1/2
y0 <- 0 + imove * sqrt(3)/2
z0 <- -1

# prefill points of one hexagon
hex1 <- data.frame(x = rep(x0, 6 * (pps - 1)), 
                   y = rep(y0, 6 * (pps - 1)))
# Angle directions to walk around hexagon
direction <- c(rep(0, pps - 1), 
               rep(pi/3, pps - 1), 
               rep(2*pi/3, pps - 1), 
               rep(pi, pps - 1), 
               rep(4*pi/3, pps - 1), 
               rep(5*pi/3, pps - 2))
# Wrap points around hexagon
for (i in 1:(6 * (pps - 1) - 1)) {
  hex1$x[i+1] <- hex1$x[i] + cos(direction[i])
  hex1$y[i+1] <- hex1$y[i] + sin(direction[i])
}
# Convert to unit hex side length 1
unit_hex <- hex1/(pps - 1 + imove)

# Make hex grid with axial coordinates
make_grid <- function(n) {
  c <- -n:n
  df <- data.frame(q=integer(), r=integer())
  for (i in 1:length(c)) {
    if (c[i] <= 0) {
      df <- rbind(df, data.frame(q = c[i], 
                                 r = -c[1:(2*n+1+c[i])]))
    }else{
      df <- rbind(df, data.frame(q = c[i], 
                                 r = -c[(1+c[i]):(2*n+1)]))
    }
  }
  df
}

# Centers grid where three hexagons meet
center_three <- function(cart_grid) {
  hex_size <- attributes(cart_grid)$hex_size
  centered <- cart_grid %>%
    # remove leftmost column
    filter(x != min(x)) %>%
    group_by(x) %>%
    # remove top of remaining columns
    filter(y != max(y)) %>%
    mutate(x = x - .5*hex_size, 
           y = y + (sqrt(3)/2)*hex_size) %>%
    ungroup() %>%
    `attr<-`("hex_size", attr(cart_grid,"hex_size") )
}

# Convert axial coordinates to cartesian
qr_xy_convert <- function(qr, size = 1) {
  n <- nrow(qr)
  pts <- data.frame(x = integer(), y = integer())
  for (i in 1:n) {
    qi <- qr$q[i]
    ri <- qr$r[i]
    pts[i,1] <- size * (3/2*qi)
    pts[i,2] <- size * (sqrt(3)/2 * qi + sqrt(3)*ri)
  }
  attr(pts, "hex_size") <- size
  pts
}

# Function that makes a hexagon centered at a coordinate. Will be mapply'ed
# to grid
make_hex <- function(x, y, hex_size = 1, scale=1) {
  # move from center to lower left corner
  x0 <- x - .5*scale*hex_size
  y0 <- y - sqrt(3)/2*scale*hex_size
  d <- sqrt(x0^2 + y0^2 + 4) # Distance from (0,0,1) to (x0,y0,-1)
  d_min <- sqrt(.75^2 + 0^2 + 4) # Minimum distance
  pps_0 <- 50 # initial points per side
  k <- pps_0 * d_min # proportion constant for pps
  pps <- ceiling(k / d) # pps for hex
  # Direction to walk around starting at lower left corner
  direction <- c(rep(0, pps - 1), rep(pi/3, pps - 1), rep(2*pi/3, pps - 1), rep(pi, pps - 1), rep(4*pi/3, pps - 1), rep(5*pi/3, pps - 2))
  hex1 <- data.frame(x = rep(x0, 6 * (pps - 1)), 
                     y = rep(y0, 6 * (pps - 1)))
  for (i in 1:(6 * (pps - 1) - 1)) {
    hex1$x[i+1] <- hex1$x[i] + cos(direction[i])/((pps-1)/(scale*hex_size))
    hex1$y[i+1] <- hex1$y[i] + sin(direction[i])/((pps-1)/(scale*hex_size))
  }
  hex1
}

# Function to help apply the make_hex function to each point of the grid
hex_grid_populate <- function(hex_grid, scale = 1) {
  mapply(make_hex, 
         x = hex_grid$x, 
         y = hex_grid$y, 
         MoreArgs = list(scale = scale, 
                         hex_size = attributes(hex_grid)$hex_size)) %>% #makes nested matrix
    t() %>% #transpose so x and y are on columns instead of rows
    data.frame() %>% #convert to df
    unnest(c(x,y)) %>% #unnest
    cbind(z = -1) # add z coordinate of flat plane
}

# Function to help apply the make_hex function to each point of the grid
# If scale is less than 1, will add extra points to fill in blank space between hexagons
hex_grid_populate2 <- function(hex_grid, scale = 1) {
  output <- data.frame(x = numeric(), y = numeric())
  if(scale < 1) {
    ds <- (1-scale)/.025 #delta scale
    for (i in 0:ds) {
      newhex <- mapply(make_hex,
                       x = hex_grid$x,
                       y = hex_grid$y,
                       MoreArgs = list(scale = scale+i*.025,
                                       hex_size = attributes(hex_grid)$hex_size)) %>% #makes nested matrix
        t() %>% #transpose so x and y are on columns instead of rows
        data.frame() %>% #convert to df
        unnest(c(x,y)) %>% #unnest
        cbind(z = -1) # add z coordinate of flat plane
      output <- rbind(output, newhex)
    }}
  output
}

# Make function to project the input grid onto sphere
hex_grid_projection <- function(hex_grid, include_grid = TRUE) {
  n <- nrow(hex_grid) # find number of points
  # Create empty dataframe of same size so R doesn't have to reassign memory
  pts <- data.frame(x = rep(NA, n), 
                    y = rep(NA, n), 
                    z = rep(NA, n))
  # loop over each point in grid, projecting onto sphere
  for (i in 1:n) {
    # following algorithm projects grid through center of sphere
    # dividing coords of plane on bottom of unit sphere by 2 moves them to middle
    xi <- hex_grid[i,1]/2
    yi <- hex_grid[i,2]/2
    R <- sqrt(xi^2 + yi^2)
    pts[i,1] <- 2*xi / (R^2 + 1)
    pts[i,2] <- 2*yi / (R^2 + 1)
    pts[i,3] <- (R^2 - 1) / (R^2 + 1)
  }
  # Add grid to list of points
  if(include_grid) {
    pts <- rbind(pts, hex_grid)
  } else {
    pts
  }
} 



full_hex_generator <- function(n = 1, size = 1, scale = 1, include_grid = TRUE) {
  n %>%
    make_grid() %>%
    qr_xy_convert(size = size) %>%
    center_three() %>%
    hex_grid_populate(scale = scale) %>%
    hex_grid_projection(include_grid) 
}

full_hex_generator2 <- function(n = 1, size = 1, scale = 1, include_grid = TRUE) {
  n %>%
    make_grid() %>%
    qr_xy_convert(size = size) %>%
    center_three() %>%
    hex_grid_populate2(scale = scale) %>%
    hex_grid_projection(include_grid) 
}

# The following creates a grid with n generations of hexagons from center. 
# open3d(silent = TRUE);plot3d(full_hex_generator(n = 4, scale = .4, include_grid = FALSE))
# plot3d(full_hex_generator2(n = 2, scale = .5, include_grid = FALSE))

pts <- full_hex_generator2(n = 4, scale = .8, size = .8, include_grid = FALSE)

# The current generation has too many points where hexagons meet, to reduce these
# the following function will remove points that are too close. 
remove_near_duplicates <- function(points, threshold = 1e-3) {
  points <- as.matrix(points)  # ensure numeric matrix
  keep <- rep(TRUE, nrow(points))
  for (i in 1:(nrow(points)-1)) {
    if (!keep[i]) next
    remaining <- points[(i+1):nrow(points), , drop = FALSE]
    current <- matrix(points[i, ], nrow = nrow(remaining), ncol = 3, byrow = TRUE)
    dists <- sqrt(rowSums((remaining - current)^2))
    keep[(i+1):nrow(points)] <- keep[(i+1):nrow(points)] & dists > threshold
  }
  points[keep, ]
}


pts2 <- dist(pts)

# write.table(100*pts, "hexprojection.xyz",
#             col.names = FALSE, 
#             row.names = FALSE)
