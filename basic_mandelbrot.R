xmin <- -2; xmax <- .5; ymin <- -1.2; ymax <- 1.2; max_iterations <- 50
spacing <- .01; xpixel <- (xmax-xmin)/spacing + 1; ypixel <- (ymax-ymin)/spacing + 1
x <- seq(xmin, xmax, length.out = xpixel)
y <- seq(ymin, ymax, length.out = ypixel)
# Produces matrix of complex numbers, note that 'x' changes across the columns and 'y' changes down the rows. 
# C is the value added in each iteration of Mandelbrot set
c <- outer(x, y * 1i, FUN = "+")
# Initialize empty matrix to hold calculated z's
z <- matrix(0, nrow = xpixel, ncol = ypixel)
# Initialize empty matrix to hold iteration counts
k <- matrix(0, nrow = xpixel, ncol = ypixel)

# Calculate the complex numbers up till escape and escape iteration. 

for (j in 1:max_iterations) {
  # Note escape value of 1000. If this is changed, make sure to alter in calculation of nu below. 
  index <- which(abs(z) < 1000)
  z[index] <- z[index]^2 + c[index]
  k[index] <- k[index] + 1
}

image(x,y,k, asp=1, col = hcl.colors(12, "Inferno", rev = TRUE))
