library(RColorBrewer)
library(ggplot2)

# https://loiseaujc.github.io/Scientific_Computing_on_a_Laptop/Maths/Mandelbrot/buddhabrot.html
# https://loiseaujc.github.io/Scientific_Computing_on_a_Laptop/Maths/Mandelbrot/binary_mandelbrot.html

trajectory <- function(c, maxiter) {
  z <- data.frame(comp = rep(0, maxiter))
  z$comp[1] <- c
  c0 <- c
  k <- 0
  for (i in 1:(maxiter-1)) {
    k <- i
    c <- c^2 + c0
    z$comp[i+1] <- c
    if (abs(c) > 4)
      break
  }
  # if ((k != (maxiter - 1)) & (k > .7 * maxiter)) {
  if (k != (maxiter - 1)) {
    z[1:(k+1),]
  }
}

# Sampling grid of complex plane same way as done in Mandelbrot set generation
# Problem noticed that it creates a regular grid of points. 
x <- seq(-2, 1, length.out = 100)
y <- seq(-1.5, 1.5, length.out = 100)
cgrid <- expand.grid(x, y*1i)
v1 <- cgrid$Var1+cgrid$Var2

zs <- unlist(lapply(X = v1, FUN = trajectory, maxiter = 10), use.names = FALSE)

data2 <- data.frame(x = Re(zs), y = Im(zs))

ggplot(data = data2, aes(x=y, y=x)) + 
  stat_bin_2d(
    bins = c(length(x), length(y)), 
              geom = "raster", 
              interpolate = TRUE, 
              drop = FALSE) + 
  lims(y = c(1, -2), x = c(-1.5, 1.5)) + 
  #theme_void() + 
  theme(legend.position = "none") +
  coord_equal() + 
  scale_fill_gradientn(colors = c("black","red"),
                       na.value = "black")

############## Alternative method to points for set. 
# Finds points known to be in the main cardioid of mandelbrot set
in_main_cardioid <- function(c) {
  q <- (Re(c) - 1/4)^2 + Im(c)^2
  q*(q + (Re(c) - 1/4)) <= Im(c)^2 /4
}

# Find points in period 2 bulb of mandelbrot set
in_period2bulb <- function(c) {
  (Re(c) + 1)^2 + Im(c)^2 <= 1/16
}

# Sample complex numbers randomly from circle in complex plane radius 2
uniform_sampling <- function() {
  c <- 0
  while (in_main_cardioid(c) | in_period2bulb(c)) {
    # Random radius 0 to 2, random angle -pi to pi times i
    c <- runif(1,0,2) * exp(runif(1, -pi, pi)*1i)
  }
  c
}

uniform_sampling_v <- replicate(100000, uniform_sampling())
zs <- unlist(lapply(X = uniform_sampling_v, 
                    FUN = trajectory, 
                    maxiter = 10), use.names = FALSE)

data2 <- data.frame(x = Re(zs), y = Im(zs))

ggplot(data = data2, aes(x=y, y=x)) + 
  stat_bin_2d(
    bins = c(100, 100), 
    geom = "raster", 
    interpolate = TRUE, 
    drop = FALSE) + 
  lims(y = c(1, -2), x = c(-1.5, 1.5)) + 
  #theme_void() + 
  theme(legend.position = "none") +
  coord_equal() + 
  scale_fill_gradientn(colors = c("black","red"),
                       na.value = "black")

##### COLOR CHANNELS
uniform_sampling_v <- replicate(1000000, uniform_sampling())
zs <- unlist(lapply(X = uniform_sampling_v, 
                    FUN = trajectory, 
                    maxiter = 10), use.names = FALSE)

zs1 <- unlist(lapply(X = uniform_sampling_v, 
                    FUN = trajectory, 
                    maxiter = 10), use.names = FALSE)
zs1data <- data.frame(x = Re(zs1), y = Im(zs1))

zs2 <- unlist(lapply(X = uniform_sampling_v, 
                    FUN = trajectory, 
                    maxiter = 100), use.names = FALSE)
zs2data <- data.frame(x = Re(zs2), y = Im(zs2))

zs3 <- unlist(lapply(X = uniform_sampling_v, 
                    FUN = trajectory, 
                    maxiter = 1000), use.names = FALSE)
zs3data <- data.frame(x = Re(zs3), y = Im(zs3))

ch1 <- bin2(data.matrix(zs1data), 
            matrix( c(-2,-2,1,2), 2, 2), 
            nbin = c(300,300))$nc

ch2 <- bin2(data.matrix(zs2data), 
            matrix( c(-2,-2,1,2), 2, 2), 
            nbin = c(300,300))$nc

ch3 <- bin2(data.matrix(zs3data), 
            matrix( c(-2,-2,1,2), 2, 2), 
            nbin = c(300,300))$nc

col1 <- rgb(ch1/max(ch1), ch2/max(ch2), ch3/max(ch3))

ggplot(data = expand.grid(x = seq(-2, 1, length.out = 300), 
                          y = seq(-2, 2, length.out = 300)), aes(y, -x)) + 
  geom_raster(fill = col1, interpolate = TRUE) + 
  coord_fixed()
