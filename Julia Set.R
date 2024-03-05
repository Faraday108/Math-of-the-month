library(dplyr)
library(RColorBrewer)

julia_original <- function(xmin = -2, xmax = 2, ymin = -2, ymax = 2, 
                           c = -.77146-.10119i,
                           max_iterations = 1000, size = 500) {
  x <- seq(xmin, xmax, length.out = size)
  y <- seq(ymin, ymax, length.out = size)
  cgrid <- outer(x, y * 1i, FUN = "+")
  k <- matrix(0, nrow = size, ncol = size)
  pb <- txtProgressBar(min = 0, max = max_iterations)
  
  for (j in 1:max_iterations) {
    index <- which(abs(cgrid) < 2)
    cgrid[index] <- cgrid[index]^2 + c
    k[index] <- k[index]+1
    setTxtProgressBar(pb, j)
  }
  
  k0 <- k
  k2 <- k
  t1 <- cumsum(table(k)/size^2)
  
  for (i in 1:length(t1)) {
    k[k == as.numeric(names(t1)[i])] <- t1[i]
  }
  
  nu <- ifelse(abs(cgrid) < 2, 0, log(log(abs(cgrid))/log(2))/log(2))
  
  for (i in 1:length(t1)) {
    k2[k2 == as.numeric(names(t1)[i])] <- lag(t1, default = 0)[i]
  }
  
  k3 <- k + (k2 - k) * nu
  
  image(x, y, 1-k3, col = hcl.colors(100, "Blue-Red 3", rev = TRUE),asp = 1)
}


julia_faster <- function(xmin = -2, xmax = 2, ymin = -2, ymax = 2, 
                         c = -.77146-.10119i,
                         max_iterations = 1000, size = 500, showplt = TRUE, degree = 2) {
  r <- (ymin - ymax) / (xmin - xmax)
  if (r > 1) {
    x <- seq(xmin, xmax, length.out = size/r)
    y <- seq(ymin, ymax, length.out = size)
  } else {
    x <- seq(xmin, xmax, length.out = size)
    y <- seq(ymin, ymax, length.out = size*r)
  }
  cgrid <- outer(x, y * 1i, FUN = "+")
  k <- matrix(0, nrow = length(x), ncol = length(y))
  pb <- txtProgressBar(min = 0, max = max_iterations)
  
  subindex <- 1:length(cgrid)
  for (j in 1:max_iterations) {
    index <- which(abs(cgrid[subindex]) < 1000)
    cgrid[subindex[index]] <- cgrid[subindex[index]]^degree + c
    k[subindex[index]] <- k[subindex[index]]+1
    subindex <- subindex[index]
    
    setTxtProgressBar(pb, j)
  }
  k0 <- k
  k2 <- k
  
  # Histogram binning for colors
  t1 <- cumsum(table(k)/size^2)
  
  for (i in 1:length(t1)) {
    k[k == as.numeric(names(t1)[i])] <- t1[i]
  }
  
  for (i in 1:length(t1)) {
    k2[k2 == as.numeric(names(t1)[i])] <- lag(t1, default = 0)[i]
  }
  
  nu <- ifelse(abs(cgrid) < 1000, 0, log(log(abs(cgrid))/log(max_iterations))/log(2))
  
  k3 <- k + (k2 - k) * nu
  
  #k3[which(k0 == 1000)] <- abs(cgrid[which(k0 == 1000)])/max(abs(cgrid[which(k0 == 1000)]))
  k3[which(k0 == max_iterations)] <- abs(cgrid[which(k0 == max_iterations)])
  
  # Subset divergent points, set convergent to NA
  div <- k3
  div[k0 == max_iterations] <- NA
  div <- div/max(div, na.rm = TRUE)
  # Subset convergent points, set divergent to NA
  con <- k3
  con[k0 < 1000] <- NA
  con <- (con - min(con, na.rm = TRUE))/(max(con, na.rm = TRUE) - min(con, na.rm = TRUE))
  
  
  if (showplt == TRUE) {
    par(mar = c(0,0,0,0))
    image(x, y, div, 
          col = colorRampPalette(c(brewer.pal(9, "YlOrRd"), "black"))(1000), 
          asp = 1, axes = FALSE, ann = FALSE)
    image(x, y, con, 
          col = colorRampPalette(c(brewer.pal(9, "Blues"), "black"))(1000), 
          add = TRUE, asp = 1, ann = FALSE)
  } else {
    list(x = x, y = y, cgrid = cgrid, con = con, div = div, k0 = k0, k = k, k2 = k2, nu = nu, k3 = k3)
  }
  #image(x, y, k0 %% 2, col = grey.colors(2, 0, 1), asp = 1)
}

# TODO Edit this to only compute the convergent and divergent sets. Consider applying
# the convergence scheme from "Julia set testing". 
julia_faster_data<- function(xmin = -2, xmax = 2, ymin = -2, ymax = 2, 
                             c = -.77146-.10119i,
                             max_iterations = 1000, size = 500, showplt = TRUE, degree = 2) {
  r <- (ymin - ymax) / (xmin - xmax)
  if (r > 1) {
    x <- seq(xmin, xmax, length.out = size/r)
    y <- seq(ymin, ymax, length.out = size)
  } else {
    x <- seq(xmin, xmax, length.out = size)
    y <- seq(ymin, ymax, length.out = size*r)
  }
  cgrid <- outer(x, y * 1i, FUN = "+")
  k <- matrix(0, nrow = length(x), ncol = length(y))
  pb <- txtProgressBar(min = 0, max = max_iterations)
  
  subindex <- 1:length(cgrid)
  for (j in 1:max_iterations) {
    index <- which(abs(cgrid[subindex]) < 1000)
    cgrid[subindex[index]] <- cgrid[subindex[index]]^degree + c
    k[subindex[index]] <- k[subindex[index]]+1
    subindex <- subindex[index]
    
    setTxtProgressBar(pb, j)
  }
  k0 <- k
  k2 <- k
  
  # Histogram binning for colors
  t1 <- cumsum(table(k)/size^2)
  
  for (i in 1:length(t1)) {
    k[k == as.numeric(names(t1)[i])] <- t1[i]
  }
  
  for (i in 1:length(t1)) {
    k2[k2 == as.numeric(names(t1)[i])] <- lag(t1, default = 0)[i]
  }
  
  nu <- ifelse(abs(cgrid) < 1000, 0, log(log(abs(cgrid))/log(max_iterations))/log(2))
  
  k3 <- k + (k2 - k) * nu
  
  #k3[which(k0 == 1000)] <- abs(cgrid[which(k0 == 1000)])/max(abs(cgrid[which(k0 == 1000)]))
  k3[which(k0 == max_iterations)] <- abs(cgrid[which(k0 == max_iterations)])
  
  # Subset divergent points, set convergent to NA
  div <- k3
  div[k0 == max_iterations] <- NA
  div <- div/max(div, na.rm = TRUE)
  # Subset convergent points, set divergent to NA
  con <- k3
  con[k0 < 1000] <- NA
  con <- (con - min(con, na.rm = TRUE))/(max(con, na.rm = TRUE) - min(con, na.rm = TRUE))
  
  
  if (showplt == TRUE) {
    image(x, y, div, 
          col = colorRampPalette(c(brewer.pal(9, "YlOrRd"), "black"))(1000), 
          asp = 1, axes = FALSE, ann = FALSE)
    image(x, y, con, 
          col = colorRampPalette(c(brewer.pal(9, "Blues"), "black"))(1000), 
          add = TRUE, asp = 1, ann = FALSE)
  } else {
    list(x = x, y = y, cgrid = cgrid, con = con, div = div, k0 = k0, k = k, k2 = k2, nu = nu, k3 = k3)
  }
  #image(x, y, k0 %% 2, col = grey.colors(2, 0, 1), asp = 1)
}
