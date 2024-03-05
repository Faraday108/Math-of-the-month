library(dplyr)

## This testing version of the julia set includes a section that works with convergent points
# according to the method described at https://www.sekinoworld.com/fractal/coloring.htm
# This method is very slow to calculate as it requires 80,000 iterations on the set of
# convergent points. 

julia_faster <- function(xmin = -2, xmax = 2, ymin = -2, ymax = 2, 
                         c = -.77146-.10119i,
                         max_iterations = 1000, size = 500, showplt = TRUE) {
  r <- (ymin - ymax) / (xmin - xmax)
  if (r > 1) {
    x <- seq(xmin, xmax, length.out = size/r)
    y <- seq(ymin, ymax, length.out = size)
  } else {
    x <- seq(xmin, xmax, length.out = size)
    y <- seq(ymin, ymax, length.out = size*r)
  }
  cgrid <- outer(x, y * 1i, FUN = "+")
  cgrid2 <- cgrid
  k <- matrix(0, nrow = length(x), ncol = length(y))
  pb <- txtProgressBar(min = 0, max = max_iterations*2)
  
  subindex <- 1:length(cgrid)
  for (j in 1:max_iterations) {
    index <- which(abs(cgrid[subindex]) < 1000)
    cgrid[subindex[index]] <- cgrid[subindex[index]]^2 + c
    k[subindex[index]] <- k[subindex[index]]+1
    subindex <- subindex[index]
    
    setTxtProgressBar(pb, j)
  }
  
  # For each point where m = M, initialize z_2
  z0 <- cgrid2
  z2 <- cgrid2
  con_index <- which(k == max_iterations)
  z2[con_index] <- (cgrid2[con_index]^2 + c)^2 + c
  k_con <- matrix(0, nrow = length(x), ncol = length(y))
  k_con[which(k < 1000)] <- max_iterations
  eps <- .0001

  for (j in 1:max_iterations) {
    # This computes |z_{m+2} - z_m| to find m when || stops being greater than eps
    index <- which(abs((z0^2+c)^2+c-z0) > eps)
    #index <- which(abs(z2 - z0) > eps)
    z0[index] <- z0[index]^2 + c
    #z2[index] <- z2[index]^2 + c
    k_con[index] <- k_con[index] + 1
    setTxtProgressBar(pb, max_iterations+j)
  }
  
  k[k == max_iterations] <- k_con[k == max_iterations]
  
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
  
  nu <- ifelse(abs(cgrid) < 1000, 0, log(log(abs(cgrid))/log(1000))/log(2))
  
  # Interpolates colors
  k3 <- k + (k2 - k) * nu
  
  # Basic convergent point coloring by abs of value after max iterations
  k3[which(k0 == 1000)] <- abs(cgrid[which(k0 == 1000)])/max(abs(cgrid[which(k0 == 1000)]))
  
  # Subset divergent points, set convergent to NA
  div <- k3
  div[k0 == max_iterations] <- NA
  # Subset convergent points, set divergent to NA
  con <- k3
  con[k0 < 1000] <- NA
  
  if (showplt == TRUE) {
    image(x, y, div, 
          col = colorRampPalette(c(brewer.pal(9, "BuGn"), "black"))(1000), 
          asp = 1, axes = FALSE)
    image(x, y, con, 
          col = colorRampPalette(c(brewer.pal(9, "PuRd"), "black"))(1000), 
          add = TRUE,asp = 1)
  } else {
    list(x = x, y = y, cgrid = cgrid, cgrid2 = cgrid2, k0 = k0, k3 = k3, k_con = k_con, z0 = z0, z2 = z2)
  }
  #image(x, y, k0 %% 2, col = grey.colors(2, 0, 1), asp = 1)
}
