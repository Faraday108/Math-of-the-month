source("Julia Set.R")
library(magick)
dir_out <- file.path(tempdir(), "julia_img")
dir.create(dir_out, recursive = TRUE)

cs <- -.77146 + seq(-1, 1, length.out = 30)*1i
for (i in 1:length(cs)){
  png(file.path(dir_out, paste0(sprintf("%04d", i), ".png")), 
      width = 1000, height = 1000)
  vector_julia(c = cs[i], xpixel = 1000, ypixel = 1000)
  dev.off()
}

imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 10)
img_animated
image_write(image = img_animated,
            path = "julia3.gif")
unlink(dir_out, recursive = TRUE)
dir.exists(dir_out)
