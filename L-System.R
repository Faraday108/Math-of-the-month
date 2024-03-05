library(stringi)
library(ggplot2)
library(gsubfn)
library(gganimate)
curve <- list(list(name = "Koch Island",
                   axiom = "F-F-F-F",
                   rules = list("F" = "F-F+F+FF-F-F+F"),
                   angle = 90,
                   n = 2,
                   alpha0 = 90),
              list(name = "Sierpinski Triangle", 
                   axiom = "R", 
                   rules = list("L" = "R+L+R", "R" = "L-R-L"), 
                   angle = 60, 
                   n = 6, 
                   alpha0 = 0), 
              list(name = "fractal plant1", 
                   axiom = "X", 
                   rules = list("X" = "F+[[X]-X]-F[-FX]+X", 
                                "F" = "FF"), 
                   angle = 25, 
                   n = 4, 
                   alpha0 = 65), 
              list(name = "plant_a", 
                   axiom = "F", 
                   rules = list("F" = "F[+F]F[-F]F"), 
                   angle = 25.7, 
                   n = 5, 
                   alpha0 = 90), 
              list(name = "plant_b", 
                   axiom = "F", 
                   rules = list("F" = "F[+F]F[-F][F]"), 
                   angle = 20, 
                   n = 5, 
                   alpha0 = 90),
              list(name = "plant_c", 
                   axiom = "F", 
                   rules = list("F" = "FF-[-F+F+F]+[+F-F-F]"), 
                   angle = 22.5, 
                   n = 4, 
                   alpha0 = 90), 
              list(name = "plant_d", 
                   axiom = "X", 
                   rules = list("X" = "F[+X]F[-X]+X", 
                                "F" = "FF"), 
                   angle = 20, 
                   n = 7, 
                   alpha0 = 90), 
              list(name = "dragon1", 
                   axiom = "L", 
                   rules = list("L" = "L+R++R-L--LL-R+", 
                                "R" = "-L+RR++R+L--L-R"), 
                   angle = 60, 
                   n = 4, 
                   alpha0 = 0), 
              list(name = "binary_tree", 
                   axiom = "X", 
                   rules = list("X" = "F[+X][-X]"), 
                   angle = 30, 
                   n = 6, 
                   r = .9,
                   alpha0 = 90)
)

draw_l_curve <- function(curve, c_bground = "white", c_fractal = "black", 
                         animate = FALSE, random = FALSE) {
  rules <- curve$rules; alpha0 <- curve$alpha0; iterations <- curve$n
  axiom <- curve$axiom
  
  for (i in 1:iterations) axiom <- gsubfn(".", replacement = rules, axiom)
  
  actions <- stri_extract_all(axiom, regex = ".")[[1]]
  actions <- actions[actions != "X"]
  points <- data.frame(x = 0, y = 0, alpha = alpha0, time = 0)
  pop_points <- data.frame(x = numeric(), y = numeric(), alpha = numeric(), pop = numeric())
  pop_lvl <- 0
  
  for (i in 1:length(actions)) {
    if (actions[i] == "F" | actions[i] == "L" | actions[i] == "R") {
      r <- ifelse(random, runif(1, .5, 1.1), 1)
      x <- points[nrow(points), "x"] + r*cos(points[nrow(points), "alpha"]*(pi/180))
      y <- points[nrow(points), "y"] + r*sin(points[nrow(points), "alpha"]*(pi/180))
      alpha <- points[nrow(points), "alpha"]
      points <- rbind(points, data.frame(x=x, y = y, alpha = alpha, time = nrow(points)))
    } else if (actions[i] == "-" | actions[i] == "+") {
      dt <- ifelse(random, runif(1, -5, 5), 0)
      alpha <- dt + points[nrow(points), "alpha"]
      points[nrow(points), "alpha"] <- eval(parse(text = paste0(alpha, actions[i], curve$angle)))
    } else if (actions[i] == "[") {
      pop_lvl <- pop_lvl + 1
      pop_points[pop_lvl,] <- c(points[nrow(points),1:3], pop = pop_lvl)
    } else {
      tme <- points[nrow(points), "time"]
      points <- rbind(points, 
                      c(NA, NA, NA, tme + 1), 
                      c(pop_points[pop_lvl, 1:3], time = tme + 2))
      pop_lvl <- pop_lvl - 1
    }
  }
  
  if (!animate == TRUE) {
  ggplot(data = points, aes(x,y)) + 
    geom_path(color = c_fractal) + 
    coord_fixed() + 
    theme(legend.position = "none", 
          panel.background = element_rect(fill = c_bground), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank())
  } else {
  ggplot(data = points, aes(x,y)) + 
    geom_path(color = c_fractal) + 
    coord_fixed() + 
    theme(legend.position = "none", 
          panel.background = element_rect(fill = c_bground), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank())
   # transition_manual(frames = time, cumulative = TRUE)
  #animate(anim, nframes = nrow(points), fps = 50)
  }
}

stochastic_curve <- list(list(name = "stochastic_plant_a",
                              axiom = "F", 
                              rules = list("F" = c("F[+F]F[-F]F", 
                                           "F[+F]F", 
                                           "F[-F]F")), 
                              angle = 20, 
                              n = 5, 
                              alpha0 = 90), 
                         list(name = "stochastic_plant_b", 
                              axiom = "F", 
                              rules = list("F" = c("FF-[-F+F+F]+[+F-F-F]",
                                                   "FF+[+F-F-F]",
                                                   "FF-[-F+F+F]")), 
                              probs = c(.9, .05, .05),
                              angle = 22.5, 
                              n = 4, 
                              alpha0 = 90))

draw_stochastic_l_curve <- function(curve, c_bground = "white", c_fractal = "black", animate = FALSE) {
  rules <- curve$rules; alpha0 <- curve$alpha0; iterations <- curve$n
  axiom <- curve$axiom; probs = curve$probs
  
  for (i in 1:iterations) {
    #rules <- list("F" = sample(unlist(rules$F), 1))
    axiom <- gsubfn(".", 
                    replacement = list("F" = sample(unlist(rules$F), 1, 
                                                    prob = probs)),
                    axiom)
  }
  
  actions <- stri_extract_all(axiom, regex = ".")[[1]]
  actions <- actions[actions != "X"]
  points <- data.frame(x = 0, y = 0, alpha = alpha0, time = 0)
  pop_points <- data.frame(x = numeric(), y = numeric(), alpha = numeric(), pop = numeric())
  pop_lvl <- 0
  for (i in 1:length(actions)) {
    if (actions[i] == "F" | actions[i] == "L" | actions[i] == "R") {
      x <- points[nrow(points), "x"] + cos(points[nrow(points), "alpha"]*(pi/180))
      y <- points[nrow(points), "y"] + sin(points[nrow(points), "alpha"]*(pi/180))
      alpha <- points[nrow(points), "alpha"]
      points <- rbind(points, data.frame(x=x, y = y, alpha = alpha, time = nrow(points)))
    } else if (actions[i] == "-" | actions[i] == "+") {
      alpha <- points[nrow(points), "alpha"] + runif(1, 0, 15)
      points[nrow(points), "alpha"] <- eval(parse(text = paste0(alpha, actions[i], curve$angle)))
    } else if (actions[i] == "[") {
      pop_lvl <- pop_lvl + 1
      pop_points[pop_lvl,] <- c(points[nrow(points),1:3], pop = pop_lvl)
    } else {
      tme <- points[nrow(points), "time"]
      points <- rbind(points, 
                      c(NA, NA, NA, tme + 1), 
                      c(pop_points[pop_lvl, 1:3], time = tme + 2))
      pop_lvl <- pop_lvl - 1
    }
  }
  
  if (!animate == TRUE) {
    ggplot(data = points, aes(x,y)) + 
      geom_path(color = c_fractal) + 
      coord_fixed() + 
      theme(legend.position = "none", 
            panel.background = element_rect(fill = c_bground), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(), 
            axis.title = element_blank(), 
            axis.text = element_blank())
  } else {
    ggplot(data = points, aes(x,y)) + 
      geom_path(color = c_fractal) + 
      coord_fixed() + 
      theme(legend.position = "none", 
            panel.background = element_rect(fill = c_bground), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(), 
            axis.title = element_blank(), 
            axis.text = element_blank())
    # transition_manual(frames = time, cumulative = TRUE)
    #animate(anim, nframes = nrow(points), fps = 50)
  }
}

draw_l_curve(curve[[6]], c_fractal = "darkgreen", random = TRUE)

# A little buggy, I haven't worked to implement it for anything beyond curve 1. 
draw_stochastic_l_curve(stochastic_curve[[1]])

