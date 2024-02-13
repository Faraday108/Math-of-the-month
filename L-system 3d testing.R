library(rgl); 
source("vector_operations.R")
## Working on 3D rotation 

tropism <- c(0,-1,0)

# Testing rotation viewer
draw_T0(T0%*%RH(30))
draw_T0(RT(T0%*%RH(30), e=e), add = T)

# Define initial state of turtle; H = straight up, L = along x-axis, U = along y-axis
H0 <- c(0,0,1); L0 <- c(1,0,0); U0 <- c(0,1,0)
T0 <- matrix(c(H0, L0, U0), nrow = 3)

# Rotate initial state around U by a
T0 %*% RU(pi/3)

# Define a function to apply production rules
# Old apply, doesn't include parameter
apply_rules <- function(symbol, rules) {
  if (symbol$symbol %in% names(rules)) {
    return(rules[[symbol$symbol]])
  } else {
    return(symbol)
  }
}

apply_rules2 <- function(symbol, rules) {
  if (symbol$symbol %in% names(rules)) {
    transformed_symbol <- rules[[symbol$symbol]]
    # Apply parameter transformation if symbol is F or !
    if(symbol$symbol %in% c("F", "!")) {
      # for (i in seq_along(transformed_symbol)) {
        # Change parameter value of old symbol by product with new parameter
        transformed_symbol[[1]]$param <- transformed_symbol[[1]]$param * symbol$param
      #}
    }
    return(transformed_symbol)
  } else {
    return(list(symbol))
  }
}

# Define function to generate L-system using apply_rules
generate_l_system <- function(axiom, rules, iterations) {
  sequence <- axiom
  for (i in 1:iterations) {
    new_sequence <- vector(mode = "list", length = 0)
    for (symbol in sequence) {
      transformed_symbol <- apply_rules2(symbol, rules)
      new_sequence <- c(new_sequence, transformed_symbol)
    }
    sequence <- new_sequence
  }
  return(sequence)
}

# Testing axiom and parameter
axiom <- list(
  list(symbol = "A", param = 1),
  list(symbol = "B", param = 2)
)

rules <- list(
  A = list(
    list(symbol = "A", param = .5),
    list(symbol = "B", param = .25)
  ),
  B = list(
    list(symbol = "A", param = .75),
    list(symbol = "B", param = .125)
  )
)

# Tree axiom for figure 2.8 from ABOP
axiom2 <- list(
  list(symbol = "!", param = 1), 
  list(symbol = "F", param = 200), 
  list(symbol = "/", param = 45), 
  list(symbol = "A")
)

# Rules, initial state 2.8a parameters
rules2 <- function(d1 = 94.74, d2 = 132.63, a = 18.95, lr = 1.109, vr = 1.732) {list(
  A = list(
    list(symbol = "!", param = vr), 
    list(symbol = "F", param = 50), 
    list(symbol = "["), 
    list(symbol = "&", param = a), 
    list(symbol = "F", param = 50), 
    list(symbol = "A"), 
    list(symbol = "]"), 
    list(symbol = "/", param = d1), 
    list(symbol = "["),
    list(symbol = "&", param = a), 
    list(symbol = "F", param = 50), 
    list(symbol = "A"), 
    list(symbol = "]"), 
    list(symbol = "/", param = d2), 
    list(symbol = "["), 
    list(symbol = "&", param = a), 
    list(symbol = "F", param = 50), 
    list(symbol = "A"), 
    list(symbol = "]")
  ), 
  F = list(
    list(symbol = "F", param = lr)
  ), 
  "!" = list(
    list(symbol = "!", param = vr)
  )
)}


# ## TESTING GRAPHING OF FUNCTION
# # To have differing line widths, need to run through multiple lines3d
# M <- matrix(sample(1:36, 36, FALSE), 3, 12, dimnames = list(c('x', 'y', 'z'),rep(LETTERS[1:4], 3)))
# plot3d(t(M), size = 0, type = "l")
# for (i in 1:11) {lines3d(t(M[,i:(i+1)]), lwd = i)}

# ## TESTING STORING AS DF WITH LWD COLUMN
# M2 <- data.frame(t(M), lwd = 1:12)
# plot3d(M2, type = "n"); for (i in 1:11) lines3d(M2[i:(i+1),], lwd = M2[i,]$lwd)

# Calls generate_l_system, converts list of parameters to points
draw_3d_lsystem <- function(axiom, rules, iterations, tropism = c(0,0,-1), e = .14) {
  # Initial state: heading up, left on x, up on y
  H0 <- c(0,0,1); L0 <- c(1,0,0); U0 <- c(0,1,0)
  T0 <- matrix(c(H0, L0, U0), nrow = 3)
  x0 <- 0; y0 <- 0; z0 <- 0
  lwd <- 0
  turtle <- data.frame(x = x0, y = y0, z = z0, lwd = lwd, seg = 0)
  
  tree <- generate_l_system(axiom, rules, iterations)
  branch_pop <- list()

  pop_lvl <- 0
  for (action in tree) {
    if(action$symbol == "F") {
      # move forward distance of param
      lwd <- turtle[nrow(turtle), "lwd"]
      x <- turtle[nrow(turtle), "x"] + action$param * T0[1,1]
      y <- turtle[nrow(turtle), "y"] + action$param * T0[2,1]
      z <- turtle[nrow(turtle), "z"] + action$param * T0[3,1]
      
      seg <- turtle[nrow(turtle),"seg"]
      turtle <- rbind(turtle, data.frame(x=x, y=y, z=z, lwd=lwd, seg = seg+1))
      # TROPISM - results not entirely consistent with book
      T0 <- RT(T0, tropism, e)
      
    } else if (action$symbol == "!") {
      # set lwd to param
      lwd <- action$param
      turtle[nrow(turtle), "lwd"] <- lwd
    } else if (action$symbol == "/") {
      # Roll right by angle param, using rotation matrix $R_H(param)$
      T0 <- T0 %*% RH(action$param)

    } else if (action$symbol == "&") {
      # Pitch down by angle param, using rotation matrix $R_L(param)$
      T0 <- T0 %*% RL(action$param)
      
    } else if (action$symbol == "[") {
      # add current state to branch_pop
      pop_lvl <- pop_lvl + 1
      branch_pop <- c(branch_pop, list(list(pos = turtle[nrow(turtle),], 
                                            T = T0)))
      
    } else if (action$symbol == "A") {
      # Do nothing, consider removing A from list before creating turtle
      
    } else {
      # set top stack value to current state
      turtle <- rbind(turtle, 
                      #c(NA, NA, NA, NA), 
                      branch_pop[[pop_lvl]]$pos)
      T0 <- branch_pop[[pop_lvl]]$T
      
      # Remove last of stack. 
      branch_pop <- branch_pop[-length(branch_pop)]
      pop_lvl <- pop_lvl - 1
    }
  }
  turtle
}

if(!file.exists("tree_figa.gif")) {
  tropism <- c(0,0, -1); e = .22; n = 5
  rulesa <- rules2()
  treea <- draw_3d_lsystem(axiom2, rulesa, n, tropism, e)
  
  # colors
  yb<-colorRampPalette(c("#1B0000","#4d2B0b","chocolate4"))
  # Find max number of tree segments, make last green 
  cols <- c(yb(max(treea$seg)-1),"green4")
  # plot
  open3d()
  plot3d(treea, type = "n")
  for (i in 1:(nrow(treea)-1)) {
    if(treea[i,"seg"] < treea[i+1,"seg"]) {
      lines3d(treea[i:(i+1),], lwd = treea[i+1,]$lwd/1.73, 
              col = cols[treea[i,"seg"]+1])
    }
  }
  # Export movie
  movie3d(spin3d(), movie = "tree_figa", duration = 12, webshot = FALSE, dir = ".")
}

if(!file.exists("first_tree.gif")){
  # generate current tree
  test_tree2 <- draw_3d_lsystem(axiom2, rules2, 8); 
  # Setup plot extents
  plot3d(test_tree2, type = "n")
  # plot each line segment, lines3d seems unable to take lwd as something that varies
  for (i in 1:(nrow(test_tree2)-1)) lines3d(test_tree2[i:(i+1),], lwd = test_tree2[i,]$lwd)
  # save movie
  movie3d(spin3d(), movie = "first_tree", duration = 12, webshot = FALSE, dir = ".")
}


if(!file.exists("second_tree.gif")) {
  d1 <- 137.5; d2 <- 137.5; a <- 18.95; lr <- 1.109; vr <- 1.732
  tropism <- c(0, -1, 0); e = .14; n = 8
  rulesb <- rules2(d1, d2, a, lr, vr)
  treeb <- draw_3d_lsystem(axiom2, rulesb, n, tropism, e)
  
  # colors
  yb<-colorRampPalette(c("#1B0000","#4d2B0b","chocolate4"))
  # Find max number of tree segments, make last green 
  cols <- c(yb(max(treeb$seg)-1),"green4")
  # plot
  open3d()
  plot3d(treeb, type = "n")
  for (i in 1:(nrow(treeb)-1)) {
    if(treeb[i,"seg"] < treeb[i+1,"seg"]) {
      lines3d(treeb[i:(i+1),], lwd = treeb[i,]$lwd/1.73, 
              col = cols[treeb[i,"seg"]+1])
    }
  }
  # Export movie
  movie3d(spin3d(), movie = "second_tree", duration = 12, webshot = FALSE, dir = ".")
}

if(!file.exists("tree_figc.gif")) {
  d1 <- 112.5; d2 <- 157.5; a <- 22.5; lr <- 1.790; vr <- 1.732
  
  test_tree3 <- draw_3d_lsystem(axiom2, rules2, 8, tropism = c(-.02, 0, -1), e=.27)
  # colors
  yb<-colorRampPalette(c("#1B0000","#4d2B0b","chocolate4"))
  # Find max number of tree segments, make last green 
  cols <- c(yb(max(test_tree3$seg)-1),"green4")
  # plot
  open3d()
  plot3d(test_tree3, type = "n")
  for (i in 1:(nrow(test_tree3)-1)) {
    if(test_tree3[i,"seg"] < test_tree3[i+1,"seg"]) {
      lines3d(test_tree3[i:(i+1),], lwd = test_tree3[i,]$lwd/1.73, 
              col = cols[test_tree3[i,"seg"]+1])
    }
  }
  # Export movie
  movie3d(spin3d(), movie = "tree_figc", duration = 12, webshot = FALSE, dir = ".")
}

if(!file.exists("tree_figd.gif")) {
  d1 <- 180; d2 <- 252; a <- 36; lr <- 1.07; vr <- 1.732
  test_tree4 <- draw_3d_lsystem(axiom2, rules2, 6, tropism = c(-.61, .77, -.19), e=.4)
  # colors
  yb<-colorRampPalette(c("#1B0000","#4d2B0b","chocolate4"))
  # Find max number of tree segments, make last green 
  cols <- c(yb(max(test_tree4$seg)-1),"green4")
  # plot
  open3d()
  plot3d(test_tree4, type = "n")
  for (i in 1:(nrow(test_tree4)-1)) {
    if(test_tree4[i,"seg"] < test_tree4[i+1,"seg"]) {
      lines3d(test_tree4[i:(i+1),], lwd = test_tree4[i,]$lwd/1.73, 
              col = cols[test_tree4[i,"seg"]+1])
    }
  }
  # Export movie
  movie3d(spin3d(), movie = "tree_figd", duration = 12, webshot = FALSE, dir = ".")
}

