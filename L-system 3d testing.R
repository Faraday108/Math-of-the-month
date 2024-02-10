## Working on 3D rotation 

cross <- function(x, y, i=1:3) {
  create3D <- function(x) head(c(x, rep(0, 3)), 3)
  x <- create3D(x)
  y <- create3D(y)
  j <- function(i) (i-1) %% 3+1
  return (x[j(i+1)]*y[j(i+2)] - x[j(i+2)]*y[j(i+1)])
}

norm_vec <- function(x) sqrt(sum(x^2))

cross(v1, m0_RU[,1]) / norm_vec(cross(v1, m0_RU[,1]))

RU <- function(a) {
  a <- a*pi/180
  matrix(c(cos(a), -sin(a), 0, sin(a), cos(a), 0, 0, 0, 1), nrow = 3)
}
RL <- function(a) {
  a <- a*pi/180
  matrix(c(cos(a), 0, sin(a), 0, 1, 0, -sin(a), 0, cos(a)), nrow = 3)
}
RH <- function(a) {
  a <- a*pi/180
  matrix(c(1, 0, 0, 0, cos(a), sin(a), 0, -sin(a), cos(a)), nrow = 3)
}

# Define initial state of turtle; H = straight up, L = along x-axis, U = along y-axis
H0 <- c(0,0,1); L0 <- c(1,0,0); U0 <- c(0,1,0)
T0 <- matrix(c(H0, L0, U0), nrow = 3)

# Rotate initial state around U by a
T0 %*% RU(pi/3)

# Define a function to apply production rules
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

# Define function to generate L-system
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
axiom2 <- list(
  list(symbol = "!", param = 1), 
  list(symbol = "F", param = 200), 
  list(symbol = "/", param = 45), 
  list(symbol = "A")
)
d1 <- 94.74; d2 <- 132.63; a <- 18.95; lr <- 1.109; vr <- 1.732
d1 <- 137.5; d2 <- 137.5; a <- 18.95; lr <- 1.109; vr <- 1.732
rules2 <- list(
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
)

iterations <- 1

#parametric_l <- generate_l_system(axiom, rules, iterations = 3)

# Create testing of functions, works well currently!
test_tree <- generate_l_system(axiom2, rules2, iterations = 2)

## TESTING GRAPHING OF FUNCTION
# To have differing line widths, need to run through multiple lines3d
M <- matrix(sample(1:36, 36, FALSE), 3, 12, dimnames = list(c('x', 'y', 'z'),rep(LETTERS[1:4], 3)))
plot3d(t(M), size = 0, type = "l")
for (i in 1:11) {lines3d(t(M[,i:(i+1)]), lwd = i)}

## TESTING STORING AS DF WITH LWD COLUMN
M2 <- data.frame(t(M), lwd = 1:12)
plot3d(M2, type = "n"); for (i in 1:11) lines3d(M2[i:(i+1),], lwd = M2[i,]$lwd)

### TO DO 
# create function to send list of directions to turtle. 
# Turtle attributes need to include Position, Thickness, Heading, Left, and Up

draw_3d_lsystem <- function(axiom, rules, iterations) {
  # Initial state: heading up, left on x, up on y
  H0 <- c(0,0,1); L0 <- c(1,0,0); U0 <- c(0,1,0)
  T0 <- matrix(c(H0, L0, U0), nrow = 3)
  x0 <- 0; y0 <- 0; z0 <- 0
  lwd <- 0
  turtle <- list(pos = P0, dir = T0, lwd = lwd)
  turtle <- data.frame(x = x0, y = y0, z = z0, lwd = lwd)
  
  tree <- generate_l_system(axiom, rules, iterations)
  branch_pop <- list()
  # branch_pop <- list(list(state = numeric(), T = numeric()))
  pop_lvl <- 0
  for (action in tree) {
    if(action$symbol == "F") {
      # move forward distance of param
      lwd <- turtle[nrow(turtle), "lwd"]
      x <- turtle[nrow(turtle), "x"] + action$param * T0[1,1]
      y <- turtle[nrow(turtle), "y"] + action$param * T0[2,1]
      z <- turtle[nrow(turtle), "z"] + action$param * T0[3,1]
      
      turtle <- rbind(turtle, data.frame(x=x, y=y, z=z, lwd=lwd))
      
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

if(!file.exists("first_tree.gif")){
  test_tree2 <- draw_3d_lsystem(axiom2, rules2, 8); 
  plot3d(test_tree2, type = "n")
  for (i in 1:(nrow(test_tree2)-1)) lines3d(test_tree2[i:(i+1),], lwd = test_tree2[i,]$lwd)
  movie3d(spin3d(), movie = "first_tree", duration = 12, webshot = FALSE, dir = ".")
}