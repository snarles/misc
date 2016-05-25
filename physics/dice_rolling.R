library(pracma)

###
# Utility functions
###

zattach <- function (ll) 
{
  for (i in 1:length(ll)) {
    assign(names(ll)[i], ll[[i]], envir = globalenv())
  }
}

do.call2 <- function (f, ll, ...) 
{
  dots <- eval(substitute(alist(...)))
  ll <- modifyList(ll, dots)
  do.call(f, ll)
}

###
#  Code
###

rmat <- function(theta) {
  matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2, 2)
}

draw_dice <- function(xl, yl, dice_pos, dice_angle, add = FALSE, ...) {
  if (!add) {
    plot(NA, NA, xlim = xl, ylim = yl, ann = FALSE, axes = FALSE, asp = 1)
    abline(yl[1],0, lwd = 2)
    abline(yl[2],0, lwd = 2)
    abline(v = xl[1], lwd = 2)
    abline(v = xl[2], lwd = 2)
  }
  dcoors <- dice_verts %*% rmat(dice_angle) + repmat(dice_pos, 4, 1)
  lines(dcoors[2:3, ], col = "grey", lwd = 2)
  lines(dcoors[3:4, ], col = "grey", lwd = 2)
  lines(dcoors[c(4, 1), ], col = "grey", lwd = 2)
  ## the first edge of the dice is red
  lines(dcoors[1:2, ], col = "red", lwd = 2)
}

apply_uncons <- function(gcons, dice_pos, dice_angle, dice_v, dice_omega, drag_f, eps, tt, ...) {
  #dcoors <- dice_verts %*% rmat(dice_angle) + repmat(dice_pos, 4, 1)
  dice_v[2] <- dice_v[2] - eps * gcons
  dice_angle <- dice_angle + eps * dice_omega
  dice_pos <- dice_pos + eps * dice_v
  dice_v <- dice_v * (1 - eps* drag_f)
  dice_omega <- dice_omega * (1- eps*drag_f)
  tt <- tt + eps
  list(dice_pos = dice_pos, dice_angle = dice_angle, dice_v = dice_v, dice_omega = dice_omega,
       tt = tt)
}

apply_wall <- function(xl, yl, dice_pos, dice_angle, dice_v, dice_omega, dice_mass,
                       dice_inertia, newton_e, ...) {
  ## compute the coordinates
  dcoors <- dice_verts %*% rmat(dice_angle) + repmat(dice_pos, 4, 1)
  dcoors0 <- dice_verts %*% rmat(dice_angle) 
  dcoors_perp <- cbind(-dcoors0[, 2], dcoors0[, 1])
  ## vertex velocities
  vvs <- repmat(dice_v, 4, 1) + dcoors_perp * dice_omega
  ## normals
  normal_vs <- list(x1 = c(1, 0), x2 = c(-1, 0), y1 = c(0, 1), y2 = c(0, -1))
  ## hit checks
  hitX1 <- dcoors[, 1] < xl[1] & (vvs %*% normal_vs$x1) < 0
  hitX2 <- dcoors[, 1] > xl[2] & (vvs %*% normal_vs$x2) < 0
  hitY1 <- dcoors[, 2] < yl[1] & (vvs %*% normal_vs$y1) < 0
  hitY2 <- dcoors[, 2] > yl[2] & (vvs %*% normal_vs$y2) < 0
  hitlist <- list(x1 = hitX1, x2 = hitX2, y1 = hitY1, y2 = hitY2)
  # i <- 3
  for (i in sample(4)) {
    normal_v <- normal_vs[[i]]
    if (sum(hitlist[[i]]) > 0) {
      ind <- which(hitlist[[i]])[sample(sum(hitlist[[i]]), 1)]
      r <- dcoors0[ind, ]
      rperp <- c(-r[2], r[1])
      vp <- vvs[ind, ]
      j <- compute_j(newton_e, normal_v, vp, r, dice_mass, dice_inertia)
      dice_v <- dice_v + j/dice_mass * normal_v
      dice_omega <- dice_omega + j/dice_inertia * sum(rperp * normal_v)
    }
  }
  list(dice_pos = dice_pos, dice_angle = dice_angle, dice_v = dice_v, dice_omega = dice_omega)
}

project_back <- function(xl, yl, dice_pos, dice_angle, ...) {
  dcoors <- dice_verts %*% rmat(dice_angle) + repmat(dice_pos, 4, 1)
  if (min(dcoors[, 1]) < xl[1]) dice_pos[1] <- dice_pos[1] + (xl[1] - min(dcoors[, 1]))
  if (max(dcoors[, 1]) > xl[2]) dice_pos[1] <- dice_pos[1] + (xl[2] - max(dcoors[, 1]))
  if (min(dcoors[, 2]) < yl[1]) dice_pos[2] <- dice_pos[2] + (yl[1] - min(dcoors[, 2]))
  if (max(dcoors[, 2]) > yl[2]) dice_pos[2] <- dice_pos[2] + (yl[2] - max(dcoors[, 2]))
  list(dice_pos = dice_pos, dice_angle = dice_angle)
}

compute_j <- function(newton_e, normal_v, v, r, Ma, Ia) {
  rperp <- c(-r[2], r[1])
  -(1 + newton_e) * sum(v * normal_v)/(1/Ma + sum(rperp * normal_v)^2/Ia)
}

simulate_dice <- function(xl, yl, gcons, drag_f, dice_pos, dice_angle,
                          dice_v, dice_omega, dice_mass, dice_inertia,
                          newton_e, eps, framerate, ...) {
  tt <- 0
  params <- list(xl = xl, yl = yl, gcons = gcons, drag_f = drag_f, 
                 dice_pos = dice_pos, dice_angle = dice_angle,
                 dice_v = dice_v, dice_omega = dice_omega,
                 dice_mass = dice_mass, dice_inertia = dice_inertia,
                 newton_e = newton_e, eps = eps, framerate = framerate)
}

get_energy <- function(gcons, dice_pos, dice_angle,
                        dice_v, dice_omega, dice_mass, ...) {
  gpot <- gcons * dice_mass * dice_pos[2]
  dcoors <- dice_verts %*% rmat(dice_angle) + repmat(dice_pos, 4, 1)
  dcoors0 <- dice_verts %*% rmat(dice_angle) 
  dcoors_perp <- cbind(-dcoors0[, 2], dcoors0[, 1])
  vvs <- repmat(dice_v, 4, 1) + dcoors_perp * dice_omega
  kinet <- 1/2 * sum(rowSums(vvs^2) * dice_mass/4)
  gpot + kinet
}

get_dcoords <- function(dice_pos, dice_angle, ...) {
  dice_verts %*% rmat(dice_angle) + repmat(dice_pos, 4, 1)
}

get_hitlist0 <- function(xl, yl,  dice_pos, dice_angle,  ...) {
  dcoors <- dice_verts %*% rmat(dice_angle) + repmat(dice_pos, 4, 1)
  hitX1 <- dcoors[, 1] < xl[1]
  hitX2 <- dcoors[, 1] > xl[2]
  hitY1 <- dcoors[, 2] < yl[1]
  hitY2 <- dcoors[, 2] > yl[2]
  cbind(x1 = hitX1, x2 = hitX2, y1 = hitY1, y2 = hitY2)
}

get_vvs <- function(dice_pos = .GlobalEnv[["dice_pos"]],
                    dice_angle = .GlobalEnv[["dice_angle"]],
                    dice_v = .GlobalEnv[["dice_v"]],
                    dice_omega = .GlobalEnv[["dice_omega"]],
                    ...) {
  ## compute the coordinates
  dcoors <- dice_verts %*% rmat(dice_angle) + repmat(dice_pos, 4, 1)
  dcoors0 <- dice_verts %*% rmat(dice_angle) 
  dcoors_perp <- cbind(-dcoors0[, 2], dcoors0[, 1])
  ## vertex velocities
  repmat(dice_v, 4, 1) + dcoors_perp * dice_omega
}

find_collision_time <- function(eps = 0.05, eps_power = 10,
                                xl, yl, gcons, drag_f, newton_e, dice_mass, dice_inertia,
                                tt, dice_pos, dice_angle, dice_v, dice_omega, ...
) {
  eps0 <- eps
  params <- list(xl = xl, yl = yl, gcons = gcons, drag_f = drag_f, newton_e = newton_e,
                 dice_mass = dice_mass, dice_inertia = dice_inertia,
                 tt = tt, dice_pos = dice_pos, dice_angle = dice_angle, 
                 dice_v = dice_v, dice_omega = dice_omega, eps = eps)
  ft <- TRUE
  count <- 1
  for (i in 1:eps_power) {
    eps <- eps/2
    h0 <- do.call(get_hitlist0, params)
    while(sum(h0) == 0) {
      params$eps <- eps
      update <- do.call2(apply_uncons, params)
      h0 <- do.call(get_hitlist0, modifyList(params, update))
      if (sum(h0) == 0) {
        params <- modifyList(params, update)
        count <- count + 1
        if (count %% 3 == 0) {
          params$add = TRUE
          polygon(c(-3, 3, 3, -3), c(5, 5, 0, 0), col = rgb(1,1,1,0.1),
                  border = "black")
          do.call(draw_dice, params)
          params$add = FALSE
          
        }
        
        if (!ft) { 
          Sys.sleep(0.2)
        } else {
          ft <- FALSE
        }
      }
    }
  }
  params <- modifyList(params, update)
  update <- do.call2(apply_wall, params)
  params <- modifyList(params, update)
  ## run until noncollision
  eps <- eps/16
  (h0 <- do.call(get_hitlist0, modifyList(params, update)))
  counter <- 0
  CMAX <- 1000
  while(sum(h0) > 0 && counter < CMAX) {
    counter <- counter + 1
    params$eps <- eps
    update <- do.call2(apply_uncons, params)
    (h0 <- do.call(get_hitlist0, modifyList(params, update)))
    params <- modifyList(params, update)
    do.call(get_dcoords, params)
    do.call(get_vvs, params)
  }
  if (counter == CMAX) params$terminal = TRUE
  params$eps <- eps0
  params
}

####
##  DEMO
####

## simulation step size
eps0 <- 0.05
eps_power <- 10
## hack to fix simulation
# collision_thres <- 0.1

## gravity
gcons <- 9
## newton e const
newton_e <- 0.2
## air_res effect
drag_f <- 0

## size of box
xl <- c(-3, 3)
yl <- c(0, 5)

## size of dice
dice_sz <- 0.9
## coords of dice relative to center of mass
dice_verts <- dice_sz * 
  rbind(c(-.5, .5),
        c(.5, .5),
        c(.5, -.5),
        c(-.5, -.5))
## mass of the dice
dice_mass <- 4
## moment of inertia
dice_inertia <- dice_mass/4 * sum(rowSums(dice_verts^2))

## initial position of dice
dice_pos <- c(0, 3)
dice_angle <- pi/4

## initial velocity of dice
dice_v <- c(0 + 2 * runif(1), 0)
dice_omega <- 0.5 + 2 * runif(1)

## time variable
tt <- 0

## graphical params
framerate <- 0.1

# list of params for copy/paste
# xl, yl, gcons, drag_f, newton_e, dice_mass, dice_inertia,
# tt, dice_pos, dice_angle, dice_v, dice_omega, 

## put everything into a list
params0 <- list(xl = xl, yl = yl, gcons = gcons, drag_f = drag_f, 
               dice_pos = dice_pos, dice_angle = dice_angle,
               dice_v = dice_v, dice_omega = dice_omega,
               dice_mass = dice_mass, dice_inertia = dice_inertia,
               newton_e = newton_e, eps0 = eps0, framerate = framerate, 
               eps = eps0, eps_power = eps_power, tt = tt)

####
##  Initialize
####

params <- params0
do.call(get_energy, params)
do.call(draw_dice, params)

####
##  Find collision time
####

## check energy before collision


## find collision and apply wall
while(is.null(params[["terminal"]])) {
  oldparams <- params
  update <- do.call2(find_collision_time, params)
  params <- modifyList(params, update)
  update <- do.call2(apply_wall, params)
  params <- modifyList(params, update)
  en <- do.call(get_energy, params)
  do.call(draw_dice, params); title("post-collision", sub = en)
  print(params$tt - oldparams$tt)
}
params[["terminal"]]
do.call(get_dcoords, params)
do.call(get_hitlist0, params)
do.call(get_vvs, params)


# params <- oldparams
# zattach(params)

# saveRDS(params, "physics/testcase.rds")
# params <- readRDS("physics/testcase.rds")
# params$eps <- 1e-7
# update <- do.call(apply_uncons, params)
# 
# params <- modifyList(params, update)
# update <- do.call(apply_uncons, params)
# do.call(get_dcoords, update)
# do.call(get_vvs, update)
