library(pracma)

rmat <- function(theta) {
  matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2, 2)
}

draw_dice <- function(xl = .GlobalEnv[["xl"]], 
                      yl = .GlobalEnv[["yl"]], 
                      dice_pos = .GlobalEnv[["dice_pos"]],
                      dice_angle = .GlobalEnv[["dice_angle"]], add = FALSE) {
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

apply_uncons <- function(gcons = .GlobalEnv[["gcons"]],
                         dice_pos = .GlobalEnv[["dice_pos"]],
                         dice_angle = .GlobalEnv[["dice_angle"]],
                         dice_v = .GlobalEnv[["dice_v"]],
                         dive_omega = .GlobalEnv[["dice_omega"]],
                         drag_f = .GlobalEnv[["drag_f"]],
                         eps =  .GlobalEnv[["eps"]]) {
  #dcoors <- dice_verts %*% rmat(dice_angle) + repmat(dice_pos, 4, 1)
  dice_v[2] <- dice_v[2] - eps * gcons
  dice_angle <- dice_angle + eps * dice_omega
  dice_pos <- dice_pos + eps * dice_v
  dice_v <- dice_v * (1 - eps* drag_f)
  dice_omega <- dice_omega * (1- eps*drag_f)
  list(dice_pos = dice_pos, dice_angle = dice_angle, dice_v = dice_v, dice_omega = dice_omega)
  
}

apply_wall <- function(xl = .GlobalEnv[["xl"]], 
                       yl = .GlobalEnv[["yl"]], 
                       dice_pos = .GlobalEnv[["dice_pos"]],
                       dice_angle = .GlobalEnv[["dice_angle"]],
                       dice_v = .GlobalEnv[["dice_v"]],
                       dice_omega = .GlobalEnv[["dice_omega"]],
                       dice_mass = .GlobalEnv[["dice_mass"]],
                       dice_inertia = .GlobalEnv[["dice_inertia"]],
                       newton_e = .GlobalEnv[["newton_e"]]) {
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

project_back <- function(xl = .GlobalEnv[["xl"]], 
                         yl = .GlobalEnv[["yl"]], 
                         dice_pos = .GlobalEnv[["dice_pos"]],
                         dice_angle = .GlobalEnv[["dice_angle"]]) {
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

simulate_dice <- function(xl = .GlobalEnv[["xl"]], 
                          yl = .GlobalEnv[["yl"]], 
                          gcons = .GlobalEnv[["gcons"]],
                          drag_f = .GlobalEnv[["drag_f"]],
                          dice_pos = .GlobalEnv[["dice_pos"]],
                          dice_angle = .GlobalEnv[["dice_angle"]],
                          dice_v = .GlobalEnv[["dice_v"]],
                          dice_omega = .GlobalEnv[["dice_omega"]],
                          dice_mass = .GlobalEnv[["dice_mass"]],
                          dice_inertia = .GlobalEnv[["dice_inertia"]],
                          newton_e = .GlobalEnv[["newton_e"]],
                          eps =  .GlobalEnv[["eps"]],
                          framerate = .GlobalEnv[["framerate"]]) {
  tt <- 0
  params <- list(xl = xl, yl = yl, gcons = gcons, drag_f = drag_f, 
                 dice_pos = dice_pos, dice_angle = dice_angle,
                 dice_v = dice_v, dice_omega = dice_omega,
                 dice_mass = dice_mass, dice_inertia = dice_inertia,
                 newton_e = newton_e, eps = eps, framerate = framerate)
}

energy_dice <- function(gcons = .GlobalEnv[["gcons"]],
                        dice_pos = .GlobalEnv[["dice_pos"]],
                        dice_angle = .GlobalEnv[["dice_angle"]],
                        dice_v = .GlobalEnv[["dice_v"]],
                        dice_omega = .GlobalEnv[["dice_omega"]],
                        dice_mass = .GlobalEnv[["dice_mass"]]) {
  gpot <- gcons * dice_mass * dice_pos[2]
  dcoors <- dice_verts %*% rmat(dice_angle) + repmat(dice_pos, 4, 1)
  dcoors0 <- dice_verts %*% rmat(dice_angle) 
  dcoors_perp <- cbind(-dcoors0[, 2], dcoors0[, 1])
  vvs <- repmat(dice_v, 4, 1) + dcoors_perp * dice_omega
  kinet <- 1/2 * sum(rowSums(vvs^2) * dice_mass/4)
  gpot + kinet
}

force_energy_conservation <- function(en0, gcons = .GlobalEnv[["gcons"]],
                                      dice_pos = .GlobalEnv[["dice_pos"]],
                                      dice_angle = .GlobalEnv[["dice_angle"]],
                                      dice_v = .GlobalEnv[["dice_v"]],
                                      dice_omega = .GlobalEnv[["dice_omega"]],
                                      dice_mass = .GlobalEnv[["dice_mass"]]) {
  en <- energy_dice(gcons, dice_pos, dice_angle, dice_v, dice_omega, dice_mass)
  while (en > en0 && sum(abs(dice_v) + abs(dice_omega)) > 1e-3) {
    dice_v <- dice_v * c(0.999, 0.99)
    dice_omega <- dice_omega * 0.95
    en <- energy_dice(gcons, dice_pos, dice_angle, dice_v, dice_omega, dice_mass)
  }
  list(dice_pos = dice_pos, dice_angle = dice_angle, dice_v = dice_v, dice_omega = dice_omega)
}

## simulation step size
eps <- 0.05
## hack to fix simulation
collision_thres <- 0.1

## gravity
gcons <- 2
## newton e const
newton_e <- 0.9
## air_res effect
drag_f <- 0.05

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
dice_v <- c(4.3 + 0.1 * runif(1), 0)
dice_omega <- 0.5 + 0.1 * runif(1)

## graphical params
framerate <- 0.1


# eps0 <- 0.05
# eps <- pmin(0.2/abs(dice_omega + 1), eps0)
draw_dice()
for (i in 1:100) {
  en_prev <- energy_dice()
  update <- apply_uncons()
  lineId::zattach(update)
  update <- apply_wall()
  lineId::zattach(update)
  update <- project_back()
  lineId::zattach(update)
  update <- force_energy_conservation(en_prev)
  lineId::zattach(update)
  if (i %%5 == 0) {
    polygon(c(xl[1], xl[2], xl[2], xl[1]), c(yl[1], yl[1], yl[2], yl[2]), col = rgb(1,1,1,0.05))
    draw_dice(add = TRUE)
  }
}
list(dice_omega = dice_omega, dice_v = dice_v, dice_en = en_prev)
