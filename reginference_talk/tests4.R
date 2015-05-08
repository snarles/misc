


## OLS

res_o_pf16 <- OLS_results(x_pf16, y_pf16)
res_o_fMRI <- OLS_results(x_fMRI, y_fMRI)
res_o_HIV <- OLS_results(x_HIV, y_HIV)
res_o_gal <- OLS_results(x_gal, y_gal)


## covTest

res_cs_pf16 <- cov_test_results(x_pf16, y_pf16, maxp = 70)
res_cs_fMRI <- cov_test_results(x_fMRI, y_fMR, maxp = 70I)
res_cs_HIV <- cov_test_results(x_HIV, y_HIV, maxp = 70)
res_cs_gal <- cov_test_results(x_gal, y_gal, maxp = 70)

## sslasso

res_s_pf16 <- sslasso_results(x_pf16, y_pf16)
res_s_fMRI <- sslasso_results(x_fMRI, y_fMRI)
res_s_HIV <- sslasso_results(x_HIV, y_HIV)
res_s_gal <- sslasso_results(x_gal, y_gal)

## knockoffs

res_ks_pf16 <- knockoff_results(x_pf16, y_pf16)
res_ks_fMRI <- knockoff_results(x_fMRI, y_fMRI)
res_ks_HIV <- knockoff_results(x_HIV, y_HIV)
res_ks_gal <- knockoff_results(x_gal, y_gal)

res_k_pf16 <- res_ks_pf16[[1]]
res_k_fMRI <- res_ks_fMRI[[1]]
res_k_HIV <- res_ks_HIV[[1]]
res_k_gal <- res_ks_gal[[1]]


## LO ss results

set.seed(0)
xs_pf16 <- noised_projections(pf16[, 1:105], q = 895, sigma = 2, adjoin = TRUE)
ys_pf16 <- pf16[, 106]
smp <- sample(2000, 100)
xs_pf16 <- xs_pf16[smp, ]
ys_pf16 <- ys_pf16[smp]

set.seed(0)
xs_fMRI <- noised_projections(fMRI[, 1:53], q = 822, sigma = 1, adjoin = TRUE)
ys_fMRI <- fMRI[, 54]
smp <- sample(1750, 100)
xs_fMRI <- xs_fMRI[smp, ]
ys_fMRI <- ys_fMRI[smp]

set.seed(0)
xs_HIV <- noised_projections(HIV[, -1], q = 214, sigma = 1, adjoin = TRUE)
ys_HIV <- HIV[, 1]
smp <- sample(842, 100)
xs_HIV <- xs_HIV[smp, ]
ys_HIV <- ys_HIV[smp]
xs_HIV <- xs_HIV[, colSums(abs(xs_HIV)) > 0] # dim <- 149

set.seed(0)
xs_gal <- noised_projections(galaxy[, 1:4], q = 157, sigma = 1, adjoin = TRUE)
ys_gal <- galaxy[, 5]
smp <- sample(323, 100)
xs_gal <- xs_gal[smp, ]
ys_gal <- ys_gal[smp]



ress_s_pf16 <- sslasso_results(xs_pf16, ys_pf16)
ress_s_fMRI <- sslasso_results(xs_fMRI, ys_fMRI)
ress_s_HIV <- sslasso_results(xs_HIV, ys_HIV)
ress_s_gal <- sslasso_results(xs_gal, ys_gal)

ress_cs_pf16 <- cov_test_results(xs_pf16, ys_pf16, maxp = 70, sigma.est = shat_pf16)
ress_cs_fMRI <- cov_test_results(xs_fMRI, ys_fMRI, maxp = 70, sigma.est = shat_fMRI)
ress_cs_HIV <- cov_test_results(xs_HIV, ys_HIV, maxp = 70, sigma.est = shat_HIV)
ress_cs_gal <- cov_test_results(xs_gal, ys_gal, maxp = 70, sigma.est = shat_gal)

save(file = paste0("test4_", seed, ".RData"),
     list = c("res_cs_pf16", "res_cs_fMRI", "res_cs_HIV", "res_cs_gal",
              "res_s_pf16", "res_s_fMRI", "res_s_HIV", "res_s_gal",
              "res_ks_pf16", "res_ks_fMRI", "res_ks_HIV", "res_ks_gal",
              "ress_s_pf16", "ress_s_fMRI", "ress_s_HIV", "ress_s_gal",
              "ress_cs_pf16", "ress_cs_fMRI", "ress_cs_HIV", "ress_cs_gal"))

good_pf16 <- max(c(res_cs_pf16$naive[50, 3], res_s_pf16[50, 3], res_ks_pf16$k0[50, 3]))
good_fMRI <- max(c(res_cs_fMRI$naive[50, 3], res_s_fMRI[50, 3], res_ks_fMRI$k0[50, 3]))
good_HIV <- max(c(res_cs_HIV$naive[50, 3], res_s_HIV[50, 3], res_ks_HIV$k0[50, 3]))
good_gal <- max(c(res_cs_gal$naive[50, 3], res_s_gal[50, 3], res_ks_gal$k0[50, 3]))


lars_pf16 <- lars(as.matrix(x_pf16), y_pf16)
lars_fMRI <- lars(as.matrix(x_fMRI), y_fMRI)
lars_HIV <- lars(as.matrix(x_HIV), y_HIV)
lars_gal <- lars(as.matrix(x_gal), y_gal)

