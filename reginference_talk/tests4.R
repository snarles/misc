
## marginal results

png('mar_pf16.png')
matplot(res_mar(x_pf16, y_pf16)[1:20, 2:1], type = "o", pch = c("1", "0"), ylab = "")
dev.off()

png('mar_fMRI.png')
matplot(res_mar(x_fMRI, y_fMRI)[1:20, 2:1], type = "o", pch = c("1", "0"), ylab = "")
dev.off()

png('mar_HIV.png')
matplot(res_mar(x_HIV, y_HIV)[1:20, 2:1], type = "o", pch = c("1", "0"), ylab = "")
dev.off()

png('mar_gal.png')
matplot(res_mar(x_gal, y_gal)[1:20, 2:1], type = "o", pch = c("1", "0"), ylab = "")
dev.off()


## covTest

res_c_pf16 <- cov_test_results(x_pf16, y_pf16)
res_c_fMRI <- cov_test_results(x_fMRI, y_fMRI)
res_c_HIV <- cov_test_results(x_HIV, y_HIV)
res_c_gal <- cov_test_results(x_gal, y_gal)

## sslasso

res_s_pf16 <- sslasso_results(x_pf16, y_pf16)
res_s_fMRI <- sslasso_results(x_fMRI, y_fMRI)
res_s_HIV <- sslasso_results(x_HIV, y_HIV)
res_s_gal <- sslasso_results(x_gal, y_gal)

## knockoffs

res_k_pf16 <- knockoff_results(x_pf16, y_pf16)
res_k_fMRI <- knockoff_results(x_fMRI, y_fMRI)
res_k_HIV <- knockoff_results(x_HIV, y_HIV)
res_k_gal <- knockoff_results(x_gal, y_gal)


save(file = paste0("test4_", seed, ".RData"),
     list = c("res_c_pf16", "res_c_fMRI", "res_c_HIV", "res_c_gal",
              "res_s_pf16", "res_s_fMRI", "res_s_HIV", "res_s_gal",
              "res_k_pf16", "res_k_fMRI", "res_k_HIV", "res_k_gal"))

good_pf16 <- max(c(res_c_pf16[50, 3], res_s_pf16[50, 3], res_k_pf16[50, 3]))
good_fMRI <- max(c(res_c_fMRI[50, 3], res_s_fMRI[50, 3], res_k_fMRI[50, 3]))
good_HIV <- max(c(res_c_HIV[50, 3], res_s_HIV[50, 3], res_k_HIV[50, 3]))
good_gal <- max(c(res_c_gal[50, 3], res_s_gal[50, 3], res_k_gal[50, 3]))

