## marginal results

png('mar_pf16.png')
matplot(res_mar(x_pf16, y_pf16)[1:20, 2:1], type = "o", pch = c("1", "0"), ylab = "", lwd = 3)
dev.off()

png('mar_fMRI.png')
matplot(res_mar(x_fMRI, y_fMRI)[1:20, 2:1], type = "o", pch = c("1", "0"), ylab = "", lwd = 3)
dev.off()

png('mar_HIV.png')
matplot(res_mar(x_HIV, y_HIV)[1:20, 2:1], type = "o", pch = c("1", "0"), ylab = "", lwd = 3)
dev.off()

png('mar_gal.png')
matplot(res_mar(x_gal, y_gal)[1:20, 2:1], type = "o", pch = c("1", "0"), ylab = "", lwd = 3)
dev.off()


cols <- rgb(c(0,0,0.5,0.4), c(0,0.7,0.1,0), c(1,0,0.1,0.4))

## OLS Type I

png('res_o_type1.png')
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), ylab = expression(FP/p[0]), xlab = expression(alpha))
abline(0, 1, col = "red", lwd = 2)
lines(1:50/50, res_o_pf16[2 * 1:50, 2]/895, type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:50/50, res_o_fMRI[2 * 1:50, 2]/822, type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:50/50, res_o_HIV[2 * 1:50, 2]/214, type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:50/50, res_o_gal[2 * 1:50, 2]/157, type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()

## COvTest Type I

res_c_pf16 <- res_cs_pf16$naive
res_c_fMRI <- res_cs_fMRI$naive
res_c_HIV <- res_cs_HIV$naive
res_c_gal <- res_cs_gal$naive

png('res_c_type1.png')
plot(NA, NA, ylim = c(0, 2.1), xlim = c(0, 0.1), ylab = expression(FP), xlab = expression(alpha))
abline(0, 1, col = "red", lwd = 2)
lines(1:10/100, res_c_pf16[1:10, 2], type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:10/100, res_c_fMRI[1:10, 2], type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:10/100, res_c_HIV[1:10, 2], type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:10/100, res_c_gal[1:10, 2], type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()

fdr_e <- function(res) {
  res[, 2]/pmax(1, res[, 2] + res[, 3])
}

png('res_c_fs_type1.png')
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), ylab = expression(FDR), xlab = expression(alpha))
abline(0, 1, col = "red", lwd = 2)
lines(1:50/50, fdr_e(res_cs_pf16$fs)[2 * 1:50], type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:50/50, fdr_e(res_cs_fMRI$fs)[2 * 1:50], type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:50/50, fdr_e(res_cs_HIV$fs)[2 * 1:50], type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:50/50, fdr_e(res_cs_gal$fs)[2 * 1:50], type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()

png('res_c_ss_type1.png')
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), ylab = expression(FDR), xlab = expression(alpha))
abline(0, 1, col = "red", lwd = 2)
lines(1:50/50, fdr_e(res_cs_pf16$ss)[2 * 1:50], type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:50/50, fdr_e(res_cs_fMRI$ss)[2 * 1:50], type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:50/50, fdr_e(res_cs_HIV$ss)[2 * 1:50], type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:50/50, fdr_e(res_cs_gal$ss)[2 * 1:50], type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()


## SSlasso Type I

png('res_s_type1.png')
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), ylab = expression(FP/p[0]), xlab = expression(alpha))
abline(0, 1, col = "red", lwd = 2)
lines(1:50/50, res_s_pf16[2 * 1:50, 2]/895, type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:50/50, res_s_fMRI[2 * 1:50, 2]/822, type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:50/50, res_s_HIV[2 * 1:50, 2]/214, type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:50/50, res_s_gal[2 * 1:50, 2]/157, type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()

## knockoffs Type I

t1e <- function(res_k) res_k[, 2]/(res_k[, 2] + res_k[, 3] + 100/(1:100))

png('res_k_type1.png')
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), ylab = "FDR*", xlab = expression(alpha))
abline(0, 1, col = "red", lwd = 2)
lines(1:50/50, t1e(res_k_pf16)[2 * 1:50], type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:50/50, t1e(res_k_fMRI)[2 * 1:50], type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:50/50, t1e(res_k_HIV)[2 * 1:50],  type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:50/50, t1e(res_k_gal)[2 * 1:50],  type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()

# Power

## OLS
png('res_o_power.png')
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), ylab = "Rel. Power", xlab = expression(alpha))
lines(1:50/50, res_s_pf16[2 * 1:50, 3]/good_pf16, type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:50/50, res_s_fMRI[2 * 1:50, 3]/good_fMRI, type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:50/50, res_s_HIV[2 * 1:50, 3]/good_HIV, type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:50/50, res_s_gal[2 * 1:50, 3]/good_gal, type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()

## Cov
png('res_c_power.png')
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 0.1), ylab = "Rel. Power", xlab = expression(alpha))
lines(1:10/100, res_c_pf16[1:10, 3]/good_pf16, type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:10/100, res_c_fMRI[1:10, 3]/good_fMRI, type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:10/100, res_c_HIV[1:10, 3]/good_HIV, type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:10/100, res_c_gal[1:10, 3]/good_gal, type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()

png('res_c_fs_power.png')
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), ylab = "Rel. Power", xlab = expression(alpha))
lines(1:50/50, res_cs_pf16$fs[2 * 1:50, 3]/good_pf16, type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:50/50, res_cs_fMRI$fs[2 * 1:50, 3]/good_fMRI, type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:50/50, res_cs_HIV$fs[2 * 1:50, 3]/good_HIV, type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:50/50, res_cs_gal$fs[2 * 1:50, 3]/good_gal, type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()


png('res_c_ss_power.png')
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), ylab = "Rel. Power", xlab = expression(alpha))
lines(1:50/50, res_cs_pf16$ss[2 * 1:50, 3]/good_pf16, type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:50/50, res_cs_fMRI$ss[2 * 1:50, 3]/good_fMRI, type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:50/50, res_cs_HIV$ss[2 * 1:50, 3]/good_HIV, type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:50/50, res_cs_gal$ss[2 * 1:50, 3]/good_gal, type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()


## SS
png('res_s_power.png')
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), ylab = "Rel. Power", xlab = expression(alpha))
lines(1:50/50, res_s_pf16[2 * 1:50, 3]/good_pf16, type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:50/50, res_s_fMRI[2 * 1:50, 3]/good_fMRI, type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:50/50, res_s_HIV[2 * 1:50, 3]/good_HIV, type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:50/50, res_s_gal[2 * 1:50, 3]/good_gal, type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()


## Knockoff
png('res_k_power.png')
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), ylab = "Rel. Power", xlab = expression(alpha))
lines(1:50/50, res_k_pf16[2 * 1:50, 3]/good_pf16, type = "o", ylab = "", col = cols[1], pch = "P")
lines(1:50/50, res_k_fMRI[2 * 1:50, 3]/good_fMRI, type = "o", ylab = "", col = cols[2], pch = "F")
lines(1:50/50, res_k_HIV[2 * 1:50, 3]/good_HIV, type = "o", ylab = "", col = cols[3], pch = "H")
lines(1:50/50, res_k_gal[2 * 1:50, 3]/good_gal, type = "o", ylab = "", col = cols[4], pch = "G")
dev.off()

## TP vs FP

mcols <- c(rgb(0.1, 0.1, 0.1), rgb(0, 1, 0), rgb(1, 0, 0), rgb(0, 0, 1))

png('pf16_tvf.png')
plot(res_o_pf16[, -1], col = mcols[1], pch = "o", ylab = "TP", xlab = "FP", xlim = c(0, 400), ylim = c(0, 30))
lines(res_mar(x_pf16, y_pf16))
lines(res_lars(lars_pf16), col = "green")
points(res_c_pf16[, -1], col = mcols[2], pch = "c")
points(res_s_pf16[, -1], col = mcols[3], pch = "s")
points(res_k_pf16[, -1], col = mcols[4], pch = "k")
dev.off()

png('fMRI_tvf.png')
plot(res_o_fMRI[, -1], col = mcols[1], pch = "o", ylab = "TP", xlab = "FP", xlim = c(0, 400), ylim = c(0, 30))
lines(res_mar(x_fMRI, y_fMRI))
lines(res_lars(lars_fMRI), col = "green")
points(res_c_fMRI[, -1], col = mcols[2], pch = "c")
points(res_s_fMRI[, -1], col = mcols[3], pch = "s")
points(res_k_fMRI[, -1], col = mcols[4], pch = "k")
dev.off()

png('HIV_tvf.png')
plot(res_o_HIV[, -1], col = mcols[1], pch = "o", ylab = "TP", xlab = "FP", xlim = c(0, 120), ylim = c(0, 120))
lines(res_mar(x_HIV, y_HIV))
lines(res_lars(lars_HIV), col = "green")
points(res_c_HIV[, -1], col = mcols[2], pch = "c")
points(res_s_HIV[, -1], col = mcols[3], pch = "s")
points(res_k_HIV[, -1], col = mcols[4], pch = "k")
dev.off()

png('gal_tvf.png')
plot(res_o_gal[, -1], col = mcols[1], pch = "o", ylab = "TP", xlab = "FP")#, xlim = c(0, 120), ylim = c(0, 120))
lines(res_mar(x_gal, y_gal))
lines(res_lars(lars_gal), col = "green")
points(res_c_gal[, -1], col = mcols[2], pch = "c")
points(res_s_gal[, -1], col = mcols[3], pch = "s")
points(res_k_gal[, -1], col = mcols[4], pch = "k")
dev.off()

