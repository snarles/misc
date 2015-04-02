lambdas <- 0:1000/40000
misc_errors <- readRDS('misc_error.rds')/25
pr_errors <- readRDS('pr_error.rds')

dim(misc_errors)
ses <- apply(misc_errors, 2, sd)/sqrt(200)
mus <- apply(misc_errors, 2, mean)

pdf('../paper/exp1_1.pdf')
plot(lambdas, mus, type = 'l', ylim = c(.5, .8), ylab = 'misclass.', xlab = expression(lambda))
title('Identification')
lines(lambdas, mus + ses, lty = 2)
lines(lambdas, mus - ses, lty = 2)
abline(v = lambdas[mus == min(mus)])
dev.off()

pdf('../paper/exp1_2.pdf')
pmus <- apply(pr_errors, 2, mean)
pses <- apply(pr_errors, 2, sd)/sqrt(200)
plot(lambdas, pmus, type = 'l', ylab = 'test SSE', xlab = expression(lambda))
title('Regression')
lines(lambdas, pmus + pses, lty = 2)
lines(lambdas, pmus - pses, lty = 2)
abline(v = lambdas[pmus == min(pmus)])
dev.off()


dim(misc_errors)
misc_min_lam <- apply(misc_errors, 1, function(v) min(lambdas[v == min(v)]))
misc_max_lam <- apply(misc_errors, 1, function(v) max(lambdas[v == min(v)]))
pr_lam <- apply(pr_errors, 1, function(v) lambdas[v == min(v)])
sum(misc_max_lam < pr_lam)/200
sum(misc_min_lam < pr_lam)/200
sum(misc_max_lam > pr_lam)/200
sum(misc_min_lam > pr_lam)/200

