Sys.setlocale('LC_CTYPE', 'Korean_Korea.utf8')
source('R/app_bootstrap.R', encoding = 'UTF-8')
load_app_packages(check = FALSE)
source_app_modules()

# Tied, small samples expose a material mismatch between z and corrected p.
d <- data.frame(y = c(1, 2, 2, 3, 3, 4, 4, 5), g = rep(1:2, each = 4))
vi <- data.frame(name = c('y', 'g'), measurement = c('continuous', 'binary'))
fit <- ttest_single_result(d, 'y', 'g', vi, character(), NULL,
                          list(force_nonparametric = TRUE))
z <- ttest_mann_whitney_z(d$y, d$g)
expected <- 2 * pnorm(-abs(z))
stopifnot(identical(fit$table$p[1], format_p(expected)))
stopifnot(!identical(fit$table$p[1], format_p(wilcox.test(y ~ g, d, exact = FALSE)$p.value)))
# Reversing group order changes direction but preserves two-sided inference.
d$g <- 3 - d$g
reversed <- ttest_single_result(d, 'y', 'g', vi, character(), NULL,
                               list(force_nonparametric = TRUE))
stopifnot(identical(reversed$table$p[1], fit$table$p[1]))
stopifnot(abs(ttest_mann_whitney_z(d$y, d$g) + z) < 1e-12)
cat('Mann-Whitney displayed z/p consistency passed.\n')
