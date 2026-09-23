Sys.setlocale('LC_CTYPE', 'Korean_Korea.utf8')
source('R/app_bootstrap.R', encoding = 'UTF-8')
load_app_packages(check = FALSE)
source_app_modules()
set.seed(738)
g <- factor(rep(1:2, each = 20))
y <- matrix(rnorm(120), 40, 3)
y[, 3] <- y[, 3] * 2
shifted <- y
shifted[g == 2, ] <- sweep(shifted[g == 2, ], 2, c(15, -20, 30), '+')
a <- mixed_rm_group_sphericity(y, g)
b <- mixed_rm_group_sphericity(shifted, g)
# Adding a group-specific mean profile cannot change within-group covariance.
stopifnot(max(abs(unlist(a[c('w', 'p', 'epsilon', 'hf')]) -
                  unlist(b[c('w', 'p', 'epsilon', 'hf')]))) < 1e-10)
stopifnot(abs(paired_rm_sphericity(y)$epsilon - paired_rm_sphericity(shifted)$epsilon) > .01)
# Check the displayed ANOVA uses the same corrected sphericity object.
display <- mixed_rm_formatted_anova(shifted, g)
stopifnot(identical(display[['epsilon(GG)']][display$Effect == 'Time'], format_decimal3(b$epsilon)))
stopifnot(identical(display[['epsilon(HF)']][display$Effect == 'Time'], format_decimal3(b$hf)))
cat('Mixed RM within-group sphericity invariance passed.\n')
