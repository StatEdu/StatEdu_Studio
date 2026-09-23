Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()

# Exercise the alternate decoder directly, independently of haven's success.
sav <- tempfile(fileext = ".sav")
fixture <- data.frame(
  score = haven::labelled_spss(c(1, 2, 97, 98, 99, NA),
    labels = c("정상" = 1, "무응답" = 99), na_values = 99,
    na_range = c(97, 98), label = "검사 점수"),
  group = haven::labelled_spss(c("A", "B", "X", "A", "B", "X"),
    labels = c("집단 A" = "A", "제외" = "X"), na_values = "X",
    label = "집단"), check.names = FALSE)
haven::write_sav(fixture, sav)
legacy <- read_sav_legacy(sav)
for (name in names(fixture)) {
  stopifnot(isTRUE(all.equal(as.vector(legacy[[name]]), as.vector(fixture[[name]]))))
  stopifnot(identical(attr(legacy[[name]], "label"), attr(fixture[[name]], "label")))
  stopifnot(isTRUE(all.equal(sort(attr(legacy[[name]], "labels")), sort(attr(fixture[[name]], "labels")))))
  stopifnot(identical(is.na(haven::zap_missing(legacy[[name]])),
                      is.na(haven::zap_missing(fixture[[name]]))))
}
unlink(sav)

# Fisher information calculated at the final coefficients must agree with SEs.
set.seed(819)
d <- data.frame(x = rnorm(600), z = rnorm(600))
d$y <- rbinom(600, 1, plogis(-5 + 2 * d$x - d$z))
fit <- fit_logistic_model(d, "y", c("x", "z"), "binary")$model
X <- model.matrix(fit)
mu <- plogis(drop(X %*% coef(fit)))
information <- crossprod(X, X * (mu * (1 - mu)))
expected_se <- sqrt(diag(solve(information)))
stopifnot(fit$converged, max(abs(expected_se - sqrt(diag(vcov(fit))))) < 1e-7)
cat("SPSS import metadata and logistic precision regressions passed.\n")
