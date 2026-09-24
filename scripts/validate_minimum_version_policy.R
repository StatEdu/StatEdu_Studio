if (.Platform$OS.type == "windows") Sys.setlocale("LC_ALL", "English_United States.utf8")
`%||%` <- function(x, y) if (is.null(x)) y else x
source("R/update_check.R", encoding = "UTF-8")
normalize_app_language <- function(x) x
statedu_initial_language <- function(request = NULL) "ko"
statedu_request_token_authorized <- function(request = NULL) !identical(request, "denied")
statedu_token_rejection_response <- function() "Forbidden"
statedu_t <- function(key, language) key
Sys.setenv(STATEDU_PRODUCT_EDITION = "free")

manifest <- list(latest_version = "1.4.0", minimum_version = "1.3.0")
stopifnot(!statedu_version_policy(manifest, "1.3.0")$blocked,
  !statedu_version_policy(manifest, "1.3.1-dev")$blocked,
  statedu_version_policy(manifest, "1.2.9")$blocked,
  !statedu_version_policy(manifest, "1.10.0")$blocked)
legacy <- list(version = "1.4.0", minimumSupportedVersion = "1.4.0")
stopifnot(statedu_version_policy(legacy, "1.3.0")$blocked)
for (bad in list("garbage", "", NA_character_, c("1.3.0", "1.4.0"), list("1.4.0"), "9.0.0")) {
  candidate <- manifest
  candidate$minimum_version <- bad
  stopifnot(!statedu_version_policy(candidate, "1.3.0")$valid)
}
stopifnot(!statedu_version_policy(legacy, "1.3.0", "pro")$valid)
editions <- list(editions = list(free = legacy,
  pro = list(latest_version = "1.3.0", minimum_version = "1.3.0")))
stopifnot(statedu_version_policy(editions, "1.3.0", "free")$blocked,
  !statedu_version_policy(editions, "1.3.0", "pro")$blocked)

root <- tempfile("version-policy-tests-")
dir.create(root)
fixture <- file.path(root, "manifest.json")
cache <- file.path(root, "cache.rds")
url <- "https://example.test/policy.json"
check_fixture <- function(current_version, ...) statedu_check_update(current_version, fixture)
offline <- function(...) stop("offline")
jsonlite::write_json(legacy, fixture, auto_unbox = TRUE)
result <- statedu_startup_update_policy("1.3.0", url, cache, check_fixture)
stopifnot(result$status == "update_required", result$policy_source == "network")
cached <- statedu_startup_update_policy("1.3.0", url, cache, offline)
stopifnot(cached$status == "update_required", cached$policy_source == "cache")
stopifnot(statedu_startup_update_policy("1.4.0", url, cache, offline)$status == "current",
  statedu_startup_update_policy("1.3.0", paste0(url, "other"), cache, offline)$status == "error")
# Invalid server policies cannot replace the last valid policy.
jsonlite::write_json(list(version = "1.4.0", minimum_version = "9.0.0"), fixture, auto_unbox = TRUE)
stopifnot(statedu_startup_update_policy("1.3.0", url, cache, check_fixture)$status == "update_required")
# A valid rollback removes the restriction, including for later offline starts.
jsonlite::write_json(manifest, fixture, auto_unbox = TRUE)
stopifnot(statedu_startup_update_policy("1.3.0", url, cache, check_fixture)$status == "update_available",
  statedu_startup_update_policy("1.3.0", url, cache, offline)$status == "current")
writeLines("damaged cache", cache)
stopifnot(statedu_startup_update_policy("1.3.0", url, cache, offline)$status == "error")
unlink(cache)
stopifnot(statedu_startup_update_policy("1.3.0", url, cache, offline)$status == "error",
  statedu_startup_update_policy("1.3.0", "http://example.test", cache,
    function(...) stop("must not be called"))$status == "error")
jsonlite::write_json(editions, fixture, auto_unbox = TRUE)
stopifnot(check_fixture("1.3.0")$status == "update_required")
Sys.setenv(STATEDU_PRODUCT_EDITION = "pro")
stopifnot(check_fixture("1.3.0")$status == "current")
Sys.setenv(STATEDU_PRODUCT_EDITION = "free")

guard <- statedu_guarded_application(result, stop("Normal UI must not initialize"),
  stop("Analysis handlers must not initialize"))
stopifnot(is.null(guard$server(NULL, NULL, NULL)), identical(guard$ui("denied"), "Forbidden"))
html <- as.character(guard$ui(NULL))
stopifnot(grepl("1.4.0", html, fixed = TRUE), grepl("https://studio.statedu.com/download/", html, fixed = TRUE))
normal <- statedu_guarded_application(list(status = "current"), "normal-ui", "normal-server")
stopifnot(normal$ui == "normal-ui", normal$server == "normal-server")
unlink(root, recursive = TRUE)
cat("PASS: minimum version policy, edition separation, cache/offline/rollback, server gate and notice rendering\n")
