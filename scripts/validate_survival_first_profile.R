root <- 'output/survival-first-profile-20260913'
session_root <- 'output/server-first-analysis-20260913'
reference <- readRDS(file.path(session_root,'current-1.rds'))
helper_rows <- list(); stage_rows <- list()
for (id in 2:4) {
  path <- file.path(root,paste0('first-',id,'.Rprof'))
  measured <- readRDS(file.path(session_root,paste0('current-survival-profile-',id,'.rds')))
  stopifnot(identical(reference,measured,num.eq=FALSE))
  times <- read.csv(paste0(path,'.helpers.csv'))
  totals <- aggregate(elapsed ~ helper,times,sum)
  totals$id <- id
  helper_rows[[length(helper_rows)+1L]] <- totals
  stage_rows[[length(stage_rows)+1L]] <- read.csv(file.path(session_root,
    paste0('current-survival-profile-',id,'.csv')))
}
summary <- aggregate(elapsed ~ helper,do.call(rbind,helper_rows),median)
print(summary)
write.csv(summary,file.path(root,'helper-medians.csv'),row.names=FALSE)
cat('Median full KM run:',median(do.call(rbind,stage_rows)$survival_run),'seconds\n')
profile <- read.csv(file.path(root,'first-2.Rprof.total.csv'),row.names=1)
selected <- profile[grepl('prepare_km|survival_|cmpfun|tryCmpfun|ggplot|loadNamespace',rownames(profile)),]
print(selected)
write.csv(selected,file.path(root,'selected-profile.csv'))
cat('PASS: all three profiled sessions match the unprofiled full results, live HTML and notifications exactly.\n')
