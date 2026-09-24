Sys.setlocale('LC_CTYPE', 'English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R', encoding='UTF-8')
load_app_packages(check=FALSE); source_app_modules()
# Reuse actual invalid-data fixtures and engine regressions, with full app dependencies.
eval(parse('scripts/validate_survival_preflight.R', encoding='UTF-8'), envir=.GlobalEnv)
audits <- Filter(function(x) is.list(x) && is.data.frame(x$issues) &&
  all(c('severity','code','message') %in% names(x$issues)) && identical(x$ok,FALSE),
  mget(ls(envir=.GlobalEnv), envir=.GlobalEnv))
required <- c('invalid_time_encoding','invalid_event_map','unsupported_other_state',
  'entry_not_before_exit','start_not_before_stop','overlapping_intervals',
  'interval_after_event','multiple_subject_events','missing_subject_id_value')
observed <- unique(unlist(lapply(audits,function(x) x$issues$code)))
stopifnot(all(required %in% observed))
for (lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  for (audit in audits) {
    before <- audit
    error <- tryCatch(survival_preflight_stop(audit), error=identity)
    blocking <- audit$issues[audit$issues$severity %in% c('error','block'),,drop=FALSE]
    expected <- vapply(seq_len(nrow(blocking)),function(i) {
      row <- blocking[i,,drop=FALSE]
      base <- survival_input_error_text(simpleError(row$message),lang)
      if (!identical(base,row$message)) return(base)
      survival_issue_text(row$code,row$message,lang)
    },character(1))
    stopifnot(identical(survival_input_error_text(error,lang),paste(unique(expected),collapse=' ')),
      identical(audit,before))
    if(lang=='en')stopifnot(identical(survival_input_error_text(error,lang),conditionMessage(error)))
    for(i in which(blocking$code %in% required)) {
      translated <- survival_issue_text(blocking$code[i],blocking$message[i],lang)
      if(lang!='en')stopifnot(nzchar(translated),translated!=blocking$message[i])
    }
  }
  raw <- '사용자 <&> %s. Time values must be finite and nonnegative.'
  custom <- list(ok=FALSE,issues=survival_issue('block','external_custom',variable=raw,message=raw))
  error <- tryCatch(survival_preflight_stop(custom),error=identity)
  stopifnot(identical(survival_input_error_text(error,lang),raw))
  cat('PASS:',lang,length(audits),'actual blocked preflight fixtures; localized notifications; unchanged audit and external text\n')
}
