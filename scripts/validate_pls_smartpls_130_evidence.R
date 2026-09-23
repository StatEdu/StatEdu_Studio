validate_pls_smartpls_130_evidence <- function(private_root) {
  pub <- file.path('docs','evidence','release_1_3_0','pls','smartpls_4_1_1_8_hs_first100')
  priv <- pls_evidence_resolve_private_file(private_root, 'hs100', '1.3.0 HS100 bundle')
  run <- jsonlite::fromJSON(file.path(pub,'external_run.json'), simplifyVector=TRUE)
  manifest <- jsonlite::fromJSON(file.path(pub,'benchmark_manifest.json'), simplifyVector=TRUE)
  citation <- 'Ringle, C. M., Wende, S., and Becker, J.-M. (2024). SmartPLS 4. Bönningstedt: SmartPLS GmbH. https://www.smartpls.com .'
  stopifnot(identical(run$software_citation,citation),identical(run$citation_basis,'SmartPLS Terms §8.1'),
    grepl('SmartPLS Terms §3.4',run$private_artifact_publication_basis,fixed=TRUE),
    any(grepl(citation,readLines(file.path(pub,'README.md'),encoding='UTF-8',warn=FALSE),fixed=TRUE)))
  stopifnot(identical(run$statedu_version,'1.3.0'), identical(run$finalization_status,'finalized'),
            identical(manifest$statedu$version,'1.3.0'), identical(run$run_date,'2026-09-11'),
            identical(run$profile,manifest$profile$id), identical(run$data_sha256,manifest$data$sha256),
            identical(run$model_sha256,manifest$model$sha256),
            identical(run$source_data_sha256,manifest$data$source_sha256),
            identical(run$settings_contract$initial_weights_strategy,'DEFAULT'),
            identical(run$construct_aliases,list(x='visual',t='textual',s='speed')))
  public_files <- sort(list.files(pub,recursive=TRUE,full.names=TRUE))
  before <- vapply(public_files,pls_evidence_sha256,character(1))
  tracked <- system2('git',c('ls-files','--',gsub('\\\\','/',pub)),stdout=TRUE)
  stopifnot(!any(grepl('\\.(png|jpg|jpeg|splsm)$|\\.settings\\.json$|/smartpls_imported_data\\.txt$',tracked,ignore.case=TRUE)))
  validated <- finalize_pls_external_evidence(pub,run$software,run$version,run$run_date,TRUE,
    run$reported_decimal_places,run$absolute_tolerance,run$relative_tolerance,priv)
  stopifnot(nrow(validated$comparison)==6L,all(validated$comparison$Pass),
    all(validated$comparison$`Absolute error` <= 0.0005 + .Machine$double.eps))
  artifact_names <- c(unlist(run$execution_artifacts[grepl('_file$',names(run$execution_artifacts))]),
    unlist(lapply(run$runs,function(x) x[grepl('_file$',names(x))])))
  stopifnot(length(artifact_names)==14L,length(unique(artifact_names))==14L)
  settings <- jsonlite::fromJSON(file.path(priv,run$execution_artifacts$algorithm_settings_file))
  stopifnot(identical(settings$plsAlgorithmSettings$initialWeightsStrategy,'DEFAULT'),
    identical(settings$plsAlgorithmSettings$initialWeights$lv,rep(c('x','t','s'),each=3)))
  fresh <- tempfile('smartpls130-')
  dir.create(fresh)
  on.exit(unlink(fresh,recursive=TRUE),add=TRUE)
  regenerated <- generate_pls_external_benchmark(fresh,run$profile)
  exact <- pls_fit_compare_external(regenerated$statedu_path,file.path(pub,'statedu_fit.csv'),0,0)
  stopifnot(nrow(exact)==6L,all(exact$Pass),identical(before,vapply(public_files,pls_evidence_sha256,character(1))))
  message('SmartPLS 1.3.0 validation PASS: 14 fresh private artifacts, exact StatEdu regeneration, 6/6 displayed comparisons. Historical TAM is not counted as revalidated.')
}
