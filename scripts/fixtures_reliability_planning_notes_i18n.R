out <- 'tmp/reliability-planning-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base <- list(sample_size_reliability_target='sample_size',sample_size_reliability_value='.8',sample_size_reliability_confidence='.95',sample_size_reliability_half_width='.1',sample_size_reliability_items='5',sample_size_reliability_categories='2',sample_size_reliability_dropout='10')
designs <- c('bland_altman','alpha','icc','kappa','alpha-minimum')
results <- c(lapply(designs[1:4],function(d)sample_size_calculate('reliability',modifyList(base,list(sample_size_reliability_design=d)))),list(sample_size_calculate('reliability',modifyList(base,list(sample_size_reliability_design='alpha',sample_size_reliability_half_width='.5',sample_size_reliability_items='20')))))
formula_keys <- paste0('sample_size.result.',c('planning_reliability_ba','planning_reliability_alpha','planning_reliability_icc','planning_reliability_kappa','planning_reliability_alpha'))
keys <- paste0('sample_size.result.',c('note_reliability_ba','note_reliability_alpha_items','note_reliability_icc_items','note_reliability_kappa','note_reliability_alpha_items'))
z<-qnorm(.975);expected_n<-c(ceiling((z*sqrt(3)*.8/.1)^2),max(6,ceiling(2*5/4*(z/(.1/.2))^2+2)),ceiling((z/(.1/(1-.8^2)))^2/5+2),ceiling(z^2*.9*.1/.5^2/.1^2),21)
for(i in 1:5)stopifnot(is.null(results[[i]]$error),results[[i]]$total==expected_n[i],results[[i]]$adjusted_total==ceiling(expected_n[i]/.9))
clean<-function(x)gsub('[[:space:]\u00a0]+','',x,perl=TRUE)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(i in 1:5){template<-statedu_t(keys[i],lang);expected<-if(i %in% c(2,3,5))sprintf(template,if(i==5)'20' else '5') else template
 stopifnot(identical(sample_size_result_text(results[[i]]$method_note,lang),expected))
 actual<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(results[[i]],lang))))
 for(part in trimws(strsplit(result_sci_note_text(estimation=expected),';',fixed=TRUE)[[1]]))stopifnot(grepl(clean(sub('[.。]$','',part)),clean(actual),fixed=TRUE))
 }
 for(key in keys[2:3]){en<-sprintf(statedu_t(key,'en'),'0005');stopifnot(identical(sample_size_result_text(en,lang),sprintf(statedu_t(key,lang),'0005')),identical(sample_size_result_text(paste0(en,' extra'),lang),paste0(en,' extra')))}
}
cat('PASS five reliability planning cases, independent sizes/dropout/minimum-n and eight-language count preservation\n')
