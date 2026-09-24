source('scripts/fixtures_advanced_planning_i18n.R',encoding='UTF-8')
results <- results[1:3];designs <- designs[1:3];formula_keys <- formula_keys[1:3]
results <- c(results,list(sample_size_calculate('cluster',modifyList(cinput,list(sample_size_cluster_target='power'))),sample_size_calculate('cluster',modifyList(cinput,list(sample_size_cluster_target='power',sample_size_cluster_outcome='binary')))))
stopifnot(results[[2]]$clusters_group1==ceiling(results[[2]]$raw_total_clusters/2),results[[2]]$clusters_group1==results[[2]]$clusters_group2,results[[2]]$total_clusters==2*results[[2]]$clusters_group1,results[[2]]$total==10*results[[2]]$total_clusters)
designs <- c(designs,'WebPower-power','binary-DE-power');formula_keys<-c(formula_keys,formula_keys[2:3])
out <- 'tmp/cluster-planning-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
keys <- paste0('sample_size.result.',c('note_cluster_simulations','note_cluster_webpower','note_cluster_design_effect','note_cluster_webpower','note_cluster_design_effect'))
clean <- function(x)gsub('[[:space:]\u00a0]+','',x,perl=TRUE)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in 1:5){
 stopifnot(is.null(results[[i]]$error))
 template<-statedu_t(keys[i],lang);expected<-if(i==1)sprintf(template,'20') else if(i %in% c(3,5))sprintf(template,'1.450') else template
 stopifnot(identical(sample_size_result_text(results[[i]]$method_note,lang),expected))
 actual<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(results[[i]],lang))))
 for(part in trimws(strsplit(result_sci_note_text(estimation=expected),';',fixed=TRUE)[[1]]))stopifnot(grepl(clean(sub('[.。]$','',part)),clean(actual),fixed=TRUE))
}
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(j in c(1,3)){
 token<-if(j==1)'00020' else '001.450';en<-sprintf(statedu_t(keys[j],'en'),token)
 stopifnot(identical(sample_size_result_text(en,lang),sprintf(statedu_t(keys[j],lang),token)),identical(sample_size_result_text(paste0(en,' extra'),lang),paste0(en,' extra')))
}
cat('PASS five cluster engine/n/power cases and exact dynamic token preservation across eight languages\n')
