out <- 'tmp/t-rank-planning-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
methods <- c(rep('ttest',5),rep('nonparametric',3))
designs <- c('two_sample','paired','one_sample','two_sample','two_sample','two_independent','paired','one_sample')
results <- lapply(seq_along(methods),function(i) {
 m <- methods[i]; values <- list(target=if(i==5)'power' else 'sample_size',design=designs[i],effect='.3',alpha='.05',power='.8',n='100',ratio=if(i %in% 4:5)'2' else '1',alternative='two.sided',dropout='10')
 sample_size_calculate(m,setNames(values,paste0('sample_size_',m,'_',names(values))))
})
formula_keys <- paste0('sample_size.result.',c('note_plan_t_two','note_plan_t_paired','note_plan_t_one','note_plan_t_unequal_n','note_plan_t_unequal_power',rep('note_plan_rank_are',3)))
for(i in seq_along(results))stopifnot(is.null(results[[i]]$error),identical(results[[i]]$method_note,statedu_t(formula_keys[i],'en')))
types <- c('two.sample','paired','one.sample')
for(i in 1:3) {
 n <- ceiling(stats::power.t.test(delta=.3,sd=1,sig.level=.05,power=.8,type=types[i],alternative='two.sided')$n)
 if(i==1)stopifnot(results[[i]]$group1==n,results[[i]]$total==2*n,results[[i]]$adjusted_total==2*ceiling(n/.9)) else stopifnot(results[[i]]$total==n,results[[i]]$adjusted_total==ceiling(n/.9))
}
base_n <- 1.5*(qnorm(.975)+qnorm(.8))^2/.3^2
stopifnot(results[[4]]$group1==ceiling(base_n),results[[4]]$group2==ceiling(2*base_n),isTRUE(all.equal(results[[5]]$power,pnorm(.3*sqrt(100/1.5)-qnorm(.975)))))
stopifnot(results[[6]]$group1==ceiling(results[[1]]$group1/.955),results[[7]]$total==ceiling(results[[2]]$total/.955),results[[8]]$total==ceiling(results[[3]]$total/.955))
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))stopifnot(grepl('ARE = 0.955',statedu_t(formula_keys[6],lang),fixed=TRUE))
cat('PASS eight actual t/rank planning cases; exact t, unequal normal approximation, dropout and ARE references\n')
