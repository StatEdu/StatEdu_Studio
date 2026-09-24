Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
literal<-'Review 사용자 <&> %s, edge.1'
messages<-c('Multi-group structural-path scope must be either all or selected.',
 'PLS-MGA path scope must be either all or selected.',
 'Select at least one structural path',
 'selected-path scope requires at least one selected structural path',
 'Selected-path inference requires at least one resolved latent regression path',
 'Model has no latent-to-latent structural path',
 paste0('selected structural paths are missing or no longer valid in the current canvas: ',literal,'. Re-select the paths before analysis.'),
 'duplicate latent regression paths','duplicate structural paths','requires unique, non-empty edge IDs','requires unique structural edge IDs',
 'Selected-path equality constraints did not produce the expected model degrees-of-freedom change')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 result<-vapply(messages,structural_canvas_error_message,character(1),language=language)
 if(language=='en')stopifnot(identical(unname(result),messages)) else stopifnot(all(result!=messages),length(unique(result))==6L)
 stopifnot(grepl(literal,result[7],fixed=TRUE))
 unknown<-paste0('Unrecognized engine failure: ',literal)
 stopifnot(structural_canvas_error_message(simpleError(unknown),language)==unknown)
 cat('PASS:',language,'12 recognized forms, six guidance branches, literal edge IDs and unknown errors\n')
}
