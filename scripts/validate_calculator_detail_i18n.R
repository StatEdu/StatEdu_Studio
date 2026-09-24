Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
doc <- function(ui) xml2::read_html(as.character(ui))
cells <- function(ui, xpath) xml2::xml_text(xml2::xml_find_all(doc(ui), xpath))
languages <- c('en','ko','ja','zh','es','fr','de','vi')
numbers <- cells(hint8_weight_values_table('en'), '//tbody/tr/td[position()>1]')
custom <- '사용자_<script>& %s'
for(lang in languages) {
 for(key in c('tg_transform','race_codes','missing_output','rule','result'))
   stopifnot(nzchar(statedu_t(paste0('calculator.detail.',key),lang,fallback='')))
 stopifnot(identical(numbers,cells(hint8_weight_values_table(lang),'//tbody/tr/td[position()>1]')))
 stopifnot(identical(cells(hint8_output_table(lang),'//td[2]'),'hint8_score'))
 reference <- cells(ascvd10_reference_table(lang),'//td')
 stopifnot(identical(reference[1],calculator_field_label('Race',lang)),
           identical(reference[3],calculator_field_label('Sex',lang)))
 stopifnot(identical(regmatches(reference[2],gregexpr('[123]',reference[2]))[[1]],c('1','2','3')))
 exclusions <- ascvd10_exclusion_table(custom,lang)
 stopifnot(identical(cells(exclusions,'//tbody/tr/td[2]'),c('1','>= 190')),
           identical(cells(exclusions,'//tbody/tr/td[3]'),rep(sprintf(statedu_t('calculator.detail.missing_output',lang),custom),2)),
           length(xml2::xml_find_all(doc(exclusions),'//script'))==0L)
 stopifnot(all(grepl('ascvd10_score',cells(ascvd10_exclusion_table('',lang),'//tbody/tr/td[3]'),fixed=TRUE)))
}
cat('PASS remaining calculator captions: 8 languages, exact HINT8 coefficients, ASCVD codes/rules and escaped custom output names\n')
source('scripts/validate_calculators.R',encoding='UTF-8')
