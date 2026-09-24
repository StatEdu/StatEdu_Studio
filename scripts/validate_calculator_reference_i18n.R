Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
languages<-c('en','ko','ja','zh','es','fr','de','vi')
doc<-function(ui)xml2::read_html(as.character(ui))
numeric_cells<-function(ui)xml2::xml_text(xml2::xml_find_all(doc(ui),'//tbody/tr/td[position()>1]'))
eq_data<-setNames(as.data.frame(matrix(c(rep(1,5),rep(2,5)),nrow=2,byrow=TRUE)),paste0('사용자',1:5))
for(type in c('3L','5L'))for(country in unname(eq5d_value_set_catalog(type))) {
 input<-c(as.list(setNames(names(eq_data),eq5d_item_specs()$id)),list(eq5d_type=type,eq5d_value_set=country,
   eq5d_profile_11111_as_one=FALSE,eq5d_options_tab='eq5d_values_tab',eq5d_output_var='사용자_<&> %s'))
 reference<-serialize(eq5d_reference_values(type,country),NULL)
 scores<-eq5d_score(eq_data,type,country,FALSE)
 numbers<-numeric_cells(eq5d_reference_table(type,country,'en'))
 for(lang in languages) {
   choices<-eq5d_value_set_choices(type,lang)
   stopifnot(identical(unname(choices),unname(eq5d_value_set_catalog(type))))
   stopifnot(all(vapply(unname(choices),function(code)nzchar(statedu_t(paste0('calculator.country.',code),lang,fallback='')),logical(1))))
   ui<-doc(shiny::isolate(eq5d_setup_ui(list(name='fixture'),eq_data,NULL,input,language=lang)))
   stopifnot(xml2::xml_attr(xml2::xml_find_first(ui,'//select[@id="eq5d_value_set"]/option[@selected]'),'value')==country,
     xml2::xml_attr(xml2::xml_find_first(ui,'//select[@id="eq5d_type"]/option[@selected]'),'value')==type,
     length(xml2::xml_find_all(ui,'//input[@id="eq5d_profile_11111_as_one"][@checked]'))==0L,
     length(xml2::xml_find_all(ui,'//li[contains(@class,"active")]/a[@data-value="eq5d_values_tab"]'))==1L,
     identical(numbers,numeric_cells(eq5d_reference_table(type,country,lang))),
     identical(reference,serialize(eq5d_reference_values(type,country),NULL)),identical(scores,eq5d_score(eq_data,type,country,FALSE)))
 }
}
cat('PASS EQ-5D 29 type/country combinations x 8 languages: codes, tab, unchecked profile, table values and scores preserved\n')
for(preset in names(metabolic_reference_sets()))for(lang in languages) {
 input<-list(metabolic_reference_set=preset,metabolic_ref_wc_m=91.25,metabolic_ref_wc_f=81.5,
   metabolic_wc_unit='inch',metabolic_lipid_unit='mmol_l',metabolic_glucose_unit='mmol_l')
 before<-serialize(metabolic_reference_inputs(input),NULL)
 choices<-metabolic_reference_set_choices(lang)
 stopifnot(identical(unname(choices),names(metabolic_reference_sets())))
 controls<-doc(shiny::isolate(metabolic_reference_controls(input,lang)))
 stopifnot(xml2::xml_attr(xml2::xml_find_first(controls,'//input[@id="metabolic_ref_wc_m"]'),'value')=='91.25',
   xml2::xml_attr(xml2::xml_find_first(controls,'//input[@id="metabolic_ref_wc_f"]'),'value')=='81.5')
 table<-doc(metabolic_reference_table(input,lang));text<-xml2::xml_text(table)
 refs<-metabolic_reference_inputs(input)
 for(value in unlist(refs))stopifnot(grepl(as.character(value),text,fixed=TRUE))
 diagnosis<-xml2::xml_text(xml2::xml_find_first(table,'//tbody/tr[last()]/td[2]'))
 expected<-strsplit(statedu_t(paste0('calculator.reference.',if(preset=='japan')'japan_rule' else 'count_rule'),lang),'\n',fixed=TRUE)[[1]]
 for(part in expected)stopifnot(grepl(part,diagnosis,fixed=TRUE))
 stopifnot(identical(before,serialize(metabolic_reference_inputs(input),NULL)))
}
cat('PASS metabolic 5 presets x 8 languages: diagnosis text, exact cutoff values and custom numeric inputs\n')
