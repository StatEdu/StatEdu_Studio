penalized_menu_methods <- function() c(ridge="Ridge",lasso="LASSO",elastic_net="Elastic Net")
penalized_error_ui_text <- function(message, language = statedu_initial_language()) {
  keys <- c(
    "연속형 종속변수를 선택하세요. / Select continuous outcomes." = "outcome",
    "연속형 종속변수를 선택하세요. / The outcome must be continuous." = "outcome_type",
    "독립변수를 선택하세요. / Select predictors." = "predictors",
    "부트스트랩 반복수는 1~5000입니다. / Bootstrap resamples: 1–5000." = "resamples",
    "STATEDU_PENALIZED_BOOTSTRAP_WORKERS/workers must be a positive integer." = "workers",
    "Package 'glmnet' is required. Install it with install.packages(\"glmnet\")." = "package",
    "glmnet was unavailable in a penalized-bootstrap worker." = "worker_package",
    "Unknown penalized regression method." = "method",
    "Penalized regression requires at least 4 complete cases for cross-validation." = "cases_cv",
    "Insufficient finite training data or constant outcome." = "training",
    "No valid tuning fit." = "tuning",
    "중첩 교차검증에는 완전한 사례가 최소 12개 필요합니다. / Nested CV requires at least 12 complete cases." = "nested_cases",
    "Nested CV produced incomplete predictions." = "predictions",
    "중첩 교차검증 반복수는 1~20의 정수입니다. / Nested CV repetitions must be an integer from 1 to 20." = "nested_repeats",
    "Nested CV produced non-finite repeat metrics." = "metrics",
    "Multi-split inference is available for LASSO and Elastic Net only." = "split_method",
    "선택 후 검정에는 완전한 사례가 최소 20개 필요합니다. / Multi-split inference requires at least 20 complete cases." = "split_cases",
    "Multi-split repetitions must be an integer from 20 to 500." = "split_repeats")
  if(length(message)!=1L || is.na(message)) return(message)
  key <- unname(keys[message])
  if(is.na(key)) return(message)
  statedu_t(paste0("analysis.penalized_error.",key),language)
}
penalized_menu_title <- function(key,language=statedu_initial_language()) {
  if(identical(key,"regularized"))return(analysis_scope_text(language,"릿지·라소·엘라스틱넷","Ridge / LASSO / Elastic Net"))
  if(identical(normalize_app_language(language),"ko")) return(c(ridge="릿지 회귀",lasso="라소 회귀",elastic_net="엘라스틱넷 회귀")[[key]])
  paste(penalized_menu_methods()[[key]],"Regression")
}

penalized_menu_panel <- function(key,language=statedu_initial_language()) {
  prefix<-paste0("penalized_",key)
  div(class="page-shell",h2(penalized_menu_title(key,language)),
    div(class="workspace-panel analysis-three-block-workspace",uiOutput(paste0(prefix,"_setup")),
      analysis_three_block_action_row(class="hierarchical-action-row",run_button=actionButton(paste0("run_",prefix),analysis_scope_text(language,"분석 실행","Run analysis"),class="btn-primary"),save_control=uiOutput(paste0(prefix,"_save")))),
    div(class="regression-results",uiOutput(paste0(prefix,"_results"))))
}

prepare_penalized_menu <- function(data,outcome,predictors,method,info=NULL,labels=character(),categories=NULL,seed=default_seed(),resamples=500L,post_selection=FALSE,inference_splits=50L,validation_repeats=5L) {
  excluded<-analysis_scope_excluded(data)
  outcome<-setdiff(intersect(outcome,names(data)),excluded)
  predictors<-setdiff(intersect(predictors,names(data)),c(outcome,excluded))
  if(!length(outcome))stop("연속형 종속변수를 선택하세요. / Select continuous outcomes.")
  if(any(!vapply(outcome,function(v)is.numeric(data[[v]]),logical(1))) || (is.data.frame(info)&&any(info$measurement[match(intersect(outcome,info$name),info$name)]%in%setdiff(unique(info$measurement),c("continuous","scale","numeric"))))) stop("연속형 종속변수를 선택하세요. / The outcome must be continuous.")
  if(!length(predictors))stop("독립변수를 선택하세요. / Select predictors.")
  if(!is.finite(resamples)||resamples<1||resamples>5000)stop("부트스트랩 반복수는 1~5000입니다. / Bootstrap resamples: 1–5000.")
  model_data<-prepare_regression_model_data_static(data,c(outcome,predictors),info,regression_reference_values_static(categories))
  reference<-lapply(outcome,function(y){
    formula<-make_formula(y,predictors)
    baseline<-lm(formula,data=model_data,na.action=na.omit)
    list(formula=formula,coef_table=data.frame(Term=names(coef(baseline)),B=unname(coef(baseline))))
  })
  fit_penalized_models(reference,model_data,info,labels,seed=seed,category_table=categories,
    selection_bootstrap_resamples=as.integer(resamples),methods=method,nested_validation=TRUE,
    post_selection=isTRUE(post_selection)&&method!="Ridge",inference_splits=inference_splits,validation_repeats=validation_repeats)
}

register_penalized_menu <- function(key,input,output,session,dataset_fn,selected_names_fn,info_fn,labels_fn,categories_fn,language_fn) {
  prefix<-paste0("penalized_",key);id<-function(s)paste0(prefix,"_",s)
  tr<-function(ko,en)analysis_scope_text(language_fn(),ko,en)
  outcome<-reactiveVal(character());predictors<-reactiveVal(character());result<-analysis_scope_result_val(NULL)
  output[[id("setup")]]<-renderUI({
    data<-dataset_fn();req(is.data.frame(data));variables<-intersect(selected_names_fn(),names(data))
    items<-function(x)analysis_variable_items(x,info_fn(),labels_fn())
    target_panel<-function(role,title,values,size,measurements,panel_class) {
      panel<-hierarchical_target_panel(paste0(title," (",length(values),")"),id(role),items(values),
        isolate(input[[id(role)]])%||%character(),size,id(paste0(role,"_up")),id(paste0(role,"_down")),measurements,language_fn())
      panel$attribs$class<-paste(panel$attribs$class,panel_class)
      panel
    }
    div(class="hierarchical-setup-grid penalized-menu-grid",
      div(class="analysis-transfer-column analysis-transfer-panel",
        analysis_field_label_tag("Variables",language=language_fn()),
        analysis_transfer_listbox_input(id("available"),items(setdiff(variables,c(outcome(),predictors()))),size=17)),
      div(class="hierarchical-target-stack hierarchical-target-stack-compact",
        div(class="hierarchical-target-row hierarchical-dependent-row",
          div(class="hierarchical-target-move-cell",analysis_variable_move_button(id("move_y"))),
          target_panel("y",tr("종속변수","Dependent variables"),outcome(),3,"continuous","hierarchical-dependent-panel")),
        div(class="hierarchical-target-row hierarchical-active-block-row hierarchical-active-block1",
          div(class="hierarchical-target-move-cell",analysis_variable_move_button(id("move_x"))),
          target_panel("x",tr("독립변수","Independent variables"),predictors(),7,analysis_allowed_measurements_all(),"hierarchical-block1-panel"))),
      div(class="analysis-options-panel",style="overflow-y:auto;",div(class="analysis-field-label",tr("분석 옵션","Options")),
        radioButtons(id("method"),tr("분석 방법","Method"),choices=setNames(names(penalized_menu_methods()),c(tr("릿지 (α = 0)","Ridge (α = 0)"),tr("라소 (α = 1)","LASSO (α = 1)"),tr("엘라스틱넷 (α 교차검증 탐색)","Elastic Net (cross-validated α)"))),selected=isolate(input[[id("method")]])%||%"ridge"),
        numericInput(id("bootstrap"),tr("선택 안정성 부트스트랩 반복수","Selection stability bootstrap resamples"),isolate(input[[id("bootstrap")]])%||%500,min=1,max=5000,step=100),
        numericInput(id("seed"),tr("난수 시드","Random seed"),isolate(input[[id("seed")]])%||%default_seed(),min=1),
        numericInput(id("validation_repeats"),tr("중첩 교차검증 반복수","Nested CV repetitions"),isolate(input[[id("validation_repeats")]])%||%5,min=1,max=20,step=1),
        p(tr("예측 성능: 외부 5폴드·내부 최대 10폴드 중첩 교차검증의 반복 평균. 분할 간 변동은 신뢰구간이 아닙니다. 본표 계수는 전체 자료의 λ.1se입니다.","Performance: repeated outer 5-fold nested CV, up to 10 inner folds. Split variability is not a confidence interval. Main coefficients use full-data λ.1se.")),
        conditionalPanel(paste0("input.",id("method")," !== 'ridge'"),
          checkboxInput(id("post_selection"),tr("선택 후 유의성 검정","Post-selection inference"),isTRUE(isolate(input[[id("post_selection")]]))),
          conditionalPanel(paste0("input.",id("post_selection")),
            numericInput(id("inference_splits"),tr("표본 분할 반복수","Sample-split repetitions"),isolate(input[[id("inference_splits")]])%||%50,min=20,max=500,step=10),
            p(tr("50:50 분할로 선택과 검정을 분리합니다. 범주별 검정과 범주형 변수 전체 검정의 보정·통합 p값을 별도 표에 제시합니다.","50:50 splits separate screening and testing. Individual-contrast and omnibus categorical p-values appear in separate tables."))))))
  })
  active_list<-reactiveVal("available")
  for(role in c("y","x"))local({r<-role
    register_analysis_reorder(input,session,id(r),function(payload){
      target<-if(r=="y")outcome else predictors
      updated<-analysis_reorder_items(target(),payload)
      if(isTRUE(updated$changed))target(updated$order)
    })
  })
  for(role in c("y","x"))local({r<-role
    observe(updateActionButton(session,id(paste0("move_",r)),label=analysis_variable_move_label(active_list(),r,input[[id(r)]])))
  })
  for(role in c("available","y","x"))local({r<-role
    observeEvent(input[[id(r)]],{if(length(input[[id(r)]]))active_list(r)},ignoreInit=TRUE)
  })
  transfer<-function(role) {
    target<-if(role=="y")outcome else predictors
    if(identical(active_list(),role))target(setdiff(target(),input[[id(role)]])) else {
      values<-setdiff(intersect(input[[id("available")]],names(dataset_fn())),c(outcome(),predictors()))
      if(role=="y")values<-Filter(function(v)is.numeric(dataset_fn()[[v]]) && (!v%in%info_fn()$name||info_fn()$measurement[match(v,info_fn()$name)]=="continuous"),values)
      target(union(target(),values))
    }
  }
  observeEvent(input[[id("move_y")]],transfer("y"))
  observeEvent(input[[id("move_x")]],transfer("x"))
  for(role in c("y","x"))for(direction in c("up","down"))local({r<-role;d<-direction
    observeEvent(input[[id(paste0(r,"_",d))]],{
      target<-if(r=="y")outcome else predictors
      moved<-move_order_item(target(),input[[id(r)]],d)
      if(isTRUE(moved$changed))target(moved$order)
    })
  })
  for(role in c("y","x"))local({r<-role
    observeEvent(input[[id(paste0(r,"_doubleclick"))]],{
      target<-if(r=="y")outcome else predictors
      target(setdiff(target(),input[[id(paste0(r,"_doubleclick"))]]$value))
    },ignoreInit=TRUE)
  })
  register_analysis_command_handler(paste0("run_",prefix),input,output,session,
    states=list(outcome=outcome,predictors=predictors),dataset_fn=dataset_fn,
    context_fn=function()list(selected=selected_names_fn(),variables=info_fn(),labels=labels_fn(),categories=categories_fn()),
    run_fn=function(){
      value<-withProgress(message=penalized_menu_title(key,language_fn()),value=.2,
        tryCatch(prepare_penalized_menu(dataset_fn(),outcome(),predictors(),penalized_menu_methods()[[input[[id("method")]]%||%"ridge"]],info_fn(),labels_fn(),categories_fn(),
          input[[id("seed")]]%||%default_seed(),input[[id("bootstrap")]]%||%500,
          isTRUE(input[[id("post_selection")]]),input[[id("inference_splits")]]%||%50,input[[id("validation_repeats")]]%||%5),
          error=function(e) stop(penalized_error_ui_text(conditionMessage(e),language_fn()),call.=FALSE)))
      value$display_title<-penalized_menu_title(input[[id("method")]]%||%"ridge",language_fn());value$cv_plot_id<-id("cv_plot");value$path_plot_id<-id("path_plot")
      result(value)
    })
  output[[id("results")]]<-renderUI(penalized_result_block(result()))
  output[[id("cv_plot")]]<-renderPlot({req(result());plot_penalized_cv_curve(result())},res=96)
  output[[id("path_plot")]]<-renderPlot({req(result());plot_penalized_coefficient_path(result())},res=96)
  output[[id("save")]]<-renderUI({req(result());analysis_save_buttons(
    html_button_id=paste0("save_",prefix,"_html_dialog"),pdf_button_id=paste0("save_",prefix,"_pdf_dialog"),
    excel_button_id=paste0("save_",prefix,"_excel_dialog"),figure_button_id=paste0("save_",prefix,"_figures_dialog"),add_result_button_id=paste0("add_",prefix,"_result"),has_figures=TRUE,language=language_fn())})
  observeEvent(input[[paste0("save_",prefix,"_figures_dialog")]],{
    req(result());session$sendCustomMessage("easyflow-capture-result-snapshot",list(outputId=id("results"),inputId=id("picture_snapshot")))
  })
  observeEvent(input[[id("picture_snapshot")]],{
    tryCatch({
      payload<-input[[id("picture_snapshot")]];if(nzchar(payload$error%||%""))stop(payload$error)
      images<-xml2::xml_find_all(xml2::read_html(payload$html),"//img[starts-with(@src,'data:image/png')]")
      req(length(images));directory<-choose_figure_save_dir();if(!length(directory)||!nzchar(directory[[1]]))return()
      files<-lapply(seq_along(images),function(i)list(name=paste0("figure_",i,".png"),data=xml2::xml_attr(images[[i]],"src")))
      save_canvas_figure_snapshots(files,directory[[1]],prefix)
    },error=function(e)showNotification(conditionMessage(e),type="error"))
  },ignoreInit=TRUE)
  register_canvas_report_exports(input,session,paste0("save_",prefix,"_html_dialog"),paste0("save_",prefix,"_pdf_dialog"),id("results"),NULL,
    function()penalized_menu_title(key,language_fn()),result,language_fn,excel_id=paste0("save_",prefix,"_excel_dialog"))
  register_add_result_snapshot(input,session,paste0("add_",prefix,"_result"),function()penalized_menu_title(key,language_fn()),id("results"),app_language_fn=language_fn)
  invisible(result)
}
