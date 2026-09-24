register_canvas_score_editor <- function(input, output, session, dataset_fn, prefix, canvas_input, analysis_type, fit_result, mark_settings_dirty, app_language_fn) {
  if (!analysis_type %in% c("cfa","cbsem","sem")) return(invisible(NULL))
  id <- function(x)paste0(prefix,"_score_",x)
  tr <- function(ko,en)canvas_score_text(en,statedu_current_language(app_language_fn))
  request <- shiny::reactiveVal(NULL)
  preview <- shiny::reactiveVal(NULL)
  chosen <- shiny::reactiveVal(character())
  allowed <- shiny::reactiveVal(character())
  output[[id("item_list")]] <- shiny::renderUI({
    values <- chosen()
    shiny::tagList(
      shiny::p(paste(tr("선택한 원문항:","Selected items:"),length(values))),
      analysis_transfer_listbox_input(id("items"),lapply(values,function(v)list(value=v,label=v,measurement="scale")),
        selected=shiny::isolate(input[[id("items")]]),size=6))
  })
  shiny::observeEvent(input[[id("add_items")]], {
    values <- intersect(as.character(input[[id("add_items")]]$values),allowed())
    chosen(unique(c(chosen(),values))); preview(NULL)
  },ignoreInit=TRUE)
  shiny::observeEvent(input[[id("remove_items")]], {
    chosen(setdiff(chosen(),input[[id("items")]] %||% character())); preview(NULL)
  },ignoreInit=TRUE)
  register_analysis_reorder(input,session,id("items"),function(payload) {
    updated <- analysis_reorder_items(chosen(),payload)
    if(isTRUE(updated$changed)){chosen(updated$order);preview(NULL)}
  })
  shiny::observeEvent(input[[id("close")]], {
    shiny::removeUI(paste0("#",id("panel")));request(NULL);preview(NULL)
  },ignoreInit=TRUE)
  config <- function() {
    items <- chosen()
    list(mode=input[[id("mode")]] %||% "single",items=as.list(items),
      method=input[[id("method")]] %||% "omega",scoring=input[[id("scoring")]] %||% "mean",
      minResponse=if(identical(input[[id("scoring")]] %||% "mean","mean"))as.numeric(input[[id("min_response")]] %||% .8) else 1,
      allocationMethod="loading_balance",count=as.numeric(input[[id("count")]] %||% 3L),
      reviewed=isTRUE(input[[id("reviewed")]]),rationale=input[[id("rationale")]] %||% "")
  }
  output[[id("allocation")]] <- shiny::renderUI({
    if(!identical(input[[id("mode")]],"parcels"))return(NULL)
    if(length(chosen())<2L)return(shiny::p(tr("원문항을 먼저 선택하세요.","Select original items first.")))
    tryCatch({
      allocation <- canvas_parcel_balance(dataset_fn(),chosen(),input[[id("count")]] %||% 3L,diagnostics=TRUE)
      canvas_parcel_allocation_ui(allocation,statedu_current_language(app_language_fn))
    },error=function(e)shiny::div(class="text-danger",conditionMessage(e)))
  })
  output[[id("preview")]] <- shiny::renderUI({
    value <- preview()
    if(is.null(value))return(NULL)
    if(!is.null(value$error))return(shiny::div(class="text-danger",value$error))
    shiny::div(class="canvas-score-preview-ready",shiny::p(tr("계산 완료. 아래 내용을 확인하고 모형에 적용하세요.","Calculated. Review the details before applying.")),
      canvas_score_audit_ui(value$snapshot,statedu_current_language(app_language_fn)))
  })
  shiny::observeEvent(input[[paste0(prefix,"_canvas_score_request")]], {
    req <- input[[paste0(prefix,"_canvas_score_request")]]
    tryCatch({
      target <- canvas_score_target(req$snapshot,req$nodeId)
      if(length(unique(target$ids))!=1L)stop(tr("측정변수가 하나인 잠재변수를 선택하세요.","Select a latent variable with exactly one indicator."))
      data <- dataset_fn(); shiny::req(is.data.frame(data))
      design <- target$latent$scoreDesign %||% list()
      req$design <- design; request(req); preview(NULL)
      choices <- setdiff(names(data)[vapply(data,is.numeric,logical(1))],c(analysis_scope_excluded(data),custom_model_canvas_node_variable(target$original)))
      allowed(choices);chosen(intersect(unlist(design$items),choices))
      shiny::removeUI(".canvas-score-panel",multiple=TRUE,immediate=TRUE)
      shiny::insertUI(selector="body",where="beforeEnd",ui=shiny::div(
        id=id("panel"),class="canvas-score-panel",role="region",
        `aria-label`=tr("원문항 · 신뢰도 / Parcel","Original items / reliability / parcels"),
        `data-canvas-root`=paste0(prefix,"-canvas-root"),`data-add-input`=id("add_items"),
        shiny::h4(paste(tr("원문항 · 신뢰도 / Parcel:", "Original items / reliability / parcels:"),custom_model_canvas_node_variable(target$original))),
        shiny::p(tr("현재 불러온 데이터에서 원문항을 선택합니다. 역문항은 먼저 역채점하세요. 원본 데이터는 변경하지 않습니다.","Select original items from the loaded data. Reverse-score items beforehand. Source data are preserved.")),
        shiny::selectInput(id("mode"),tr("적용 방식","Mode"),choices=stats::setNames(c("single","parcels"),c(tr("합산척도 단일지표 제약","Single aggregate-score indicator"),tr("문항 합계·평균 Parcel 지표","Sum/mean parcel indicators"))),selected=design$mode %||% "single"),
        shiny::div(class="canvas-score-item-drop",
          shiny::strong(tr("원문항","Original items")),
          shiny::p(tr("왼쪽 관측변수에서 Ctrl/Shift로 여러 문항을 선택해 여기에 끌어 넣으세요.","Select multiple items with Ctrl/Shift in the left variable list and drag them here.")),
          shiny::uiOutput(id("item_list"))),
        shiny::tags$button(type="button",class="btn btn-default canvas-score-add-selected",tr("왼쪽 선택 문항 추가","Add selected items")),
        shiny::actionButton(id("remove_items"),tr("선택 문항 제거","Remove selected items")),
        shiny::radioButtons(id("scoring"),tr("점수 계산","Scoring"),choices=stats::setNames(c("sum","mean"),c(tr("합계","Sum"),tr("평균","Mean"))),selected=design$scoring %||% "mean",inline=TRUE),
        shiny::conditionalPanel(sprintf("input['%s'] == 'single'",id("mode")),
          shiny::conditionalPanel(sprintf("input['%s'] == 'mean'",id("scoring")),
            shiny::radioButtons(id("min_response"),tr("최소 문항 응답률","Minimum item response"),choices=c("80%"="0.8","100%"="1"),selected=as.character(design$minResponse %||% if(length(design))1 else .8),inline=TRUE)),
          shiny::radioButtons(id("method"),tr("신뢰도","Reliability"),choices=stats::setNames(c("alpha","omega"),c(canvas_score_text("Cronbach alpha",statedu_current_language(app_language_fn)),canvas_score_text("McDonald omega",statedu_current_language(app_language_fn)))),selected=design$method %||% "omega",inline=TRUE),
          shiny::p(tr("원문항의 합계·평균이 기존 점수와 일치하는지 검증합니다. 평균은 최소 응답률을 충족한 사례의 응답 문항으로 계산합니다. α는 문항쌍별 공분산, ω는 결측 시 FIML을 사용하며 점수 분산(N−1)은 같은 분석 사례에서 계산합니다. 부분 응답 점수에 공통 오차분산을 적용하는 것은 근사입니다. 부하량=1, 오차분산=(1−신뢰도)×분산. 재분석 시 현재 분석 대상 사례로 다시 계산하며 bootstrap에서는 해당 제약을 고정합니다.","The selected items must reproduce the existing score. Means use answered items in eligible cases. Alpha uses pairwise covariances; omega uses FIML with missing items. Score variance (N−1) uses those analysis cases. A common error variance for partial scores is an approximation. Loading=1; error variance=(1−reliability)×variance. Reanalysis recalculates from current analysis cases; bootstrap conditions on those fixed constraints."))),
        shiny::conditionalPanel(sprintf("input['%s'] == 'parcels'",id("mode")),
          shiny::numericInput(id("count"),tr("Parcel 수","Parcel count"),value=if(length(design$groups))max(unlist(design$groups)) else 3,min=3,max=5,step=1),
          shiny::p(tr("요인부하량 균형 배정: 원문항 단일요인 모형의 표준화 부하량을 내림차순으로 정렬하고 정방향·역방향으로 번갈아 배정합니다. 배정표와 요인모형 적합도를 검토하세요. 이 배정만으로 단일차원성이 입증되지는 않습니다. Parcel 내 결측이 있으면 해당 점수는 결측 처리합니다.","Loading-balanced allocation: sort standardized one-factor loadings in descending order and alternate forward/reverse parcel assignments. Review the allocation and factor-model fit; balancing does not establish unidimensionality. A missing item makes its parcel score missing.")),
          shiny::uiOutput(id("allocation")),
          shiny::textAreaInput(id("rationale"),tr("차원성 검토 및 문항 배정 근거","Dimensionality review and allocation rationale"),value=design$rationale %||% ""),
          shiny::checkboxInput(id("reviewed"),tr("문항 구조와 배정 근거를 검토했습니다.","I have reviewed the item structure and allocation rationale."),value=FALSE)),
        shiny::actionButton(id("calculate"),tr("계산 / 미리보기","Calculate / preview")),shiny::uiOutput(id("preview")),
        shiny::div(class="canvas-score-panel-footer",shiny::actionButton(id("close"),tr("닫기","Close")),shiny::actionButton(id("apply"),tr("모형에 적용","Apply to model"),class="btn-primary"))
      ))
    },error=function(e)shiny::showNotification(conditionMessage(e),type="error"))
  },ignoreInit=TRUE)
  shiny::observeEvent(input[[id("calculate")]], {
    req <- request(); shiny::req(req)
    tryCatch({
      cfg <- config(); snap <- canvas_score_apply(req$snapshot,req$nodeId,dataset_fn(),cfg)
      preview(list(snapshot=snap,config=cfg))
    },error=function(e)preview(list(error=conditionMessage(e))))
  },ignoreInit=TRUE)
  shiny::observeEvent(input[[id("apply")]], {
    req <- request(); value <- preview(); shiny::req(req)
    tryCatch({
      if(is.null(value$snapshot) || !identical(config(),value$config))stop(tr("설정 변경 후 계산 / 미리보기를 먼저 실행하세요.","Calculate/preview the current settings first."))
      snap <- canvas_score_apply(req$snapshot,req$nodeId,dataset_fn(),value$config)
      if(!identical(snap,value$snapshot))stop(tr("데이터가 변경되었습니다. 다시 계산하세요.","Data changed. Calculate again."))
      session$sendCustomMessage("custom-model-canvas-score",list(rootId=paste0(prefix,"-canvas-root"),token=req$token,snapshot=snap,layoutTarget=canvas_score_target(req$snapshot,req$nodeId)$latent$id))
      fit_result(NULL); mark_settings_dirty(); shiny::removeUI(paste0("#",id("panel")))
    },error=function(e)shiny::showNotification(conditionMessage(e),type="error"))
  },ignoreInit=TRUE)
}
