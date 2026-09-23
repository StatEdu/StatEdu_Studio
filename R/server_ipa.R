ipa_title <- function(language=statedu_initial_language()) ipa_text("Importance–Performance Analysis (IPA)",language,"중요도–수행도 분석(IPA)")

ipa_tab_panel <- function(language=statedu_initial_language()) {
  tr <- function(ko,en)ipa_text(en,language,ko)
  tabPanel(ipa_title(language),value="IPA",div(class="page-shell",
    div(class="workspace-panel analysis-three-block-workspace",
      div(class="analysis-workspace-heading",div(class="analysis-workspace-heading-main",h3(ipa_title(language)))),
      div(class="correlation-setup-grid ipa-setup-grid",
        div(class="analysis-transfer-column analysis-transfer-panel ipa-available-panel",
          analysis_field_label_tag("Variables",language=language),uiOutput("ipa_available_panel")),
        div(class="ipa-target-stack",uiOutput("ipa_setup")),
        div(class="correlation-options-column",
          div(class="analysis-options-panel analysis-tabbed-options analysis-calculator-tabs ipa-options-panel",
            tabsetPanel(id="ipa_options_tab",type="tabs",
            tabPanel(tr("분석","Analysis"),value="analysis",
            selectInput("ipa_mode",tr("중요도 산출 방식","Importance method"),setNames(c("direct","derived"),c(tr("직접 측정","Direct ratings"),tr("추정 중요도","Derived importance")))),
            selectInput("ipa_design",tr("분석 설계","Design"),setNames(c("overall","independent","paired","grouped_paired"),c(tr("전체 분석","Overall"),tr("독립집단 비교","Independent groups"),tr("사전–사후 비교","Pre/post paired"),tr("집단별 사전·사후 비교","Pre/post by group")))),
            conditionalPanel("(input.ipa_design === 'paired' || input.ipa_design === 'grouped_paired')",selectInput("ipa_layout",tr("사전·사후 구분 방식","Identify pre/post"),setNames(c("wide","long"),c(tr("사전·사후 변수를 각각 선택","Select separate pre/post variables"),tr("시점 변수로 구분","Use a time variable"))))),
            conditionalPanel("input.ipa_mode === 'derived'",selectInput("ipa_derived",tr("추정 중요도 산식","Derived-importance estimator"),setNames(c("partial","log_partial"),c(tr("편상관","Partial correlation"),tr("로그 수행도의 편상관(수정 IPA)","Log-performance partial correlation")))),
              numericInput("ipa_resamples",tr("부트스트랩 반복수","Bootstrap resamples"),1000,min=100,max=10000,step=100),numericInput("ipa_seed",tr("난수 시드","Seed"),20260917,min=0,step=1)),
            uiOutput("ipa_extra_setup"),
            tags$p(tr("중요도와 수행도는 입력 순서대로 1:1 매칭됩니다. 두 목록의 변수 수를 같게 지정하세요.","Importance and performance are matched by their entered order. Use equal numbers of variables."))),
            tabPanel(tr("그래프","Chart"),value="chart",
            conditionalPanel("input.ipa_group_plot !== 'separate'",
            selectInput("ipa_reference",tr("사분면 기준선","Quadrant reference"),setNames(c("pooled","custom","within"),c(tr("전체 공통 평균","Pooled common means"),tr("사용자 지정 공통 기준","Custom common reference"),tr("집단·시점별 평균(보조)","Within-group/time means")))),
            conditionalPanel("input.ipa_reference === 'custom'",numericInput("ipa_x_reference",tr("수행도 기준값","Performance reference"),3),numericInput("ipa_y_reference",tr("중요도 기준값","Importance reference"),3))),
            selectInput("ipa_group_plot",tr("집단·시점 그래프","Group/time charts"),setNames(c("overlay","separate"),c(tr("겹쳐 그리기","Overlay"),tr("각각 그리기","Separate"))),selected="overlay"),
            conditionalPanel("input.ipa_group_plot === 'separate'",selectInput("ipa_separate_mean",tr("각각 그리기 평균 기준","Separate-chart mean reference"),setNames(c("pooled","within"),c(tr("전체 평균","Overall mean"),tr("집단 평균","Group mean"))),selected="pooled")),
            textInput("ipa_x_label",tr("수행도 축 이름","Performance axis name"),"Performance"),
            textInput("ipa_y_label",tr("중요도 축 이름","Importance axis name"),"Importance"),
            conditionalPanel("input.ipa_group_plot === 'overlay' || input.ipa_design === 'grouped_paired'",uiOutput("ipa_connection_setup"))
            ),
            tabPanel(tags$span(`data-ipa-group-title`=tr("집단 표식","Group markers"),`data-ipa-time-title`=tr("시점 표식","Time markers"),tr("집단 표식","Group markers")),value="markers",uiOutput("ipa_point_setup"),uiOutput("ipa_marker_controls")),
            tabPanel(tr("사분면","Quadrants"),value="quadrants",
            checkboxInput("ipa_show_quadrants",tr("사분면 이름 출력","Show quadrant names"),TRUE),
            conditionalPanel("input.ipa_show_quadrants",
              numericInput("ipa_quadrant_font_size",tr("사분면 이름 글꼴 크기 (pt)","Quadrant name font size (pt)"),9,min=4,max=36,step=1),
              textInput("ipa_quadrant_1",tr("왼쪽 위 이름","Upper-left name"),"Concentrate here"),
              textInput("ipa_quadrant_2",tr("오른쪽 위 이름","Upper-right name"),"Keep up"),
              textInput("ipa_quadrant_3",tr("왼쪽 아래 이름","Lower-left name"),"Low priority"),
              textInput("ipa_quadrant_4",tr("오른쪽 아래 이름","Lower-right name"),"Possible overkill"))
            ))))),
      analysis_three_block_action_row(class="correlation-action-row ipa-action-row",
        run_button=actionButton("run_ipa",tr("분석 실행","Run analysis"),class="btn-primary"),
        reset_control=uiOutput("ipa_reset_control"),save_control=uiOutput("ipa_save"))),
    div(class="regression-results",uiOutput("ipa_results"))))
}

register_ipa_handlers <- function(input,output,session,dataset_fn,selected_names_fn,language_fn,mark_settings_dirty,variable_table_fn=NULL,category_table_fn=NULL) {
  result <- analysis_scope_result_val(NULL)
  tr <- function(ko,en)ipa_text(en,language_fn(),ko)
  roles <- c("importance","performance","post_importance","post_performance","group","id","time","outcome","post_outcome")
  selections <- setNames(lapply(roles,function(role)reactiveVal(character())),roles)
  active_list <- reactiveVal("ipa_available")
  active_target <- reactiveVal("importance")
  time_block <- reactiveVal("pre")
  observeEvent(input$ipa_design,{
    updateSelectInput(session,"ipa_group_plot",selected=if(identical(input$ipa_design,"grouped_paired"))"separate" else "overlay")
  },ignoreInit=TRUE)
  for(block in c("pre","post"))local({b<-block
    observeEvent(input[[paste0("ipa_block_",b)]],{
      time_block(b);active_list("ipa_available")
      role<-sub("^post_","",active_target())
      if(!role %in% c("importance","performance"))role<-if(identical(input$ipa_mode,"derived"))"performance" else "importance"
      active_target(paste0(if(b=="post")"post_" else "",role))
    },ignoreInit=TRUE)
  })
  eligible <- reactive({
    data <- dataset_fn();req(is.data.frame(data))
    setdiff(intersect(selected_names_fn(),names(data)),analysis_scope_excluded(data))
  })
  active_roles <- reactive({
    value <- if(identical(input$ipa_mode%||%"direct","direct"))c("importance","performance") else c("performance","outcome")
    if(((input$ipa_design %||% "overall") %in% c("paired","grouped_paired"))) {
      if(identical(input$ipa_layout%||%"wide","wide"))value <- c(value,"post_performance",if(identical(input$ipa_mode%||%"direct","direct"))"post_importance" else "post_outcome") else value <- c(value,"id","time")
    }
    if(!identical(input$ipa_design,"paired"))value <- c(value,"group")
    value
  })
  single_roles <- c("group","id","time","outcome","post_outcome")
  observe({
    if(!length(selections$group()) && !((input$ipa_design %||% "overall") %in% c("paired","grouped_paired")) && identical(input$ipa_options_tab,"markers"))
      updateTabsetPanel(session,"ipa_options_tab",selected="chart")
  })
  chart_id <- function(prefix,value)paste0("ipa_",prefix,"_",paste(format(charToRaw(enc2utf8(value))),collapse=""))
  chart_groups <- reactive({
    if(identical(input$ipa_design,"grouped_paired")) {
      g<-selections$group();if(!length(g))return(character())
      return(ipa_group_time_map(ipa_sorted_groups(dataset_fn()[[g]]))$Series)
    }
    if(((input$ipa_design %||% "overall") %in% c("paired","grouped_paired")))return(c("Pre","Post"))
    g<-selections$group()
    if(!length(g))return("Overall")
    ipa_sorted_groups(dataset_fn()[[g]])
  })
  point_styles <- reactive({
    groups<-chart_groups();defaults<-ipa_default_point_styles(groups)
    if(identical(input$ipa_design,"grouped_paired") && length(groups)) {
      defaults$Color<-rep(ipa_contrast_colors(length(groups)/2L),each=2)
      defaults$Shape<-rep(c(16L,17L),length(groups)/2L)
    }
    for(j in seq_along(groups)) {
      defaults$Shape[j]<-as.integer(input[[chart_id("shape",groups[j])]]%||%defaults$Shape[j])
      defaults$Color[j]<-input[[chart_id("color",groups[j])]]%||%defaults$Color[j]
      defaults$Size[j]<-input[[chart_id("size",groups[j])]]%||%defaults$Size[j]
    };defaults
  })
  group_labels <- reactive({
    groups<-chart_groups();g<-selections$group()
    if(identical(input$ipa_design,"grouped_paired")) {
      if(!length(g))return(setNames(character(),character()))
      map<-ipa_group_time_map(ipa_sorted_groups(dataset_fn()[[g]]))
      labels<-if(is.function(category_table_fn))frequency_value_display_labels(g,map$Group,category_table_fn()) else map$Group
      return(setNames(paste(ifelse(labels==map$Group,map$Group,paste0(labels,"(",map$Group,")")),map$Time,sep=" · "),map$Series))
    }
    if(((input$ipa_design %||% "overall") %in% c("paired","grouped_paired")) || !length(g) || !is.function(category_table_fn))return(setNames(groups,groups))
    labels<-frequency_value_display_labels(g,groups,category_table_fn())
    setNames(ifelse(labels==groups,groups,paste0(labels,"(",groups,")")),groups)
  })
  connected_items <- reactive({
    if(isTRUE(input$ipa_connect_items%||%TRUE))selections$performance() else character()
  })
  output$ipa_point_setup <- renderUI({
    groups<-chart_groups();selected<-isolate(input$ipa_marker_group)
    if(!length(groups))return(helpText(tr("집단 변수를 배정하세요.","Assign a group variable.")))
    labels <- unname(group_labels())
    if(identical(input$ipa_design,"grouped_paired")) {
      labels <- sub(" · Pre$",paste0(" · ",ipa_text("Pre",language_fn())),labels)
      labels <- sub(" · Post$",paste0(" · ",ipa_text("Post",language_fn())),labels)
    } else if(identical(input$ipa_design,"paired") || !length(selections$group()))
      labels <- vapply(labels,ipa_text,character(1),language=language_fn())
    selectInput("ipa_marker_group",if(((input$ipa_design %||% "overall") %in% c("paired","grouped_paired")))tr("시점 표식","Time marker") else tr("집단 표식","Group marker"),setNames(groups,labels),selected=if(length(selected)==1L && selected %in% groups)selected else groups[1])
  })
  output$ipa_marker_controls <- renderUI({
    groups<-chart_groups();req(length(groups));g<-input$ipa_marker_group%||%groups[1];j<-match(g,groups);req(!is.na(j))
    styles<-isolate(point_styles())
    palette<-c("#1565C0","#D84315","#2E7D32","#6A1B9A","#F9A825","#00838F","#E16A86","#00AD9A","#253746","#757575")
    labels<-vapply(c("Blue","Orange","Green","Purple","Yellow","Teal","Pink","Mint","Black","Gray"),ipa_text,character(1),language=language_fn())
    if(!styles$Color[j] %in% palette){palette<-c(palette,styles$Color[j]);labels<-c(labels,tr("기본 색상","Default color"))}
    tagList(
      selectInput(chart_id("shape",groups[j]),tr("모양","Shape"),setNames(0:25,c("□","○","△","+","×","◇","▽","□ ×","✳","◇ +","○ +","△▽","□ +","○ ×","□ △","■","●","▲","◆",paste0("● (",ipa_text("Solid",language_fn()),")"),"•",paste0(c("○","□","◇","△","▽")," (",ipa_text("Filled",language_fn()),")"))),selected=styles$Shape[j]),
      div(class="ipa-color-palette",radioButtons(chart_id("color",g),tr("색상","Color"),choiceNames=lapply(seq_along(palette),function(k)
        tags$span(title=labels[k],tags$span(class="ipa-color-swatch",style=paste0("background-color:",palette[k]),`aria-hidden`="true"),tags$span(class="sr-only",labels[k]))),choiceValues=palette,selected=styles$Color[j],inline=TRUE)),
      numericInput(chart_id("size",groups[j]),tr("크기 (배율)","Size (scale)"),styles$Size[j],min=.3,max=4,step=.1))
  })
  output$ipa_connection_setup <- renderUI({
    if(length(chart_groups())<2L)return(NULL)
    checkboxInput("ipa_connect_items",tr("문항별 연결선 표시","Show item connections"),isolate(input$ipa_connect_items)%||%TRUE)
  })
  observeEvent(list(point_styles(),connected_items(),group_labels(),input$ipa_separate_mean,input$ipa_x_label,input$ipa_y_label),{result(NULL);mark_settings_dirty()},ignoreInit=TRUE)
  set_role <- function(role,values) {
    values <- unique(as.character(values%||%character()))
    if(!all(values %in% eligible()))stop(tr("현재 사용 가능한 변수를 선택하세요.","Select currently available variables."))
    if(role %in% single_roles && length(values)>1L)stop(tr("이 패널에는 변수 하나만 지정하세요.","This panel accepts one variable."))
    added<-setdiff(values,selections[[role]]())
    if(length(added))validate_role_types(role,added)
    if(!role %in% c("group","id","time") && any(!vapply(dataset_fn()[values],is.numeric,logical(1))))stop(tr("점수에는 숫자형 변수를 지정하세요.","Score roles require numeric variables."))
    other <- setdiff(active_roles(),role)
    used <- unlist(lapply(selections[other],function(x)x()),use.names=FALSE)
    if(length(intersect(values,used)))stop(tr("다른 패널에 배정된 변수를 먼저 제거하세요.","Remove variables from their other panel before reassignment."))
    selections[[role]](values);result(NULL);mark_settings_dirty()
  }
  safely <- function(expr)tryCatch(force(expr),error=function(e)showNotification(ipa_error_message(conditionMessage(e),language_fn()),type="warning",duration=6))
  named_items <- function(values,numbered=FALSE) {
    table <- if(is.function(variable_table_fn))variable_table_fn() else NULL
    items <- analysis_variable_items(values,table)
    for(j in seq_along(items)) {
      if(!nzchar(items[[j]]$measurement))items[[j]]$measurement <- infer_measurement(dataset_fn()[[values[j]]])
      # Keep the original variable name and positional matching visible.
      items[[j]]$label <- if(numbered)paste0(j,". ",values[j]) else values[j]
    }
    items
  }
  role_measurements<-function(role) {
    if(role %in% c("importance","performance","post_importance","post_performance"))"continuous" else if(role=="group")c("binary","category") else character()
  }
  validate_role_types<-function(role,values) {
    allowed<-role_measurements(role)
    if(!length(allowed) || !length(values))return(invisible(TRUE))
    table<-if(is.function(variable_table_fn))variable_table_fn() else NULL
    items<-analysis_variable_items(values,table)
    types<-vapply(seq_along(values),function(j){
      m<-items[[j]]$measurement
      if(is.null(m)||!nzchar(m))m<-infer_measurement(dataset_fn()[[values[j]]])
      tolower(m)
    },character(1))
    if(any(!types %in% allowed))stop(if(role=="group")tr("집단에는 이분형·범주형 변수만 지정할 수 있습니다.","Group accepts only binary or categorical variables.") else tr("중요도·수행도에는 연속형 변수만 지정할 수 있습니다.","Importance and performance accept only continuous variables."))
    invisible(TRUE)
  }
  output$ipa_available_panel <- renderUI({
    used <- unlist(lapply(selections[active_roles()],function(x)x()),use.names=FALSE)
    analysis_transfer_listbox_input("ipa_available",named_items(setdiff(eligible(),used)),selected=isolate(input$ipa_available),size=17)
  })
  target_panel <- function(role,title,single=FALSE,extra=FALSE) {
    values <- selections[[role]](); input_id <- paste0("ipa_",role)
    div(class=paste("ipa-target-row",if(single)"ipa-single-row" else "ipa-score-row",if(extra)"ipa-extra-row" else "",if(role=="id")"ipa-id-row" else ""),
      div(class="ipa-target-arrow",analysis_variable_move_button(paste0("ipa_move_",role),
        label=analysis_variable_move_label(isolate(active_list()),input_id,isolate(input[[input_id]])))),
      div(class="analysis-transfer-column analysis-transfer-panel ipa-target-panel",
        analysis_field_label_tag(paste0(title," (",length(values),")"),role_measurements(role),language_fn()),
        analysis_transfer_listbox_input(input_id,named_items(values,!single),selected=isolate(input[[input_id]]),size=if(single)1 else 5,min_size=if(single)1 else 3),
        if(!single)div(class="analysis-order-actions",actionButton(paste0(input_id,"_up"),tr("위로","Up"),class="btn-default btn-sm"),
          actionButton(paste0(input_id,"_down"),tr("아래로","Down"),class="btn-default btn-sm"))))
  }
  output$ipa_setup <- renderUI({
    direct <- identical(input$ipa_mode%||%"direct","direct")
    wide <- ((input$ipa_design %||% "overall") %in% c("paired","grouped_paired")) && identical(input$ipa_layout%||%"wide","wide")
    show_group <- !identical(input$ipa_design,"paired")
    post<-identical(time_block(),"post")
    block_nav<-div(class="ipa-time-block-nav hierarchical-block-title-row",
      div(class="analysis-field-label",if(post)tr("블록 2 · 사후","Block 2 · Post") else tr("블록 1 · 사전","Block 1 · Pre")),
      div(class="hierarchical-block-nav",
        actionButton("ipa_block_pre",tr("‹ 사전","‹ Pre"),class="btn-default btn-sm",disabled=if(!post)"disabled" else NULL),
        actionButton("ipa_block_post",tr("사후 ›","Post ›"),class="btn-default btn-sm",disabled=if(post)"disabled" else NULL)))
    div(class=paste("ipa-primary-panels",if(!wide)"ipa-with-group" else "ipa-with-post"),
      if(wide)tagList(block_nav,
        if(direct)target_panel(if(post)"post_importance" else "importance",tr("중요도","Importance")),
        target_panel(if(post)"post_performance" else "performance",tr("수행도","Performance")),
        if(identical(input$ipa_design,"grouped_paired"))target_panel("group",tr("집단","Group"),TRUE)) else tagList(
        if(direct)target_panel("importance",tr("중요도","Importance")),target_panel("performance",tr("수행도","Performance")),
        if(show_group)target_panel("group",tr("집단","Group"),TRUE) else target_panel("time",tr("시점","Time"),TRUE),
        if(identical(input$ipa_design,"grouped_paired"))target_panel("time",tr("시점","Time"),TRUE)))
  })
  time_choices<-reactive({
    if(length(selections$time())) {
      time_var<-selections$time()[1];x<-dataset_fn()[[time_var]]
      levels<-as.character(sort(unique(x[!is.na(x)])));levels<-levels[nzchar(levels)]
      labels<-if(is.function(category_table_fn))frequency_value_display_labels(time_var,levels,category_table_fn()) else levels
      return(setNames(levels,ifelse(labels==levels,levels,paste0(labels," (",levels,")"))))
    };character()
  })
  time_reversed<-reactiveVal(FALSE)
  observeEvent(list(selections$time(),unname(time_choices())),time_reversed(FALSE),ignoreNULL=FALSE)
  time_pair<-reactive({
    values<-time_choices()
    if(length(values)!=2L)return(character())
    if(isTRUE(time_reversed()))rev(values) else values
  })
  observeEvent(input$ipa_swap_times,{
    if(length(time_pair())==2L)time_reversed(!time_reversed())
  },ignoreInit=TRUE)
  output$ipa_extra_setup <- renderUI({
    paired <- ((input$ipa_design %||% "overall") %in% c("paired","grouped_paired"));wide <- identical(input$ipa_layout%||%"wide","wide")
    tagList(
      if(identical(input$ipa_mode,"derived"))tagList(target_panel("outcome",if(paired && wide)tr("사전 전반적 만족도","Pre overall satisfaction") else tr("전반적 만족도","Overall satisfaction"),TRUE,TRUE),
        if(paired && wide)target_panel("post_outcome",tr("사후 전반적 만족도","Post overall satisfaction"),TRUE,TRUE)),
      if(paired && !wide)target_panel("id",tr("응답자 ID (필수)","Respondent ID (required)"),TRUE,TRUE),
      if(paired && !wide)tagList(
        if(length(time_pair())==2L)div(class="ipa-time-direction",
          div(paste0(tr("사전: ","Pre: "),names(time_pair())[1]," → ",tr("사후: ","Post: "),names(time_pair())[2])),
          actionButton("ipa_swap_times",tr("순서 바꾸기","Swap order"),class="btn-default btn-sm")) else
          helpText(if(!length(selections$time()))tr("시점 변수를 배정하면 사전·사후가 자동으로 표시됩니다.","Assign a time variable to identify pre/post automatically.") else
            tr("사전·사후 비교에는 두 수준의 시점 변수가 필요합니다.","Pre/post comparison requires a time variable with two levels."))))
  })
  observeEvent(input$ipa_available_active,active_list("ipa_available"),ignoreInit=TRUE)
  for(role_name in roles)local({role<-role_name;input_id<-paste0("ipa_",role)
    state <- function(value)if(missing(value))selections[[role]]() else set_role(role,value)
    activate <- function(){active_list(input_id);active_target(role)}
    observeEvent(input[[paste0(input_id,"_active")]],activate(),ignoreInit=TRUE)
    observe(updateActionButton(session,paste0("ipa_move_",role),label=analysis_variable_move_label(active_list(),input_id,input[[input_id]])))
    observeEvent(input[[paste0("ipa_move_",role)]],safely({
      if(!role %in% active_roles())return()
      active_target(role)
      if(identical(active_list(),input_id) && length(input[[input_id]]))state(setdiff(state(),input[[input_id]])) else {
        chosen <- intersect(as.character(input$ipa_available%||%character()),eligible())
        if(!length(chosen))return()
        state(c(state(),setdiff(chosen,state())))
      }
      session$sendCustomMessage("easyflow-clear-transfer-selection",list(inputIds=c("ipa_available",input_id)))
    }))
    observeEvent(input[[paste0(input_id,"_doubleclick")]],safely(state(setdiff(state(),input[[paste0(input_id,"_doubleclick")]]$value))),ignoreInit=TRUE)
    register_dual_transfer_drop_observer(input,session,"ipa_available",input_id,state,eligible,active_list,
      validate_next=function(next_values,target,chosen) {
        if(!role %in% active_roles())return(FALSE)
        valid <- tryCatch({
          if(target==input_id) {
            validate_role_types(role,setdiff(next_values,state()))
            if(role %in% single_roles && length(next_values)>1L)stop(tr("이 패널에는 변수 하나만 지정하세요.","This panel accepts one variable."))
            if(!role %in% c("group","id","time") && any(!vapply(dataset_fn()[next_values],is.numeric,logical(1))))stop(tr("숫자형 점수 변수를 선택하세요.","Select numeric scores."))
            used <- unlist(lapply(selections[setdiff(active_roles(),role)],function(x)x()),use.names=FALSE)
            if(length(intersect(next_values,used)))stop(tr("다른 패널에 이미 배정된 변수입니다.","Variable already assigned to another panel."))
          };TRUE
        },error=function(e){showNotification(conditionMessage(e),type="warning");FALSE})
        valid
      },after_change=function(...)active_target(role))
    register_analysis_reorder(input,session,input_id,function(payload) {
      order<-analysis_reorder_items(state(),payload)
      if(isTRUE(order$changed))safely(state(order$order))
    })
    for(direction in c("up","down"))local({dir<-direction
      observeEvent(input[[paste0(input_id,"_",dir)]],{
        order<-move_order_item(state(),input[[input_id]],dir)
        if(isTRUE(order$changed))safely(state(order$order))
      })
    })
  })
  observeEvent(input$ipa_available_doubleclick,safely({
    role<-active_target();if(!role %in% active_roles())role<-active_roles()[1]
    value<-input$ipa_available_doubleclick$value
    set_role(role,c(selections[[role]](),setdiff(value,selections[[role]]())))
  }),ignoreInit=TRUE)
  observeEvent(eligible(),{
    for(role in roles)selections[[role]](intersect(selections[[role]](),eligible()))
    result(NULL)
  },ignoreInit=TRUE)
  output$ipa_reset_control <- renderUI({
    analysis_reset_button("ipa_reset",enabled=any(vapply(selections,function(x)length(x())>0L,logical(1))),language=language_fn())
  })
  observeEvent(input$ipa_reset,{
    for(role in roles)selections[[role]](character())
    time_block("pre")
    result(NULL);mark_settings_dirty();active_list("ipa_available")
  })
  items <- function() {
    p<-selections$performance();i<-selections$importance()
    direct<-identical(input$ipa_mode%||%"direct","direct")
    wide<-((input$ipa_design %||% "overall") %in% c("paired","grouped_paired")) && identical(input$ipa_layout%||%"wide","wide")
    required<-c("performance",if(direct)"importance",if(wide)c("post_performance",if(direct)"post_importance"))
    counts<-vapply(selections[required],function(x)length(x()),integer(1))
    if(!length(p) || any(counts!=length(p)))stop(tr("중요도·수행도 목록의 변수 수를 같게 지정하세요. 같은 순번끼리 매칭됩니다.","All importance/performance lists must have the same nonzero count; matching follows list order."))
    data.frame(Item=p,Performance=p,Importance=if(direct)i else rep("",length(p)),
      PostPerformance=if(wide)selections$post_performance() else rep("",length(p)),
      PostImportance=if(wide && direct)selections$post_importance() else rep("",length(p)),stringsAsFactors=FALSE)
  }
  # Hide old results when analytical options or the analysis data change.
  observeEvent(list(dataset_fn(),selected_names_fn(),input$ipa_mode,input$ipa_design,input$ipa_layout,
    time_pair(),input$ipa_derived,
    input$ipa_reference,input$ipa_x_reference,input$ipa_y_reference,input$ipa_resamples,input$ipa_seed,
    input$ipa_group_plot,input$ipa_show_quadrants,input$ipa_quadrant_font_size,
    input$ipa_quadrant_1,input$ipa_quadrant_2,input$ipa_quadrant_3,input$ipa_quadrant_4),{
      result(NULL);mark_settings_dirty()
    },ignoreInit=TRUE)
  register_analysis_command_handler("run_ipa",input,output,session,
    states=setNames(lapply(roles,function(role)local({r<-role;function(value)if(missing(value))selections[[r]]() else set_role(r,value)})),roles),dataset_fn=dataset_fn,
    context_fn=function()list(selected=selected_names_fn()),run_fn=function(){
      result(NULL)
      tryCatch({
        for(role in active_roles())validate_role_types(role,selections[[role]]())
        if(((input$ipa_design %||% "overall") %in% c("paired","grouped_paired")) && identical(input$ipa_layout,"long") && length(time_pair())!=2L)
          stop(tr("사전·사후 비교에는 두 수준의 시점 변수가 필요합니다.","Pre/post comparison requires a time variable with two levels."))
        active_columns <- c(items()$Performance,if(identical(input$ipa_mode,"direct"))items()$Importance else selections$outcome(),
          if(((input$ipa_design %||% "overall") %in% c("paired","grouped_paired")) && identical(input$ipa_layout,"wide"))c(items()$PostPerformance,
            if(identical(input$ipa_mode,"direct"))items()$PostImportance else selections$post_outcome()))
        if(!all(active_columns %in% selected_names_fn()))stop(tr("현재 선택된 변수로 항목을 다시 지정하세요.","Map attributes using the currently selected variables."))
        value <- withProgress(message=tr("IPA 계산 중","Computing IPA"),value=.2,
          prepare_ipa(dataset_fn(),items(),input$ipa_mode%||%"direct",
            if(!((input$ipa_design %||% "overall") %in% c("paired","grouped_paired")) && length(selections$group()))"independent" else input$ipa_design%||%"overall",input$ipa_layout%||%"wide",
            group=selections$group(),id=if(identical(input$ipa_layout,"long"))selections$id() else character(),time=selections$time(),pre=unname(time_pair()[1]),post=unname(time_pair()[2]),
            outcome=selections$outcome(),post_outcome=selections$post_outcome(),derived=input$ipa_derived%||%"partial",
            reference=if(identical(input$ipa_group_plot,"separate"))input$ipa_separate_mean%||%"pooled" else input$ipa_reference%||%"pooled",x_reference=input$ipa_x_reference%||%3,y_reference=input$ipa_y_reference%||%3,
            resamples=input$ipa_resamples%||%1000L,seed=input$ipa_seed%||%20260917L,
            group_plot=input$ipa_group_plot%||%"overlay",show_quadrants=input$ipa_show_quadrants%||%TRUE,
            quadrant_font_size=input$ipa_quadrant_font_size%||%9,
            quadrant_labels=c(input$ipa_quadrant_1%||%"Concentrate here",input$ipa_quadrant_2%||%"Keep up",
              input$ipa_quadrant_3%||%"Low priority",input$ipa_quadrant_4%||%"Possible overkill")))
        # Store rendered figures once; export reuses the displayed snapshot.
        value$point_styles <- ipa_validate_point_styles(point_styles(),unique(value$coordinates$Group))
        value$connected_items <- connected_items()
        value$group_labels <- group_labels()
        value$x_label <- if(nzchar(trimws(input$ipa_x_label%||%"")))input$ipa_x_label else "Performance"
        value$y_label <- if(nzchar(trimws(input$ipa_y_label%||%"")))input$ipa_y_label else if(value$mode=="direct")"Importance" else "Derived importance (partial r)"
        value$language <- language_fn()
        value$html <- as.character(ipa_results_ui(value,value$language));result(value)
      },error=function(e)showNotification(ipa_error_message(conditionMessage(e),language_fn()),type="error",duration=12))
    })
  output$ipa_results <- renderUI({
    value <- result();req(value)
    HTML(if(identical(value$language,language_fn()))value$html else as.character(ipa_results_ui(value,language_fn())))
  })
  output$ipa_save <- renderUI({req(result());analysis_save_buttons(html_button_id="save_ipa_html_dialog",pdf_button_id="save_ipa_pdf_dialog",
    excel_button_id="save_ipa_excel_dialog",figure_button_id="save_ipa_figures_dialog",add_result_button_id="add_ipa_result",has_figures=TRUE,language=language_fn())})
  observeEvent(input$save_ipa_figures_dialog,{
    req(result());session$sendCustomMessage("easyflow-capture-result-snapshot",list(outputId="ipa_results",inputId="ipa_picture_snapshot"))
  })
  observeEvent(input$ipa_picture_snapshot,{
    tryCatch({
      payload<-input$ipa_picture_snapshot;if(nzchar(payload$error%||%""))stop(payload$error)
      images<-xml2::xml_find_all(xml2::read_html(payload$html),"//img[starts-with(@src,'data:image/png')]")
      req(length(images));directory<-choose_figure_save_dir();if(!length(directory)||!nzchar(directory[[1]]))return()
      files<-lapply(seq_along(images),function(i)list(name=paste0("ipa_",i,".png"),data=xml2::xml_attr(images[[i]],"src")))
      save_canvas_figure_snapshots(files,directory[[1]],"ipa")
    },error=function(e)showNotification(conditionMessage(e),type="error"))
  },ignoreInit=TRUE)
  register_canvas_report_exports(input,session,"save_ipa_html_dialog","save_ipa_pdf_dialog","ipa_results",NULL,
    function()ipa_title(language_fn()),result,language_fn,excel_id="save_ipa_excel_dialog")
  register_add_result_snapshot(input,session,"add_ipa_result",function()ipa_title(language_fn()),"ipa_results",app_language_fn=language_fn)
  invisible(list(result=result,selections=selections,items=items))
}
