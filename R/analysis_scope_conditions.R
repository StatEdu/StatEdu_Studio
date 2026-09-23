# Conditions are interpreted from a small expression grammar; no R code is evaluated.
analysis_scope_expression <- function(values, expression, variable = "x") {
  if (!nzchar(trimws(expression)) || nchar(expression) > 4000L) stop("조건식을 입력하세요. / Enter a condition (up to 4000 characters).")
  parsed <- tryCatch(parse(text=expression,keep.source=FALSE),error=function(e) stop("조건식의 형식을 확인하세요. / Invalid condition syntax."))
  if (length(parsed)!=1L) stop("조건식은 하나만 입력하세요. / Enter one expression.")
  visit <- function(node, depth=0L) {
    if(depth>30L) stop("조건식이 너무 복잡합니다. / Condition is too complex.")
    if(is.atomic(node) && length(node)==1L) return(node)
    if(is.symbol(node) && as.character(node) %in% c("x",variable)) return(values)
    if(!is.call(node)) stop("x와 비교 연산자를 사용하세요. / Use x and comparison operators.")
    op <- as.character(node[[1]])
    if(length(op)!=1L || !op %in% c("(","!","&","|","==","!=","<","<=",">",">=","%in%","c","-","+")) stop("지원하지 않는 조건식입니다. / Unsupported condition expression.")
    args<-as.list(node)[-1]
    if(op=="c") {
      if(!all(vapply(args,function(a)is.atomic(a)&&length(a)==1L,logical(1)))) stop("c()에는 값만 입력하세요. / c() accepts literal values only.")
      return(unlist(args,use.names=FALSE))
    }
    unary<-op %in% c("(","!","-","+")
    if(length(args)!=if(unary)1L else 2L) stop("연산자 형식을 확인하세요. / Invalid operator arguments.")
    a<-visit(args[[1]],depth+1L)
    if(unary)return(switch(op,"("=a,"!"=!a,"-"=-a,"+"=+a))
    b<-visit(args[[2]],depth+1L)
    switch(op,"&"=a&b,"|"=a|b,"=="=a==b,"!="=a!=b,"<"=a<b,"<="=a<=b,">"=a>b,">="=a>=b,"%in%"=a%in%b)
  }
  result<-visit(parsed[[1]])
  if(!is.logical(result)||!length(result)%in%c(1L,length(values)))stop("참/거짓 조건식이 필요합니다. / A logical condition is required.")
  rep_len(result,length(values)) & !is.na(values) & !is.na(rep_len(result,length(values)))
}

analysis_scope_rule_mask <- function(values, rule) {
  mode<-rule$mode %||% "categories"
  if(mode=="expression")return(analysis_scope_expression(values,rule$expression,rule$variable))
  if(mode=="range") {
    x<-recode_numeric_values(values)
    lower<-rule$lower %||% -Inf; upper<-rule$upper %||% Inf
    if(is.na(lower)||is.na(upper)||lower>upper||(lower==upper&&(!isTRUE(rule$lower_inclusive)||!isTRUE(rule$upper_inclusive))))stop("범위의 하한과 상한을 확인하세요. / Invalid range bounds.")
    return(!is.na(x) & (if(isTRUE(rule$lower_inclusive))x>=lower else x>lower) & (if(isTRUE(rule$upper_inclusive))x<=upper else x<upper))
  }
  !is.na(values) & as.character(values)%in%rule$values
}

analysis_scope_condition_groups <- function(data, variable, conditions, label_fn=identity) {
  if(!is.data.frame(data)||length(variable)!=1L||!variable%in%names(data))stop("분할 변수를 확인하세요. / Split variable is unavailable.")
  if(!length(conditions))return(analysis_scope_groups(data,variable,label_fn))
  masks<-lapply(conditions,function(rule)analysis_scope_rule_mask(data[[variable]],rule))
  if(length(masks)>1L && any(Reduce(`+`,masks)>1L))stop("분할 집단의 조건이 겹칩니다. 경계값을 조정하세요. / Split conditions overlap; adjust the boundaries.")
  groups<-lapply(seq_along(conditions),function(i)list(label=paste0(variable,": ",conditions[[i]]$label),data=data[masks[[i]],,drop=FALSE]))
  Filter(function(group)nrow(group$data)>0L,groups)
}

analysis_scope_condition_ui <- function(kind, language, variable, continuous, choices) {
  names(choices)<-ifelse(names(choices)!=unname(choices),paste0(names(choices)," [",unname(choices),"]"),names(choices))
  id<-function(suffix)paste0("scope_",kind,"_",suffix)
  tr<-function(ko,en)analysis_scope_text(language,ko,en)
  modes<-if(continuous)c("range","expression")else c("categories","expression")
  labels<-if(continuous)c(tr("값·범위","Value / range"),tr("조건식","Condition"))else c(tr("카테고리 선택","Select categories"),tr("조건식","Condition"))
  div(class="scope-condition-editor",
    h4(tr("조건 설정","Conditions")),
    div(class="scope-condition-body",
    div(class="scope-condition-variable",span(tr("선택 변수","Selected variable")),tags$strong(variable)),
    tags$fieldset(class="scope-condition-group",
    tags$legend(tr("선택 조건","Selection condition")),
    radioButtons(id("mode"),NULL,setNames(modes,labels),selected=modes[[1]],inline=FALSE),
    if(!continuous)conditionalPanel(paste0("input.",id("mode")," == 'categories'"),
      div(class="scope-category-list",checkboxGroupInput(id("values"),tr("포함할 카테고리","Categories to include"),choices,width="100%"))),
    if(continuous)conditionalPanel(paste0("input.",id("mode")," == 'range'"),
      div(class="scope-condition-range-header",
        tags$label(`for`=id("lower"),tr("하한","Lower")),span(),span(),span(),
        tags$label(`for`=id("upper"),tr("상한","Upper"))),
      div(class="scope-condition-range",
        textInput(id("lower"),NULL,"",width="100%"),
        selectInput(id("lower_op"),NULL,c("≤"="ge","<"="gt"),selectize=FALSE,width="100%"),
        span(class="recode-builder-range-value","x"),
        selectInput(id("upper_op"),NULL,c("≤"="le","<"="lt"),selectize=FALSE,width="100%"),
        textInput(id("upper"),NULL,"",width="100%")),
      p(class="scope-condition-range-help",tr("빈칸: 하한은 최솟값, 상한은 최댓값까지 포함합니다.","Blank bounds include the minimum / maximum.")),
      p(class="scope-condition-help",tr("같은 값을 양쪽에 입력하면 해당 값만 선택합니다.","Enter the same inclusive bounds to select a single value."))),
    conditionalPanel(paste0("input.",id("mode")," == 'expression'"),
      textAreaInput(id("expression"),tr("조건식 (x = 선택한 변수)","Condition (x = selected variable)"),placeholder=if(continuous)"x >= 20 & x < 40"else 'x == "Male" | x == "Female"',rows=3,width="100%"),
      p(class="scope-condition-help",tr("원자료의 값/코드를 사용합니다. 지원: ==, !=, <, ≤(<=), >, ≥(>=), &, |, !, %in%, c().","Use raw values/codes. Operators: ==, !=, <, <=, >, >=, &, |, !, %in%, c().")))),
    div(class="scope-condition-add",
      textInput(id("label"),tr(if(kind=="split")"집단 이름"else"조건 이름",if(kind=="split")"Group name"else"Condition name"),"",width="100%"),
      actionButton(id("add_condition"),tr("조건 추가","Add condition"),class="btn-default")),
    uiOutput(id("rules")),
    p(class="scope-condition-help",tr(if(kind=="cases")"추가한 조건 중 하나라도 만족하는 케이스를 선택합니다."else"범위·조건식마다 별도 집단을 만듭니다. 집단 간 조건은 겹칠 수 없습니다.",
      if(kind=="cases")"Include cases matching any added condition."else"Each range/condition defines a separate group. Groups must not overlap."))),
    div(class="scope-condition-footer",
      div(class="scope-condition-actions",
        actionButton(id("apply"),tr("적용","Apply"),class="btn-primary"),
        actionButton(id("clear"),tr("해제","Turn off"))),
      div(class="scope-condition-status",`aria-live`="polite",uiOutput(id("status")))))
}

analysis_scope_register_controls <- function(input,output,session,data_fn,info_fn,labels_fn,value_choices,language_fn,editable) {
  targets<-list(cases=reactiveVal(""),split=reactiveVal(""))
  drafts<-list(cases=reactiveVal(list()),split=reactiveVal(list()))
  target<-function(kind) { chosen<-targets[[kind]]();if(nzchar(chosen))chosen else c(as.character(input[[paste0("scope_",kind,"_variable")]]),"")[[1]] }
  continuous<-function(variable) {
    info<-info_fn(); measurement<-if(is.data.frame(info)&&all(c("name","measurement")%in%names(info)))as.character(info$measurement[match(variable,info$name)])else NA_character_
    if(length(measurement)&&!is.na(measurement)&&nzchar(measurement))return(measurement%in%c("continuous","scale","numeric"))
    is.numeric(data_fn()[[variable]]) && !length(attr(data_fn()[[variable]],"labels"))
  }
  build<-function(kind) {
    id<-function(s)paste0("scope_",kind,"_",s)
    variable<-target(kind);if(!nzchar(variable)||!variable%in%names(data_fn()))stop("먼저 2블럭에서 변수를 선택하세요. / Select a variable in block 2.")
    mode<-input[[id("mode")]] %||% if(continuous(variable))"range"else"categories"
    rule<-list(variable=variable,mode=mode,label=trimws(input[[id("label")]]%||%""))
    if(mode=="categories") {
      rule$values<-as.character(input[[id("values")]]%||%character())
      if(!length(rule$values)&&kind=="split")rule$values<-unname(value_choices(data_fn(),variable))
      if(!length(rule$values))stop("카테고리를 선택하세요. / Select categories.")
      description<-paste(names(value_choices(data_fn(),variable))[match(rule$values,value_choices(data_fn(),variable))],collapse=", ")
    } else if(mode=="range") {
      bound<-function(name,default){text<-trimws(input[[id(name)]]%||%"");if(!nzchar(text))return(default);n<-suppressWarnings(as.numeric(text));if(!is.finite(n))stop("범위에는 숫자를 입력하세요. / Enter numeric bounds.");n}
      rule$lower<-bound("lower",-Inf);rule$upper<-bound("upper",Inf)
      rule$lower_inclusive<-!identical(input[[id("lower_op")]],"gt");rule$upper_inclusive<-!identical(input[[id("upper_op")]],"lt")
      description<-paste(if(is.finite(rule$lower))rule$lower else "Min",if(rule$lower_inclusive)"≤"else"<","x",if(rule$upper_inclusive)"≤"else"<",if(is.finite(rule$upper))rule$upper else "Max")
    } else if(mode=="expression") {
      rule$expression<-trimws(input[[id("expression")]]%||%"");description<-rule$expression
    } else stop("지원하지 않는 조건입니다. / Unsupported condition mode.")
    rule$description<-description
    if(!nzchar(rule$label))rule$label<-description
    analysis_scope_rule_mask(data_fn()[[variable]],rule)
    rule
  }
  lapply(c("cases","split"),function(kind)local({
    k<-kind;id<-function(s)paste0("scope_",k,"_",s)
    output[[id("controls")]]<-renderUI({
      data<-data_fn();req(is.data.frame(data));info<-info_fn()
      div(class="scope-three-block",
        div(class="analysis-transfer-column analysis-transfer-panel",h4(analysis_scope_text(language_fn(),"변수","Variables")),
          analysis_transfer_listbox_input(id("available"),items=analysis_variable_items(names(data),info,labels_fn()),size=18)),
        div(class="analysis-transfer-controls",actionButton(id("move"),"→",class="analysis-move-button")),
        div(class="analysis-transfer-column analysis-transfer-panel",h4(analysis_scope_text(language_fn(),"선택 변수","Selected variable")),uiOutput(id("target"))),
        div(class="analysis-options-column analysis-options-panel",uiOutput(id("condition"))))
    })
    output[[id("target")]]<-renderUI({
      chosen<-targets[[k]]()
      analysis_transfer_listbox_input(id("variable"),items=analysis_variable_items(if(nzchar(chosen))chosen else character(),info_fn(),labels_fn()),selected=chosen,size=18)
    })
    choose<-function(value) {
      if(!editable())return()
      value<-intersect(as.character(value),names(data_fn()))
      if(!length(value))return()
      if(!identical(targets[[k]](),value[[1]]))drafts[[k]](list())
      targets[[k]](value[[1]])
    }
    observeEvent(input[[id("move")]],choose(input[[id("available")]]))
    observeEvent(input[[id("available_doubleclick")]],choose(input[[id("available_doubleclick")]]$value),ignoreInit=TRUE)
    output[[id("condition")]]<-renderUI({
      variable<-target(k)
      if(!nzchar(variable)||!variable%in%names(data_fn()))return(div(class="scope-condition-editor",
        h4(analysis_scope_text(language_fn(),"조건 설정","Conditions")),
        div(class="scope-condition-body scope-condition-empty",p(analysis_scope_text(language_fn(),"변수를 2블럭으로 옮긴 뒤 조건을 설정하세요.","Move a variable to block 2, then configure conditions."))),
        div(class="scope-condition-footer",actionButton(id("clear"),analysis_scope_text(language_fn(),"해제","Turn off")),
          div(class="scope-condition-status",`aria-live`="polite",uiOutput(id("status"))))))
      analysis_scope_condition_ui(k,language_fn(),variable,continuous(variable),value_choices(data_fn(),variable))
    })
    output[[id("rules")]]<-renderUI({
      rules<-drafts[[k]]();if(!length(rules))return(NULL)
      tags$fieldset(class="scope-condition-group scope-rules",
        tags$legend(analysis_scope_text(language_fn(),"추가한 조건","Added conditions")),
        lapply(seq_along(rules),function(index)div(class="scope-rule",
        div(tags$strong(rules[[index]]$label),p(rules[[index]]$description)),
        tags$button(type="button",class="btn btn-default btn-xs",
          `aria-label`=paste(analysis_scope_text(language_fn(),"조건 삭제","Remove condition"),rules[[index]]$label),
          onclick=sprintf("Shiny.setInputValue('%s',{index:%d,nonce:Date.now()},{priority:'event'})",id("remove_condition"),index),"×"))))
    })
    observeEvent(input[[id("remove_condition")]],{
      if(!editable())return();index<-as.integer(input[[id("remove_condition")]]$index);rules<-drafts[[k]]()
      if(length(index)==1L&&!is.na(index)&&index>=1L&&index<=length(rules))drafts[[k]](rules[-index])
    },ignoreInit=TRUE)
    observeEvent(input[[id("add_condition")]],{
      if(!editable())return()
      tryCatch({rule<-build(k);drafts[[k]](c(drafts[[k]](),list(rule)))},error=function(e)showNotification(conditionMessage(e),type="warning"))
    })
  }))
  list(read=function(kind){
    rules<-drafts[[kind]]()
    if(!length(rules)) {
      rule<-build(kind)
      if(kind=="split"&&rule$mode=="categories") {
        choices<-value_choices(data_fn(),rule$variable)
        rules<-lapply(rule$values,function(value){r<-rule;r$values<-value;r$label<-names(choices)[match(value,choices)];r})
      }else rules<-list(rule)
    }
    list(variable=target(kind),rules=rules)
  },reset=function(kind){drafts[[kind]](list())})
}
