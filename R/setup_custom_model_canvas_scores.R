canvas_score_text <- function(text, language = statedu_initial_language(), values = list()) {
  key <- paste0("canvas.score.",gsub("^_+|_+$","",gsub("[^a-z0-9]+","_",tolower(text))))
  if(grepl(":$",text))key <- paste0(key,"_colon")
  translated <- statedu_t(key,language,text)
  for(name in names(values))translated <- gsub(paste0("{",name,"}"),as.character(values[[name]]),translated,fixed=TRUE)
  translated
}

# Score-definition tables follow the editor language, unlike journal effect tables.
canvas_score_html_table <- function(table, language) {
  result <- structural_canvas_basic_html_table(table, role="main", orientation="portrait", language=language)
  if (!is.null(result)) {
    result$attribs$lang <- normalize_app_language(language)
    result$attribs[["data-result-table-language"]] <- normalize_app_language(language)
  }
  result
}

# Model-local score definitions. Raw data are never changed or embedded in models.
canvas_score_items <- function(data, items) {
  items <- as.character(unlist(items, use.names = FALSE))
  if (length(items) < 2L || anyDuplicated(items)) stop(canvas_score_text("Select at least two distinct original items."))
  if (any(!items %in% names(data))) stop(canvas_score_text("Original items are missing from the loaded data."))
  if (length(intersect(items, analysis_scope_excluded(data)))) stop(canvas_score_text("Case-selection/split variables cannot be score items."))
  x <- data[, items, drop = FALSE]
  if (any(!vapply(x, is.numeric, logical(1)))) stop(canvas_score_text("Items must be numeric and reverse-scored beforehand."))
  if (any(vapply(x, function(v) any(is.infinite(v)), logical(1)))) stop(canvas_score_text("Infinite item values are not supported."))
  x
}

canvas_score_calculate <- function(data, indicator, items, method = "alpha", scoring = "sum", min_response = 1) {
  x <- canvas_score_items(data, items)
  if (!scoring %in% c("sum", "mean") || !method %in% c("alpha", "omega")) stop(canvas_score_text("Invalid score settings."))
  if (!indicator %in% names(data) || !is.numeric(data[[indicator]])) stop(canvas_score_text("The score indicator must be a loaded numeric variable."))
  score <- data[[indicator]]
  min_response <- as.numeric(min_response)
  if(length(min_response)!=1L || !is.finite(min_response) || !min_response %in% c(.8,1) || (scoring!="mean" && min_response!=1))stop(canvas_score_text("Invalid minimum response rule."))
  complete <- stats::complete.cases(x)
  eligible <- rowSums(!is.na(x)) >= ceiling(ncol(x)*min_response)
  if (any(!eligible & !is.na(score))) stop(canvas_score_text("Scores exist below the minimum item-response threshold."))
  keep <- eligible & is.finite(score)
  incomplete_n <- sum(keep & !complete)
  x <- x[keep, , drop = FALSE]; score <- score[keep]
  if (nrow(x) < max(5L, ncol(x) + 1L)) stop(canvas_score_text("Too few eligible observations."))
  expected <- if (scoring == "sum") rowSums(x) else rowMeans(x,na.rm=TRUE)
  if (any(abs(score - expected) > 1e-7 * pmax(1, abs(expected)))) stop(canvas_score_text("The selected items do not reproduce the score. Check sum/mean, items and reverse coding."))
  covariance <- stats::cov(x,use="pairwise.complete.obs")
  pair_n <- crossprod(!is.na(as.matrix(x)))
  if(any(pair_n<5) || any(!is.finite(covariance)) || any(diag(covariance)<=0) || min(eigen(covariance,symmetric=TRUE,only.values=TRUE)$values)< -1e-8)stop(canvas_score_text("Insufficient or inconsistent item covariance information."))
  variance <- stats::var(score)
  if (!is.finite(variance) || variance <= 0) stop(canvas_score_text("Score variance must be positive."))
  if (method == "alpha") {
    reliability <- ncol(x)/(ncol(x)-1) * (1 - sum(diag(covariance))/sum(covariance))
  } else {
    if (ncol(x) < 3L) stop(canvas_score_text("Omega requires at least three items."))
    # Covariance-scale congeneric omega, appropriate to an unweighted raw sum/mean.
    names(x) <- paste0("item", seq_len(ncol(x)))
    fit <- lavaan::cfa(paste("score_factor =~", paste(names(x), collapse = " + ")), data = x, std.lv = TRUE, estimator = "ML",missing=if(incomplete_n>0)"fiml" else "listwise")
    if (!isTRUE(lavaan::lavInspect(fit, "converged")) || !isTRUE(lavaan::lavInspect(fit, "post.check"))) stop(canvas_score_text("The omega measurement model is inadmissible."))
    est <- lavaan::lavInspect(fit, "est")
    common <- as.numeric(sum(est$lambda)^2 * est$psi[1,1])
    reliability <- common/(common + sum(est$theta))
  }
  if (!is.finite(reliability) || reliability <= 0 || reliability >= 1) stop(canvas_score_text("Reliability must be strictly between 0 and 1; review the items."))
  list(method = method, reliability = unname(reliability), variance = unname(variance),
       residual = unname((1-reliability)*variance), n = nrow(x), scoring = scoring,
       min_response=min_response,incomplete_n=incomplete_n,min_pair_n=min(pair_n),
       items = as.list(as.character(unlist(items))), indicator = indicator)
}

canvas_parcel_balance <- function(data, items, count = 3L, diagnostics = FALSE) {
  items <- as.character(unlist(items, use.names=FALSE))
  count <- as.numeric(count)
  if(length(count)!=1L || !is.finite(count) || !count %in% 3:5 || length(items)<2*count)
    stop(canvas_score_text("Use 3\u20135 parcels with at least two items each."))
  # Stable item names break ties independently of the selection/list order.
  ordered <- sort(items)
  x <- canvas_score_items(data,ordered)
  x <- x[rowSums(!is.na(x))>0,,drop=FALSE]
  if(nrow(x)<max(10L,ncol(x)+1L) || any(vapply(x,function(v)sum(is.finite(v))<5L || !is.finite(var(v,na.rm=TRUE)) || var(v,na.rm=TRUE)<=0,logical(1))))
    stop(canvas_score_text("Insufficient item information for factor analysis."))
  names(x) <- paste0("item",seq_along(ordered))
  fit <- lavaan::cfa(paste("parcel_factor =~",paste(names(x),collapse=" + ")),data=x,std.lv=TRUE,estimator="ML",missing="fiml")
  if(!isTRUE(lavaan::lavInspect(fit,"converged")) || !isTRUE(lavaan::lavInspect(fit,"post.check")))
    stop(canvas_score_text("The item factor model is inadmissible."))
  loadings <- as.numeric(lavaan::lavInspect(fit,"std")$lambda[,1])
  if(sum(loadings)<0)loadings <- -loadings
  invalid <- !is.finite(loadings) | loadings<=0 | loadings>1
  issue <- NULL
  if(any(invalid)) {
    values <- paste0(ordered[invalid]," = ",sprintf("%.3f",loadings[invalid]),collapse=", ")
    issue <- canvas_score_text("Automatic allocation stopped: standardized loadings outside (0, 1]: {items}. This does not prove a reverse-coding error. Review item direction and the one-factor structure. The review checkbox does not change calculated loadings.",values=list(items=values))
    if(!isTRUE(diagnostics))stop(issue)
  }
  rank <- order(-loadings,ordered)
  # Alternating rank blocks: 1..k, k..1. High and lower loadings are paired.
  positions <- seq_along(rank)-1L
  sequence <- ifelse((positions %/% count) %% 2L==0L,positions %% count+1L,count-positions %% count)
  groups <- integer(length(items));groups[rank] <- sequence
  index <- match(items,ordered)
  list(method="loading_balance",algorithm="descending standardized loadings; alternating rank blocks",items=as.list(items),
    groups=if(is.null(issue))as.list(groups[index]) else NULL,loadings=as.list(loadings[index]),n=nrow(x),issue=issue,issueItems=if(is.null(issue))NULL else values,
    fit=as.list(lavaan::fitMeasures(fit,c("cfi","tli","rmsea","srmr"))))
}

canvas_score_target <- function(snapshot, node_id) {
  node <- structural_canvas_node(snapshot, node_id)
  if (is.null(node)) stop(canvas_score_text("Select a score indicator or its latent variable."))
  edges <- snapshot$edges %||% list()
  if (node$role %in% c("error", "disturbance")) {
    out <- Filter(function(e) identical(e$from, node$id) && !identical(e$kind, "covariance"), edges)
    if (length(out) != 1L) stop(canvas_score_text("Select an indicator with one measurement parent."))
    node <- structural_canvas_node(snapshot, out[[1]]$to)
  }
  if (identical(node$role, "indicator")) {
    parents <- Filter(function(e) !identical(e$kind, "covariance") &&
      ((identical(e$to,node$id) && identical(structural_canvas_node(snapshot,e$from)$role,"latent")) ||
       (identical(e$from,node$id) && identical(structural_canvas_node(snapshot,e$to)$role,"latent"))), edges)
    if (length(parents) != 1L) stop(canvas_score_text("The indicator must belong to exactly one latent variable."))
    e <- parents[[1]]; node <- structural_canvas_node(snapshot, if (e$from == node$id) e$to else e$from)
  }
  if (!identical(node$role,"latent")) stop(canvas_score_text("Select a latent variable or its indicator."))
  measures <- Filter(function(e) !identical(e$kind,"covariance") &&
    ((identical(e$from,node$id) && identical(structural_canvas_node(snapshot,e$to)$role,"indicator")) ||
     (identical(e$to,node$id) && identical(structural_canvas_node(snapshot,e$from)$role,"indicator"))), edges)
  ids <- vapply(measures, function(e) if (e$from == node$id) e$to else e$from, character(1))
  if (length(ids) != 1L && is.null(node$scoreDesign)) stop(canvas_score_text("Start from a latent variable with one aggregate-score indicator."))
  original <- node$scoreDesign$original %||% structural_canvas_node(snapshot, ids[[1]])
  list(latent = node, ids = ids, original = original, measures = measures)
}

canvas_score_apply <- function(snapshot, node_id, data, config) {
  target <- canvas_score_target(snapshot, node_id)
  mode <- config$mode %||% "single"
  if (!mode %in% c("single","parcels")) stop(canvas_score_text("Invalid score mode."))
  items <- as.character(unlist(config$items, use.names = FALSE))
  x <- canvas_score_items(data, items)
  scoring <- config$scoring %||% "sum"
  if (!scoring %in% c("sum","mean")) stop(canvas_score_text("Invalid scoring rule."))
  original <- target$original
  original$scoreSpec <- NULL
  indicator <- custom_model_canvas_node_variable(original)
  design <- list(mode=mode, items=as.list(items), scoring=scoring, original=original,
                 missing="complete items; no prorating", method=config$method %||% "alpha")
  design$minResponse <- if(mode=="single" && scoring=="mean")as.numeric(config$minResponse %||% 1) else 1
  if(isTRUE(design$minResponse==.8))design$missing <- "mean of available items; at least 80% answered"
  if (mode == "single") {
    design$calculation <- canvas_score_calculate(data,indicator,items,design$method,scoring,design$minResponse)
  } else {
    if(identical(config$allocationMethod,"loading_balance")) {
      design$allocation <- canvas_parcel_balance(data,items,config$count %||% 3L)
      groups <- as.integer(unlist(design$allocation$groups,use.names=FALSE))
    } else groups <- suppressWarnings(as.integer(unlist(config$groups)))
    if (length(groups) != length(items) || anyNA(groups) || !length(groups) ||
        !identical(sort(unique(groups)), seq_len(max(groups))) || !max(groups) %in% 3:5 || any(table(groups)<2)) {
      stop(canvas_score_text("Assign every item once to 3\u20135 parcels, with at least two items per parcel."))
    }
    if (!isTRUE(config$reviewed)) stop(canvas_score_text("Confirm the dimensionality and allocation rationale."))
    design$groups <- as.list(groups)
    design$rationale <- trimws(as.character(config$rationale %||% ""))
    if (!nzchar(design$rationale)) stop(canvas_score_text("Record the parcel allocation rationale."))
  }
  # Refuse to silently destroy covariance or other substantive paths on replaced indicators.
  errors <- Filter(function(e) e$to %in% target$ids && identical(structural_canvas_node(snapshot,e$from)$role,"error") && !identical(e$kind,"covariance"), snapshot$edges)
  error_ids <- vapply(errors,function(e)e$from,character(1))
  removed <- c(target$ids,error_ids)
  allowed <- c(vapply(target$measures,function(e)e$id,character(1)),vapply(errors,function(e)e$id,character(1)))
  extras <- Filter(function(e) any(c(e$from,e$to) %in% removed) && !e$id %in% allowed, snapshot$edges)
  if (length(extras)) stop(canvas_score_text("Remove or review paths/covariances attached to the score/error before replacing it."))
  snapshot$nodes <- Filter(function(n)!n$id %in% removed,snapshot$nodes)
  snapshot$edges <- Filter(function(e)!e$id %in% allowed,snapshot$edges)
  li <- which(vapply(snapshot$nodes,function(n)identical(n$id,target$latent$id),logical(1)))
  snapshot$nodes[[li]]$scoreDesign <- design
  placement <- target$latent$measurementPlacement %||% target$latent$effectiveMeasurementPlacement
  if(is.null(placement) || !placement %in% c("left","right","top","bottom")) {
    dx <- as.numeric(original$x %||% 100)-as.numeric(target$latent$x %||% 300)
    dy <- as.numeric(original$y %||% 100)-as.numeric(target$latent$y %||% 100)
    placement <- if(abs(dx)>=abs(dy))if(dx<0)"left" else "right" else if(dy<0)"top" else "bottom"
  }
  snapshot$nodes[[li]]$measurementPlacement <- placement
  snapshot$nodes[[li]]$effectiveMeasurementPlacement <- placement
  count <- if(mode=="single") 1L else max(groups)
  occupied <- c(names(data),vapply(snapshot$nodes,custom_model_canvas_node_variable,character(1)))
  for (i in seq_len(count)) {
    id <- paste0(target$latent$id,"__score",i)
    name <- if(mode=="single") indicator else paste0(make.names(indicator),"_parcel",i)
    if(mode=="parcels" && name %in% occupied) stop(canvas_score_text("Parcel variable name already exists: {name}",values=list(name=name)))
    ids <- c(id,paste0(id,"_error"))
    if(any(vapply(snapshot$nodes,function(n)n$id %in% ids,logical(1)))) stop(canvas_score_text("Generated node IDs conflict with existing nodes."))
    n <- original; n$id <- id; n$name <- name; n$variableId <- name
    n$canvasLabel <- if(mode=="single") original$canvasLabel %||% "" else name
    n$dataLabel <- if(mode=="single") original$dataLabel %||% "" else name
    n$x <- as.numeric(original$x %||% 100)+if(placement %in% c("top","bottom"))(i-1)*135 else 0
    n$y <- as.numeric(original$y %||% 100)+if(placement %in% c("left","right"))(i-1)*75 else 0
    if(mode=="parcels") n$scoreSpec <- list(items=as.list(items[groups==i]),scoring=scoring,owner=target$latent$id)
    ew <- 26; iw <- as.numeric(n$width %||% 110); ih <- as.numeric(n$height %||% 38)
    ex <- switch(placement,left=n$x-ew-40,right=n$x+iw+40,n$x+iw/2-ew/2)
    ey <- switch(placement,top=n$y-ew-40,bottom=n$y+ih+40,n$y+ih/2-ew/2)
    err <- list(id=ids[[2]],name=paste0("δ",i),role="error",x=ex,y=ey,width=ew,height=ew,autoRole=TRUE)
    loading <- list(id=paste0(id,"_loading"),from=target$latent$id,to=id,kind="regression",free=i!=1L,fixedValue=if(i==1L)1 else NULL)
    residual <- list(id=paste0(id,"_residual"),from=err$id,to=id,kind="regression",free=mode!="single",fixedValue=if(mode=="single")design$calculation$residual else NULL)
    snapshot$nodes <- c(snapshot$nodes,list(n,err)); snapshot$edges <- c(snapshot$edges,list(loading,residual))
  }
  snapshot
}

canvas_score_data <- function(snapshot, data) {
  for (node in snapshot$nodes %||% list()) {
    spec <- node$scoreSpec
    if (is.null(spec)) next
    x <- canvas_score_items(data,spec$items)
    name <- custom_model_canvas_node_variable(node)
    expected <- if (identical(spec$scoring,"mean")) rowMeans(x) else rowSums(x)
    if (name %in% names(data) && !isTRUE(all.equal(data[[name]],expected,check.attributes=FALSE))) stop(canvas_score_text("Parcel conflicts with loaded variable: {name}",values=list(name=name)))
    data[[name]] <- expected
  }
  data
}

canvas_score_refresh <- function(snapshot, data) {
  for (index in seq_along(snapshot$nodes)) {
    node <- snapshot$nodes[[index]]
    d <- node$scoreDesign
    if (is.null(d) || !identical(d$mode,"single")) next
    target <- canvas_score_target(snapshot,node$id)
    if(length(target$ids)!=1L)stop(canvas_score_text("The single-score design no longer has one indicator. Reconfigure its original items."))
    indicator <- custom_model_canvas_node_variable(structural_canvas_node(snapshot,target$ids[[1]]))
    calc <- canvas_score_calculate(data,indicator,d$items,d$method,d$scoring,d$minResponse %||% 1)
    snapshot$nodes[[index]]$scoreDesign$calculation <- calc
    found <- FALSE
    for(j in seq_along(snapshot$edges)) {
      e <- snapshot$edges[[j]]
      if(e$id %in% vapply(target$measures,function(e)e$id,character(1))) {
        snapshot$edges[[j]]$free <- FALSE; snapshot$edges[[j]]$fixedValue <- 1
      }
      if(identical(e$to,target$ids[[1]]) && identical(structural_canvas_node(snapshot,e$from)$role,"error") && !identical(e$kind,"covariance")) {
        snapshot$edges[[j]]$free <- FALSE; snapshot$edges[[j]]$fixedValue <- calc$residual;found <- TRUE
      }
    }
    if(!found)stop(canvas_score_text("The score's error node was removed. Reapply the score definition."))
  }
  snapshot
}

canvas_score_audit <- function(snapshot) {
  rows <- list()
  for(node in snapshot$nodes %||% list()) {
    d <- node$scoreDesign
    if(is.null(d)) next
    calc <- d$calculation
    rows[[length(rows)+1L]] <- data.frame(Construct=structural_canvas_name(node),Mode=d$mode,
      Scoring=d$scoring,Items=paste(unlist(d$items),collapse=", "),
      Method=if(d$mode=="single")d$method else "",
      Reliability=if(d$mode=="single")calc$reliability else NA_real_,
      `Score variance`=if(d$mode=="single")calc$variance else NA_real_,
      `Fixed error variance`=if(d$mode=="single")calc$residual else NA_real_,
      N=if(d$mode=="single")calc$n else NA_integer_,
      Allocation=if(d$mode=="parcels")paste(paste0(unlist(d$items),"=P",unlist(d$groups)),collapse="; ") else "",
      Rationale=d$rationale %||% "",`Minimum response %`=100*(d$minResponse %||% 1),
      `Incomplete cases`=calc$incomplete_n %||% 0,`Minimum pair N`=calc$min_pair_n %||% calc$n %||% NA_integer_,check.names=FALSE)
  }
  if(length(rows))do.call(rbind,rows) else data.frame()
}

canvas_parcel_allocation_ui <- function(allocation, language="en") {
  ko <- identical(language,"ko")
  items <- unlist(allocation$items,use.names=FALSE)
  loading <- unlist(allocation$loadings,use.names=FALSE)
  groups <- unlist(allocation$groups,use.names=FALSE)
  blocked <- !is.null(allocation$issue)
  detail <- data.frame(Item=items,Loading=sprintf("%.3f",loading),Parcel=if(blocked)rep("—",length(items)) else paste("P",groups,sep=""))
  summary <- do.call(rbind,lapply(sort(unique(groups)),function(g)data.frame(Parcel=paste0("P",g),Items=sum(groups==g),`Mean loading`=sprintf("%.3f",mean(loading[groups==g])),check.names=FALSE)))
  names(detail) <- vapply(c("Item","Loading","Parcel"),canvas_score_text,character(1),language=language)
  if(!blocked)names(summary) <- vapply(c("Parcel","Item count","Mean loading"),canvas_score_text,character(1),language=language)
  fit <- unlist(allocation$fit)
  shiny::div(class="canvas-parcel-balanced",
    shiny::h4(canvas_score_text("Loading-balanced parcel allocation",language)),
    canvas_score_html_table(detail,language),
    if(blocked)shiny::div(class="text-danger canvas-parcel-diagnostic",canvas_score_text("Automatic allocation stopped: standardized loadings outside (0, 1]: {items}. This does not prove a reverse-coding error. Review item direction and the one-factor structure. The review checkbox does not change calculated loadings.",language,list(items=allocation$issueItems))) else canvas_score_html_table(summary,language),
    result_note_paragraph(paste0("N = ",allocation$n,"; ",paste(paste0(toupper(names(fit))," = ",sprintf("%.3f",fit)),collapse="; "),". ",
      if(blocked) {canvas_score_text("Item-level one-factor CFA (ML/FIML) diagnostics. No automatic allocation was performed.",language)} else canvas_score_text("Standardized item loadings from one-factor CFA (ML/FIML), descending rank with alternating forward/reverse blocks; ties use variable names. Balancing does not establish unidimensionality. Stored assignments remain fixed for reanalysis and bootstrap.",language))))
}

canvas_score_audit_ui <- function(snapshot, language = "en") {
  rows <- canvas_score_audit(snapshot)
  if(!nrow(rows))return(NULL)
  ko <- identical(language,"ko")
  # Long item lists get a two-column audit table for all screen/export writers.
  details <- do.call(rbind,lapply(seq_len(nrow(rows)),function(i) {
    selected <- if(rows$Mode[[i]]=="single")c(1:9,12:14) else c(1:4,10:11)
    labels <- vapply(names(rows),canvas_score_text,character(1),language=language)
    values <- vapply(selected,function(j){v <- rows[[j]][[i]];if(is.numeric(v))format(v,digits=8,trim=TRUE) else as.character(v)},character(1))
    # Translate only typed metadata, never user labels, items or rationale.
    for(j in seq_along(selected)) {
      col <- selected[[j]]
      source <- if(col==2)if(values[[j]]=="single")"Single aggregate-score indicator" else "Parcel" else
        if(col==3)if(values[[j]]=="sum")"Sum" else "Mean" else
        if(col==5)if(values[[j]]=="alpha")"Cronbach alpha" else "McDonald omega" else NULL
      if(!is.null(source))values[[j]] <- canvas_score_text(source,language)
    }
    result <- data.frame(Field=paste(rows$Construct[[i]],labels[selected],sep=" · "),Value=values,check.names=FALSE)
    names(result)<-vapply(c("Field","Value"),canvas_score_text,character(1),language=language)
    result
  }))
  allocation_tables <- lapply(snapshot$nodes,function(node) {
    allocation <- node$scoreDesign$allocation
    if(is.null(allocation))return(NULL)
    shiny::tagList(shiny::h4(structural_canvas_name(node)),canvas_parcel_allocation_ui(allocation,language))
  })
  shiny::tagList(allocation_tables,shiny::h4(canvas_score_text("Original items · reliability constraints / parcel definitions",language)),
    canvas_score_html_table(details,language),
    result_note_paragraph(canvas_score_text("Single indicators use cases meeting the minimum response rule with an available score. Means use answered items. Alpha uses pairwise available covariances; omega uses a one-factor model (FIML with missing items). Score variance is the sample variance (N−1) of those analysis cases. Applying full-scale reliability as a common error variance when item counts vary is an approximation. Loading=1; error variance=(1−reliability)×score variance. Bootstrap fixes these constraints and does not include reliability-estimation uncertainty. Parcels use complete item sums/means; missing items yield missing parcels.",language)))
}
