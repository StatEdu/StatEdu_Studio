# Importance-performance analysis. All matching occurs before complete-case filtering.
ipa_text <- function(en, language=statedu_initial_language(), ko=NULL) {
  key <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(en))))
  special <- c("‹ Pre"="previous_pre","Post ›"="next_post","Pre: "="pre_prefix","Post: "="post_prefix")
  if(en %in% names(special))key<-unname(special[[en]])
  statedu_t(paste0("analysis.ipa.",key),language,
    fallback=if(identical(normalize_app_language(language),"ko"))ko%||%en else en)
}
ipa_error_message <- function(message,language=statedu_initial_language()) {
  parts<-strsplit(message," / ",fixed=TRUE)[[1]]
  ipa_text(tail(parts,1),language,if(length(parts)>1)parts[1] else message)
}
ipa_empty_items <- function() data.frame(Item=character(), Performance=character(), Importance=character(),
  PostPerformance=character(), PostImportance=character(), stringsAsFactors=FALSE)

ipa_partial <- function(x, y, logarithmic=FALSE) {
  x <- as.matrix(x)
  if (logarithmic) {
    if (any(x <= 0)) stop("로그 IPA의 수행도는 모두 양수여야 합니다. / Log IPA requires positive performance scores.")
    x <- log(x)
  }
  k <- ncol(x)
  if (nrow(x) <= k + 2L || any(apply(cbind(x,y),2,sd) <= 0)) return(rep(NA_real_, k))
  r <- cor(cbind(x,y))
  if (!is.finite(rcond(r)) || rcond(r) < 1e-10) return(rep(NA_real_, k))
  precision <- solve(r)
  -precision[seq_len(k),k+1L] / sqrt(diag(precision)[seq_len(k)] * precision[k+1L,k+1L])
}

ipa_mean_ci <- function(x) {
  n <- length(x); m <- mean(x)
  if (n < 2L) return(c(m,NA,NA))
  margin <- qt(.975,n-1) * sd(x)/sqrt(n)
  c(m,m-margin,m+margin)
}

ipa_difference <- function(a,b,paired=FALSE) {
  # Always second minus first, including paired time changes.
  delta <- mean(b)-mean(a)
  test <- tryCatch(t.test(b,a,paired=paired), error=function(e) NULL)
  if (is.null(test)) return(c(delta,NA,NA,NA))
  c(delta,unname(test$conf.int),test$p.value)
}

prepare_ipa <- function(data, items, mode="direct", design="overall", layout="wide",
                        group=character(), id=character(), time=character(), pre="", post="",
                        outcome=character(), post_outcome=character(), derived="partial",
                        reference="pooled", x_reference=3, y_reference=3,
                        resamples=1000L, seed=20260917L,
                        show_quadrants=TRUE, quadrant_font_size=9,
                        quadrant_labels=c("Concentrate here","Keep up","Low priority","Possible overkill"),
                        group_plot="overlay") {
  if(identical(design,"grouped_paired"))return(ipa_prepare_grouped(as.list(environment())))
  if(!group_plot %in% c("overlay","separate") || length(quadrant_font_size)!=1L ||
     !is.finite(quadrant_font_size) || quadrant_font_size<4 || quadrant_font_size>36 ||
     length(quadrant_labels)!=4L || anyNA(quadrant_labels)) stop("Invalid IPA chart options.")
  stopifnot(is.data.frame(data),is.data.frame(items))
  if (!mode %in% c("direct","derived") || !design %in% c("overall","independent","paired") ||
      !layout %in% c("wide","long") || !derived %in% c("partial","log_partial") ||
      !reference %in% c("pooled","within","custom")) stop("Invalid IPA options.")
  if (!nrow(items) || !all(names(ipa_empty_items()) %in% names(items)) ||
      anyNA(items) || any(!nzchar(trimws(items$Item))) || anyDuplicated(items$Item)) stop("고유한 항목 이름과 변수쌍을 지정하세요. / Specify unique item labels and variable pairs.")
  group <- as.character(group); id <- as.character(id); time <- as.character(time)
  outcome <- as.character(outcome); post_outcome <- as.character(post_outcome)
  variables <- unique(c(items$Performance,if(mode=="direct")items$Importance else outcome,
    if(design=="paired" && layout=="wide")c(items$PostPerformance,if(mode=="direct")items$PostImportance else post_outcome)))
  structural <- c(if(design=="independent")group,if(design=="paired")id,if(design=="paired" && layout=="long")time)
  all_variables <- unique(c(variables,structural))
  scoped <- all_variables
  analysis_scope_prepare_variables(data,environment(),"scoped")
  if (!setequal(scoped,all_variables)) stop("케이스 선택·파일 분할 변수는 IPA 분석변수로 사용할 수 없습니다. / Scope variables cannot be IPA terms.")
  if (any(!nzchar(variables)) || !all(all_variables %in% names(data))) stop("필요한 변수를 모두 지정하세요. / Select all required variables.")
  if (mode=="derived" && (length(outcome)!=1L || (design=="paired" && layout=="wide" && length(post_outcome)!=1L))) stop("전반적 만족도 변수를 지정하세요. / Select the overall satisfaction outcome.")
  if (design=="independent" && length(group)!=1L) stop("집단변수를 지정하세요. / Select a group variable.")
  if (design=="paired" && layout=="long" && (length(id)!=1L || length(time)!=1L || !nzchar(pre) || !nzchar(post) || pre==post)) stop("ID, 시점, 서로 다른 사전·사후 값을 지정하세요. / Select ID and two distinct times.")
  if (anyDuplicated(items$Performance) || (mode=="direct" && anyDuplicated(items$Importance)) ||
      (design=="paired" && layout=="wide" && (anyDuplicated(items$PostPerformance) || (mode=="direct" && anyDuplicated(items$PostImportance))))) stop("각 역할 내 변수는 중복할 수 없습니다. / Duplicate variables within an indicator role.")
  used_columns <- c(items$Performance,if(mode=="direct")items$Importance else outcome,
    if(design=="paired" && layout=="wide")c(items$PostPerformance,if(mode=="direct")items$PostImportance else post_outcome))
  if (anyDuplicated(used_columns) || length(intersect(structural,variables))) stop("서로 다른 역할에 동일한 변수를 배정할 수 없습니다. / Measurement roles must use distinct variables.")
  if (!all(vapply(data[variables],is.numeric,logical(1)))) stop("점수 변수는 숫자형이어야 합니다. / Scores must be numeric.")
  if (any(vapply(data[variables],function(x)any(is.infinite(x)),logical(1)))) stop("무한대 점수를 처리한 후 실행하세요. / Infinite scores are not supported.")
  if (!is.finite(seed) || seed < 0 || seed > .Machine$integer.max || seed != as.integer(seed)) stop("Invalid random seed.")
  if (mode=="derived" && (length(resamples)!=1L || !is.finite(resamples) || resamples<100 || resamples>10000 || resamples!=as.integer(resamples))) stop("부트스트랩 반복수는 100~10000 정수입니다. / Use 100–10000 bootstrap resamples.")
  if (reference=="custom" && (any(!is.finite(c(x_reference,y_reference))) || length(x_reference)!=1L || length(y_reference)!=1L)) stop("Invalid reference coordinates.")
  make_block <- function(frame, after=FALSE) {
    pc <- if(after)items$PostPerformance else items$Performance
    ic <- if(after)items$PostImportance else items$Importance
    list(p=as.matrix(frame[pc]),i=if(mode=="direct")as.matrix(frame[ic]) else NULL,
      y=if(mode=="derived")frame[[if(after)post_outcome else outcome]] else NULL)
  }
  audit <- data.frame(Group=character(),Available=integer(),Analyzed=integer(),Excluded=integer())
  blocks <- list()
  if (design=="paired") {
    if (layout=="long") {
      frame <- data[!is.na(data[[time]]) & as.character(data[[time]]) %in% c(pre,post),,drop=FALSE]
      frame <- frame[!is.na(frame[[id]]) & nzchar(as.character(frame[[id]])),,drop=FALSE]
      a <- frame[as.character(frame[[time]])==pre,,drop=FALSE]
      b <- frame[as.character(frame[[time]])==post,,drop=FALSE]
      if (anyDuplicated(a[[id]]) || anyDuplicated(b[[id]])) stop("ID·시점별 관측치는 하나여야 합니다. / Duplicate ID-time records.")
      available <- length(union(as.character(a[[id]]),as.character(b[[id]])))
      pos <- match(a[[id]],b[[id]]); present <- !is.na(pos)
      a <- a[present,,drop=FALSE]; b <- b[pos[present],,drop=FALSE]
      keep <- complete.cases(a[variables]) & complete.cases(b[variables])
      blocks <- list(Pre=make_block(a[keep,,drop=FALSE]),Post=make_block(b[keep,,drop=FALSE]))
    } else {
      if (length(id)) {
        if(length(id)!=1L || anyNA(data[[id]]) || anyDuplicated(data[[id]]) || any(!nzchar(as.character(data[[id]])))) stop("가로형 자료의 ID는 결측 없이 고유해야 합니다. / Wide-data IDs must be unique and nonmissing.")
      }
      available <- nrow(data); keep <- complete.cases(data[variables])
      frame <- data[keep,,drop=FALSE]
      blocks <- list(Pre=make_block(frame),Post=make_block(frame,TRUE))
    }
    n <- nrow(blocks[[1]]$p)
    audit <- data.frame(Group=c("Pre","Post"),Available=available,Analyzed=n,Excluded=available-n)
  } else {
    groups <- if(design=="independent") ipa_sorted_groups(data[[group]]) else "Overall"
    if(design=="independent" && length(groups)<2L) stop("비교할 집단이 두 개 이상 필요합니다. / At least two groups are required.")
    if(length(groups)>20L) stop("집단 수는 최대 20개입니다. / At most 20 groups are supported.")
    for (g in groups) {
      frame <- if(design=="independent")data[!is.na(data[[group]]) & as.character(data[[group]])==g,,drop=FALSE] else data
      keep <- complete.cases(frame[variables]); blocks[[g]] <- make_block(frame[keep,,drop=FALSE])
      audit <- rbind(audit,data.frame(Group=g,Available=nrow(frame),Analyzed=sum(keep),Excluded=sum(!keep)))
    }
  }
  if(any(vapply(blocks,function(b)nrow(b$p)<3L,logical(1)))) stop("집단/시점별 완전한 응답자가 최소 3명 필요합니다. / At least 3 complete respondents per group/time are required.")
  if(mode=="derived" && derived=="log_partial" && any(vapply(blocks,function(b)any(b$p<=0),logical(1)))) stop("로그 변환은 양수 수행도에만 적용합니다. / Log transformation requires positive performance scores.")
  estimates <- lapply(blocks,function(b)if(mode=="direct")colMeans(b$i) else ipa_partial(b$p,b$y,derived=="log_partial"))
  if(mode=="derived" && any(!is.finite(unlist(estimates)))) stop("추정 중요도를 계산할 수 없습니다: 표본 수, 상수 변수 또는 다중공선성을 확인하세요. / Derived importance unavailable: check sample size, constant variables and collinearity.")
  boots <- NULL
  if(mode=="derived") {
    had_seed <- exists(".Random.seed",.GlobalEnv,inherits=FALSE)
    if(had_seed) old_seed <- get(".Random.seed",.GlobalEnv)
    on.exit(if(had_seed)assign(".Random.seed",old_seed,.GlobalEnv) else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv),add=TRUE)
    set.seed(as.integer(seed))
    boots <- lapply(blocks,function(b)matrix(NA_real_,resamples,nrow(items)))
    for(r in seq_len(resamples)) {
      shared <- if(design=="paired")sample.int(nrow(blocks[[1]]$p),replace=TRUE) else NULL
      for(g in seq_along(blocks)) {
        b <- blocks[[g]]; ix <- if(is.null(shared))sample.int(nrow(b$p),replace=TRUE) else shared
        boots[[g]][r,] <- ipa_partial(b$p[ix,,drop=FALSE],b$y[ix],derived=="log_partial")
      }
    }
  }
  boot_ci <- function(x) {
    good <- is.finite(x)
    if(sum(good)<max(50L,ceiling(.8*resamples))) return(c(NA,NA))
    unname(quantile(x[good],c(.025,.975),type=7))
  }
  coordinates <- do.call(rbind,lapply(seq_along(blocks),function(g) {
    b <- blocks[[g]]
    do.call(rbind,lapply(seq_len(nrow(items)),function(j) {
      pc <- ipa_mean_ci(b$p[,j]); ic <- if(mode=="direct")ipa_mean_ci(b$i[,j]) else c(estimates[[g]][j],boot_ci(boots[[g]][,j]))
      data.frame(Group=names(blocks)[g],Item=items$Item[j],N=nrow(b$p),Performance=pc[1],Importance=ic[1],
        PerformanceSD=sd(b$p[,j]),ImportanceSD=if(mode=="direct")sd(b$i[,j]) else NA_real_,
        PerformanceLower=pc[2],PerformanceUpper=pc[3],ImportanceLower=ic[2],ImportanceUpper=ic[3],
        ValidBoot=if(mode=="derived")sum(is.finite(boots[[g]][,j])) else NA_integer_)
    }))
  }))
  weighted <- function(column)weighted.mean(coordinates[[column]],coordinates$N)
  totals <- do.call(rbind,lapply(seq_along(blocks),function(g) {
    b <- blocks[[g]]
    data.frame(Group=names(blocks)[g],N=nrow(b$p),Performance=mean(rowMeans(b$p)),
      PerformanceSD=sd(rowMeans(b$p)),Importance=if(mode=="direct")mean(rowMeans(b$i)) else NA_real_,
      ImportanceSD=if(mode=="direct")sd(rowMeans(b$i)) else NA_real_)
  }))
  refs <- data.frame(Group=names(blocks),Performance=if(reference=="custom")x_reference else weighted("Performance"),
    Importance=if(reference=="custom")y_reference else weighted("Importance"))
  if(reference=="within") for(g in seq_len(nrow(refs))) {
    rows <- coordinates$Group==refs$Group[g]
    refs$Performance[g] <- mean(coordinates$Performance[rows]); refs$Importance[g] <- mean(coordinates$Importance[rows])
  }
  coordinates$Quadrant <- vapply(seq_len(nrow(coordinates)),function(j) {
    ref <- refs[match(coordinates$Group[j],refs$Group),]
    dx <- coordinates$Performance[j]-ref$Performance; dy <- coordinates$Importance[j]-ref$Importance
    if(abs(dx)<1e-10 || abs(dy)<1e-10) "On reference" else if(dx>0 && dy>0)"Keep up" else
      if(dx<0 && dy>0)"Concentrate here" else if(dx<0 && dy<0)"Low priority" else "Possible overkill"
  },character(1))
  comparisons <- data.frame()
  if(length(blocks)>1L) {
    combinations <- combn(seq_along(blocks),2,simplify=FALSE)
    comparisons <- do.call(rbind,lapply(combinations,function(pair)do.call(rbind,lapply(seq_len(nrow(items)),function(j) {
      a <- blocks[[pair[1]]]; b <- blocks[[pair[2]]]
      do.call(rbind,lapply(c("Performance","Importance"),function(metric) {
        derived_metric <- mode=="derived" && metric=="Importance"
        if(derived_metric) {
          replicates <- boots[[pair[2]]][,j]-boots[[pair[1]]][,j]
          stat <- c(estimates[[pair[2]]][j]-estimates[[pair[1]]][j],boot_ci(replicates),NA)
        } else stat <- ipa_difference(if(metric=="Performance")a$p[,j] else a$i[,j],if(metric=="Performance")b$p[,j] else b$i[,j],design=="paired")
        data.frame(First=names(blocks)[pair[1]],Second=names(blocks)[pair[2]],Item=items$Item[j],Metric=metric,
          Difference=stat[1],Lower=stat[2],Upper=stat[3],p=stat[4],
          ValidBoot=if(derived_metric)sum(is.finite(replicates)) else NA_integer_,
          Method=if(derived_metric)"Paired-case bootstrap" else if(design=="paired")"Paired t" else "Welch t")
      }))
    }))))
    comparisons$Method[comparisons$Method=="Paired-case bootstrap" & design!="paired"] <- "Independent-case bootstrap"
    comparisons$Holm <- NA_real_
    finite_p <- is.finite(comparisons$p)
    comparisons$Holm[finite_p] <- p.adjust(comparisons$p[finite_p],"holm")
  }
  limits <- ipa_axis_limits(list(coordinates=coordinates,references=refs))
  list(type="ipa",mode=mode,design=design,layout=layout,derived=derived,reference=reference,seed=seed,resamples=resamples,
    items=items,audit=audit,coordinates=coordinates,totals=totals,comparisons=comparisons,references=refs,outcome=outcome,post_outcome=post_outcome,
    show_quadrants=show_quadrants,quadrant_font_size=quadrant_font_size,quadrant_labels=quadrant_labels,group_plot=group_plot,
    xlim=limits$x,ylim=limits$y,time_labels=if(design=="paired" && layout=="long")c(pre,post) else c("Pre","Post"),
    missing_group=if(design=="independent")sum(is.na(data[[group]])) else 0L)
}

ipa_axis_limits <- function(result,ci=FALSE) {
  bounds <- function(metric) {
    d <- result$coordinates
    values <- c(d[[metric]],result$references[[metric]])
    if(ci)values <- c(values,d[[paste0(metric,"Lower")]],d[[paste0(metric,"Upper")]])
    extent <- range(values,finite=TRUE)
    span <- diff(extent)
    # Reserve left/bottom gutters for reference labels, separate from markers
    # and their item numbers. Retain tight top/right limits and shared axes.
    unit <- if(span>0)span else max(abs(extent[1])*.125,.125)
    lower_pad <- if(metric=="Performance") .24 else .16
    extent + unit*c(-lower_pad,.08)
  }
  list(x=bounds("Performance"),y=bounds("Importance"))
}

ipa_reference_labels <- function(result,group) {
  ref <- result$references[match(group,result$references$Group),]
  prefix <- if(result$reference=="custom")"Reference" else "M"
  c(Performance=sprintf("%s = %.3f",prefix,ref$Performance),Importance=sprintf("%s = %.3f",prefix,ref$Importance))
}

ipa_group_symbols <- function(groups) setNames(rep(c(16L,17L,15L,18L,3L,7L,8L,0L,1L,2L,5L,6L,9L,10L,11L,12L,13L,14L,4L,19L,20L,21L,22L,23L,24L,25L),length.out=length(groups)),groups)

ipa_sorted_groups <- function(x) {
  values<-unique(as.character(x[!is.na(x)]))
  numeric_values<-suppressWarnings(as.numeric(values))
  if(length(values) && all(is.finite(numeric_values)))values[order(numeric_values,values)] else sort(values)
}

ipa_contrast_colors <- function(n) {
  palette<-c("#0072B2","#D55E00","#009E73","#CC79A7","#E69F00","#56B4E9","#332288","#882255","#44AA99","#999933")
  if(n<=length(palette))return(palette[seq_len(n)])
  c(palette,grDevices::hcl.colors(n-length(palette),"Dark 3"))
}

ipa_group_time_map <- function(groups) {
  data.frame(Series=paste0(rep(nchar(groups),each=2),":",rep(groups,each=2),":",rep(c("Pre","Post"),length(groups))),
    Group=rep(groups,each=2),Time=rep(c("Pre","Post"),length(groups)),stringsAsFactors=FALSE)
}

ipa_prepare_grouped <- function(args) {
  data<-args$data;group<-args$group
  if(length(group)!=1L || !group %in% names(data))stop("집단변수를 지정하세요. / Select a group variable.")
  scoped<-group;analysis_scope_prepare_variables(data,environment(),"scoped")
  if(!identical(scoped,group))stop("Scope variables cannot be IPA terms.")
  if(group %in% c(args$items$Performance,args$items$Importance,args$items$PostPerformance,args$items$PostImportance,args$id,args$time,args$outcome,args$post_outcome))stop("Group and measurement/ID/time variables must be distinct.")
  groups<-ipa_sorted_groups(data[[group]])
  if(length(groups)<2L || length(groups)>20L)stop("집단은 2~20개여야 합니다. / Use 2–20 groups.")
  map<-ipa_group_time_map(groups)
  parts<-lapply(seq_along(groups),function(j) {
    part<-args;part$design<-"paired";part$group<-character()
    part$data<-data[!is.na(data[[group]]) & as.character(data[[group]])==groups[j],,drop=FALSE]
    part$seed<-as.integer((as.double(args$seed)+j-1)%%.Machine$integer.max)
    r<-do.call(prepare_ipa,part);keys<-map$Series[map$Group==groups[j]]
    for(name in c("audit","coordinates","totals","references"))r[[name]]$Group<-keys[match(r[[name]]$Group,c("Pre","Post"))]
    r$comparisons$First<-keys[match(r$comparisons$First,c("Pre","Post"))]
    r$comparisons$Second<-keys[match(r$comparisons$Second,c("Pre","Post"))]
    r
  })
  result<-parts[[1]];result$design<-"grouped_paired";result$series_map<-map;result$seed<-args$seed
  for(name in c("audit","coordinates","totals","comparisons","references"))result[[name]]<-do.call(rbind,lapply(parts,`[[`,name))
  result$missing_group<-sum(is.na(data[[group]]))
  result$group_labels<-setNames(paste(map$Group,map$Time,sep=" · "),map$Series)
  for(metric in c("Performance","Importance")) {
    if(args$reference=="pooled")result$references[[metric]]<-weighted.mean(result$coordinates[[metric]],result$coordinates$N)
    if(args$reference=="within")for(g in groups) {
      keys<-map$Series[map$Group==g];rows<-result$coordinates$Group %in% keys
      result$references[[metric]][result$references$Group %in% keys]<-weighted.mean(result$coordinates[[metric]][rows],result$coordinates$N[rows])
    }
  }
  d<-result$coordinates;ref<-result$references[match(d$Group,result$references$Group),]
  dx<-d$Performance-ref$Performance;dy<-d$Importance-ref$Importance
  result$coordinates$Quadrant<-ifelse(abs(dx)<1e-10|abs(dy)<1e-10,"On reference",ifelse(dx>0 & dy>0,"Keep up",ifelse(dx<0 & dy>0,"Concentrate here",ifelse(dx<0 & dy<0,"Low priority","Possible overkill"))))
  ok<-is.finite(result$comparisons$p);result$comparisons$Holm[ok]<-p.adjust(result$comparisons$p[ok],"holm")
  result$point_styles<-data.frame(Group=map$Series,Shape=rep(c(16L,17L),length(groups)),Color=rep(ipa_contrast_colors(length(groups)),each=2),Size=1.3)
  bounds<-ipa_axis_limits(result);result$xlim<-bounds$x;result$ylim<-bounds$y
  result
}

ipa_default_point_styles <- function(groups) data.frame(Group=groups,Shape=unname(ipa_group_symbols(groups)),
  Color=if(length(groups)==1L)"#253746" else ipa_contrast_colors(length(groups)),Size=rep(1.3,length(groups)))

ipa_validate_point_styles <- function(styles,groups) {
  if(is.null(styles))return(ipa_default_point_styles(groups))
  if(!all(c("Group","Shape","Color","Size") %in% names(styles)) || anyDuplicated(styles$Group) || !all(groups %in% styles$Group))stop("Invalid IPA point styles.")
  styles<-styles[match(groups,styles$Group),]
  if(anyNA(styles) || any(!styles$Shape %in% 0:25) || any(!is.finite(styles$Size) | styles$Size<.3 | styles$Size>4) ||
     any(!grepl("^#[0-9a-fA-F]{6}$",styles$Color)))stop("점 설정을 확인하세요: 색상 #RRGGBB, 크기 0.3–4. / Use #RRGGBB colors and sizes from 0.3 to 4.")
  styles
}

ipa_plot <- function(result, groups=unique(result$coordinates$Group), comparison=FALSE, ci=FALSE) {
  limits <- ipa_axis_limits(result,ci)
  result$xlim <- limits$x; result$ylim <- limits$y
  all_groups <- unique(result$coordinates$Group)
  styles <- ipa_validate_point_styles(result$point_styles,all_groups)
  colors <- setNames(styles$Color,all_groups);symbols<-setNames(styles$Shape,all_groups);sizes<-setNames(styles$Size,all_groups)
  old <- par(mar=c(4.2,4.5,2.6,1.2),mgp=c(2.6,.7,0),tcl=-.25,las=1,family="sans",cex=.9)
  on.exit(par(old))
  plot(NA,xlim=result$xlim,ylim=result$ylim,xlab=result$x_label%||%"Performance",ylab=result$y_label%||%if(result$mode=="direct")"Importance" else "Derived importance (partial r)",
    xaxs="i",yaxs="i",bty="l")
  dx <- diff(result$xlim);dy <- diff(result$ylim)
  same_reference<-length(unique(result$references$Performance[result$references$Group %in% groups]))==1L && length(unique(result$references$Importance[result$references$Group %in% groups]))==1L
  if(result$reference!="within" || !comparison || same_reference) {
    ref <- result$references[match(groups[1],result$references$Group),]
    abline(v=ref$Performance,h=ref$Importance,lty=2,lwd=.8,col="#7b8794")
    labels <- ipa_reference_labels(result,groups[1])
    # Edge callouts remain beside the dashed lines instead of covering central data.
    label_box <- function(x,y,label,adj) {
      w<-strwidth(label,cex=.78);h<-strheight(label,cex=.78)
      rect(x-adj[1]*w-.007*dx,y-adj[2]*h-.006*dy,x+(1-adj[1])*w+.007*dx,y+(1-adj[2])*h+.006*dy,col="white",border=NA)
      text(x,y,label,adj=adj,cex=.78,col="#253746")
    }
    label_box(ref$Performance+.012*dx,result$ylim[1]+.025*dy,labels[["Performance"]],c(0,0))
    label_box(result$xlim[1]+.012*dx,ref$Importance+.018*dy,labels[["Importance"]],c(0,0))
  }
    if(isTRUE(result$show_quadrants %||% TRUE)) for(k in 1:4) text(result$xlim[c(1,2,1,2)[k]]+c(1,-1,1,-1)[k]*.015*dx,
      result$ylim[c(2,2,1,1)[k]]+c(-1,-1,1,1)[k]*.075*dy,
      (result$quadrant_labels %||% c("Concentrate here","Keep up","Low priority","Possible overkill"))[k],
      adj=c(c(0,1,0,1)[k],.5),cex=(result$quadrant_font_size %||% 9)/par("ps"),col="#64748b")
  if(comparison && length(groups)>1L) for(j in seq_len(length(groups)-1L)) {
    if(identical(result$design,"grouped_paired")) {
      map<-result$series_map
      if(map$Group[match(groups[j],map$Series)]!=map$Group[match(groups[j+1L],map$Series)])next
    }
    selected <- result$connected_items %||% result$items$Item
    a <- result$coordinates[result$coordinates$Group==groups[j] & result$coordinates$Item %in% selected,]
    b <- result$coordinates[result$coordinates$Group==groups[j+1L] & result$coordinates$Item %in% selected,]
    b <- b[match(a$Item,b$Item),]
    if(result$design %in% c("paired","grouped_paired")) {
      moving <- abs(a$Performance-b$Performance)+abs(a$Importance-b$Importance)>1e-10
      arrows(a$Performance[moving],a$Importance[moving],b$Performance[moving],b$Importance[moving],length=.07,col="#9aa5b1",lwd=.7)
    } else segments(a$Performance,a$Importance,b$Performance,b$Importance,col="#9aa5b1",lty=3,lwd=.7)
  }
  for(g in groups) {
    d <- result$coordinates[result$coordinates$Group==g,]
    if(ci) {
      segments(d$PerformanceLower,d$Importance,d$PerformanceUpper,d$Importance,col=adjustcolor(colors[g],alpha.f=.45),lwd=.85)
      segments(d$Performance,d$ImportanceLower,d$Performance,d$ImportanceUpper,col=adjustcolor(colors[g],alpha.f=.45),lwd=.85)
      segments(d$PerformanceLower,d$Importance-.007*dy,d$PerformanceLower,d$Importance+.007*dy,col=colors[g],lwd=.6)
      segments(d$PerformanceUpper,d$Importance-.007*dy,d$PerformanceUpper,d$Importance+.007*dy,col=colors[g],lwd=.6)
      segments(d$Performance-.007*dx,d$ImportanceLower,d$Performance+.007*dx,d$ImportanceLower,col=colors[g],lwd=.6)
      segments(d$Performance-.007*dx,d$ImportanceUpper,d$Performance+.007*dx,d$ImportanceUpper,col=colors[g],lwd=.6)
    }
    points(d$Performance,d$Importance,pch=symbols[g],col=colors[g],bg=colors[g],cex=sizes[g])
    text(d$Performance,d$Importance,labels=match(d$Item,result$items$Item),pos=3,offset=.45,cex=.72,col=colors[g])
  }
  if(comparison)legend("top",inset=c(0,-.10),xpd=NA,legend=unname((result$group_labels%||%setNames(all_groups,all_groups))[groups]),col=colors[groups],pt.bg=colors[groups],pch=symbols[groups],pt.cex=sizes[groups],bty="n",horiz=length(groups)<=4L,cex=.72)
}

ipa_plot_image <- function(result,groups,comparison=FALSE,ci=FALSE) {
  path <- tempfile(fileext=".png"); on.exit(unlink(path),add=TRUE)
  grDevices::png(path,width=1843,height=1600,res=300,pointsize=10)
  tryCatch(ipa_plot(result,groups,comparison,ci),finally=grDevices::dev.off())
  paste0("data:image/png;base64,",base64enc::base64encode(path))
}

ipa_results_ui <- function(result,language=statedu_initial_language()) {
  if(is.null(result))return(NULL)
  ko <- identical(normalize_app_language(language),"ko")
  tr <- function(en,kr)ipa_text(en,language,kr)
  fmt <- function(table) {
    for(n in names(table))if(is.numeric(table[[n]]) && !n %in% c("No.","N","Available","Analyzed","Excluded","Valid B"))
      table[[n]] <- vapply(table[[n]],function(x)if(!is.finite(x))"—" else if(n %in% c("p","Holm p") && x<.001)"<.001" else formatC(x,digits=3,format="f"),character(1))
    for(n in names(table))table[[n]][is.na(table[[n]])] <- "—"
    table
  }
  tbl <- function(x,title,note=NULL,appendix=FALSE) {
    if("Group" %in% names(x) && !is.null(result$group_labels))x$Group<-unname(result$group_labels[x$Group])
    x <- fmt(x)
    user_columns <- which(names(x) %in% c("Group","Item","Attribute","Importance","Performance"))
    user_columns <- user_columns[vapply(x[user_columns],is.character,logical(1))]
    if(appendix) {
      headers <- c("No."="번호",Group="집단",Item="항목",Attribute="항목",Available="분석 전 N",Analyzed="분석 N",Excluded="제외 N",Performance="수행도",Importance="중요도",Estimate="추정값",Quadrant="사분면",Difference="차이","Valid B"="유효 반복",Setting="설정",Value="내용")
      old <- names(x)
      names(x) <- vapply(old,function(n)ipa_text(n,language,if(n %in% names(headers))headers[[n]] else n),character(1))
      if("Quadrant" %in% old) {
        q <- c("Concentrate here"="집중 개선","Keep up"="유지 강화","Low priority"="낮은 우선순위","Possible overkill"="과잉 노력 가능","On reference"="기준선 위")
        x[[match("Quadrant",old)]] <- vapply(x[[match("Quadrant",old)]],function(v)ipa_text(v,language,q[[v]]),character(1))
      }
    }
    attr(x,"result_user_columns") <- names(x)[user_columns]
    sheet <- structural_canvas_basic_html_table(x,class="ipa-publication-table",role=if(appendix)"appendix" else "main",
      language=if(appendix)language else "en",title=title,note=note,orientation="portrait")
    sheet$attribs$class <- paste(sheet$attribs$class,"ipa-publication-sheet")
    sheet
  }
  groups <- unique(result$coordinates$Group); coords <- result$coordinates
  display_group<-function(g)unname((result$group_labels%||%setNames(groups,groups))[g])
  direct <- result$mode=="direct"
  method <- if(direct)"Direct importance ratings" else if(result$derived=="log_partial")"Partial correlation of log performance" else "Partial correlation of performance"
  overview <- data.frame(Setting=c("Design","Importance method","Attributes","Analysis sample","Quadrant reference"),
    Value=c(switch(result$design,overall="Overall",independent="Independent groups",paired="Paired pre/post",grouped_paired="Pre/post by group"),method,nrow(result$items),
      paste(paste0(display_group(result$audit$Group),": N = ",result$audit$Analyzed),collapse="; "),
      switch(result$reference,pooled="Common respondent-weighted attribute means",within="Within-group/time attribute means",custom="Custom common coordinates")))
  if(!direct)overview <- rbind(overview,data.frame(Setting="Bootstrap",Value=paste0(result$resamples," resamples; percentile 95% CI; seed = ",result$seed)))
  mean_sd <- function(m,s)ifelse(is.finite(s),sprintf("%.3f ± %.3f",m,s),sprintf("%.3f ± —",m))
  descriptives <- lapply(groups,function(g) {
    d<-coords[coords$Group==g,];x<-data.frame(`No.`=match(d$Item,result$items$Item),Attribute=d$Item,N=d$N,check.names=FALSE)
    x[[if(direct)"Importance, M ± SD" else "Importance, partial r"]] <- if(direct)mean_sd(d$Importance,d$ImportanceSD%||%rep(NA_real_,nrow(d))) else sprintf("%.3f",d$Importance)
    x[["Performance, M ± SD"]] <- mean_sd(d$Performance,d$PerformanceSD%||%rep(NA_real_,nrow(d)))
    if(!is.null(result$totals)) {
      total <- result$totals[result$totals$Group==g,]
      bottom <- x[1,,drop=FALSE];bottom[["No."]] <- NA_integer_;bottom$Attribute <- "Overall";bottom$N <- total$N
      bottom[[4]] <- if(direct)mean_sd(total$Importance,total$ImportanceSD) else "—"
      bottom[[5]] <- mean_sd(total$Performance,total$PerformanceSD)
      x <- rbind(x,bottom)
    }
    tbl(x,paste0("Table 2",if(length(groups)>1)paste0(".",match(g,groups)) else "",". Importance and performance descriptive statistics — ",display_group(g)),
      if(direct)"M = mean; SD = standard deviation. Overall statistics use each respondent's mean across the selected attributes. Item numbers identify the same attributes in all figures. Importance and performance variables are matched in their entered order." else
        "M = mean; SD = standard deviation; partial r = partial Pearson correlation with overall satisfaction, controlling for other performance attributes. Overall performance uses each respondent's mean across selected attributes. Derived importance is a coefficient, not a mean score; its overall mean and SD are not defined. Item numbers identify the same attributes in all figures.")
  })
  figure_number <- 0L
  item_key <- paste(paste0(seq_len(nrow(result$items))," = ",result$items$Item),collapse="; ")
  figures <- function(ci=FALSE) {
    configs <- if(length(groups)>1L && identical(result$group_plot %||% "overlay","overlay"))
      list(list(groups=groups,comparison=TRUE)) else lapply(groups,function(g)list(groups=g,comparison=FALSE))
    if(identical(result$design,"grouped_paired") && identical(result$group_plot,"separate"))
      configs<-lapply(unique(result$series_map$Group),function(g)list(groups=result$series_map$Series[result$series_map$Group==g],comparison=TRUE,title=paste0("IPA — ",sub(" · Pre$","",display_group(result$series_map$Series[result$series_map$Group==g][1])))))
    lapply(configs,function(config) {
      figure_number <<- figure_number+1L
      name <- config$title%||%if(config$comparison)"IPA comparison" else paste0("IPA — ",display_group(config$groups))
      caption <- paste0("Figure ",figure_number,". ",name,if(ci)" with 95% confidence intervals" else "")
      rrset<-result$references[result$references$Group %in% config$groups,]
      refs <- if(config$comparison && result$reference=="within" && (length(unique(rrset$Performance))>1L || length(unique(rrset$Importance))>1L))"Group-specific benchmarks differ; common reference lines are omitted." else {
        rr <- result$references[match(config$groups[1],result$references$Group),]
        paste0(if(result$reference=="custom")"Reference" else "Mean"," performance = ",sprintf("%.3f",rr$Performance),"; ",
          if(result$reference=="custom")"reference" else "mean"," importance = ",sprintf("%.3f",rr$Importance),".")
      }
      tags$div(class="ipa-publication-sheet ipa-figure-sheet",`data-paper-size`="B5",`data-result-table-sheet`="true",
        `data-result-table-role`="main",`data-result-table-orientation`="portrait",`data-orientation`="portrait",
        tags$h5(class="structural-result-table-title",caption),
        tags$img(src=ipa_plot_image(result,config$groups,config$comparison,ci),alt=caption,style="display:block;width:100%;height:auto;"),
        result_note_paragraph(paste0(item_key,". ",if(ci)"CI = confidence interval. Bars show marginal, pointwise 95% intervals, not a joint confidence region. " else "Points show attribute estimates. ",refs)))
    })
  }
  appendix_note <- tr("Complete cases are used across selected scores within each group; paired analyses use the same respondents at both times. Quadrants are descriptive, not significance tests.",
    "집단 내 선택된 점수의 완전사례를 사용하며, 전후비교는 두 시점에 모두 응답한 동일 대상자를 분석합니다. 사분면은 기술적 분류이며 유의성 검정이 아닙니다.")
  if(!direct)appendix_note <- paste(appendix_note,tr("Importance is the partial Pearson correlation with overall satisfaction. Negative coefficients retain their sign. Bootstrap limits require at least 80% and 50 valid replicates.",
    "추정 중요도는 전반적 만족도와의 편상관이며 음수 부호를 유지합니다. 부트스트랩 신뢰한계는 요청 반복의 80% 이상 및 최소 50회의 유효 반복이 있어야 표시합니다."))
  if(result$missing_group>0)appendix_note <- paste(appendix_note,sprintf(tr("Excluded %s records with missing groups.","집단값 결측 %s건을 제외했습니다."),result$missing_group))
  if(result$design %in% c("paired","grouped_paired"))appendix_note <- paste(appendix_note,if(result$layout=="wide")tr("Each row must contain the same respondent's pre/post scores.","각 행은 동일 응답자의 사전·사후 점수여야 합니다.") else sprintf(tr("Pre = %s; Post = %s.","사전 = %s; 사후 = %s."),result$time_labels[1],result$time_labels[2]))
  if(result$design=="grouped_paired")appendix_note<-paste(appendix_note,tr("Changes are tested within each group; no group-by-time interaction test is reported.","변화는 각 집단 내에서 검정하며 집단×시점 상호작용 검정은 제시하지 않습니다."))
  appendix <- list(tbl(result$audit,tr("Appendix A1. Analysis sample","부록 A1. 분석 표본"),appendix_note,TRUE),
    tbl(result$references,tr("Appendix A2. Quadrant reference coordinates","부록 A2. 사분면 기준값"),appendix=TRUE))
  counter <- 2L
  add_appendix <- function(x,title,note=NULL) {
    counter <<- counter+1L
    appendix[[length(appendix)+1L]] <<- tbl(x,paste0(tr("Appendix A","부록 A"),counter,". ",title),note,TRUE)
  }
  mapping <- data.frame(`No.`=seq_len(nrow(result$items)),Importance=if(direct)result$items$Importance else rep("—",nrow(result$items)),Performance=result$items$Performance,check.names=FALSE)
  add_appendix(mapping,tr("Input variable pairs","입력 변수 대응"))
  if(result$design %in% c("paired","grouped_paired") && result$layout=="wide") {
    mapping$Importance <- if(direct)result$items$PostImportance else "—";mapping$Performance<-result$items$PostPerformance
    add_appendix(mapping,tr("Post variable pairs","사후 변수 대응"))
  }
  for(g in groups) {
    d <- coords[coords$Group==g,]
    add_appendix(data.frame(`No.`=match(d$Item,result$items$Item),Item=d$Item,Quadrant=d$Quadrant,check.names=FALSE),paste(tr("Quadrant classification —","사분면 분류 —"),display_group(g)))
    x <- data.frame(`No.`=match(d$Item,result$items$Item),Item=d$Item,N=d$N,check.names=FALSE)
    for(metric in c("Importance","Performance")) {
      number <- function(v)ifelse(is.finite(v),sprintf("%.3f",v),"—")
      heading <- paste(ipa_text(metric,language,if(metric=="Importance")"중요도" else "수행도"),
        if(!direct && metric=="Importance")"partial r (LLCI~ULCI)" else "M (LLCI~ULCI)")
      x[[heading]] <- paste0(number(d[[metric]]),"(",number(d[[paste0(metric,"Lower")]]),"~",number(d[[paste0(metric,"Upper")]]),")")
      if(!direct && metric=="Importance")x[["Valid B"]]<-d$ValidBoot
    }
      add_appendix(x,paste(display_group(g),tr("Importance and performance 95% CI","중요도·수행도 95% CI")),
        tr("95% CI = 95% confidence interval; LLCI = lower confidence limit; ULCI = upper confidence limit. Mean intervals use Student's t; derived-importance intervals use percentile bootstrap.",
          "95% CI = 95% 신뢰구간; LLCI = 하한; ULCI = 상한. 평균은 t 분포, 추정 중요도는 백분위수 부트스트랩 신뢰구간입니다."))
  }
  if(nrow(result$comparisons)) {
    ctab <- result$comparisons
    pairs <- unique(ctab[c("First","Second")])
    for(j in seq_len(nrow(pairs)))for(metric in c("Importance","Performance")) {
      d <- ctab[ctab$First==pairs$First[j] & ctab$Second==pairs$Second[j] & ctab$Metric==metric,]
      x <- data.frame(`No.`=match(d$Item,result$items$Item),Difference=d$Difference,`95% CI lower`=d$Lower,`95% CI upper`=d$Upper,check.names=FALSE)
      if(!direct && metric=="Importance")x[["Valid B"]]<-d$ValidBoot else {x$p<-d$p;x[["Holm p"]]<-d$Holm}
      add_appendix(x,paste0(tr("Group/time differences: ","집단·시점 차이: "),display_group(pairs$Second[j])," − ",display_group(pairs$First[j]),"; ",ipa_text(metric,language,if(metric=="Importance")"중요도" else "수행도")),
        tr("95% CI = 95% confidence interval; LLCI = lower confidence limit; ULCI = upper confidence limit. Differences are Second minus First. Score tests use Welch t for independent groups or paired t for matched times. Holm p adjusts all finite score tests; CIs are pointwise. Derived-importance differences use case-bootstrap CIs without t-test p values.",
          "95% CI = 95% 신뢰구간; LLCI = 하한; ULCI = 상한. 차이는 뒤 집단·시점에서 앞 집단·시점을 뺀 값입니다. 점수는 독립집단 Welch t 또는 대응 t 검정을 적용하며 Holm p는 전체 유효 점수 검정을 보정합니다. CI는 개별 구간입니다. 추정 중요도 차이는 사례 부트스트랩 CI이며 t 검정 p값을 제시하지 않습니다."))
    }
  }
  tags$div(class="ipa-publication-results",`data-paper-size`="B5",`data-orientation`="portrait",
    tags$h3("Importance–performance analysis"),
    tags$h4(class="ipa-section-title","1. Model overview"),
    tbl(overview,"Table 1. Model overview","IPA = importance–performance analysis. Axis ranges follow the displayed estimates (including interval limits in CI charts), with small label margins. Groups/times share the same ranges within each chart type; default reference lines are common across groups/times."),
    tags$h4(class="ipa-section-title","2. Importance and performance descriptive statistics"),descriptives,
    tags$h4(class="ipa-section-title","3. IPA charts"),figures(FALSE),
    tags$h4(class="ipa-section-title","4. IPA 95% CI charts"),figures(TRUE),
    tags$h4(class="ipa-section-title",tr("5. Appendix tables","5. 부록표")),appendix)
}
