stat_methods <- c('Descriptive statistics','Student t-test','Welch t-test','One-way ANOVA','Tukey','Dunnett','Mann-Whitney U','Kruskal-Wallis','Dunn')
stat_assumptions <- function(method) {
 if(method %in% c('Mann-Whitney U','Kruskal-Wallis','Dunn')) 'Independent experimental units; ordinal/continuous response; rank tests do not generally test means. Similar distribution shapes needed for location interpretation.'
 else if(method %in% c('Student t-test','One-way ANOVA','Tukey','Dunnett')) 'Independent experimental units; approximately normal errors; equal within-group variances. Summary analyses use sufficient statistics, without reconstructing observations.'
 else 'Independent experimental units; approximately normal sampling distribution of means. Welch allows unequal variances.'
}
run_analysis <- function(d,method,groups=NULL,control=NULL,alpha=.05,adjust='holm') {
 validate_data(d)
 if(!method %in% stat_methods) fail('Unsupported statistical method.')
 if(length(unique(d$dataset_id))!=1 || length(unique(d$endpoint))!=1) fail('Inference requires exactly one independent study and endpoint; studies are never pooled automatically.')
 if(any(vapply(d[factors],function(x) length(unique(x))>1,logical(1)))) fail('Select one level of every experimental stratum before inference. Do not pool sex, generation, exposure, collection time or subgroup.')
 if(!is.numeric(alpha) || !is.finite(alpha) || alpha<=0 || alpha>=1) fail('Alpha must lie between 0 and 1.')
 if(!adjust %in% stats::p.adjust.methods) fail('Unsupported multiple-comparison adjustment.')
 if(length(groups)) d <- d[d$treatment %in% groups,,drop=FALSE]
 if(!nrow(d)) fail('No analyzed observations.')
 s <- summarise_groups(d,1-alpha)
 if(anyDuplicated(s$treatment)) fail('Treatment labels span multiple dose groups. Give each experimental group a unique treatment name.')
 s <- s[order(s$treatment),,drop=FALSE]; k <- nrow(s)
 result <- function(comparison,estimate=NA_real_,lower=NA_real_,upper=NA_real_,statistic=NA_real_,df=NA_real_,p=NA_real_,correction='none')
   data.frame(method=method,study=unique(d$dataset_id),endpoint=unique(d$endpoint),comparison=comparison,
     groups=paste(s$treatment,collapse=' | '),sample_sizes=paste(paste0(s$treatment,': ',s$n),collapse=' | '),
     estimate=estimate,lower=lower,upper=upper,statistic=statistic,df=df,p=p,alpha=alpha,confidence=1-alpha,correction=correction,
     assumptions=stat_assumptions(method),stringsAsFactors=FALSE)
 if(method=='Descriptive statistics') {
   return(dplyr::bind_rows(lapply(seq_len(k),function(i) result(s$treatment[i],s$mean[i],s$lower[i],s$upper[i],df=s$n[i]-1))))
 }
 if(k<2 || any(!is.finite(s$n)) || any(s$n<2) || any(!is.finite(s$sd))) fail('Inference requires at least two groups with n >= 2 and estimable SD.')
 if(method %in% c('Mann-Whitney U','Kruskal-Wallis','Dunn') && any(d$mode!='raw')) fail('Rank-based tests require raw observations; mean/SD/n are insufficient.')
 if(method %in% c('Student t-test','Welch t-test')) {
   if(k!=2) fail('Select exactly two experimental groups for a t-test.')
   delta <- s$mean[1]-s$mean[2]; v <- s$sd^2/s$n
   if(method=='Welch t-test') {se <- sqrt(sum(v)); df <- sum(v)^2/sum(v^2/(s$n-1))}
   else {df <- sum(s$n)-2; pooled <- sum((s$n-1)*s$sd^2)/df; se <- sqrt(pooled*sum(1/s$n))}
   if(!is.finite(se) || se<=0) fail('Cannot estimate sampling error: within-group variance is zero.')
   t <- delta/se; q <- stats::qt(1-alpha/2,df)*se
   return(result(paste(s$treatment,collapse=' minus '),delta,delta-q,delta+q,t,df,2*stats::pt(-abs(t),df)))
 }
 if(method=='Mann-Whitney U') {
   if(k!=2) fail('Mann-Whitney requires two groups.')
   fit <- stats::wilcox.test(d$value[d$treatment==s$treatment[1]],d$value[d$treatment==s$treatment[2]],exact=FALSE,conf.int=TRUE,conf.level=1-alpha)
   return(result(paste(s$treatment,collapse=' vs '),unname(fit$estimate),fit$conf.int[1],fit$conf.int[2],unname(fit$statistic),p=fit$p.value))
 }
 if(method=='Kruskal-Wallis') {
   fit <- stats::kruskal.test(value~treatment,data=d)
   return(result('All selected groups',statistic=unname(fit$statistic),df=unname(fit$parameter),p=fit$p.value))
 }
 if(method=='Dunn') {
   ranks <- rank(d$value); N <- nrow(d); ties <- table(d$value)
   variance <- N*(N+1)/12-sum(ties^3-ties)/(12*(N-1))
   if(variance<=0) fail('All observations tied; rank comparisons are not estimable.')
   mr <- tapply(ranks,d$treatment,mean); pairs <- utils::combn(s$treatment,2,simplify=FALSE)
   out <- dplyr::bind_rows(lapply(pairs,function(pair) {
     n <- s$n[match(pair,s$treatment)]; z <- (mr[pair[1]]-mr[pair[2]])/sqrt(variance*sum(1/n))
     result(paste(pair,collapse=' vs '),statistic=unname(z),p=2*stats::pnorm(-abs(z)),correction=adjust)
   })); out$p <- stats::p.adjust(out$p,method=adjust); return(out)
 }
 df <- sum(s$n)-k; mse <- sum((s$n-1)*s$sd^2)/df
 if(!is.finite(mse) || mse<=0) fail('Residual variance is zero; ANOVA/post-hoc tests are not estimable.')
 if(method=='One-way ANOVA') {
   grand <- weighted.mean(s$mean,s$n); F <- sum(s$n*(s$mean-grand)^2)/(k-1)/mse
   out <- result('All selected groups',statistic=F,df=k-1,p=stats::pf(F,k-1,df,lower.tail=FALSE)); out$residual_df <- df; return(out)
 }
 if(method=='Tukey') {
   pairs <- utils::combn(seq_len(k),2,simplify=FALSE)
   return(dplyr::bind_rows(lapply(pairs,function(ix) {
     delta <- s$mean[ix[1]]-s$mean[ix[2]]; se <- sqrt(mse/2*sum(1/s$n[ix])); q <- stats::qtukey(1-alpha,k,df)*se
     result(paste(s$treatment[ix],collapse=' minus '),delta,delta-q,delta+q,abs(delta)/se,df,
       stats::ptukey(abs(delta)/se,k,df,lower.tail=FALSE),'Tukey-Kramer family-wise')
   })))
 }
 if(method=='Dunnett') {
   if(length(control)!=1 || !control %in% s$treatment) fail('Select a valid control group for Dunnett comparisons.')
   ix <- which(s$treatment!=control); K <- matrix(0,nrow=length(ix),ncol=k)
   K[cbind(seq_along(ix),ix)] <- 1; K[,match(control,s$treatment)] <- -1
   rownames(K) <- paste(s$treatment[ix],'minus',control)
   model <- multcomp::parm(coef=stats::setNames(s$mean,s$treatment),vcov=diag(mse/s$n),df=df)
   fit <- multcomp::glht(model,linfct=K)
   # Local deterministic integration leaves the caller's RNG stream unchanged.
   seed <- if(exists('.Random.seed',.GlobalEnv)) get('.Random.seed',.GlobalEnv) else NULL
   on.exit(if(is.null(seed)) {if(exists('.Random.seed',.GlobalEnv)) rm('.Random.seed',envir=.GlobalEnv)} else assign('.Random.seed',seed,.GlobalEnv),add=TRUE)
   set.seed(2026); sm <- summary(fit,test=multcomp::adjusted('single-step')); ci <- stats::confint(fit,level=1-alpha)$confint
   return(result(rownames(K),ci[,'Estimate'],ci[,'lwr'],ci[,'upr'],unname(sm$test$tstat),df,as.numeric(sm$test$pvalues),'Dunnett single-step family-wise'))
 }
 fail('Unsupported method.')
}
