moment_repart_inheritance <- function(
  dt, 
  moment_var = "moment_simulations",
  Hreceived_var = "hr",
  lbounds = NULL,
  filter_pre_2015 = TRUE,
  cut_in_thresholds = TRUE,
  year = 2015,
  min_moment = 0
){
  
  if (isTRUE(filter_pre_2015)){
    inheritance_2015_predicted <- dt[get(Hreceived_var)>0,.SD[.N], by = "Id"][annee<=year]
  } else{
    inheritance_2015_predicted <- data.table::copy(dt)
  }
  
  if (isTRUE(cut_in_thresholds)){
    if (is.null(lbounds)) stop("lbounds needs to be provided")
    inheritance_2015_predicted[,c(moment_var) := cut(
      log(get(Hreceived_var)),
      breaks = c(0, lbounds, Inf),
      labels = seq(1, length(lbounds)+1))
    ]
  } else{
    data.table::setnames(inheritance_2015_predicted, old = Hreceived_var, new = moment_var)
  }
  
  inheritance_2015_predicted_mom <- inheritance_2015_predicted[,.N, by = c(moment_var)]
  inheritance_2015_predicted_mom[, `:=`(p = N/sum(N), weight = N/sum(N))]
  
  inheritance_2015_predicted_mom <- inheritance_2015_predicted_mom[order(get(moment_var))]
  inheritance_2015_predicted_mom[, Nmoment := as.numeric(as.character(get(moment_var))) +min_moment]
  inheritance_2015_predicted_mom[, c(moment_var) := NULL]
  data.table::setnames(inheritance_2015_predicted_mom, old = "p", new = moment_var)
  
  return(inheritance_2015_predicted_mom[,.SD,.SDcols = c(moment_var,
                                                         "weight", "Nmoment")])
  
}