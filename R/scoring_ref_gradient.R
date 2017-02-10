#==============================================================================
#'Scoring Method: Reference Gradient
#'
#'@param long.df <- a data frame of samples to be scored.
#'@return Scores the samples.
#'@export

ref_gradient <- function(metrics.df, sensitivity.df, first.metric, condition.colname, ref.cond){
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  #============================================================================
  ref.df <- metrics.df[metrics.df[, condition.colname] %in% ref.cond, ]
  ref.quant <- data.frame(t(sapply(ref.df[, metric_col.1:ncol(ref.df)], quantile,
                                   c(0.05, 0.95))))
  names(ref.quant) <- c("REF_5%", "REF_95%")
  ref.quant$METRICS <- row.names(ref.quant)
  sensitivity.df <- sensitivity.df[, c("METRICS", "DISTURBANCE")]
  score.info <- merge(sensitivity.df, ref.quant, by = "METRICS")
  #============================================================================
  long.df <- tidyr::gather_(metrics.df, "METRICS", "REPORTING_VALUE",
                            noquote(names(metrics.df[, metric_col.1:ncol(metrics.df)])))
  long.df <- merge(long.df, score.info, by = "METRICS")
  #============================================================================
  long.df$REF_GRADIENT <- ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                   long.df$REPORTING_VALUE <= long.df$`REF_5%`, 0,
                                 ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                          long.df$REPORTING_VALUE > long.df$`REF_5%` &
                                          long.df$REPORTING_VALUE < long.df$`REF_95%` ,
                                        ((long.df$REPORTING_VALUE - long.df$`REF_5%`) /
                                           (long.df$`REF_95%` - long.df$`REF_5%`)) * 100,
                                        ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                                 long.df$REPORTING_VALUE >= long.df$`REF_95%`, 100,
                                               ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                        long.df$REPORTING_VALUE <= long.df$`REF_5%`, 100,
                                                      ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                               long.df$REPORTING_VALUE > long.df$`REF_5%` &
                                                               long.df$REPORTING_VALUE < long.df$`REF_95%` ,
                                                             ((long.df$`REF_95%` - long.df$REPORTING_VALUE) /
                                                                (long.df$`REF_95%` - long.df$`REF_5%`)) * 100,
                                                             ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                                      long.df$REPORTING_VALUE >= long.df$`REF_95%`, 0, 100000))))))
  #============================================================================
  long.df$REF_GRADIENT <- round(long.df$REF_GRADIENT, digits = 2)
  #============================================================================
  remove.cols <- c("REPORTING_VALUE", "REF_5%", "REF_95%", "DISTURBANCE")
  long.df <- long.df[, !names(long.df) %in% remove.cols]
  #============================================================================
  final.df <- tidyr::spread(long.df, METRICS, REF_GRADIENT)
  #============================================================================
  site_info.cols <- names(metrics.df[, 1:(metric_col.1 - 1)])
  metrics.cols <- names(metrics.df[, metric_col.1:ncol(metrics.df)])
  final.df <- final.df[, c(site_info.cols, metrics.cols)]
  
  return(final.df)
}
