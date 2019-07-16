#' ggplotDiagnosticPlots: function for diagnostic plots for INLA model
#'
#' @param model Model object from incidenceMapR::modelTrainR
#' @param observed The observed values. If NULL defined by model family
#' @param CI Add credible intervals to the fitted values?
#' @param binwidth The size of the bins used for the histogram. If NULL ggplot guesses for you.
#' @param se Plot a ribbon showing the standard error of the smoother.
#' @param method What method should be used for the smoother. Defaults to loess unless data is large. Other options include 'gam', 'loess', 'lm'. See geom_smooth for details.
#' @return List of 3 ggplot objects: default plots, observed vs fitted and histogram, standardized residuals
#'
#' @import ggplot2
#' @import INLAutils
#' 
#' @export
#' @examples
#' ggplotDiagnosticPlots(model)
#'
ggplotDiagnosticPlots <- function(model, observed = NULL, CI = FALSE, binwidth = NULL, se = TRUE, method = "auto"){
  

  plots <- list()
  
  # for default plots (marginals, )
  plots[[1]] <- autoplot(model$inla)
  
  if (is.null(observed))
  {
      if (model$modelDefinition$family == 'poisson') 
        observed <- model$modelDefinition$observedData$positive
      else
        return('observed is missing and can not be defined from model family!!! provide observed data.')
  }
  
  # for observed vs fitted and histogram plot
  plots[[2]] <- ggplot_inla_residuals(model$inla, observed, CI, binwidth)  
  # for Standardized Residuals
  plots[[3]] <- ggplot_inla_residuals2(model$inla, observed, CI, se, method) 
  print(plots[[3]])
  
 return(invisible(plots))
  
}