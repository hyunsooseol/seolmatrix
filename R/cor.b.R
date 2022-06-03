
# This file is a generated template, your changes will not be overwritten
#' @importFrom psych polychoric
#' @importFrom ShinyItemAnalysis plot_corr
#' @importFrom stats hclust
#' @importFrom stats cor
#' @import ggplot2
#' @import ggdendro
#' @import psych
#' @import ShinyItemAnalysis
#' @importFrom ggdendro ggdendrogram
#' @export

corClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "corClass",
    inherit = corBase,
    private = list(
        .run = function() {

            if (is.null(self$options$vars))
                return()
            
            vars  <- self$options$vars
            
            # get the data
            
            data <- self$data
            
            data <- na.omit(data)
            
            
          #  mat<-  stats::cor(data, method = self$options$type)
            
            
           corP <- psych::polychoric(data) 
               
           dis <- as.dist(1-corP$rho)
           
           # hc <- stats::hclust(as.dist(1 - corP$rho), method = self$options$method)  
               
           
            # heatmap plot----------
            
            image <- self$results$plot
            image$setState(data)
          
            # dendrogram plot-------
            
            image <- self$results$plot1
            image$setState(dis)
            
              
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            
            if (is.null(self$options$vars))
                return()
            
            
            data <- image$state
            
            plot <- ShinyItemAnalysis::plot_corr(data, cor=self$options$type,
                                                 clust_method = self$options$method,
                                                 n_clust=self$options$k, 
                                                 labels_size = self$options$size,
                                                 labels=TRUE)
           
           # plot <- plot+ggtheme
            print(plot)
            TRUE
        },
        
        .plot1 = function(image, ggtheme, theme, ...) {
            
            if (is.null(self$options$vars))
                return()
            
            
            dis <- image$state
            
            hc <- stats::hclust(dis, method = self$options$method) 
            
            plot1<- ggdendro::ggdendrogram(hc) # dendrogram
            
           
           # plot1 <- plot1+ggtheme
            print(plot1)
            TRUE
        }
        
        
        
    )
)

