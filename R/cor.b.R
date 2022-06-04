
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

        #------------------------------------
        
        .init = function() {
            if (is.null(self$data) | is.null(self$options$vars)) {
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            self$results$instructions$setContent(
                "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
           <p><b>Instructions</b></p>
            <p>____________________________________________________________________________________</p>
            <p> 1. Computes and visualizes an item correlation matrix, offering several correlation types and clustering methods.</p>
            <p> 2. The heatmap is estimated by using <b>ShinyItemAnalysis::plot_corr</b> function.</p>
            <p> 3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub</a>.</p>
            <p>____________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },
        
        
        .run = function() {

            if (is.null(self$options$vars))
                return()
            
            vars  <- self$options$vars
            
            # get the data
            
            data <- self$data
            
            data <- na.omit(data)
            
            # pearson and spearman  correlation------
            
            mat<-  stats::cor(data, method = self$options$type)
            mat1 <- as.dist(1-mat)
            
            
            # heatmap plot----------
            
            image <- self$results$plot
            image$setState(mat)
            
            # dendrogram plot-------
            
            image <- self$results$plot1
            image$setState(mat1)
            
 
            if(self$options$poly==TRUE){
                
            #Polychoric correlation------
                
                corP <- psych::polychoric(data)
                
                dis <- as.dist(1-corP$rho)
                         
       # Polychoric heatmap plot---------
            
            image <- self$results$plot2
            image$setState(data)
            
       # Polychoric dendrogram plot---------
            
            image <- self$results$plot3
            image$setState(dis)
            
            }
            
       },
        
        .plot = function(image, ggtheme, theme, ...) {
            
            if (is.null(self$options$vars))
                return()
            
            
             mat<- image$state
            
            plot <- ShinyItemAnalysis::plot_corr(mat, 
                                                 cor=self$options$type,
                                                 clust_method = self$options$method,
                                                 n_clust=self$options$k, 
                                                 labels_size = self$options$size,
                                                 line_col = "red",
                                                 line_size = 1.5, labels = TRUE)
                                                 
                                                 
          
            print(plot)
            TRUE
        },
        
        .plot1 = function(image, ggtheme, theme, ...) {
            
            if (is.null(self$options$vars))
                return()
            
            
           mat1 <- image$state
           
           hc <- stats::hclust(mat1, method = self$options$method) 
            
           
            if(self$options$horiz == TRUE){
            
            plot1<- ggdendro::ggdendrogram(hc, rotate=TRUE)
            
            } else {
                
                plot1<- ggdendro::ggdendrogram(hc)
                
            } 
            
           
            print(plot1)
            TRUE
        },
       
       .plot2 = function(image, ggtheme, theme, ...) {
           
           if (is.null(self$options$vars))
               return()
           
           
           data<- image$state
           
           plot2 <- ShinyItemAnalysis::plot_corr(data, 
                                                cor='poly',
                                                clust_method = self$options$method,
                                                n_clust=self$options$k, 
                                                labels_size = self$options$size,
                                                line_col = "red",
                                                line_size = 1.5, labels = TRUE)
           
           
           
           print(plot2)
           TRUE
       },
        

    .plot3 = function(image, ggtheme, theme, ...) {
      
      if (is.null(self$options$vars))
          return()
      
      
      dis <- image$state
      
      hc <- stats::hclust(dis, method = self$options$method) 
      
      
       if(self$options$horiz == TRUE){
      
           plot3<- ggdendro::ggdendrogram(hc, rotate=TRUE)
      
       } else {
      
           plot3<- ggdendro::ggdendrogram(hc)
      
       }
      
      
      print(plot3)
      TRUE
  } 
        
    )
)

