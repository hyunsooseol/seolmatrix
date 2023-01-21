
# This file is a generated template, your changes will not be overwritten

#' Interrater Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import irr
#' @importFrom irr icc
#' @import lpSolve
#' @import qgraph
#' @import psych
#' @importFrom irr kappam.fleiss
#' @export


kappaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "kappaClass",
    inherit = kappaBase,
    private = list(

      # ==========================================================
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
             <p>___________________________________________________________________________________
             <p>1. Fleiss' kappa can be used with binary or nominal-scale.</p> 
             <p>2. The <b>irr</b> R package is described in the <a href='https://cran.r-project.org/web/packages/irr/irr.pdf' target = '_blank'>page</a>.</p>
             <p>3. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub</a>.</p>
             <p>___________________________________________________________________________________
             </div>
             </body>
             </html>"
        )
        
        
      },
      
      #======================================++++++++++++++++++++++
      
              .run = function() {

                
                vars <- self$options$vars
                data <- self$data
                data <- jmvcore::naOmit(data)
                
                
                ###Fleiss' kappa================
                
                kap<- irr::kappam.fleiss(ratings = data)
                
               # get subjects-------
                
                nk <- kap$subjects
                
                # get raters--------
                
                raterk <- kap$raters
                
                # get statistic------------
                
                statistick <- kap$value
                
                # z value----------------
                
                zk <- kap$statistic
                
                # p value-------------------
                
                pk <- kap$p.value
                
                #-----------------------------------
                table <- self$results$fk
               
                row <- list()
                
                row[['n']] <- nk
                row[['rater']] <- raterk
                row[['statistic']] <- statistick
                row[['z']] <- zk
                row[['p']] <- pk
                
                table$setRow(rowNo = 1, values = row)
                
                
                if(isTRUE(self$options$ek)){
                
                ###Fleiss' Exact kappa================
                
                kae<- irr::kappam.fleiss(ratings = data, exact = TRUE)
                
                # get subjects-------
                
                nke <- kae$subjects
                
                # get raters--------
                
                raterke <- kae$raters
                
                # get statistic------------
                
                statisticke <- kae$value
                
                #-----------------------------------
                table <- self$results$ek
                
                row <- list()
                
                row[['n']] <- nke
                row[['rater']] <- raterke
                row[['statistic']] <- statisticke
               
                table$setRow(rowNo = 1, values = row)
                
                }
                
                
                if(isTRUE(self$options$cw)){
                  
                  cw<- irr::kappam.fleiss(ratings=data, detail=TRUE)
                  
                  c<- cw[["detail"]]
                  
                  names<- dimnames(c)[[1]]
                  
                  table <- self$results$cw  
                  
                 
                  for (name in names) {
                    
                    row <- list()
                    
                    row[['k']] <- c[name,1]
                    row[['z']] <- c[name,2]
                    row[['p']] <- c[name,3]
                    
                    table$addRow(rowKey=name, values=row)
                    
                  }
                }        
                
               
          
          
        })
)
