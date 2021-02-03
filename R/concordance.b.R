
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import epiR
#' @importFrom epiR epi.ccc
#' @export

concordanceClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "concordanceClass",
    inherit = concordanceBase,
    private = list(

        
        .init = function() {
            
            if (is.null(self$data) | is.null(self$options$dep)  | is.null(self$options$covs)) {
                
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            self$results$instructions$setContent(
                "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p> The rationale of Concordance correlation is described in the <a href='https://r-bloggers.com/2020/01/concordance-correlation-coefficient/?fbclid=IwAR2Txi_QrFTuDB9jH8NiJW8dEde_lw2Td08XqxNzoWqut9m8E-bE5RHUDiI' target = '_blank'>page.</a></p>
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/seolmatrix/'  target = '_blank'>GitHub.</a></p>

            </div>
            </body>
            </html>"
            )
            
                   },
        
        
        .run = function() {

                    
                    if (length(self$options$dep)<1) return()

                    if (length(self$options$covs)<1) return()

                    
                    #get the data--------
                    
                    data <- self$data
                    
                    dep <- self$options$dep
                    
                    covs <- self$options$covs
                    
                    
                    # get the data
                    
                    data <- self$data
                    
                    # convert to appropriate data types
                    
                    data[[dep]] <- jmvcore::toNumeric(data[[dep]])
                    
                    data[[covs]] <- jmvcore::toNumeric(data[[covs]])
                    
                    
                    data <- na.omit(data)
                    
                    
                    res <- epiR::epi.ccc(data[[dep]], data[[covs]])
                    
                    
                    # epi.ccc(x, y, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE, 
                    #         subjectid)   
                    # 
                    
                    table<- self$results$table
                    
                    r <- res$rho.c[[1]]
                    lower <- res$rho.c[[2]]
                    upper<-res$rho.c[[3]]  
                    
                    row <- list()
                    
                    row[['r']] <- r
                    row[['lower']] <- lower
                    row[['upper']] <- upper
                    
                    table$setRow(rowNo = 1, values = row)
                    
                    
                        
          })
)
