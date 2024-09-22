
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import easyAHP
#' @importFrom easyAHP easyAHP
#' @export


ahpClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ahpClass",
    inherit = ahpBase,
    private = list(
      .htmlwidget = NULL,
        #==========================================================
        .init = function() {
          private$.htmlwidget <- HTMLWidget$new()
            
            if(is.null(self$data) | is.null(self$options$vars)){
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            # self$results$instructions$setContent(
            #     "<html>
            # <head>
            # </head>
            # <body>
            # <div class='instructions'>
            # 
            # 
            # <p>____________________________________________________________________________________</p>
            # <p>1. Each row and column represents an item and a decision maker for the input data format.</p>
            # <p>2. More than 10 items or 20 decision makers are NOT allowed.</p>
            # <p>3. Each item would be graded 1-9 score by each decision maker</p>
            # <p>4. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub.</a></p>
            # <p>____________________________________________________________________________________</p>
            # </div>
            # </body>
            # </html>"
            # )
            
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title="Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li>Each row and column represents an item and a decision maker for the input data format.</li>',
                '<li>More than 10 items or 20 decision makers are NOT allowed.</li>',
                '<li>Each item would be graded 1-9 score by each decision maker.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
                
              )
              
            )
          )         
          
            if (self$options$cir)
                self$results$cir$setNote(
                    "Note",
                    "value<0.1 is acceptable for consistency ratio."
                )
            
            
        },
        
 
         .run = function() {

                   if (length(self$options$vars)<1) return() 
                   
                   
                   if(length(self$options$vars>1)){
                       
                       # get variables---------------------------------
                       
                       
                       vars <- self$options$vars
                       nVars <- length(vars)
                       
                       
                       mydata <- self$data
                       
                       mydata <- jmvcore::naOmit(mydata)
                       
                       for(v in vars)
                           mydata[[v]] <- jmvcore::toNumeric(mydata[[v]])
                       
                       # compute AHP-------
                       
                       res <- easyAHP::easyAHP(mydata)$Makers
                       
                       
                       # item matrix--------
                       
                       item <- res$Matrix
                       item<- as.matrix(item)
                       
                       names <- dimnames(item)[[1]]
                       dims <- dimnames(item)[[2]]
                       
                       table <- self$results$itemmat
                       
                       for (dim in dims) {
                           
                           table$addColumn(name = paste0(dim),
                                           type = 'number')
                       }
                       
                       
                       for (name in names) {
                           
                           row <- list()
                           
                           for(j in seq_along(dims)){
                               
                               row[[dims[j]]] <- item[name,j]
                               
                           }
                           
                           table$addRow(rowKey=name, values=row)
                           
                       }
                       
                       # item weights----------------
                       
                       weights<- res$Weights
                       weights<- as.data.frame(weights)
                       
                       names<- dimnames(weights)[[1]]
                       
                       
                       # item weights table-----------
                       
                       table <- self$results$weights
                       
                       for (name in names) {
                           
                           row <- list()
                           
                           row[['value']] <- weights[name,1]
                           
                           table$addRow(rowKey=name, values=row)
                           
                       }
                       
                       # Confidence Index and Confidence Ratio--------
                       
                       
                       Index<- res$CI
                       Ratio<- res$CR
                       cir<- rbind(Index, Ratio)
                       
                       
                       names<- dimnames(cir)[[1]]
                       
                       # Confidence index and ratio table-------
                       
                       
                       table <- self$results$cir
                       
                       for (name in names) {
                           
                           row <- list()
                           
                           row[['value']] <- cir[name,1]
                           
                           table$addRow(rowKey=name, values=row)
                           
                       }
                       
                     
            
                   }
            
            
            
            
        })
)
