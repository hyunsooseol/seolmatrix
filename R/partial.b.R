
# This file is a generated template, your changes will not be overwritten


#' Correlation Analysis
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import qgraph
#' @import psych
#' @export

# `self$data` contains the data
# `self$options` contains the options
# ..vars - variables to calculate correlations for
# ..ctrlvars - variables to control for
# ..shwSig  - show significance level for correlations (p-values)
# ..flgSig  - flag significant correlations
# ..sidSig  - one- or two-tailed significance calculations
# `self$results` contains the results object (to populate)

partialClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "partialClass",
    inherit = partialBase,
    private = list(

#==================================================================================
 .init = function(){
        
   
   if(is.null(self$dat) | is.null(self$options$vars)){
     self$results$instructions$setVisible(visible = TRUE)
     
   }
   
   self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p><b>To get started:</b></p>

            <p>The input dataset require the measure type of <b>numeric-continuous</b> in jamovi.
            <p>To get the result table of <b>Pearson correlation</b>, just highlight the variables and click the arrow to move it across into the 'Variables' box.</p>
            <p>If you move the variables into 'Controlling for'box, the result table shows <b>Partial correlation</b>.</p>

            <p>Note: When One variable is dichotomous, the other is continuous, the result table is equivalent to a <b>Point-biserial correlation</b>.</P>

            <p>Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/seolmatrix/'  target = '_blank'>GitHub</a></p>

            </div>
            </body>
            </html>")
        

   # get variables--------------------------------------
        
        matrix <- self$results$get('matrix')
        var <- self$options$get('vars')
        varCtl <- self$options$get('ctrlvars')
        
        
 # whether the procedure is controlling for variables or not-----------
        
        matrix$setTitle(ifelse(
          length(varCtl) > 0,'Partial Correlation Matrix','Correlation Matrix'
        ))
        
# Add Columns----------------------------------
        
        for (i in seq_along(var)) {
          matrix$addColumn(
            name = paste0(var[[i]], '[r]'),
            title = var[[i]],
            type = 'number',
            format = 'zto'
          )
          matrix$addColumn(
            name = paste0(var[[i]], '[rp]'),
            title = var[[i]],
            type = 'number',
            format = 'zto,pvalue',
            visible = '(shwSig)'
          )
        }
        
 # Empty cells above and put "-" in the main diagonal-------------------
        
        for (i in seq_along(var)) {
          values <- list()
          
          for (j in seq(i, length(var))) {
            values[[paste0(var[[j]], '[r]')]]  <- ''
            values[[paste0(var[[j]], '[rp]')]] <- ''
          }
          values[[paste0(var[[i]], '[r]')]]  <- '\u2014'
          values[[paste0(var[[i]], '[rp]')]] <- '\u2014'
          matrix$setRow(rowKey = var[[i]], values)
        }
        
 # initialize setNote-------------------------------------------------
        
        matrix$setNote('ctlNte', ifelse(length(varCtl) > 0, paste0('Controlling for ', paste(varCtl, collapse=", ")), 
                                        'Not controlling for any variables, the result table shows Pearson correlation matrix'))
        
        
        matrix$setNote('sigNte', paste0(
          ifelse(
            self$options$get('sidSig') == 'onetailed',
            'One-tailed significance',
            'Two-tailed significance'
          ),
          ifelse(
            self$options$get('flgSig'),
            ': * p < .05, ** p < .01, *** p < .001',
            ''
          )
        ))
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
        },
    
      
#====================================================================
      
.run = function() {
 
         # get variables--------------------------------------------------
        
        matrix <- self$results$get('matrix')
        
        var <- self$options$get('vars')
        nVar <- length(var)
        
        varCtl <- self$options$get('ctrlvars')
        nCtl   <- length(varCtl)
        
        
        data <- self$data
         
        for(v in var)
         data[[v]] <- jmvcore::toNumeric(data[[v]])
        
        for(v in varCtl)
         data[[v]] <-jmvcore::toNumeric(data[[v]])
         
        
# Computing correlations----------
        
        if (nVar > 1) {
          m  <-
            as.matrix(cor(data[, c(var, varCtl)], use = 'pairwise', method = 'pearson'))
          X  <- m[var, var]
          
          if (nCtl > 0) {
            Y  <- m[var, varCtl]
            pi <- solve(m[varCtl, varCtl])
            Rp <- cov2cor(X - Y %*% pi %*% t(Y))
          } else {
            Rp <- X
          }
          
          df <- dim(data)[1] - nCtl
          Rt <- (Rp * sqrt(df - 2)) / sqrt(1 - Rp ^ 2)
          if (self$options$sidSig == 'onetailed') {
            nt = 1
          } else {
            nt = 2
          }
          Pp <- -nt *  expm1(pt(abs(Rt), (df - 2), log.p = TRUE))
          
# populate results------------------------------------------------
          
          for (i in 2:nVar) {
            for (j in seq_len(i - 1)) {
              values <- list()
              values[[paste0(var[[j]], '[r]')]]  <-
                Rp[i, j]
              values[[paste0(var[[j]], '[rp]')]] <-
                Pp[i, j]
              matrix$setRow(rowNo = i, values)
              if (self$options$get('flgSig')) {
                if (Pp[i, j] < .001)
                  matrix$addSymbol(rowNo = i, paste0(var[[j]], '[r]'), '***')
                else if (Pp[i, j] < .01)
                  matrix$addSymbol(rowNo = i, paste0(var[[j]], '[r]'), '**')
                else if (Pp[i, j] < .05)
                  matrix$addSymbol(rowNo = i, paste0(var[[j]], '[r]'), '*')
              }
              
            }
          }
          
        }
        
 #Gaussian Graphical Models with partial correlation----------------
        
        partial <- psych::partial.r(data)
        
        # Prepare Data For Plot -------
        image <- self$results$plot
        image$setState(partial)
      },
      
 
#================================================================
      
      .plot = function(image, ...) {
        ggm <- self$options$ggm
        
        if (!ggm)
          return()
        
        
        partial <- image$state
        
        plot <- qgraph(partial, layout = "spring", details = TRUE)
        
        print(plot)
        TRUE
      }
        )
)


