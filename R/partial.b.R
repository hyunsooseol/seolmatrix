
# This file is a generated template, your changes will not be overwritten


#' Correlation Analysis
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import qgraph
#' @import psych
#' @importFrom psych partial.r
#' @importFrom qgraph EBICglasso
#' @importFrom qgraph centralityPlot
#' @importFrom qgraph cor_auto
#' @export


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
            <h2><b>Instructions</b></h2>
            <p>____________________________________________________________________________________</p>
            <p>1. If you move the variables into <b>Controlling for</b> box, the result table shows Partial correlation.</p>
            <p>2. When One variable is dichotomous, the other is continuous, the result table is equivalent to a point-biserial correlation.</P>
            <p>3. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub</a>.</p>
            <p>____________________________________________________________________________________</p>
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
            as.matrix(stats::cor(data[, c(var, varCtl)], 
                                 use="pairwise.complete.obs",
                                 method = 'pearson'))
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
        
 # Patial plot----------------
        
        partial <- psych::partial.r(data)
        
        # Prepare Data For Plot -------
        image1 <- self$results$plot1
        image1$setState(partial)
      
        # EBIC PLOT------------
        
        if(self$options$plot | self$options$plot2==TRUE){
        # Compute correlations:
        CorMat <- qgraph::cor_auto(data)
        
        # Compute graph with tuning = 0.5 (EBIC)
        EBICgraph <- qgraph::EBICglasso(CorMat, nrow(data), 0.5, threshold = TRUE)
        
        # Prepare Data For Plot -------
        image <- self$results$plot
        image$setState(EBICgraph)
       
        # Centrality plot-------
        image2 <- self$results$plot2
        image2$setState(EBICgraph)
        }
       
        },
      
 
#================================================================

.plot = function(image, ggtheme, theme,...) {
  
  
  if (is.null(image$state))
    return(FALSE)
  
  EBICgraph <- image$state
  
  plot <- qgraph( EBICgraph, layout = "spring", details = TRUE)
  
 # plot <- plot+ggtheme
  
  print(plot)
  TRUE

  },     

# Centrality plot for EBIC------------

.plot2 = function(image2, ggtheme, theme,...) {
  
  if (is.null(image2$state))
    return(FALSE)
  
  scale <- self$options$scale
  
  EBICgraph <- image2$state

  plot2<- qgraph::centralityPlot(EBIC = EBICgraph,
                                 scale=scale)

  plot2 <- plot2+ggtheme
  
  print(plot2)
  TRUE
  
},
  
  
# partial plot-----------


.plot1 = function(image1,ggtheme, theme, ...) {
      
        
  if (is.null(image1$state))
    return(FALSE)
        
        partial <- image1$state
        
        plot1 <- qgraph(partial, layout = "spring", details = TRUE)
        
      #  plot1 <- plot1+ggtheme
        
        print(plot1)
        TRUE
      }
        )
)


