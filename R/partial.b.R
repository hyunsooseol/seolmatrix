
partialClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "partialClass",
    inherit = partialBase,
    private = list(
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        
        if (is.null(self$dat) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>The network plots were implemented using the <b>qgraph</b> R package.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))
        
        if (self$options$robust)
          self$results$robust$setNote("Note", "Robust correlations handle missing data using pairwise deletion.")
        
        if (isTRUE(self$options$plot)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot2$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width3
          height <- self$options$height3
          self$results$plot3$setSize(width, height)
        }        

        # get variables--------------------------------------
        
        matrix <- self$results$get('matrix')
        var <- self$options$get('vars')
        varCtl <- self$options$get('ctrlvars')
        
        
        # whether the procedure is controlling for variables or not-----------
        
        # matrix$setTitle(ifelse(
        #   length(varCtl) > 0,
        #   'Partial Correlation Matrix',
        #   'Correlation Matrix'
        # ))
        
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
        
        # matrix$setNote(
        #   'ctlNte',
        #   ifelse(
        #     length(varCtl) > 0,
        #     paste0('Controlling for ', paste(varCtl, collapse = ", ")),
        #     'Not controlling for any variables, the result table shows Pearson correlation matrix'
        #   )
        # )
        
        
        matrix$setNote('sigNte', paste0(
          # ifelse(
          #   self$options$get('sidSig') == 'onetailed',
          #   'One-tailed significance',
          #   'Two-tailed significance'
          # ),
          ifelse(
            self$options$get('flgSig'),
            ': * p < .05, ** p < .01, *** p < .001',
            ''
          )
        ))
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
      },

      
      .run = function() {
        matrix <- self$results$get('matrix')
        var <- self$options$get('vars')
        nVar <- length(var)
        varCtl <- self$options$get('ctrlvars')
        nCtl <- length(varCtl)
        
        data <- self$data
        
        #general---
        
        convertToNumeric <- function(data, variables) {
          for (v in variables) {
            data[[v]] <- jmvcore::toNumeric(data[[v]])
          }
          return(data)
        }
        
        # partial and network data transformation---
        
        if (nVar > 1 || isTRUE(self$options$plot) || isTRUE(self$options$plot2) || isTRUE(self$options$cen)) {
          data <- convertToNumeric(data, c(var, varCtl))
        }
        
        # Computing correlations----------
        if (nVar > 1) {
          m <- as.matrix(stats::cor(data[, c(var, varCtl)], 
                                    use = self$options$missing, 
                                    method = self$options$type))
          X <- m[var, var]
          
          if (nCtl > 0) {
            Y <- m[var, varCtl]
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
          Pp <- -nt * expm1(pt(abs(Rt), (df - 2), log.p = TRUE))
          
          # populate results------------------------------------------------
          for (i in 2:nVar) {
            for (j in seq_len(i - 1)) {
              values <- list()
              values[[paste0(var[[j]], '[r]')]] <- Rp[i, j]
              values[[paste0(var[[j]], '[rp]')]] <- Pp[i, j]
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
        
        
        if (isTRUE(self$options$plot) || isTRUE(self$options$plot2) || isTRUE(self$options$cen)) {
          
          CorMat <- qgraph::cor_auto(data)
          
          # Network PLOT
          if (isTRUE(self$options$plot)) {
            n = nrow(data)
            image <- self$results$plot
            state <- list(CorMat, n)  
            image$setState(state)
          }
          
          # Centrality 
          if (isTRUE(self$options$cen)) {
            vars <- self$options$vars
            table <- self$results$cen
            # Calculate centrality measures
            res <- qgraph::centrality_auto(CorMat)
            cen <- res[["node.centrality"]]
            for (i in seq_along(vars)) {
              row <- list()
              row[["clo"]] <- cen[i, 2]
              row[["bet"]] <- cen[i, 3]
              table$setRow(rowKey = vars[i], values = row)
            }
          }
          
          # EBIC graph and Centrality plot
          if (isTRUE(self$options$plot2)) {
            # Compute graph with tuning = 0.5 (EBIC)
            EBICgraph <- qgraph::EBICglasso(CorMat, nrow(data), 0.5, threshold = TRUE)
            # Centrality plot-------
            image2 <- self$results$plot2
            image2$setState(EBICgraph)
          }
        }
        
        # Robust correlation ------------
        if (isTRUE(self$options$robust)) {
          
          robust_data <- self$data
          robust_vars <- self$options$vars
          
         
          robust_data <- convertToNumeric(robust_data, robust_vars)
          
          # robust correlation 계산
          res <- correlation::correlation(robust_data[, robust_vars, drop = FALSE], 
                                          method = self$options$method)
          
          v1 <- res$Parameter1
          v2 <- res$Parameter2
          r <- res$r
          low <- res$CI_low
          high <- res$CI_high
          t <- res$t
          df <- res$df
          p <- res$p
          n <- res$n_Obs
          
          # populate robust correlation table---
          table <- self$results$robust
          nrows <- ncol(combn(robust_vars, 2))
          
          for (i in seq_len(nrows)) {
            row <- list()
            row[['v1']] <- v1[i]
            row[['v2']] <- v2[i]
            row[['r']] <- r[i]
            row[['low']] <- low[i]
            row[['high']] <- high[i]
            row[['t']] <- t[i]
            row[['df']] <- df[i]
            row[['p']] <- p[i]
            row[['n']] <- n[i]
            
            table$addRow(rowKey = i, values = row)
          }
        }
      
        if(isTRUE(self$options$plot3)){
          
          image <- self$results$plot3
          image$setState(res)
        }

        },            
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        df <- image$state[[1]]
        n <- image$state[[2]] # for glasso
        model <- self$options$model
        layout <- self$options$layout
        shape <- self$options$shape
        plot <- qgraph::qgraph(
          df,
          graph = model,
          layout = layout,
          shape = shape,
          sampleSize = n
        )
        print(plot)
        TRUE
      },
      
      # Centrality plot for EBIC------------
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        scale <- self$options$scale
        EBICgraph <- image2$state
        plot2 <- qgraph::centralityPlot(EBIC = EBICgraph,
                                        include = 'all',
                                        scale = scale)
        plot2 <- plot2 + ggtheme
        if (self$options$angle > 0) {
          plot2 <- plot2 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot2)
        TRUE
      },
      
      .plot3 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        res <- image$state
        
        library(ggplot2)
        
        plot3<- ggplot(res, aes(x = Parameter1, y = Parameter2, fill = r)) +
          geom_tile(color = "grey80", size = 0.7) +
          scale_fill_gradient2(
            low = "#4575b4",
            mid = "white",
            high = "#d73027",
            limits = c(-1, 1),
            midpoint = 0,
            name = "Correlation"
          ) +
          geom_text(aes(label = sprintf("%.2f", r)), color = "black", size = 5, fontface = "bold") +
          labs(
            title = "",
            subtitle = "",
            x = NULL, y = NULL
          ) +
          theme_minimal(base_size = 15) +
          theme(
            axis.text.x = element_text(face = "bold"),
            axis.text.y = element_text(face = "bold"),
            panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 18)
          )
        
        if (self$options$angle1 > 0) {
          plot3 <- plot3 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle1, hjust = 1))
        }
        
        # plot <- plot + ggtheme
        print(plot3)
        TRUE
      }
    )
  )


# Patial plot----------------

# var <- self$options$vars
# varCtl <- self$options$ctrlvars
#
# if(is.null(varCtl)){
#
#   partial <- psych::partial.r(data)
#
# } else{
#
# partial <- psych::partial.r(data,x=var, y=varCtl)
#
# }

# if(isTRUE(self$options$pm)){
#   self$results$text1$setContent(partial)
# }

#        image1 <- self$results$plot1
#        image1$setState(partial)
#
#        # Matrix plot-----------
#
#        image3 <- self$results$plot3
#        image3$setState(as.matrix(partial))
#

# # Clustering plot-------
# image3 <- self$results$plot3
# image3$setState(EBICgraph)

#---------------------------------------
# if(isTRUE(self$options$ebic)){
#
#   self$results$text$setContent(CorMat)
# }

