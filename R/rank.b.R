
# This file is a generated template, your changes will not be overwritten

rankClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "rankClass",
    inherit = rankBase,
    private = list(
      .htmlwidget = NULL,
      # Add instance for HTMLWidget
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new() # Initialize the HTMLWidget instance
        
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title = "Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
              )
            )
          )
        }
        if (isTRUE(self$options$plot)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width2
          height <- self$options$height2
          self$results$plot2$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width3
          height <- self$options$height3
          self$results$plot3$setSize(width, height)
        }        
        },
      
      .run = function() {
        if (length(self$options$vars) < 2) return()
        
        vars <- self$options$vars
        type <- self$options$type
        mydata <- self$data
        mydata <- jmvcore::naOmit(mydata)
        
        for (v in vars)
          mydata[[v]] <- jmvcore::toNumeric(mydata[[v]])
        
        if (self$options$type == 'spearman') {
          #  compute spearman correlation with psych package--------
          
          spear <- psych::corr.test(mydata, method = "spearman")
          spearman <- spear$r
          
          spearman <- as.data.frame(spearman)
          names <- dimnames(spearman)[[1]]
          dims <- dimnames(spearman)[[2]]
          table <- self$results$matrix
          # creating table----------------
          for (dim in dims) {
            table$addColumn(name = paste0(dim), type = 'number')
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- spearman[name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
          # partial plot---------
          res <- psych::partial.r(spearman)
          image1 <- self$results$plot1
          image1$setState(res)
          
          # EBIC PLOT------------
          rank <- spear$r
          # Compute graph with tuning = 0.5 (EBIC)
          EBICgraph <- qgraph::EBICglasso(rank, nrow(mydata), 0.5, threshold = TRUE)
          
          # Prepare Data For Plot -------
          image <- self$results$plot
          image$setState(EBICgraph)
          
          # Centrality plot-------
          image2 <- self$results$plot2
          image2$setState(EBICgraph)
          
          # Matrix plot----------
          image3 <- self$results$plot3
          image3$setState(as.matrix(spearman))
        }
        
        if (self$options$type == 'polychoric') {
          # compute polychoric correlation-------
          poly <- psych::polychoric(mydata)$rho
          poly <- as.data.frame(poly)
          
          names <- dimnames(poly)[[1]]
          dims <- dimnames(poly)[[2]]
          table <- self$results$matrix
          # creating table----------------
          for (dim in dims) {
            table$addColumn(name = paste0(dim), type = 'number')
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- poly[name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
          
          # paritial----------------------------------------------
          
          res <- psych::partial.r(poly)
          
          image1 <- self$results$plot1
          image1$setState(res)
          # EBIC PLOT------------
          poly <- psych::polychoric(mydata)$rho
          # Compute graph with tuning = 0.5 (EBIC)
          EBICgraph <- qgraph::EBICglasso(poly, nrow(mydata), 0.5, threshold = TRUE)
          
          image <- self$results$plot
          image$setState(EBICgraph)
          
          # Centrality plot-------
          image2 <- self$results$plot2
          image2$setState(EBICgraph)
          
          # Matrix plot----------
          
          image3 <- self$results$plot3
          image3$setState(as.matrix(poly))
        }
        
        if (self$options$type == 'tetrachoric') {
          # compute tetrachoric correlation with psych package--------
          
          tetrarho <- psych::tetrachoric(mydata)$rho
          tetrarho <- as.data.frame(tetrarho)
          names <- dimnames(tetrarho)[[1]]
          dims <- dimnames(tetrarho)[[2]]
          table <- self$results$matrix
          # creating table----------------
          for (dim in dims) {
            table$addColumn(name = paste0(dim), type = 'number')
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- tetrarho[name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
          
          ### partial correlation--------
          res <- psych::partial.r(tetrarho)
          image1 <- self$results$plot1
          image1$setState(res)
          
          # EBIC PLOT------------
          tetrarho <- psych::tetrachoric(mydata)$rho
          # Compute graph with tuning = 0.5 (EBIC)
          EBICgraph <- qgraph::EBICglasso(tetrarho, nrow(mydata), 0.5, threshold = TRUE)
          image <- self$results$plot
          image$setState(EBICgraph)
          # Centrality plot-------
          image2 <- self$results$plot2
          image2$setState(EBICgraph)
          
          # Matrix plot----------
          image3 <- self$results$plot3
          image3$setState(as.matrix(tetrarho))
        }
      },
     
      #Plot---
      
      .plot3 = function(image3, ggtheme, theme, ...) {
        if (is.null(image3$state))
          return(FALSE)
        gram <- image3$state
        plot3 <- corrplot::corrplot(
          gram,
          method=self$options$method,
          type=self$options$type1
         )
        print(plot3)
        TRUE
      },
      
      .plot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        EBICgraph <- image$state
        plot <- qgraph::qgraph(EBICgraph, layout = "spring")
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

      .plot1 = function(image1, ...) {
        if (is.null(image1$state))
          return(FALSE)
        res <- image1$state
        plot1 <- qgraph::qgraph(res, layout = "spring", details = TRUE)
        print(plot1)
        TRUE
      }
      
    )
  )
