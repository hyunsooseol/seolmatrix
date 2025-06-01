
corClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "corClass",
    inherit = corBase,
    private = list(
      .htmlwidget = NULL,
      # Add instance for HTMLWidget
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new() # Initialize the HTMLWidget instance
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>The hierarchical cluster, only valid when order is hclust.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))
        
        if (isTRUE(self$options$plot)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        
      },

      .run = function() {
        
        if (is.null(self$options$vars)) return()
        
        vars  <- self$options$vars
        data <- self$data

        # pearson and spearman  correlation------
        mat <-  stats::cor(data, 
                           method = self$options$type,
                           use = self$options$missing)
        
        mat <- as.data.frame(mat)
        mat1 <- as.matrix(mat)
        
        # table---
        if(isTRUE(self$options$mat)){
        
        names <- dimnames(mat)[[1]]
        dims <- dimnames(mat)[[2]]
        table <- self$results$matrix
        
        # creating table----------------
        for (dim in dims) {
          table$addColumn(name = paste0(dim), type = 'number')
        }
        for (name in names) {
          row <- list()
          for (j in seq_along(dims)) {
            row[[dims[j]]] <- mat[name, j]
          }
          table$addRow(rowKey = name, values = row)
        }
        
        }
        
        # hclust plot----------
        if(isTRUE(self$options$plot)){
        image <- self$results$plot
        image$setState(mat1)
        }
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state)) return(FALSE)
         
         mat1 <- image$state
        
         plot <- corrplot::corrplot(
          mat1,
          method= self$options$method1,
          type= self$options$type1,
          order = self$options$order,
          hclust.method = self$options$method,
          addrect = self$options$k,
          rect.lwd = self$options$size,
          rect.col = self$options$color,
         )
        print(plot)
        TRUE
      }
    )
  )
