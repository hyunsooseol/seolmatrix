
# This file is a generated template, your changes will not be overwritten
#' @importFrom qgraph qgraph  
#' @importFrom RColorBrewer brewer.pal
#' @export
networkClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "networkClass",
    inherit = networkBase,
    private = list(
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars) | is.null(self$options$labels)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
      
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title="Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>Directed graph centrality based on <b>qgraph</b> R package.</li>',
              '<li>Undirected graph centrality is provided by the Partial correlation analysis in seolmatrix.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
              
            )
            
          )
        )         
        
        if(isTRUE(self$options$plot)){
          
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }    
      
      },
    #######################################
    .run = function() {

      if (is.null(self$options$labels)) return()
      
  if (!is.null(self$options$vars)) {
        
        vars <- self$options$vars
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        if ( ! is.null(self$options$labels)) {
          
          rownames(data) <- data[[self$options$labels]]
          data[[self$options$labels]] <- NULL
          
        }
        
        for (i in seq_along(vars))
          data[[i]] <- jmvcore::toNumeric(data[[i]]) 
      
      # Data handling---
        mat <- as.matrix(data)
        weight_matrix <- apply(mat, 2, as.numeric)

       
        # Centrality Table---
        res <- qgraph::centrality_auto(weight_matrix)
        cen<- res[["node.centrality"]]
        
        table <- self$results$cen
        
        for (i in seq_along(vars)) {
          
          row <- list()
        
          row[["bet"]] <- cen[i, 1]
          row[["clo"]] <- cen[i, 2]
          row[["ind"]] <- cen[i, 3]
          row[["out"]] <- cen[i, 4]
          row[["outex"]] <- cen[i, 5]
          row[["inex"]] <- cen[i, 6]
        
          table$setRow(rowKey = vars[i], values = row)
        }
        
        if(isTRUE(self$options$plot)){
        # qgraph---        
        image <- self$results$plot
        image$setState(weight_matrix)
        }
  }
    },
 
  .plot = function(image, ...) { 
    
     if(is.null(self$options$labels)) return()
     
      mat<- image$state
     
      node_colors <- RColorBrewer::brewer.pal(n = nrow(mat), name = "Set3")

      # g <- igraph::graph_from_adjacency_matrix(mat, mode="directed")
      # node_degrees <- igraph::degree(g)
      # node_sizes <- node_degrees * 2  
      # 
      # label_size <- 1 + (node_degrees / max(node_degrees))   
      
      # Calculate node sizes based on label length
      #label_lengths <- nchar(self$options$labels)
      #node_sizes <- label_lengths * 2  # Adjust the multiplier as needed
      
      plot<- qgraph::qgraph(mat,
                     labels=self$optios$labels,
                     directed=TRUE,
                     edge.color="black",
                     #vsize=node_sizes,
                     #label.cex=1,
                     color=node_colors)
      
      print(plot)
      TRUE
      
        })
)
