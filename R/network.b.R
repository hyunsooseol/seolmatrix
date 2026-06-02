
networkClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "networkClass",
    inherit = networkBase,
    private = list(
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) |
            is.null(self$options$vars) | is.null(self$options$labels)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>This analysis computes centrality and role-based measures(HITS and PageRank) on a thresholded directed network.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
      },
      
      #######################################
      .run = function() {
        if (is.null(self$options$labels))
          return()
        
        if (is.null(self$options$vars))
          return()
        
        vars <- self$options$vars
        lab  <- self$options$labels
        
        data <- self$data[, c(lab, vars), drop = FALSE]
        data <- jmvcore::naOmit(data)
        
        rownames(data) <- data[[lab]]
        data[[lab]] <- NULL
        
        for (v in vars)
          data[[v]] <- jmvcore::toNumeric(data[[v]])
        
        # Data handling
        mat <- as.matrix(data)
        weight_matrix <- apply(mat, 2, as.numeric)
        
        # Apply absWeight and threshold filtering before analysis/plot
        W <- weight_matrix
        
        thr <- self$options$threshold
        if (is.null(thr) || is.na(thr))
          thr <- 0
        thr <- as.numeric(thr)
        
        absw <- self$options$absWeight
        
        # Handle cases where UI passes "TRUE"/"FALSE" as strings
        if (is.character(absw))
          absw <- toupper(absw) == "TRUE"
        absw <- isTRUE(absw)
        
        if (!is.na(thr) && thr > 0) {
          if (absw) {
            W[abs(W) < thr] <- 0
          } else {
            W[W < thr] <- 0
          }
        }
        
        # Build info line shown above table/plot
        if (is.na(thr) || thr <= 0) {
          info_text <- "Centrality computed on the full network (no edge filtering)."
        } else {
          if (absw)
            info_text <- paste0("Centrality computed on thresholded network: |w| \u2265 ", thr, " (absolute weights).")
          else
            info_text <- paste0("Centrality computed on thresholded network: w \u2265 ", thr, ".")
        }
        
        info_html <- paste0(
          "<div style='font-size:12px; color:#555; margin: 4px 0 8px 0;'>",
          "<b>Edge filtering:</b> ",
          info_text,
          "</div>"
        )
        
        # Above-table info
        if (!is.null(self$results$filterInfoTable)) {
          if (isTRUE(self$options$cen))
            self$results$filterInfoTable$setContent(info_html)
          else
            self$results$filterInfoTable$setContent("")
        }
        
        # Above-plot info
        if (!is.null(self$results$filterInfoPlot)) {
          if (isTRUE(self$options$plot))
            self$results$filterInfoPlot$setContent(info_html)
          else
            self$results$filterInfoPlot$setContent("")
        }
        
        # HITS and PageRank via igraph
        hub_vec  <- rep(NA_real_, length(vars))
        auth_vec <- rep(NA_real_, length(vars))
        pr_vec   <- rep(NA_real_, length(vars))
        
        if (requireNamespace("igraph", quietly = TRUE)) {
          
          igraph_res <- tryCatch({
            
            # Ensure the matrix has dimnames for safe alignment
            if (is.null(colnames(W)))
              colnames(W) <- vars
            if (is.null(rownames(W)))
              rownames(W) <- vars
            
            g <- igraph::graph_from_adjacency_matrix(
              W,
              mode = "directed",
              weighted = TRUE,
              diag = TRUE
            )
            
            pr <- igraph::page_rank(
              g,
              weights = igraph::E(g)$weight
            )$vector
            
            hub <- igraph::hub_score(
              g,
              weights = igraph::E(g)$weight
            )$vector
            
            auth <- igraph::authority_score(
              g,
              weights = igraph::E(g)$weight
            )$vector
            
            list(
              graph = g,
              page_rank = pr,
              hub = hub,
              authority = auth
            )
            
          }, error = function(e) {
            
            if (!is.null(self$results$filterInfoTable)) {
              self$results$filterInfoTable$setContent(paste0(
                info_html,
                "<div style='font-size:12px; color:#b45309; margin: 4px 0 8px 0;'>",
                "<b>Note:</b> HITS/PageRank could not be computed. ",
                "Hub, Authority, and PageRank values are shown as missing. ",
                "Details: ", htmltools::htmlEscape(e$message),
                "</div>"
              ))
            }
            
            return(NULL)
          })
          
          if (!is.null(igraph_res)) {
            
            g <- igraph_res$graph
            pr <- igraph_res$page_rank
            hub <- igraph_res$hub
            auth <- igraph_res$authority
            
            # Align to vars in case igraph reorders vertices
            vnames <- igraph::V(g)$name
            
            if (!is.null(vnames)) {
              idx <- match(vars, vnames)
              pr_vec   <- pr[idx]
              hub_vec  <- hub[idx]
              auth_vec <- auth[idx]
            } else {
              # Fallback: assume order is the same
              pr_vec   <- as.numeric(pr)
              hub_vec  <- as.numeric(hub)
              auth_vec <- as.numeric(auth)
            }
          }
        }
        
        # Centrality table and centrality plot data
        need_centrality <- isTRUE(self$options$cen) || isTRUE(self$options$plot1)
        
        if (need_centrality) {
          
          centrality_res <- tryCatch({
            
            res <- qgraph::centrality_auto(W)
            cen <- res[["node.centrality"]]
            
            # qgraph centrality_auto usually returns:
            # Betweenness, Closeness, InDegree, OutDegree,
            # OutExpectedInfluence, InExpectedInfluence
            data.frame(
              Node = vars,
              Betweenness = as.numeric(cen[, 1]),
              Closeness = as.numeric(cen[, 2]),
              InDegree = as.numeric(cen[, 3]),
              OutDegree = as.numeric(cen[, 4]),
              OutExpectedInfluence = as.numeric(cen[, 5]),
              InExpectedInfluence = as.numeric(cen[, 6]),
              Hub = as.numeric(hub_vec),
              Authority = as.numeric(auth_vec),
              PageRank = as.numeric(pr_vec),
              check.names = FALSE
            )
            
          }, error = function(e) {
            
            self$results$instructions$setVisible(TRUE)
            self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
              title = "Analysis error",
              content = paste0(
                '<div style="border: 1px solid #fecaca; border-radius: 12px; padding: 14px; background-color: #fef2f2; margin-top: 10px;">',
                '<strong>Centrality indices could not be computed.</strong><br>',
                'Please check whether the directed network matrix is valid.<br><br>',
                '<strong>Possible causes:</strong>',
                '<ul>',
                '<li>The matrix may contain only zero or invalid edge weights after threshold filtering.</li>',
                '<li>The matrix may contain missing, infinite, or non-numeric values.</li>',
                '<li>The network may be too sparse or degenerate for centrality computation.</li>',
                '</ul>',
                '<strong>Details:</strong> ',
                htmltools::htmlEscape(e$message),
                '</div>'
              )
            ))
            
            return(NULL)
          })
          
          if (is.null(centrality_res))
            return()
          
          cen_df <- centrality_res
          
          if (isTRUE(self$options$cen)) {
            
            table <- self$results$cen
            
            for (i in seq_along(vars)) {
              row <- list()
              row[["bet"]]   <- cen_df$Betweenness[i]
              row[["clo"]]   <- cen_df$Closeness[i]
              row[["ind"]]   <- cen_df$InDegree[i]
              row[["out"]]   <- cen_df$OutDegree[i]
              row[["outex"]] <- cen_df$OutExpectedInfluence[i]
              row[["inex"]]  <- cen_df$InExpectedInfluence[i]
              
              row[["hub"]]  <- cen_df$Hub[i]
              row[["auth"]] <- cen_df$Authority[i]
              row[["pr"]]   <- cen_df$PageRank[i]
              
              table$setRow(rowKey = vars[i], values = row)
            }
          }
          
          if (isTRUE(self$options$plot1)) {
            image <- self$results$plot1
            image$setState(cen_df)
          }
        }
        
        # Network plot
        if (isTRUE(self$options$plot)) {
          image <- self$results$plot
          image$setState(W)
        }
      },
      
      .plot = function(image, ...) {
        if (is.null(self$options$labels))
          return()
        
        mat <- image$state
        node_colors <- RColorBrewer::brewer.pal(n = nrow(mat), name = "Set3")
        
        node_labels <- colnames(mat)
        if (is.null(node_labels))
          node_labels <- rownames(mat)
        
        plot <- qgraph::qgraph(
          mat,
          labels = node_labels,
          directed = TRUE,
          edge.color = "black",
          color = node_colors
        )
        print(plot)
        TRUE
      },
      
      .plot1 = function(image, ...) {
        
        cen_df <- image$state
        
        if (is.null(cen_df))
          return(FALSE)
        
        if (!is.data.frame(cen_df))
          return(FALSE)
        
        if (!"Node" %in% names(cen_df))
          return(FALSE)
        
        metric_opt <- self$options$plotMetric
        
        metric_name <- switch(metric_opt,
                              pagerank    = "PageRank",
                              indegree    = "InDegree",
                              outdegree   = "OutDegree",
                              betweenness = "Betweenness",
                              closeness   = "Closeness",
                              hub         = "Hub",
                              authority   = "Authority",
                              outex       = "OutExpectedInfluence",
                              inex        = "InExpectedInfluence",
                              "PageRank"
        )
        
        if (!metric_name %in% names(cen_df))
          return(FALSE)
        
        x <- as.numeric(cen_df[[metric_name]])
        names(x) <- cen_df$Node
        
        # Use the same pastel palette as the network plot
        n_nodes <- nrow(cen_df)
        
        if (n_nodes <= 8) {
          base_cols <- RColorBrewer::brewer.pal(
            n = max(3, n_nodes),
            name = "Set3"
          )[seq_len(n_nodes)]
        } else {
          base_cols <- grDevices::colorRampPalette(
            RColorBrewer::brewer.pal(8, "Set3")
          )(n_nodes)
        }
        
        names(base_cols) <- cen_df$Node
        bar_cols <- base_cols[names(x)]
        bar_cols <- grDevices::adjustcolor(bar_cols, alpha.f = 0.95)
        
        # Sort values so the highest centrality appears at the top
        ord <- order(x, decreasing = TRUE, na.last = TRUE)
        
        x <- x[ord]
        bar_cols <- bar_cols[ord]
        
        # barplot(horiz = TRUE) draws the first value at the bottom
        x <- rev(x)
        bar_cols <- rev(bar_cols)
        
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar), add = TRUE)
        
        graphics::par(
          mar = c(4.5, 8, 3.5, 2),
          las = 1
        )
        
        graphics::barplot(
          x,
          horiz = TRUE,
          main = paste(metric_name, "Centrality"),
          xlab = metric_name,
          col = bar_cols,
          border = "white",
          cex.names = 0.9,
          cex.axis = 0.85,
          cex.lab = 0.9,
          cex.main = 1.0
        )
        
        graphics::abline(v = 0, lty = 2, col = "gray60")
        
        TRUE
      }
    )
  )