
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

        # Data handling---
        mat <- as.matrix(data)
        weight_matrix <- apply(mat, 2, as.numeric)

        # ------------------------------------------------------------
        # Apply absWeight + threshold filtering BEFORE analysis/plot
        # ------------------------------------------------------------
        W <- weight_matrix

        thr <- self$options$threshold
        if (is.null(thr) || is.na(thr))
          thr <- 0
        thr <- as.numeric(thr)

        absw <- self$options$absWeight
        # handle cases where UI passes "TRUE"/"FALSE" as strings
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

        # ------------------------------------------------------------
        # Build info line shown above table/plot
        # ------------------------------------------------------------
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

        # ------------------------------------------------------------
        # (NEW) HITS (Hub/Authority) + PageRank via igraph
        # ------------------------------------------------------------
        hub_vec  <- rep(NA_real_, length(vars))
        auth_vec <- rep(NA_real_, length(vars))
        pr_vec   <- rep(NA_real_, length(vars))

        if (requireNamespace("igraph", quietly = TRUE)) {
          # ensure the matrix has dimnames so we can align safely
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

          # PageRank
          pr <- igraph::page_rank(g, weights = igraph::E(g)$weight)$vector

          # HITS
          hub  <- igraph::hub_score(g,     weights = igraph::E(g)$weight)$vector
          auth <- igraph::authority_score(g, weights = igraph::E(g)$weight)$vector

          # Align to vars (in case igraph reorders vertices)
          vnames <- igraph::V(g)$name
          if (!is.null(vnames)) {
            idx <- match(vars, vnames)
            pr_vec   <- pr[idx]
            hub_vec  <- hub[idx]
            auth_vec <- auth[idx]
          } else {
            # fallback: assume order is same
            pr_vec   <- as.numeric(pr)
            hub_vec  <- as.numeric(hub)
            auth_vec <- as.numeric(auth)
          }
        }

        # ------------------------------------------------------------
        # Centrality Table (qgraph) + add Hub/Authority/PageRank
        # ------------------------------------------------------------
        # ------------------------------------------------------------
        # Centrality Table + Centrality Plot data
        # ------------------------------------------------------------
        need_centrality <- isTRUE(self$options$cen) || isTRUE(self$options$plot1)
        
        if (need_centrality) {
          
          res <- qgraph::centrality_auto(W)
          cen <- res[["node.centrality"]]
          
          # qgraph centrality_auto usually returns:
          # Betweenness, Closeness, InDegree, OutDegree,
          # OutExpectedInfluence, InExpectedInfluence
          cen_df <- data.frame(
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

        # Plot
        if (isTRUE(self$options$plot)) {
          image <- self$results$plot
          image$setState(W)  # use filtered matrix for plotting as well
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
        
        # ------------------------------------------------------------
        # Node colors: use the same pastel palette as the network plot
        # ------------------------------------------------------------
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
        
        # ------------------------------------------------------------
        # Sort values: highest centrality should appear at the top
        # ------------------------------------------------------------
        ord <- order(x, decreasing = TRUE, na.last = TRUE)
        
        x <- x[ord]
        bar_cols <- bar_cols[ord]
        
        # barplot(horiz = TRUE) draws the first value at the bottom,
        # so reverse the order to place the highest value at the top.
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
