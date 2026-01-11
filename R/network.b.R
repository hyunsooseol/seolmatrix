
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
        data <- self$data
        data <- jmvcore::naOmit(data)

        # labels column -> rownames
        if (!is.null(self$options$labels)) {
          rownames(data) <- data[[self$options$labels]]
          data[[self$options$labels]] <- NULL
        }

        # ensure numeric
        for (i in seq_along(vars))
          data[[i]] <- jmvcore::toNumeric(data[[i]])

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
        if (isTRUE(self$options$cen)) {

          res <- qgraph::centrality_auto(W)
          cen <- res[["node.centrality"]]

          table <- self$results$cen

          for (i in seq_along(vars)) {
            row <- list()
            row[["bet"]]   <- cen[i, 1]
            row[["clo"]]   <- cen[i, 2]
            row[["ind"]]   <- cen[i, 3]
            row[["out"]]   <- cen[i, 4]
            row[["outex"]] <- cen[i, 5]
            row[["inex"]]  <- cen[i, 6]

            # (NEW) directed-role metrics
            row[["hub"]]  <- hub_vec[i]
            row[["auth"]] <- auth_vec[i]
            row[["pr"]]   <- pr_vec[i]

            table$setRow(rowKey = vars[i], values = row)
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
      }
    )
  )
