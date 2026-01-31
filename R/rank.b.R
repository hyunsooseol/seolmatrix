
# This file is a generated template, your changes will not be overwritten

rankClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "rankClass",
    inherit = rankBase,

    private = list(

      # ---------------------------
      # Helpers
      # ---------------------------
      .safeResult = function(name) {
        tryCatch(self$results[[name]], error = function(e) NULL)
      },

      .clearTable = function(table) {
        tryCatch({ table$clear() }, error = function(e) NULL)
      },

      .buildMatrixTable = function(table, mat, colType = 'number') {
        mat <- as.data.frame(mat)
        names <- dimnames(mat)[[1]]
        dims  <- dimnames(mat)[[2]]

        private$.clearTable(table)

        for (dim in dims)
          table$addColumn(name = paste0(dim), type = colType)

        for (name in names) {
          row <- list()
          for (j in seq_along(dims))
            row[[dims[j]]] <- mat[name, j]
          table$addRow(rowKey = name, values = row)
        }
      },

      .pairwise_n = function(df) {
        p <- ncol(df)
        N <- matrix(NA_integer_, p, p)
        for (i in 1:p) for (j in 1:p) {
          N[i, j] <- sum(stats::complete.cases(df[, c(i, j)]))
        }
        colnames(N) <- rownames(N) <- colnames(df)
        N
      },

      .p_from_r_t = function(r, n_eff) {
        r <- max(min(r, 0.999999), -0.999999)
        df <- n_eff - 2
        if (is.na(df) || df <= 0) return(NA_real_)
        tval <- r * sqrt(df / (1 - r^2))
        2 * stats::pt(abs(tval), df = df, lower.tail = FALSE)
      },

      .p_matrix_approx = function(rho, df) {
        rho <- as.matrix(rho)
        N <- private$.pairwise_n(df)
        pmat <- matrix(NA_real_, nrow(rho), ncol(rho))
        for (i in 1:nrow(rho)) for (j in 1:ncol(rho)) {
          if (i == j) pmat[i, j] <- NA_real_
          else pmat[i, j] <- private$.p_from_r_t(rho[i, j], N[i, j])
        }
        colnames(pmat) <- colnames(rho)
        rownames(pmat) <- rownames(rho)
        pmat
      },

      .stars_from_p = function(p) {
        if (is.na(p)) return("")
        if (p < .001) return("***")
        if (p < .01)  return("**")
        if (p < .05)  return("*")
        ""
      },

      .format_r_with_stars = function(rho, pmat, digits = 2) {
        rho <- as.matrix(rho)
        pmat <- as.matrix(pmat)

        out <- matrix("", nrow(rho), ncol(rho))
        colnames(out) <- colnames(rho)
        rownames(out) <- rownames(rho)

        for (i in 1:nrow(rho)) for (j in 1:ncol(rho)) {
          rtxt <- sprintf(paste0("%.", digits, "f"), rho[i, j])
          if (i == j) out[i, j] <- rtxt
          else out[i, j] <- paste0(rtxt, private$.stars_from_p(pmat[i, j]))
        }
        out
      },

      .as_binary_01 = function(df) {
        out <- df
        for (v in colnames(out)) {
          x <- out[[v]]
          ux <- sort(unique(x[!is.na(x)]))
          if (length(ux) == 2) {
            mx <- ux[2]
            out[[v]] <- ifelse(is.na(x), NA_real_, ifelse(x == mx, 1, 0))
          } else {
            out[[v]] <- x
          }
        }
        out
      },

      .fillTauTable = function(table, df_bin, tetra_obj) {
        private$.clearTable(table)

        tau <- tetra_obj$tau
        if (is.matrix(tau)) {
          tau1 <- tau[, 1, drop = TRUE]
        } else {
          tau1 <- as.numeric(tau)
          names(tau1) <- names(tau)
        }

        vars <- names(tau1)
        p1 <- sapply(df_bin[, vars, drop = FALSE], function(v) mean(v == 1, na.rm = TRUE))

        for (i in seq_along(vars)) {
          table$addRow(
            rowKey = vars[i],
            values = list(
              variable = vars[i],
              tau      = unname(tau1[i]),
              p1       = unname(p1[i])
            )
          )
        }
      },

      .setMatrixNote = function(type) {
        if (is.null(self$results$matrix)) return()
        approxTxt <- if (type %in% c("polychoric", "tetrachoric")) "; approx for poly/tetra" else ""
        self$results$matrix$setNote("Note", paste0("* p<.05, ** p<.01, *** p<.001", approxTxt))
      },

      .setPmatrixNote = function() {
        pm <- private$.safeResult("pmatrix")
        if (is.null(pm)) return()
        pm$setNote("Note", "p-values are approximate (t-test using pairwise complete cases).")
      },

      .setTauNote = function() {
        tt <- private$.safeResult("tau")
        if (is.null(tt)) return()
        tt$setNote("Note", "τ is the threshold (cut-point) used in the tetrachoric model.; Pr(Y = 1) denotes the observed proportion of responses coded as 1.")
      },

      .hideTable = function(table) {
        if (is.null(table)) return()
        private$.clearTable(table)
        tryCatch(table$setVisible(FALSE), error = function(e) NULL)
      },

      .showTable = function(table) {
        if (is.null(table)) return()
        tryCatch(table$setVisible(TRUE), error = function(e) NULL)
      },

      # ---------------------------
      # Run
      # ---------------------------
      .run = function() {
        if (length(self$options$vars) < 2) {
          private$.hideTable(private$.safeResult("pmatrix"))
          private$.hideTable(private$.safeResult("tau"))
          return()
        }

        vars <- self$options$vars
        type <- self$options$type

        mydata <- jmvcore::naOmit(self$data)
        for (v in vars)
          mydata[[v]] <- jmvcore::toNumeric(mydata[[v]])

        table_corr <- self$results$matrix
        table_p    <- private$.safeResult("pmatrix")
        table_tau  <- private$.safeResult("tau")

        useStars   <- isTRUE(self$options$sigStars)
        wantP      <- isTRUE(self$options$pmatrix) && !is.null(table_p)
        wantTau    <- isTRUE(self$options$tau) && !is.null(table_tau)

        wantPlotGMM  <- isTRUE(self$options$plot)  && !is.null(private$.safeResult("plot"))
        wantPlotCent <- isTRUE(self$options$plot2) && !is.null(private$.safeResult("plot2"))
        wantPlotPart <- isTRUE(self$options$plot1) && !is.null(private$.safeResult("plot1"))
        wantPlotMat  <- isTRUE(self$options$plot3) && !is.null(private$.safeResult("plot3"))

        wantAnyPlot <- wantPlotGMM || wantPlotCent || wantPlotPart || wantPlotMat

        if (!wantP)   private$.hideTable(table_p)   else private$.showTable(table_p)
        if (!wantTau) private$.hideTable(table_tau) else private$.showTable(table_tau)

        if (type == 'spearman') {

          spear <- psych::corr.test(mydata, method = "spearman")
          rho  <- spear$r
          pmat <- spear$p

          if (useStars && !is.null(pmat)) {
            mtxt <- private$.format_r_with_stars(rho, pmat, digits = 2)
            private$.buildMatrixTable(table_corr, mtxt, colType = 'text')
          } else {
            private$.buildMatrixTable(table_corr, rho, colType = 'number')
          }

          if (wantP && !is.null(pmat)) {
            private$.buildMatrixTable(table_p, pmat, colType = 'number')
            private$.setPmatrixNote()
          }

          if (wantAnyPlot) {
            if (wantPlotPart) private$.safeResult("plot1")$setState(psych::partial.r(rho))

            if (wantPlotGMM || wantPlotCent) {
              EBICgraph <- qgraph::EBICglasso(rho, nrow(mydata), 0.5, threshold = TRUE)
              if (wantPlotGMM)  private$.safeResult("plot")$setState(EBICgraph)
              if (wantPlotCent) private$.safeResult("plot2")$setState(EBICgraph)
            }

            if (wantPlotMat) private$.safeResult("plot3")$setState(as.matrix(rho))
          }
        }

        if (type == 'polychoric') {

          poly_obj <- psych::polychoric(mydata)
          rho  <- poly_obj$rho
          pmat <- private$.p_matrix_approx(rho, mydata)

          if (useStars) {
            mtxt <- private$.format_r_with_stars(rho, pmat, digits = 2)
            private$.buildMatrixTable(table_corr, mtxt, colType = 'text')
          } else {
            private$.buildMatrixTable(table_corr, rho, colType = 'number')
          }

          if (wantP) {
            private$.buildMatrixTable(table_p, pmat, colType = 'number')
            private$.setPmatrixNote()
          }

          if (wantAnyPlot) {
            if (wantPlotPart) private$.safeResult("plot1")$setState(psych::partial.r(rho))

            if (wantPlotGMM || wantPlotCent) {
              EBICgraph <- qgraph::EBICglasso(rho, nrow(mydata), 0.5, threshold = TRUE)
              if (wantPlotGMM)  private$.safeResult("plot")$setState(EBICgraph)
              if (wantPlotCent) private$.safeResult("plot2")$setState(EBICgraph)
            }

            if (wantPlotMat) private$.safeResult("plot3")$setState(as.matrix(rho))
          }
        }

        if (type == 'tetrachoric') {

          df_bin <- private$.as_binary_01(mydata)
          tetra_obj <- psych::tetrachoric(df_bin)
          rho  <- tetra_obj$rho
          pmat <- private$.p_matrix_approx(rho, df_bin)

          if (useStars) {
            mtxt <- private$.format_r_with_stars(rho, pmat, digits = 2)
            private$.buildMatrixTable(table_corr, mtxt, colType = 'text')
          } else {
            private$.buildMatrixTable(table_corr, rho, colType = 'number')
          }

          if (wantP) {
            private$.buildMatrixTable(table_p, pmat, colType = 'number')
            private$.setPmatrixNote()
          }

          if (wantTau) {
            private$.fillTauTable(table_tau, df_bin, tetra_obj)
            private$.setTauNote()
          }

          if (wantAnyPlot) {
            if (wantPlotPart) private$.safeResult("plot1")$setState(psych::partial.r(rho))

            if (wantPlotGMM || wantPlotCent) {
              EBICgraph <- qgraph::EBICglasso(rho, nrow(df_bin), 0.5, threshold = TRUE)
              if (wantPlotGMM)  private$.safeResult("plot")$setState(EBICgraph)
              if (wantPlotCent) private$.safeResult("plot2")$setState(EBICgraph)
            }

            if (wantPlotMat) private$.safeResult("plot3")$setState(as.matrix(rho))
          }
        }

        private$.setMatrixNote(type)
      },

      # ---------------------------
      # Plot render functions
      # ---------------------------
      .plot3 = function(image3, ggtheme, theme, ...) {
        if (is.null(image3$state)) return(FALSE)
        corrplot::corrplot(
          image3$state,
          method = self$options$method,
          type   = self$options$type1
        )
        TRUE
      },

      .plot = function(image, ...) {
        if (is.null(image$state)) return(FALSE)

        # qgraph draws via side effects, but jamovi renderers often require explicit print()
        g <- qgraph::qgraph(image$state, layout = "spring")
        print(g)

        TRUE
      },


      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state)) return(FALSE)
        plt <- qgraph::centralityPlot(
          EBIC = image2$state,
          include = "all",
          scale = self$options$scale
        )
        plt <- plt + ggtheme
        if (self$options$angle > 0) {
          plt <- plt + ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1)
          )
        }
        print(plt)
        TRUE
      },

      .plot1 = function(image1, ...) {
        if (is.null(image1$state)) return(FALSE)
        qgraph::qgraph(image1$state, layout = "spring", details = TRUE)
        TRUE
      }
    )
  )
