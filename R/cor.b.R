
corClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "corClass",
    inherit = corBase,
    private = list(
      .htmlwidget = NULL,

      # ---------------------------
      # Helpers (추가)
      # ---------------------------
      .clearTable = function(table) {
        tryCatch({ table$clear() }, error = function(e) NULL)
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
        rho  <- as.matrix(rho)
        pmat <- as.matrix(pmat)

        out <- matrix("", nrow(rho), ncol(rho))
        colnames(out) <- colnames(rho)
        rownames(out) <- rownames(rho)

        for (i in 1:nrow(rho)) for (j in 1:ncol(rho)) {
          if (is.na(rho[i, j])) {
            out[i, j] <- ""
          } else {
            rtxt <- sprintf(paste0("%.", digits, "f"), rho[i, j])
            if (i == j) out[i, j] <- rtxt
            else out[i, j] <- paste0(rtxt, private$.stars_from_p(pmat[i, j]))
          }
        }
        out
      },

      .keepLowerTriangle = function(mat, keepDiag = TRUE, blank = "") {
        m <- as.matrix(mat)
        ut <- upper.tri(m, diag = FALSE)

        if (is.numeric(m)) m[ut] <- NA_real_ else m[ut] <- blank

        if (!keepDiag) {
          if (is.numeric(m)) diag(m) <- NA_real_ else diag(m) <- blank
        }
        m
      },

      .format_p_as_text = function(pmat, digits = 3, blank = "") {
        p <- as.matrix(pmat)
        out <- matrix(blank, nrow(p), ncol(p))
        colnames(out) <- colnames(p)
        rownames(out) <- rownames(p)

        for (i in 1:nrow(p)) for (j in 1:ncol(p)) {
          if (is.na(p[i, j])) {
            out[i, j] <- blank
          } else if (p[i, j] < 0.001) {
            out[i, j] <- "<.001"
          } else {
            out[i, j] <- sprintf(paste0("%.", digits, "f"), p[i, j])
          }
        }
        out
      },

      .setAPATableCaption = function(table, type, missing, lowerOnly = TRUE, hideDiag = FALSE) {
        if (is.null(table)) return()

        # Title (APA: title above table)
        triTxt <- if (isTRUE(lowerOnly)) "Lower triangle" else "Full matrix"
        diagTxt <- if (isTRUE(hideDiag)) ", diagonal hidden" else ""
        title <- paste0(
          "Correlation Matrix (", type, ")"
        )
        tryCatch(table$setTitle(title), error = function(e) NULL)

        # Note (APA: note below table)
        tryCatch(
          table$setNote("Note", "* p < .05, ** p < .01, *** p < .001"),
          error = function(e) NULL
        )
      },

      # ---------------------------
      # Init
      # ---------------------------
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
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
      },

      # ---------------------------
      # Run
      # ---------------------------
      .run = function() {

        if (is.null(self$options$vars)) return()

        vars  <- self$options$vars
        data  <- self$data

        # 옵션(없어도 오류 안 나게 기본값 처리)
        hideDiag <- isTRUE(tryCatch(self$options$hideDiag, error = function(e) FALSE))
        wantPmat <- isTRUE(tryCatch(self$options$pmatrix, error = function(e) FALSE))

        # pearson and spearman correlation------
        rho <- stats::cor(
          data,
          method = self$options$type,
          use    = self$options$missing
        )
        rho <- as.matrix(rho)

        # p-value (t-test 근사; pairwise complete cases 기준)
        pmat <- private$.p_matrix_approx(rho, data)

        # ----------------------------
        # matrix table (r + stars) : 하삼각 + (옵션)대각선 숨김
        # ----------------------------
        if (isTRUE(self$options$mat)) {

          table <- self$results$matrix
          private$.clearTable(table)

          # r + stars (text)
          mtxt <- private$.format_r_with_stars(rho, pmat, digits = 2)
          mtxt <- private$.keepLowerTriangle(mtxt, keepDiag = !hideDiag, blank = "")

          dims <- colnames(mtxt)

          for (dim in dims) {
            table$addColumn(name = paste0(dim), type = 'text')
          }

          for (i in seq_along(dims)) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- mtxt[i, j]
            }
            table$addRow(rowKey = dims[i], values = row)
          }

          # APA caption 자동 설정
          private$.setAPATableCaption(
            table   = table,
            type    = self$options$type,
            missing = self$options$missing,
            lowerOnly = TRUE,
            hideDiag  = hideDiag
          )
        }

        # ----------------------------
        # (선택) p-value matrix table : 하삼각 + <.001 표기 + (옵션)대각선 숨김
        # ----------------------------
        if (wantPmat && !is.null(self$results$pmatrix)) {

          ptable <- self$results$pmatrix
          private$.clearTable(ptable)

          ptxt <- private$.format_p_as_text(pmat, digits = 3, blank = "")
          ptxt <- private$.keepLowerTriangle(ptxt, keepDiag = !hideDiag, blank = "")

          dims <- colnames(ptxt)

          for (dim in dims) {
            ptable$addColumn(name = paste0(dim), type = 'text')
          }

          for (i in seq_along(dims)) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- ptxt[i, j]
            }
            ptable$addRow(rowKey = dims[i], values = row)
          }

          # APA note (p-value 표에도 동일 적용)
          tryCatch(
            ptable$setNote("Note", "P-values are shown; values smaller than .001 are reported as <.001."),
            error = function(e) NULL
          )
        }

        # ----------------------------
        # hclust plot----------
        # (원본 동작 유지: plot에는 원래 전체 rho를 state로 전달)
        # ----------------------------
        if (isTRUE(self$options$plot)) {
          image <- self$results$plot
          image$setState(rho)
        }
      },

      .plot = function(image, ggtheme, theme, ...) {

        if (is.null(image$state)) return(FALSE)

        mat1 <- image$state

        plot <- corrplot::corrplot(
          mat1,
          method        = self$options$method1,
          type          = self$options$type1,
          order         = self$options$order,
          hclust.method = self$options$method,
          addrect       = self$options$k,
          rect.lwd      = self$options$size,
          rect.col      = self$options$color
        )
        print(plot)
        TRUE
      }
    )
  )
