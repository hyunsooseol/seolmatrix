
# This file is a generated template, your changes will not be overwritten

ccClass <- if (requireNamespace('jmvcore', quietly=TRUE)) 
  R6::R6Class(
  "ccClass",
  inherit = ccBase,
  private = list(
    .htmlwidget = NULL,
    .allCache = NULL,
    
    .init = function() {
      private$.htmlwidget <- HTMLWidget$new()
      
      if (is.null(self$data) | is.null(self$options$set1) | is.null(self$options$set2)) {
        self$results$instructions$setVisible(visible = TRUE)
      }
      
      self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
        title = "Instructions",
        content = paste(
          '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
          '<div style="text-align:justify;">',
          '<ul>',
          '<li>Cases with missing values on any selected variable are excluded from the analysis.</li>',
          '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
          '</ul></div></div>'
        )
      ))
    },
    
    .cleanData = function() {
      
      set1 <- self$options$set1
      set2 <- self$options$set2
      
      if (is.null(set1) || length(set1) == 0)
        return(NULL)
      
      if (is.null(set2) || length(set2) == 0)
        return(NULL)
      
      overlap <- intersect(set1, set2)
      if (length(overlap) > 0) {
        self$results$text$setContent(
          paste0(
            "The same variable cannot be assigned to both Set 1 and Set 2: ",
            paste(overlap, collapse = ", ")
          )
        )
        return(NULL)
      }
      
      vars <- unique(c(set1, set2))
      
      dat <- self$data[, vars, drop = FALSE]
      dat <- as.data.frame(dat)
      
      for (v in names(dat)) {
        if (!is.numeric(dat[[v]])) {
          self$results$text$setContent(
            paste0(
              "Canonical correlation analysis requires continuous numeric variables. ",
              "Please set the selected variables to Continuous measure type. ",
              "Problem variable: ", v
            )
          )
          return(NULL)
        }
      }
      
      rowIndex <- seq_len(nrow(dat))
      completeRows <- stats::complete.cases(dat)
      
      dat <- dat[completeRows, , drop = FALSE]
      usedRows <- rowIndex[completeRows]
      
      if (nrow(dat) < 3) {
        self$results$text$setContent("Not enough complete cases for canonical correlation analysis.")
        return(NULL)
      }
      
      x <- dat[, set1, drop = FALSE]
      y <- dat[, set2, drop = FALSE]
      
      if (ncol(x) == 0 || ncol(y) == 0)
        return(NULL)
      
      if (any(vapply(x, stats::sd, numeric(1), na.rm = TRUE) == 0)) {
        self$results$text$setContent("Set 1 contains a variable with zero variance.")
        return(NULL)
      }
      
      if (any(vapply(y, stats::sd, numeric(1), na.rm = TRUE) == 0)) {
        self$results$text$setContent("Set 2 contains a variable with zero variance.")
        return(NULL)
      }
      
      list(
        data = dat,
        x = x,
        y = y,
        set1 = set1,
        set2 = set2,
        n = nrow(dat),
        p = ncol(x),
        q = ncol(y),
        usedRows = usedRows,
        nTotal = length(rowIndex)
      )
    },
    
    .computeRES = function() {
      
      clean <- private$.cleanData()
      
      if (is.null(clean))
        return(NULL)
      
      x <- scale(clean$x)
      y <- scale(clean$y)
      
      fit <- try(stats::cancor(x, y), silent = TRUE)
      
      if (inherits(fit, "try-error")) {
        self$results$text$setContent("Canonical correlation analysis failed.")
        return(NULL)
      }
      
      r <- fit$cor
      r <- r[!is.na(r)]
      
      m <- length(r)
      
      if (m == 0) {
        self$results$text$setContent("No canonical correlations could be computed.")
        return(NULL)
      }
      
      xScores <- as.matrix(x) %*% fit$xcoef[, seq_len(m), drop = FALSE]
      yScores <- as.matrix(y) %*% fit$ycoef[, seq_len(m), drop = FALSE]
      
      colnames(xScores) <- paste0("Function ", seq_len(m))
      colnames(yScores) <- paste0("Function ", seq_len(m))
      
      eigen <- r^2 / pmax(1 - r^2, .Machine$double.eps)
      variance <- eigen / sum(eigen)
      
      corTable <- data.frame(
        func = seq_len(m),
        canonicalCorrelation = r,
        eigenvalue = eigen,
        variance = variance,
        stringsAsFactors = FALSE
      )
      
      testTable <- private$.computeTests(
        r = r,
        n = clean$n,
        p = clean$p,
        q = clean$q
      )
      
      coefTable <- private$.computeCoefTable(
        xcoef = fit$xcoef[, seq_len(m), drop = FALSE],
        ycoef = fit$ycoef[, seq_len(m), drop = FALSE],
        set1 = clean$set1,
        set2 = clean$set2,
        m = m
      )
      
      loadTable <- private$.computeLoadTable(
        x = x,
        y = y,
        xScores = xScores,
        yScores = yScores,
        set1 = clean$set1,
        set2 = clean$set2,
        m = m
      )
      
      crossTable <- private$.computeCrossTable(
        x = x,
        y = y,
        xScores = xScores,
        yScores = yScores,
        r = r,
        set1 = clean$set1,
        set2 = clean$set2,
        m = m
      )
      
      redunTable <- private$.computeRedundancyTable(
        loadTable = loadTable,
        r = r,
        m = m
      )
      
      list(
        fit = fit,
        r = r,
        m = m,
        n = clean$n,
        p = clean$p,
        q = clean$q,
        cor = corTable,
        test = testTable,
        coef = coefTable,
        load = loadTable,
        cross = crossTable,
        redun = redunTable,
        xScores = xScores,
        yScores = yScores,
        usedRows = clean$usedRows,
        nTotal = clean$nTotal
      )
    },
    
    .computeTests = function(r, n, p, q) {
      
      m <- length(r)
      out <- list()
      
      for (i in seq_len(m)) {
        
        lambda <- prod(1 - r[i:m]^2)
        
        df <- (p - i + 1) * (q - i + 1)
        
        chisq <- -(
          n - 1 - ((p + q + 1) / 2)
        ) * log(lambda)
        
        if (is.nan(chisq) || is.infinite(chisq) || chisq < 0)
          chisq <- NA_real_
        
        pvalue <- stats::pchisq(chisq, df = df, lower.tail = FALSE)
        
        testFunc <- if (i == m) {
          paste0(i)
        } else {
          paste0(i, " to ", m)
        }
        
        out[[i]] <- data.frame(
          testFunc = testFunc,
          wilks = lambda,
          chisq = chisq,
          df = df,
          p = pvalue,
          stringsAsFactors = FALSE
        )
      }
      
      do.call(rbind, out)
    },
    
    .computeCoefTable = function(xcoef, ycoef, set1, set2, m) {
      
      out <- list()
      k <- 1
      
      for (j in seq_len(m)) {
        for (i in seq_along(set1)) {
          out[[k]] <- data.frame(
            set = "Set 1",
            variable = set1[i],
            func = j,
            coefficient = xcoef[i, j],
            stringsAsFactors = FALSE
          )
          k <- k + 1
        }
        
        for (i in seq_along(set2)) {
          out[[k]] <- data.frame(
            set = "Set 2",
            variable = set2[i],
            func = j,
            coefficient = ycoef[i, j],
            stringsAsFactors = FALSE
          )
          k <- k + 1
        }
      }
      
      do.call(rbind, out)
    },
    
    .computeLoadTable = function(x, y, xScores, yScores, set1, set2, m) {
      
      out <- list()
      k <- 1
      
      for (j in seq_len(m)) {
        for (i in seq_along(set1)) {
          out[[k]] <- data.frame(
            set = "Set 1",
            variable = set1[i],
            func = j,
            loading = stats::cor(x[, i], xScores[, j]),
            stringsAsFactors = FALSE
          )
          k <- k + 1
        }
        
        for (i in seq_along(set2)) {
          out[[k]] <- data.frame(
            set = "Set 2",
            variable = set2[i],
            func = j,
            loading = stats::cor(y[, i], yScores[, j]),
            stringsAsFactors = FALSE
          )
          k <- k + 1
        }
      }
      
      do.call(rbind, out)
    },
    
    .computeCrossTable = function(x, y, xScores, yScores, r, set1, set2, m) {
      
      out <- list()
      k <- 1
      
      for (j in seq_len(m)) {
        for (i in seq_along(set1)) {
          out[[k]] <- data.frame(
            set = "Set 1",
            variable = set1[i],
            func = j,
            crossLoading = stats::cor(x[, i], yScores[, j]),
            stringsAsFactors = FALSE
          )
          k <- k + 1
        }
        
        for (i in seq_along(set2)) {
          out[[k]] <- data.frame(
            set = "Set 2",
            variable = set2[i],
            func = j,
            crossLoading = stats::cor(y[, i], xScores[, j]),
            stringsAsFactors = FALSE
          )
          k <- k + 1
        }
      }
      
      do.call(rbind, out)
    },
    
    .computeRedundancyTable = function(loadTable, r, m) {
      
      out <- list()
      k <- 1
      
      for (j in seq_len(m)) {
        
        l1 <- loadTable$loading[
          loadTable$set == "Set 1" &
            loadTable$func == j
        ]
        
        l2 <- loadTable$loading[
          loadTable$set == "Set 2" &
            loadTable$func == j
        ]
        
        ve1 <- mean(l1^2, na.rm = TRUE)
        ve2 <- mean(l2^2, na.rm = TRUE)
        
        out[[k]] <- data.frame(
          set = "Set 1",
          func = j,
          varianceExtracted = ve1,
          redundancy = ve1 * r[j]^2,
          stringsAsFactors = FALSE
        )
        k <- k + 1
        
        out[[k]] <- data.frame(
          set = "Set 2",
          func = j,
          varianceExtracted = ve2,
          redundancy = ve2 * r[j]^2,
          stringsAsFactors = FALSE
        )
        k <- k + 1
      }
      
      do.call(rbind, out)
    },
    
    .populateCorTable = function(res) {
      
      table <- self$results$cor
      d <- res$cor
      
      for (i in seq_len(nrow(d))) {
        table$addRow(
          rowKey = paste0("cor_", i),
          values = list(
            func = d$func[i],
            canonicalCorrelation = d$canonicalCorrelation[i],
            eigenvalue = d$eigenvalue[i],
            variance = d$variance[i]
          )
        )
      }
    },
    
    .populateTestTable = function(res) {
      
      table <- self$results$test
      d <- res$test
      
      for (i in seq_len(nrow(d))) {
        table$addRow(
          rowKey = paste0("test_", i),
          values = list(
            testFunc = d$testFunc[i],
            wilks = d$wilks[i],
            chisq = d$chisq[i],
            df = d$df[i],
            p = d$p[i]
          )
        )
      }
    },
    
    .populateCoefTable = function(res) {
      
      table <- self$results$coef
      d <- res$coef
      
      for (i in seq_len(nrow(d))) {
        table$addRow(
          rowKey = paste0("coef_", i),
          values = list(
            set = d$set[i],
            variable = d$variable[i],
            func = d$func[i],
            coefficient = d$coefficient[i]
          )
        )
      }
    },
    
    .populateLoadTable = function(res) {
      
      table <- self$results$load
      d <- res$load
      
      for (i in seq_len(nrow(d))) {
        table$addRow(
          rowKey = paste0("load_", i),
          values = list(
            set = d$set[i],
            variable = d$variable[i],
            func = d$func[i],
            loading = d$loading[i]
          )
        )
      }
    },
    
    .populateCrossTable = function(res) {
      
      table <- self$results$cross
      d <- res$cross
      
      for (i in seq_len(nrow(d))) {
        table$addRow(
          rowKey = paste0("cross_", i),
          values = list(
            set = d$set[i],
            variable = d$variable[i],
            func = d$func[i],
            crossLoading = d$crossLoading[i]
          )
        )
      }
    },
    
    .populateRedunTable = function(res) {
      
      table <- self$results$redun
      d <- res$redun
      
      for (i in seq_len(nrow(d))) {
        table$addRow(
          rowKey = paste0("redun_", i),
          values = list(
            set = d$set[i],
            func = d$func[i],
            varianceExtracted = d$varianceExtracted[i],
            redundancy = d$redundancy[i]
          )
        )
      }
    },
    
    .saveScores = function(res) {
      
      if (is.null(res) ||
          is.null(res$xScores) ||
          is.null(res$yScores) ||
          is.null(res$usedRows) ||
          is.null(res$nTotal))
        return()
      
      m <- min(ncol(res$xScores), ncol(res$yScores))
      
      if (m < 1)
        return()
      
      keys <- seq_len(2 * m)
      titles <- character(2 * m)
      descriptions <- character(2 * m)
      measureTypes <- rep("continuous", 2 * m)
      
      k <- 1
      for (j in seq_len(m)) {
        
        titles[k] <- paste0("CC_Set1_", j)
        descriptions[k] <- paste0("Set 1 canonical score ", j)
        k <- k + 1
        
        titles[k] <- paste0("CC_Set2_", j)
        descriptions[k] <- paste0("Set 2 canonical score ", j)
        k <- k + 1
      }
      
      self$results$scores$set(
        keys = keys,
        titles = titles,
        descriptions = descriptions,
        measureTypes = measureTypes
      )
      
      self$results$scores$setRowNums(rownames(self$data))
      
      k <- 1
      for (j in seq_len(m)) {
        
        score1 <- rep(NA_real_, res$nTotal)
        score1[res$usedRows] <- as.numeric(res$xScores[, j])
        self$results$scores$setValues(index = k, score1)
        k <- k + 1
        
        score2 <- rep(NA_real_, res$nTotal)
        score2[res$usedRows] <- as.numeric(res$yScores[, j])
        self$results$scores$setValues(index = k, score2)
        k <- k + 1
      }
    },
    
    
    .plot = function(image, ...) {
      
      if (!self$options$plot)
        return(FALSE)
      
      res <- private$.allCache
      
      if (is.null(res))
        res <- private$.computeRES()
      
      if (is.null(res) || is.null(res$r))
        return(FALSE)
      
      r <- res$r
      
      graphics::barplot(
        r,
        names.arg = seq_along(r),
        ylim = c(0, 1),
        xlab = "Function",
        ylab = "Canonical correlation",
        main = ""
      )
      
      graphics::abline(h = 0, lty = 1)
      
      TRUE
    },
    
    .run = function() {
      
      self$results$text$setContent("")
      # self$results$instructions$setVisible(visible = FALSE)
      
      if (is.null(self$data) ||
          is.null(self$options$set1) || length(self$options$set1) == 0 ||
          is.null(self$options$set2) || length(self$options$set2) == 0) {
        
        self$results$instructions$setVisible(visible = TRUE)
        return()
      }
      
      private$.allCache <- NULL
      
      res <- private$.computeRES()
      
      if (is.null(res))
        return()
      
      private$.allCache <- res
      
      if (self$options$cor)
        private$.populateCorTable(res)
      
      if (self$options$test)
        private$.populateTestTable(res)
      
      if (self$options$coef)
        private$.populateCoefTable(res)
      
      if (self$options$load)
        private$.populateLoadTable(res)
      
      if (self$options$cross)
        private$.populateCrossTable(res)
      
      if (self$options$redun)
        private$.populateRedunTable(res)
      
      if (isTRUE(self$options$scores))
        private$.saveScores(res)
    }
  )
)