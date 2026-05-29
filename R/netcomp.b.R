
netcompClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "netcompClass",
  inherit = netcompBase,
  private = list(
    
    .htmlwidget = NULL,
    .plotCache = NULL,
    .nctCache = NULL,
    .hasRun = FALSE,
    
    .init = function() {
      
      private$.htmlwidget <- HTMLWidget$new()
      private$.plotCache <- NULL
      
      self$results$instructions$setVisible(visible = TRUE)
      
      self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
        title = "Instructions",
        content = paste(
          '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
          '<div style="text-align:justify;">',
          '<p><strong>Group Network Comparison</strong> compares two networks across two groups using the Network Comparison Test.</p>',
          '<ul>',
          '<li>Select two or more numeric variables for the network.</li>',
          '<li>Choose <strong>Continuous</strong> for continuous or Likert-type variables, or <strong>Binary</strong> for binary-coded variables.</li>',
          '<li>Select one grouping variable with exactly 2 levels.</li>',
          '<li>Click <strong>Run</strong> to perform the analysis.</li>',
          '<li>Permutation-based tests may take longer with more variables or permutations.</li>',
          '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
          '</ul>',
          '</div></div>'
        )
      ))
      
      self$results$summary$setVisible(FALSE)
      self$results$tests$setVisible(FALSE)
      self$results$edges$setVisible(FALSE)
      self$results$note$setVisible(FALSE)
      self$results$progressBarHTML$setVisible(FALSE)
      
      if (!is.null(self$results$groupPlots))
        self$results$groupPlots$setVisible(FALSE)
      
     
    },
    
    #---------------------------------------------------------------
    .run = function() {
      
      private$.plotCache <- NULL
      
      self$results$summary$setVisible(FALSE)
      self$results$tests$setVisible(FALSE)
      self$results$edges$setVisible(FALSE)
      self$results$note$setVisible(FALSE)
      self$results$progressBarHTML$setVisible(FALSE)
      
      if (!is.null(self$results$groupPlots))
        self$results$groupPlots$setVisible(FALSE)
      
      
      runClicked <- !is.null(self$options$run) && self$options$run > 0
      
      if (runClicked)
        private$.hasRun <- TRUE
      
      if (!private$.hasRun)
        return()
      
            
      vars <- self$options$vars
      group <- self$options$group
      
      if (is.null(vars) || length(vars) < 2) {
        self$results$note$setVisible(TRUE)
        self$results$note$setContent(
          '<div style="color:#b91c1c;"><strong>Please select at least two numeric variables.</strong></div>'
        )
        return()
      }
      
      if (is.null(group) || group == "") {
        self$results$note$setVisible(TRUE)
        self$results$note$setContent(
          '<div style="color:#b91c1c;"><strong>Please select one grouping variable.</strong></div>'
        )
        return()
      }
      
      if (!requireNamespace("NetworkComparisonTest", quietly = TRUE)) {
        self$results$note$setVisible(TRUE)
        self$results$note$setContent(
          '<div style="color:#b91c1c;"><strong>The NetworkComparisonTest package is not available.</strong></div>'
        )
        return()
      }
      
      if (!requireNamespace("qgraph", quietly = TRUE)) {
        self$results$note$setVisible(TRUE)
        self$results$note$setContent(
          '<div style="color:#b91c1c;"><strong>The qgraph package is required to draw network plots.</strong></div>'
        )
        return()
      }
      
      dat <- self$data
      
      keep <- c(vars, group)
      dat <- dat[, keep, drop = FALSE]
      
      for (v in vars)
        dat[[v]] <- jmvcore::toNumeric(dat[[v]])

      dat[[group]] <- as.factor(dat[[group]])
      dat <- stats::na.omit(dat)

      # -----------------------------
      # Data type
      # -----------------------------
      
      dataType <- self$options$dataType
      
      if (is.null(dataType) || dataType == "")
        dataType <- "continuous"
      
      binaryData <- dataType == "binary"
      
      isBinaryVar <- function(x) {
        ux <- sort(unique(stats::na.omit(x)))
        length(ux) == 2
      }
      
      binaryVars <- vars[vapply(dat[, vars, drop = FALSE], isBinaryVar, logical(1))]
      
      if (binaryData) {
        
        if (length(binaryVars) != length(vars)) {
          
          nonBinaryVars <- setdiff(vars, binaryVars)
          
          self$results$note$setVisible(TRUE)
          self$results$note$setContent(paste0(
            '<div style="color:#b91c1c;">',
            '<strong>Binary data type was selected, but some variables are not binary.</strong><br>',
            'When Data type is set to Binary, all selected network variables must have exactly two observed values.<br>',
            'Non-binary variable(s): ',
            paste(nonBinaryVars, collapse = ", "),
            '</div>'
          ))
          return()
        }
        
        for (v in vars) {
          ux <- sort(unique(stats::na.omit(dat[[v]])))
          dat[[v]] <- ifelse(dat[[v]] == ux[1], 0, 1)
        }
        
      }
            
      if (nrow(dat) < 6) {
        self$results$note$setVisible(TRUE)
        self$results$note$setContent(
          '<div style="color:#b91c1c;"><strong>Not enough complete cases are available after removing missing values.</strong></div>'
        )
        return()
      }
      
      groups <- levels(droplevels(dat[[group]]))
      
      if (length(groups) != 2) {
        self$results$note$setVisible(TRUE)
        self$results$note$setContent(
          '<div style="color:#b91c1c;"><strong>The grouping variable must have exactly two groups.</strong></div>'
        )
        return()
      }
      
      data1 <- dat[dat[[group]] == groups[1], vars, drop = FALSE]
      data2 <- dat[dat[[group]] == groups[2], vars, drop = FALSE]
      
      # Check variables with no variation within each group
      noVar1 <- vars[vapply(data1, function(x) length(unique(stats::na.omit(x))) < 2, logical(1))]
      noVar2 <- vars[vapply(data2, function(x) length(unique(stats::na.omit(x))) < 2, logical(1))]
      
      if (length(noVar1) > 0 || length(noVar2) > 0) {
        
        msg <- c()
        
        if (length(noVar1) > 0)
          msg <- c(msg, paste0("Group 1: ", paste(noVar1, collapse = ", ")))
        
        if (length(noVar2) > 0)
          msg <- c(msg, paste0("Group 2: ", paste(noVar2, collapse = ", ")))
        
        self$results$note$setVisible(TRUE)
        self$results$note$setContent(paste0(
          '<div style="color:#b91c1c;">',
          '<strong>Some variables have no variation within a group.</strong><br>',
          'Network comparison cannot be performed when a selected variable has only one observed value within a group.<br>',
          paste(msg, collapse = "<br>"),
          '</div>'
        ))
        return()
      }
      
      
      
      if (nrow(data1) < 3 || nrow(data2) < 3) {
        self$results$note$setVisible(TRUE)
        self$results$note$setContent(
          '<div style="color:#b91c1c;"><strong>Each group must contain at least three complete cases.</strong></div>'
        )
        return()
      }
      
      nPerm <- self$options$nPerm
      
      if (is.null(nPerm) || is.na(nPerm))
        nPerm <- 200
      
      nPerm <- as.integer(nPerm)
      
      permLimited <- FALSE
      permOriginal <- nPerm
      
      if (nPerm < 100) {
        nPerm <- 100
        permLimited <- TRUE
      }
      
      if (nPerm > 500) {
        nPerm <- 500
        permLimited <- TRUE
      }
      
      
      testEdges <- isTRUE(self$options$testEdges)
      #paired <- isTRUE(self$options$paired)
      
      # -----------------------------
      # NCT cache key
      # -----------------------------
      
      dataKey <- paste(
        nrow(dat),
        ncol(dat),
        paste(vapply(dat[, vars, drop = FALSE], function(x) sum(x, na.rm = TRUE), numeric(1)), collapse = "|"),
        paste(vapply(dat[, vars, drop = FALSE], function(x) stats::var(x, na.rm = TRUE), numeric(1)), collapse = "|"),
        paste(table(dat[[group]]), collapse = "|"),
        sep = "||"
      )
      
      nctKey <- paste(
        dataKey,
        paste(vars, collapse = "|"),
        group,
        paste(groups, collapse = "|"),
        nPerm,
        dataType,
        binaryData,
        testEdges,
        sep = "||"
      )
      
      useCache <- !is.null(private$.nctCache) &&
        identical(private$.nctCache$key, nctKey)
      
      if (!useCache && !runClicked) {
        self$results$note$setVisible(TRUE)
        self$results$note$setContent(
          '<div style="color:#b91c1c;"><strong>Analysis settings have changed.</strong><br>Please click Run to update the analysis.</div>'
        )
        return()
      }
      
      if (useCache) {
        
        nct <- private$.nctCache$nct
        
      } else {
        
        self$results$progressBarHTML$setVisible(TRUE)
        self$results$progressBarHTML$setContent(
          progressSpinnerH(
            message = "Running permutation-based network comparison. Please wait..."
          )
        )
        private$.checkpoint()
        
        nct <- tryCatch({
          
          NetworkComparisonTest::NCT(
            data1 = data1,
            data2 = data2,
            it = nPerm,
            binary.data = binaryData,
            test.edges = testEdges,
            edges = "all",
            test.centrality = FALSE,
            progressbar = FALSE
          )
          
        }, error = function(e) {
          e
        })
        
        if (!inherits(nct, "error")) {
          private$.nctCache <- list(
            key = nctKey,
            nct = nct
          )
        }
      }
      
      #---------------------
      
      if (inherits(nct, "error")) {
        self$results$progressBarHTML$setVisible(FALSE)
        self$results$note$setVisible(TRUE)
        self$results$note$setContent(paste0(
          '<div style="color:#b91c1c;"><strong>Network Comparison Test failed.</strong><br>',
          htmltools::htmlEscape(nct$message),
          '</div>'
        ))
        return()
      }
      
      self$results$progressBarHTML$setVisible(TRUE)
      self$results$progressBarHTML$setContent(
        progressSpinnerH(
          message = "Preparing tables and network plots..."
        )
      )
      private$.checkpoint()
      
      
      
      # -----------------------------
      # Prepare plot cache
      # -----------------------------
      
      private$.preparePlotCache(
        data1 = data1,
        data2 = data2,
        groups = groups,
        vars = vars
      )
      
      if (isTRUE(self$options$showPlots)) {
        
        if (!is.null(self$results$groupPlots))
          self$results$groupPlots$setVisible(TRUE)
        
      
      }
      
      # -----------------------------
      # Summary table
      # -----------------------------
      
      self$results$summary$setVisible(TRUE)
      self$results$summary$setRow(rowNo = 1, values = list(
        nVars = length(vars),
        groupVar = group,
        group1 = as.character(groups[1]),
        n1 = nrow(data1),
        group2 = as.character(groups[2]),
        n2 = nrow(data2),
        permutations = nPerm
      ))
      
      # -----------------------------
      # Invariance tests table
      # -----------------------------
      
      self$results$tests$setVisible(TRUE)
      
      if (isTRUE(self$options$testStructure)) {
        
        nwinv.real <- private$.getNCTValue(nct, c("nwinv.real", "nwinv.statistic", "M"))
        nwinv.pval <- private$.getNCTValue(nct, c("nwinv.pval", "nwinv.p", "pval.nwinv"))
        
        self$results$tests$addRow(rowKey = "structure", values = list(
          test = "Network structure invariance",
          statistic = nwinv.real,
          p = nwinv.pval
        ))
      }
      
      if (isTRUE(self$options$testGlobalStrength)) {
        
        glstr.real <- private$.getNCTValue(nct, c("glstrinv.real", "glstrinv.statistic", "S"))
        glstr.pval <- private$.getNCTValue(nct, c("glstrinv.pval", "glstrinv.p", "pval.glstrinv"))
        
        self$results$tests$addRow(rowKey = "global_strength", values = list(
          test = "Global strength invariance",
          statistic = glstr.real,
          p = glstr.pval
        ))
      }
      
      # -----------------------------
      # Edge differences table
      # -----------------------------
      
      self$results$edges$setVisible(testEdges)
      
      if (testEdges) {
        
        edgeTable <- private$.extractEdgeTable(nct, vars)
        
        if (!is.null(edgeTable) && nrow(edgeTable) > 0) {
          
          for (i in seq_len(nrow(edgeTable))) {
            
            key <- paste0(
              gsub("[^A-Za-z0-9_]", "_", edgeTable$node1[i]),
              "_",
              gsub("[^A-Za-z0-9_]", "_", edgeTable$node2[i])
            )
            
            self$results$edges$addRow(rowKey = key, values = list(
              node1 = edgeTable$node1[i],
              node2 = edgeTable$node2[i],
              diff = edgeTable$diff[i],
              p = edgeTable$p[i]
            ))
          }
          
        } else {
          
          self$results$note$setVisible(TRUE)
          self$results$note$setContent(
            '<div><strong>Note.</strong> Edge differences were requested, but no edge-level table could be extracted from the NCT result object.</div>'
          )
        }
      }
      
      # -----------------------------
      # Note
      # -----------------------------
      
      # noteText <- paste0(
      #   '<div style="border: 1px solid #e5e7eb; border-radius: 10px; padding: 12px; background-color: #f9fafb;">',
      #   '<strong>Note.</strong> The Network Comparison Test uses permutation-based inference. ',
      #   'Results may vary slightly across runs unless a fixed random seed is used. ',
      #   'For stable results, consider using 1000 or more permutations when computation time allows.',
      #   '</div>'
      # )
      # 
      # self$results$note$setVisible(TRUE)
      # self$results$note$setContent(noteText)
      
      if (permLimited) {
        self$results$note$setVisible(TRUE)
        self$results$note$setContent(paste0(
          '<div style="border: 1px solid #e5e7eb; border-radius: 10px; padding: 12px; background-color: #f9fafb;">',
          '<strong>Note.</strong> The number of permutations is limited to the range 100–500. ',
          'The requested value was ', permOriginal, ', so the analysis was run with ', nPerm, ' permutations.',
          '</div>'
        ))
      }
    
      self$results$progressBarHTML$setVisible(FALSE)
      
      
      },
    
    #---------------------------------------------------------------
    .preparePlotCache = function(data1, data2, groups, vars) {
      
      minimum <- self$options$minimum
      
      if (is.null(minimum) || is.na(minimum))
        minimum <- 0
      
      minimum <- as.numeric(minimum)
      
      if (minimum < 0)
        minimum <- 0
      
      if (minimum > 1)
        minimum <- 1
      
      layoutOpt <- self$options$layout
      
      if (is.null(layoutOpt) || layoutOpt == "")
        layoutOpt <- "spring"
      
      cor1 <- stats::cor(data1, use = "pairwise.complete.obs")
      cor2 <- stats::cor(data2, use = "pairwise.complete.obs")
      
      cor1[is.na(cor1)] <- 0
      cor2[is.na(cor2)] <- 0
      
      diag(cor1) <- 0
      diag(cor2) <- 0
      
      cor1[abs(cor1) < minimum] <- 0
      cor2[abs(cor2) < minimum] <- 0
      
      maximum <- max(abs(cor1), abs(cor2), na.rm = TRUE)
      
      if (!is.finite(maximum) || maximum <= 0)
        maximum <- 1
      
      sameLayout <- isTRUE(self$options$sameLayout)
      
      layout <- layoutOpt
      
      if (sameLayout && layoutOpt == "spring") {
        
        avg <- (abs(cor1) + abs(cor2)) / 2
        
        layout <- tryCatch({
          
          qg <- qgraph::qgraph(
            avg,
            layout = "spring",
            DoNotPlot = TRUE,
            labels = vars,
            minimum = 0
          )
          
          qg$layout
          
        }, error = function(e) {
          "spring"
        })
      }
      
      if (layoutOpt == "circle")
        layout <- "circle"
      
      private$.plotCache <- list(
        cor1 = cor1,
        cor2 = cor2,
        group1 = as.character(groups[1]),
        group2 = as.character(groups[2]),
        vars = vars,
        layout = layout,
        maximum = maximum,
        minimum = minimum
      )
    },
    
    #---------------------------------------------------------------
    .plotGroup1 = function(image, ...) {
      
      if (is.null(private$.plotCache))
        return()
      
      private$.drawNetworkPlot(
        mat = private$.plotCache$cor1,
        title = paste0("Group 1 Network: ", private$.plotCache$group1)
      )
    },
    
    .plotGroup2 = function(image, ...) {
      
      if (is.null(private$.plotCache))
        return()
      
      private$.drawNetworkPlot(
        mat = private$.plotCache$cor2,
        title = paste0("Group 2 Network: ", private$.plotCache$group2)
      )
    },
    #---------------------------------------------------------------
    .drawNetworkPlot = function(mat, title) {
      
      cache <- private$.plotCache
      
      oldPar <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(oldPar), add = TRUE)
      
      graphics::par(mar = c(1, 1, 3, 1))
      
      qgraph::qgraph(
        mat,
        layout = cache$layout,
        labels = cache$vars,
        minimum = 0,
        maximum = cache$maximum,
        cut = 0,
        vsize = 7,
        label.cex = 1.1,
        legend = FALSE,
        title = title
      )
    },
    
    #---------------------------------------------------------------
    .getNCTValue = function(x, names) {
      
      for (nm in names) {
        if (!is.null(x[[nm]]))
          return(x[[nm]])
      }
      
      return(NA_real_)
    },
    
    #---------------------------------------------------------------
    .extractEdgeTable = function(nct, vars) {
      
      edgeP <- nct$einv.pvals
      
      if (is.null(edgeP))
        return(NULL)
      
      if (!is.data.frame(edgeP))
        return(NULL)
      
      nms <- names(edgeP)
      
      node1Name <- intersect(nms, c("Var1", "var1", "node1", "Node1"))[1]
      node2Name <- intersect(nms, c("Var2", "var2", "node2", "Node2"))[1]
      pName <- intersect(nms, c("p-value", "p.value", "p", "pval", "p_value"))[1]
      diffName <- intersect(nms, c("Test statistic E", "E", "diff", "difference", "Difference"))[1]
      
      if (is.na(node1Name) || is.na(node2Name) || is.na(pName))
        return(NULL)
      
      if (is.na(diffName)) {
        diff <- rep(NA_real_, nrow(edgeP))
      } else {
        diff <- suppressWarnings(as.numeric(edgeP[[diffName]]))
      }
      
      out <- data.frame(
        node1 = as.character(edgeP[[node1Name]]),
        node2 = as.character(edgeP[[node2Name]]),
        diff = diff,
        p = suppressWarnings(as.numeric(edgeP[[pName]])),
        stringsAsFactors = FALSE
      )
      
      out <- out[!is.na(out$p), , drop = FALSE]
      
      if (nrow(out) == 0)
        return(NULL)
      
      return(out)
    }
  )
)

progressSpinnerH <- function(message = '') {
  
  html <- paste0(
    '<div style="text-align:center;padding:24px;">',
    '<style>',
    '@keyframes snowsoftDotPulse {',
    '0%, 80%, 100% { transform: scale(0.7); opacity: 0.35; }',
    '40% { transform: scale(1.15); opacity: 1; }',
    '}',
    '.snowsoft-dot {',
    'display:inline-block;',
    'width:10px;',
    'height:10px;',
    'margin:0 4px;',
    'background-color:#3498db;',
    'border-radius:50%;',
    'animation:snowsoftDotPulse 1.2s infinite ease-in-out;',
    '}',
    '.snowsoft-dot:nth-child(2) { animation-delay: 0.15s; }',
    '.snowsoft-dot:nth-child(3) { animation-delay: 0.30s; }',
    '</style>',
    '<div style="margin-bottom:10px;">',
    '<span class="snowsoft-dot"></span>',
    '<span class="snowsoft-dot"></span>',
    '<span class="snowsoft-dot"></span>',
    '</div>',
    '<div style="font-size:12px;color:#666;">',
    message,
    '</div>',
    '</div>'
  )
  
  return(html)
}