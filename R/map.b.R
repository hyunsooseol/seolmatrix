# This file is a generated template, your changes will not be overwritten

#' @importFrom magrittr %>%
#' @importFrom EFA.dimensions MAP

mapClass <- if (requireNamespace("jmvcore", quietly = TRUE))
  R6::R6Class(
    "mapClass",
    inherit = mapBase,
    private = list(
      
      .run = function() {
        
        # 1) 입력 검사 ---------------------------------------------------------
        if (is.null(self$options$vars) || length(self$options$vars) < 2)
          stop("Select two or more variables.")
        
        dat <- jmvcore::select(self$data, self$options$vars)
        dat <- jmvcore::naOmit(dat)
        dat <- jmvcore::toNumeric(dat)
        
        # 2) 상관 종류 ----------------------------------------------------------
        corType <- switch(
          self$options$type,
          pearson    = "pearson",
          kendall    = "kendall",
          spearman   = "spearman",
          gamma      = "gamma",
          polychoric = "polychoric",
          "pearson"
        )
        
        # 3) MAP 실행 -----------------------------------------------------------
        res <- EFA.dimensions::MAP(
          data       = dat,
          corkind = corType
        )
        
        # 4) 고유값/분산 테이블 추출 --------------------------------------------
        mat <- NULL
        if (!is.null(res$totvarexplNOROT)) mat <- res$totvarexplNOROT
        if (is.null(mat) && !is.null(res$totvarexpl)) mat <- res$totvarexpl
        if (is.null(mat))
          stop("Unable to find eigenvalue table in MAP result.")
        
        cn <- colnames(mat)
        idxEigen <- grep("eigen",      cn, ignore.case = TRUE)[1]
        idxProp  <- grep("proportion", cn, ignore.case = TRUE)[1]
        idxCum   <- grep("cumulative", cn, ignore.case = TRUE)[1]
        if (any(is.na(c(idxEigen, idxProp, idxCum))))
          stop("Unexpected column names in MAP eigen table.")
        
        # 5) jamovi 테이블 채우기 ----------------------------------------------
        tbl <- self$results$eigen  # r.yaml의 items$name 과 동일
        k <- nrow(mat)
        
        for (i in seq_len(k)) {
          tbl$addRow(
            rowKey = as.character(i),    # 같은 키면 업데이트되므로 중복 방지
            values = list(
              factor  = i,
              eigen   = unname(mat[i, idxEigen]),
              propVar = unname(mat[i, idxProp]),
              cumVar  = unname(mat[i, idxCum])
            )
          )
        }
      
 
        # ---- Fill "Velicer's Average Squared Correlations" table ---------------------
        # assumes `res` already computed by EFA.dimensions::MAP(...)
        
        av <- NULL
        if (!is.null(res$avgsqrs))
          av <- res$avgsqrs
        
        if (!is.null(av)) {
          av <- as.data.frame(av, stringsAsFactors = FALSE)
          
          # locate columns safely
          cn <- colnames(av)
          idxRoot  <- grep("^root$", cn, ignore.case = TRUE)[1]
          idxAvgSq <- grep("avg.*corr.*sq", cn, ignore.case = TRUE)[1]
          idxPow4  <- grep("avg.*corr.*power4|power4", cn, ignore.case = TRUE)[1]
          
          # if no explicit root column, create 0..(k-1)
          if (is.na(idxRoot)) {
            rootVals <- seq.int(0, nrow(av) - 1)
          } else {
            rootVals <- av[[idxRoot]]
          }
          
          tbl2 <- self$results$avgCorr  # <- r.yaml: items$name: avgCorr
          k2 <- nrow(av)
          
          for (i in seq_len(k2)) {
            tbl2$addRow(
              rowKey = paste0("r", i),
              values = list(
                root   = rootVals[i],
                avgSq  = unname(av[i, idxAvgSq]),
                avgPow4= unname(av[i, idxPow4])
              )
            )
          }
 
        
        }
        
        
        # ---- Print revised MAP test result -------------------------------------------
        if (!is.null(res$NfactorsMAP4)) {
          txt <- paste0(
            "The number of components according to the revised (2000) MAP Test is = ",
            res[["NfactorsMAP4"]]
          )
          self$results$text$setContent(txt)
        }
        
        
        
      }
    )
  )