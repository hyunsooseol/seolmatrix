# This file is a generated template, your changes will not be overwritten

#' @importFrom magrittr %>%
#' @importFrom EFA.dimensions MAP

mapClass <- if (requireNamespace("jmvcore", quietly = TRUE))
  R6::R6Class(
    "mapClass",
    inherit = mapBase,
    private = list(
      .htmlwidget = NULL,

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
            '<li>More than two dependent variables should be specified for Multilevel correlation.</li>',
            '<li>The rationale of Multilevel Correlation is described in the <a href="https://cran.r-project.org/web/packages/correlation/vignettes/multilevel.html" target = "_blank">page</a>.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))

        if (isTRUE(self$options$screePlot)) {
          width <- self$options$width
          height <- self$options$height
          
          self$results$screePlot$setSize(width, height)
        }
       
        if (isTRUE(self$options$MapCurvePlot)) {
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$MapCurvePlot$setSize(width, height)
        }        
         
      },
      
      #---------------------------------
      
      .run = function() {
        
        # 1) 입력 검사 ---------------------------------------------------------
        if (is.null(self$options$vars) || length(self$options$vars) < 2) return()
          
        
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
          
          # [ADDED] MAP 곡선 플롯에 사용할 state 저장 ---------------------------
          dfMap <- data.frame(
            root    = rootVals,
            avgSq   = av[[idxAvgSq]],
            avgPow4 = av[[idxPow4]]
          )
          minSq   <- which.min(dfMap$avgSq)
          minPow4 <- which.min(dfMap$avgPow4)
          
          self$results$MapCurvePlot$setState(list(
            df = dfMap,
            minSq = minSq,
            minPow4 = minPow4
          ))
        }
        
        # [ADDED] Scree plot용 state 저장 --------------------------------------
        if (!is.null(mat)) {
          dfScree <- data.frame(
            factor = seq_len(nrow(mat)),
            eigen  = as.numeric(mat[, idxEigen])
          )
          self$results$screePlot$setState(dfScree)
        }
        
        # ---- Print revised MAP test result -------------------------------------------
        if (!is.null(res$NfactorsMAP4)) {
          txt <- paste0(
            "The number of components according to the revised (2000) MAP Test is = ",
            res[["NfactorsMAP4"]]
          )
          self$results$text$setContent(txt)
        }
      },
      
      # [ADDED] MAP 곡선 플롯 함수 ----------------------------------------------
      .MapCurvePlot = function(image, ggtheme, theme, ...) {
        st <- image$state
        if (is.null(st) || is.null(st$df)) return(FALSE)
        
        df <- st$df
        library(ggplot2)
        library(tidyr)
        
        # wide → long 변환
        df_long <- tidyr::pivot_longer(df,
                                       cols = c("avgSq", "avgPow4"),
                                       names_to = "series", values_to = "value"
        )
        
        # 라벨 변경
        df_long$series <- factor(df_long$series,
                                 levels = c("avgSq", "avgPow4"),
                                 labels = c("Avg.Corr.Sq.",
                                            "Avg.Corr.power4"))
        
        p <- ggplot(df_long, aes(x = root, y = value,
                                 color = series, linetype = series)) +
          geom_line() +
          geom_point() +
          # 최소점 표시
          geom_point(data = data.frame(
            root = df$root[c(st$minSq, st$minPow4)],
            value = c(df$avgSq[st$minSq], df$avgPow4[st$minPow4]),
            series = c("Avg.Corr.Sq.",
                       "Avg.Corr.power4")
          ), aes(x = root, y = value, color = series), inherit.aes = FALSE,
          size = 3, shape = 16) +
          labs(x = "Root", y = "Avg.Corr.Sq.") +
          theme_bw() +
          ggtheme +
          theme(legend.title = element_blank(),
                legend.position = "right",
                plot.title = element_blank())
        
        print(p)
        TRUE
      },
      
      
      # [ADDED] Scree Plot 함수 ------------------------------------------------
      .screePlot = function(image, ggtheme, theme, ...) {
        df <- image$state
        if (is.null(df)) return(FALSE)
        
        library(ggplot2)
        p <- ggplot(df, aes(x = factor, y = eigen)) +
          geom_line() +
          geom_point() +
          labs(x = "Factor", y = "Eigenvalue", title = "") +
          ggtheme
        
        print(p)
        TRUE
      }
    )
  )
