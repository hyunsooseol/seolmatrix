# This file is a generated template, your changes will not be overwritten
#' @importFrom magrittr %>%

mapClass <- if (requireNamespace("jmvcore", quietly = TRUE))
  R6::R6Class(
    "mapClass",
    inherit = mapBase,
    private = list(
      .htmlwidget = NULL,
      
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
            '<li>The HULL method reports only the suggested number of factors, not detailed tables.</li>',  
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        

        if (isTRUE(self$options$screePlot))
          self$results$screePlot$setSize(self$options$width,  self$options$height)
        
        if (isTRUE(self$options$MapCurvePlot))
          self$results$MapCurvePlot$setSize(self$options$width1, self$options$height1)
      },
      
      #---------------------------------
      .run = function() {
        if (is.null(self$options$vars) || length(self$options$vars) < 3) return()
        
        dat <- jmvcore::select(self$data, self$options$vars)
        dat <- jmvcore::naOmit(dat)
        dat <- jmvcore::toNumeric(dat)
        
        corType <- switch(
          self$options$type,
          pearson    = "pearson",
          kendall    = "kendall",
          spearman   = "spearman",
          gamma      = "gamma",
          polychoric = "polychoric",
          "pearson"
        )
        
        if (!requireNamespace("EFA.dimensions", quietly = TRUE))
          stop("Package 'EFA.dimensions' is required. Please install it.")
        
        if (identical(corType, "polychoric") &&
            !requireNamespace("psych", quietly = TRUE)) {
          stop("Polychoric correlations require the 'psych' package. Please install it.")
        }
        
        # ----------------- MAP -----------------
        res <- EFA.dimensions::MAP(
          data    = dat,
          corkind = corType
        )
        
        # ----- Initial Eigenvalues table -----
        mat <- NULL
        if (!is.null(res$totvarexplNOROT)) mat <- res$totvarexplNOROT
        if (is.null(mat) && !is.null(res$totvarexpl)) mat <- res$totvarexpl
        if (is.null(mat))
          stop("Unable to find eigenvalue table in MAP result.")
        
        cn <- colnames(mat)
        idxEigen <- grep("eigen",      cn, ignore.case = TRUE)[1]
        idxProp  <- grep("proportion", cn, ignore.case = TRUE)[1]
        idxCum   <- grep("cumulative", cn, ignore.case = TRUE)[1]
        
        tblEig <- self$results$eigen
        k <- nrow(mat)
        for (i in seq_len(k)) {
          tblEig$addRow(
            rowKey = as.character(i),
            values = list(
              factor  = i,
              eigen   = unname(mat[i, idxEigen]),
              propVar = unname(mat[i, idxProp]),
              cumVar  = unname(mat[i, idxCum])
            )
          )
        }
        
        # ----- Velicer’s Average Squared Correlations table -----
        if (!is.null(res$avgsqrs)) {
          av <- as.data.frame(res$avgsqrs, stringsAsFactors = FALSE)
          
          idxRoot  <- grep("^root$", colnames(av), ignore.case = TRUE)[1]
          idxAvgSq <- grep("avg.*corr.*sq", colnames(av), ignore.case = TRUE)[1]
          idxPow4  <- grep("avg.*corr.*power4|power4", colnames(av), ignore.case = TRUE)[1]
          
          rootVals <- if (is.na(idxRoot)) seq.int(0, nrow(av) - 1) else av[[idxRoot]]
          
          tblAvg <- self$results$avgCorr
          for (i in seq_len(nrow(av))) {
            tblAvg$addRow(
              rowKey = paste0("r", i),
              values = list(
                root    = rootVals[i],
                avgSq   = unname(av[i, idxAvgSq]),
                avgPow4 = unname(av[i, idxPow4])
              )
            )
          }
          
          # MAP Curve plot state
          dfMap <- data.frame(
            root    = rootVals,
            avgSq   = av[[idxAvgSq]],
            avgPow4 = av[[idxPow4]]
          )
          self$results$MapCurvePlot$setState(list(
            df     = dfMap,
            minSq  = which.min(dfMap$avgSq),
            minPow4= which.min(dfMap$avgPow4)
          ))
        }
        
        # ----- Scree plot + Parallel Analysis  -----
        dfScree <- data.frame(
          factor = seq_len(nrow(mat)),
          eigen  = as.numeric(mat[, idxEigen])
        )
        
        pa_ncomp <- NA_integer_
        if (requireNamespace("psych", quietly = TRUE)) {
          pa_try <- try(psych::fa.parallel(dat, fa = "pc", n.iter = 100,
                                           show.legend = FALSE, plot = FALSE),
                        silent = TRUE)
          if (!inherits(pa_try, "try-error")) {
            if (!is.null(pa_try$pc.sim))
              dfScree$sim <- pa_try$pc.sim[seq_len(nrow(dfScree))]
            if (!is.null(pa_try$ncomp))
              pa_ncomp <- pa_try$ncomp
            else if ("sim" %in% names(dfScree))
              pa_ncomp <- sum(dfScree$eigen > dfScree$sim, na.rm = TRUE)
          }
        }
        self$results$screePlot$setState(dfScree)
        
        # ============================================================
        # ==========  EMPKC & HULL  ==================================
        # ============================================================
        .pickN <- function(obj) {
          nms <- names(obj)
          if (!is.null(nms)) {
            idx <- grep("n[fF]actors?|n[_]?comp|k$", nms, ignore.case = TRUE)
            if (length(idx) > 0) {
              val <- obj[[idx[1]]]
              val <- suppressWarnings(as.integer(as.numeric(val)))
              if (is.finite(val)) return(val)
            }
          }
          ux <- unlist(obj, use.names = FALSE)
          ux <- suppressWarnings(as.integer(as.numeric(ux)))
          ux <- ux[is.finite(ux)]
          if (length(ux) > 0) ux[1] else NA_integer_
        }
        
        emp_res <- NULL
        m_emp   <- NA_integer_
        m_hull  <- NA_integer_
        
        if (isTRUE(self$options$emp)) {
          emp_try <- try(EFA.dimensions::EMPKC(data = dat, corkind = corType), silent = TRUE)
          if (!inherits(emp_try, "try-error")) {
            emp_res <- emp_try
            if (!is.null(emp_res$NfactorsEMPKC)) {
              m_emp <- suppressWarnings(as.integer(emp_res$NfactorsEMPKC[1]))
            } else {
              m_emp <- .pickN(emp_res)
            }
            
            if (!is.null(self$results$empkcTable)) {
              tblEMPKC <- self$results$empkcTable
              
              eigs <- emp_res[["eigenvalues"]]; if (is.null(eigs)) eigs <- emp_res$eigenvalues
              refs <- emp_res[["refvalues"]];   if (is.null(refs)) refs <- emp_res$refvalues
              
              if (!is.null(eigs) && !is.null(refs)) {
                eigs <- as.numeric(eigs); refs <- as.numeric(refs)
                n <- min(length(eigs), length(refs))
                for (i in seq_len(n)) {
                  tblEMPKC$addRow(
                    rowKey = as.character(i),      
                    values = list(
                      component = i,
                      eigen     = eigs[i],
                      ref       = refs[i]
                    )
                  )
                }
              }
              
              if (is.finite(m_emp) && is.function(tblEMPKC$setNote)) {
                tblEMPKC$setNote("Note",
                                 sprintf("Optimal number of factors : %d", m_emp))
              }
            }
          }
        }
        
        if (isTRUE(self$options$hull)) {
          hull_try <- try(
            EFA.dimensions::DIMTESTS(
              data    = dat,
              corkind = corType,
              Ncases  = nrow(dat),
              tests   = "HULL",
              display = 0
            ),
            silent = TRUE
          )
          if (!inherits(hull_try, "try-error"))
            m_hull <- .pickN(hull_try)
        }
        
        # ----------------- MAP summary (text) -----------------
        txt <- character()
        if (!is.null(res$NfactorsMAP))
          txt <- c(txt, sprintf("Original (1976) MAP suggests = %d", res$NfactorsMAP))
        if (!is.null(res$NfactorsMAP4))
          txt <- c(txt, sprintf("Revised (2000) MAP suggests = %d", res$NfactorsMAP4))
        
        if (exists("dfMap")) {
          txt <- c(txt,
                   sprintf("The smallest average squared correlation is %.5f",
                           min(dfMap$avgSq,   na.rm = TRUE)),
                   sprintf("The smallest average 4rth power correlation is %.5f",
                           min(dfMap$avgPow4, na.rm = TRUE)))
        }
        
        if (is.finite(pa_ncomp))
          txt <- c(txt, sprintf("Parallel Analysis suggests = %d", pa_ncomp))
        
        if (length(txt) > 0)
          self$results$text$setContent(paste(txt, collapse = "\n"))
        
        # ----------------- EMPKC/HULL summary (text1) -----------------
        txt1 <- character()
        if (isTRUE(self$options$emp)) {
          if (is.finite(m_emp))
            txt1 <- c(txt1, sprintf("Empirical Kaiser Criterion suggests = %d", m_emp))
          else
            txt1 <- c(txt1, "Empirical Kaiser Criterion did not return a valid result.")
        }
        if (isTRUE(self$options$hull)) {
          if (is.finite(m_hull))
            txt1 <- c(txt1, sprintf("HULL method suggests = %d", m_hull))
          else
            txt1 <- c(txt1, "HULL method did not return a valid result.")
        }
        if (length(txt1) == 0)
          txt1 <- "No method was selected."
        
        self$results$text1$setContent(paste(txt1, collapse = "\n"))
      },
      
      # ----------------- MAP Curve Plot -----------------
      .MapCurvePlot = function(image, ggtheme, theme, ...) {
        st <- image$state
        if (is.null(st) || is.null(st$df)) return(FALSE)
        
        df <- st$df
        # library(ggplot2)
        # library(tidyr)

        has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)
        
        df_long <- tidyr::pivot_longer(
          df, cols = c("avgSq", "avgPow4"),
          names_to = "series", values_to = "value"
        )
        df_long$series <- factor(
          df_long$series,
          levels = c("avgSq", "avgPow4"),
          labels = c("Avg.Corr.Sq.", "Avg.Corr.power4")
        )
        
       
        min_df <- do.call(rbind, lapply(split(df_long, df_long$series), function(d) {
          d[which.min(d$value), , drop = FALSE]
        }))
        
       
        p <- ggplot(df_long, aes(x = root, y = value,
                                 color = series, linetype = series)) +
          geom_line(linewidth = 0.9) +
          geom_point(aes(shape = series), size = 3.2, stroke = 1.2, fill = "white") +
          
          geom_point(data = min_df,
                     aes(x = root, y = value, color = series, shape = series),
                     size = 5, stroke = 1.6, fill = "white") +
          
          geom_vline(data = min_df, aes(xintercept = root, color = series),
                     linetype = "dotted", linewidth = 0.7, alpha = 0.5, show.legend = FALSE) +
       
          { if (has_ggrepel)
            ggrepel::geom_label_repel(
              data = min_df,
              aes(label = paste0("", root)),
              size = 3.2, label.size = 0.25, label.r = unit(2, "pt"),
              seed = 123, show.legend = FALSE
            ) else
              geom_label(
                data = min_df, aes(label = paste0("", root)),
                size = 3.0, label.size = 0.25, label.r = unit(2, "pt"),
                nudge_y = 0.05, show.legend = FALSE
              )
          } +
         
          scale_shape_manual(values = c("Avg.Corr.Sq." = 21, "Avg.Corr.power4" = 24)) +
          scale_color_manual(values = c("Avg.Corr.Sq." = "#D55E00",  # 오렌지
                                        "Avg.Corr.power4" = "#0072B2")) + # 블루
          labs(x = "Root", y = "Avg.Corr.", title = NULL) +
          theme_bw() + ggtheme +
          theme(
            legend.title = element_blank(),
            legend.position = "right"
          ) +
          guides(
            color = guide_legend(override.aes = list(size = 4, stroke = 1.4, fill = "white")),
            shape = guide_legend(override.aes = list(size = 4, stroke = 1.4, fill = "white"))
          )
        
        print(p)
        TRUE
      },
      
      # ----------------- Scree Plot -----------------
      .screePlot = function(image, ggtheme, theme, ...) {
        df <- image$state
        if (is.null(df)) return(FALSE)
        
        library(ggplot2)
        
        if ("sim" %in% names(df)) {
          df_long <- data.frame(
            factor = rep(df$factor, 2),
            series = factor(rep(c("Data", "Simulations"), each = nrow(df)),
                            levels = c("Data", "Simulations")),
            value  = c(df$eigen, df$sim)
          )
          p <- ggplot(df_long, aes(x = factor, y = value,
                                   color = series, linetype = series)) +
            geom_line() +
            geom_point() +
            labs(x = "Component", y = "Eigenvalue") +
            theme_bw() + ggtheme +
            theme(legend.title = element_blank(),
                  plot.title = element_blank())
        } else {
          p <- ggplot(df, aes(x = factor, y = eigen)) +
            geom_line() +
            geom_point() +
            labs(x = "Component", y = "Eigenvalue") +
            theme_bw() + ggtheme +
            theme(legend.title = element_blank(),
                  plot.title = element_blank())
        }
        
        print(p); TRUE
      }
    )
  )
