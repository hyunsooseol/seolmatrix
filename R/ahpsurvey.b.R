
#' @importFrom magrittr %>%
ahpsurveyClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "ahpsurveyClass",
    inherit = ahpsurveyBase,
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
            '<li>The R package <b>ahpsurvey</b> is described in the <a href="https://cran.r-project.org/web/packages/ahpsurvey/vignettes/my-vignette.html" target = "_blank">page</a>.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        if (self$options$sumcr)
          self$results$sumcr$setNote("Note", "Mean CR<0.1 is acceptable for consistency ratio.")
       
      },
      
      .run = function() {
        ready <- TRUE
        if (is.null(self$options$vars) || length(self$options$vars) < 2)
          ready <- FALSE
        if (ready) {
          # 1. 결측 없는 행만 뽑은 data_clean
          data_clean <- private$.cleanData()
          if (nrow(data_clean) == 0) stop("No valid data rows after removing missing or infinite values.")
          # 2. 원본 데이터에서 결측 없는 행 index (matching용)
          valid_idx <- which(apply(self$data[, self$options$vars, drop=FALSE], 1, function(x) all(is.finite(as.numeric(x)))))
          # 3. 분석 수행
          results <- private$.compute(data_clean)
          
          private$.populateApTable(results)
          private$.populateAjTable(results)
          private$.populateSumcrTable(results)
          
          # 4. CR 결과 매칭(보정) - 핵심!
          if (self$options$cr && self$results$cr$isNotFilled()) {
            cr_all <- rep(NA, nrow(self$data))     # 원본 길이만큼 NA로 초기화
            cr <- results$cr                       # 결측 없는 행에 대한 결과만 있음
            cr_all[valid_idx] <- cr                # 위치 맞춰서 결과 대입
            self$results$cr$setRowNums(rownames(self$data))
            self$results$cr$setValues(cr_all)
          }
          
          # plot1 등 다른 결과는 기존과 동일
          if (isTRUE(self$options$plot1)) {
            me <- self$options$method2
            me2 <- self$options$method3
            atts <- strsplit(self$options$atts, ',')[[1]]
            matahp <- ahpsurvey::ahp.mat(df = data_clean, atts = atts, negconvert = T)
            m <- ahpsurvey::ahp.indpref(matahp, atts, method = me)
            m2 <- ahpsurvey::ahp.indpref(matahp, atts, method = me2)
            error <- data.frame(id = 1:length(matahp),
                                maxdiff = apply(abs(m - m2), 1, max))
            image <- self$results$plot1
            image$setState(error)
          }
        }
      },
      
      .compute = function(data) {
        vars <- self$options$vars
        method <- self$options$method
        method1 <- self$options$method1
        atts <- strsplit(self$options$atts, ',')[[1]]
        
        matahp <- ahpsurvey::ahp.mat(df = data, atts = atts, negconvert = T)
        geo <- ahpsurvey::ahp.aggpref(matahp, atts, method = method)
        df <- data.frame(Value = geo)
        aj <- ahpsurvey::ahp.aggjudge(matahp, atts, aggmethod = method1)
        item <- as.matrix(aj)
        cr <- ahpsurvey::ahp.cr(matahp, atts)
        cr1 <- data %>%
          ahpsurvey::ahp.mat(atts, negconvert = T) %>%
          ahpsurvey::ahp.cr(atts)
        tab <- as.vector(table(cr1 <= 0.1))
        
        results <- list(
          'df' = df,
          'item' = item,
          'cr' = cr,
          'cr1' = cr1,
          'tab' = tab
        )
        return(results)
      },
      
      .populateSumcrTable = function(results) {
        table <- self$results$sumcr
        cr <- results$cr
        cr1 <- results$cr1
        tab <- results$tab
        mcr <- round(mean(cr), 3)
        ve <- c(tab, mcr)
        row <- list()
        row[['NO']] <- ve[[1]]
        row[['YES']] <- ve[[2]]
        row[['Mean CR']] <- ve[[3]]
        table$setRow(rowNo = 1, values = row)
      },
      
      .populateApTable = function(results) {
        table <- self$results$ap
        df <- results$df
        names <- dimnames(df)[[1]]
        for (name in names) {
          row <- list()
          row[['value']] <- df[name, 1]
          table$addRow(rowKey = name, values = row)
        }
      },
      
      .populateAjTable = function(results) {
        table <- self$results$aj
        item <- results$item
        names <- dimnames(item)[[1]]
        dims <- dimnames(item)[[2]]
        for (dim in dims) {
          table$addColumn(name = paste0(dim), type = 'number')
        }
        for (name in names) {
          row <- list()
          for (j in seq_along(dims)) {
            row[[dims[j]]] <- item[name, j]
          }
          table$addRow(rowKey = name, values = row)
        }
      },
      
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        error <- image$state
        library(ggplot2)
        plot1 <- ggplot2::ggplot(data = error, ggplot2::aes(x = id, y = maxdiff)) +
          geom_point() +
          geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 0, color = "gray50") +
          scale_x_continuous("Respondent ID") +
          scale_y_continuous("Maximum difference")
        plot1 <- plot1 + ggtheme
        print(plot1)
        TRUE
      },
      
      .cleanData = function() {
        items <- self$options$vars
        data <- list()
        for (item in items)
          data[[item]] <- jmvcore::toNumeric(self$data[[item]])
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        # 결측 또는 Inf, NaN 포함 행 제거
        idx <- apply(as.data.frame(data), 1, function(x) all(is.finite(as.numeric(x))))
        data <- as.data.frame(data)
        data <- data[idx, , drop = FALSE]
        return(data)
      }
    )
  )

# if(self$options$plot2==TRUE){
#
#   thres <- 0.1
#   atts <- strsplit(self$options$atts, ',')[[1]]
#
#   cr <- data %>%
#     ahp.mat(atts, negconvert = T) %>%
#     ahp.cr(atts)
#
#
#   cr.df <- data %>%
#     ahp.mat(atts, negconvert = TRUE) %>%
#     ahp.cr(atts) %>%
#     data.frame() %>%
#     mutate(rowid = 1:length(cr), cr.dum = as.factor(ifelse(cr <= thres, 1, 0))) %>%
#     select(cr.dum, rowid)
#
#
#   cityahp <- data %>%
#     ahp.mat(atts, negconvert = T)
#
#   eigentrue <- ahp.indpref(cityahp, atts, method = "eigen")
#
#   d <- data %>%
#     ahpsurvey::ahp.mat(atts = atts, negconvert = TRUE) %>%
#     ahpsurvey::ahp.indpref(atts, method = "eigen") %>%
#     dplyr::mutate(rowid = 1:nrow(eigentrue)) %>%
#     dplyr::left_join(cr.df, by = 'rowid') %>%
#     tidyr::gather(atts, key = "var", value = "pref")
#
#   state<-list(d, cr)
#   image <- self$results$plot2
#   image$setState(state)
# }
#



# .plot2 = function(image,ggtheme, theme,...) {
#
#   if (is.null(image$state))
#     return(FALSE)
#
#   atts <- strsplit(self$options$atts, ',')[[1]]
#   thres <- 0.1
#
#   d <- image$state[[1]]
#   cr <- image$state[[2]]
#
#  plot2 <- d |> ggplot(aes(x = var, y = pref)) +
#     geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
#     geom_jitter(alpha = 0.6, height = 0, width = 0.1, aes(color = cr.dum)) +
#     geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
#     scale_x_discrete("Attribute", label = atts) +
#     scale_y_continuous("Weight (dominant eigenvalue)",
#                        labels = scales::percent,
#                        breaks = c(seq(0,0.7,0.1))) +
#     guides(color=guide_legend(title=NULL))+
#     scale_color_discrete(breaks = c(0,1),
#                          labels = c(paste("CR >", thres),
#                                     paste("CR <", thres))) +
#     labs(NULL, caption = paste("n =", nrow(data), ",", "Mean CR =",
#                                round(mean(cr),3)))+
#     #theme_minimal()
#
#     theme(
#       axis.text.x = element_text(size = 12, color = "black")  # adjust size and darkness of x-axis text
#       #axis.text.x.bottom = element_text(angle = 90, hjust = 1)  # rotate x-axis text by 90 degrees
#     )
#
#   plot2+ggtheme
#
#     if (self$options$angle > 0) {
#     plot2 <- plot2 + ggplot2::theme(
#       axis.text.x = ggplot2::element_text(
#         angle = self$options$angle, hjust = 1
#       )
#     )
#   }
#
#
#   print(plot2)
#   TRUE
#
# },

