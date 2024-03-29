
# This file is automatically generated, you probably don't want to edit this

ahpOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "ahpOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars = NULL,
            itemmat = TRUE,
            weights = FALSE,
            cir = FALSE, ...) {

            super$initialize(
                package="seolmatrix",
                name="ahp",
                requiresData=TRUE,
                ...)

            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "numeric"))
            private$..itemmat <- jmvcore::OptionBool$new(
                "itemmat",
                itemmat,
                default=TRUE)
            private$..weights <- jmvcore::OptionBool$new(
                "weights",
                weights,
                default=FALSE)
            private$..cir <- jmvcore::OptionBool$new(
                "cir",
                cir,
                default=FALSE)

            self$.addOption(private$..vars)
            self$.addOption(private$..itemmat)
            self$.addOption(private$..weights)
            self$.addOption(private$..cir)
        }),
    active = list(
        vars = function() private$..vars$value,
        itemmat = function() private$..itemmat$value,
        weights = function() private$..weights$value,
        cir = function() private$..cir$value),
    private = list(
        ..vars = NA,
        ..itemmat = NA,
        ..weights = NA,
        ..cir = NA)
)

ahpResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "ahpResults",
    inherit = jmvcore::Group,
    active = list(
        instructions = function() private$.items[["instructions"]],
        itemmat = function() private$.items[["itemmat"]],
        weights = function() private$.items[["weights"]],
        cir = function() private$.items[["cir"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Analytic Hierarchy Process",
                refs="seolmatrix")
            self$add(jmvcore::Html$new(
                options=options,
                name="instructions",
                title="Instructions",
                visible=TRUE))
            self$add(jmvcore::Table$new(
                options=options,
                name="itemmat",
                title="Item Matrix",
                refs="easyAHP",
                visible="(itemmat)",
                clearWith=list(
                    "vars"),
                columns=list(
                    list(
                        `name`="name", 
                        `title`="Item", 
                        `type`="text", 
                        `content`="($key)"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="weights",
                title="Item Weights",
                refs="easyAHP",
                visible="(weights)",
                clearWith=list(
                    "vars"),
                columns=list(
                    list(
                        `name`="name", 
                        `title`="Item", 
                        `type`="text", 
                        `content`="($key)"),
                    list(
                        `name`="value", 
                        `title`="Weights"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="cir",
                title="Consistency Index and Ratio",
                refs="easyAHP",
                visible="(cir)",
                clearWith=list(
                    "vars"),
                columns=list(
                    list(
                        `name`="name", 
                        `title`="Consistency", 
                        `type`="text", 
                        `content`="($key)"),
                    list(
                        `name`="value", 
                        `title`="Value"))))}))

ahpBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "ahpBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "seolmatrix",
                name = "ahp",
                version = c(1,0,0),
                options = options,
                results = ahpResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Analytic Hierarchy Process
#'
#' 
#' @param data The data as a data frame.
#' @param vars .
#' @param itemmat .
#' @param weights .
#' @param cir .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$itemmat} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$weights} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$cir} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$itemmat$asDF}
#'
#' \code{as.data.frame(results$itemmat)}
#'
#' @export
ahp <- function(
    data,
    vars,
    itemmat = TRUE,
    weights = FALSE,
    cir = FALSE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("ahp requires jmvcore to be installed (restart may be required)")

    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(vars), vars, NULL))


    options <- ahpOptions$new(
        vars = vars,
        itemmat = itemmat,
        weights = weights,
        cir = cir)

    analysis <- ahpClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

