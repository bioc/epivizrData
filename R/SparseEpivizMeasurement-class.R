#' Class encapsulating a measurement description for epiviz app.
#'
#' @exportClass SparseEpivizMeasurement
SparseEpivizMeasurement <- setClass(
  "SparseEpivizMeasurement",
  slots = c(id = "character",
            datasourceId = "character",
            datasourceName = "character"),
  prototype = prototype(id = character(),
            datasourceId = character(),
            datasourceName = character()
  )
)

#' Convert \code{\link{SparseEpivizMeasurement}} object to \code{list}
#'
#' @param x \code{\link{SparseEpivizMeasurement}} object to coerce.
#' @return a \code{list} describing measurement object
#' @export
setMethod("as.list", signature(x = "SparseEpivizMeasurement"),
          function(x) {
            c(x@id)
          })

#' Display measurement datasourceId and id
#'
#' @param object a \code{\link{SparseEpivizMeasurement}} to display
#' @return A string describing measurement
#' @export
setMethod("show", signature(object = "SparseEpivizMeasurement"),
          function(object) {
            cat(paste0(object@datasourceId, ":", object@id))
          })

#' Create empty Epiviz Measurement
#' @export
.emptyEpivizSparseMeasurement <- function() {
  SparseEpivizMeasurement(id = character(),
                          datasourceId = character(),
                          datasourceName = character())
}

setGeneric(".appendEpivizMeasurement", 
           function(a, b)
             standardGeneric(".appendEpivizMeasurement"))

setMethod(".appendEpivizMeasurement", "SparseEpivizMeasurement",
          function(a, b) {
            nms <- slotNames("SparseEpivizMeasurement")
            for (nm in nms) {
              cur_val <- slot(b, nm)
              if (is.list(slot(a, nm))) {
                cur_val <- list(cur_val)
              }
              if (!is.null(slot(b, nm))) {
                slot(a, nm) <- c(slot(a, nm), cur_val)
              } else {
                slot(a, nm) <- c(slot(a, nm), list(NULL))
              }
            }
            a
          })

setGeneric(".serializeEpivizMeasurement", 
           function(x)
             standardGeneric(".serializeEpivizMeasurement"))

setMethod(".serializeEpivizMeasurement", "SparseEpivizMeasurement",
          function(x) {
            if (length(x@id) == 1) {
              nms <- slotNames("SparseEpivizMeasurement")
              for (nm in nms) {
                slot(x, nm) <- list(slot(x, nm))
              }
            }
            
            epivizrServer::json_writer(as.list(x))
          })