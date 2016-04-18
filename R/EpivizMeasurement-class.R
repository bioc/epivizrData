#' Class encapsulating a measurement description for epiviz app.
#' 
#' @exportClass EpivizMeasurement
EpivizMeasurement <- setClass("EpivizMeasurement",
  slots = c(
    id = "character",
    name = "character",
    type = "character",
    datasourceId = "character",
    datasourceGroup = "character",
    defaultChartType = "character",
    annotation = "ANY",
    minValue = "numeric",
    maxValue = "numeric",
    metadata = "ANY"
  ),
  prototype = prototype(
    id = character(),
    name = character(),
    type = character(),
    datasourceId = character(),
    datasourceGroup = character(),
    defaultChartType = character(),
    annotation = NULL,
    minValue = as.numeric(NA),
    maxValue = as.numeric(NA),
    metadata = NULL
  )
)

#' Convert \code{\link{EpivizMeasurement}} object to \code{list}
#' 
#' @param x \code{\link{EpivizMeasurement}} object to coerce.
#' @return a \code{list} describing measurement object
#' @export
setMethod("as.list", signature(x="EpivizMeasurement"),
  function(x) {
    nms <- slotNames("EpivizMeasurement")
    out <- lapply(nms, function(slot_name) slot(x, slot_name))
    names(out) <- nms
    out
  }          
)

.emptyEpivizMeasurement <- function() {
  EpivizMeasurement(id=character(),
                    name=character(),
                    type=character(),
                    datasourceId=character(),
                    datasourceGroup=character(),
                    defaultChartType=character(),
                    annotation=list(),
                    minValue=numeric(),
                    maxValue=numeric(),
                    metadata=list())
}

.appendEpivizMeasurement <- function(a, b) {
  if (!is(a, "EpivizMeasurement") || !is(b, "EpivizMeasurement")) {
    stop("a and b must be 'EpivizMeasurement' objects")
  }
  
  nms <- slotNames("EpivizMeasurement")
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
}

.serializeEpivizMeasurement <- function(obj) {
  if (!is(obj, "EpivizMeasurement")) {
    stop("obj must be of class 'EpivizMeasurement'")
  }
  
  if (length(obj@id) == 1) {
    nms <- slotNames("EpivizMeasurement")
    for (nm in nms) {
        slot(obj, nm) <- list(slot(obj, nm))
    }
  }
  
  epivizrServer::json_writer(as.list(obj))
}