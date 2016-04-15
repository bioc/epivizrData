#' Data container for interval data.
#' 
#' Used to serve data for visualizations of genomic regions only. Wraps
#' \code{\link{GenomicRanges}} objects.
#' 
#' @docType class
#' @seealso EpivizData
EpivizBlockData <- setRefClass("EpivizBlockData",
  contains="EpivizTrackData",
  methods=list(
  	initialize=function(...) {
  		callSuper(...)
  		.self$.columns <- NULL
  	}
  )
)

.valid.EpivizBlockData.ylim <- function(x) {
	if (!is.null(x$.ylim))
		return("'ylim' must be 'NULL'")
	NULL
}

.valid.EpivizBlockData <- function(x) {
	c(.valid.EpivizBlockData.ylim(x))
}

S4Vectors::setValidity2("EpivizBlockData", .valid.EpivizBlockData)

EpivizBlockData$methods(
  get_measurements = function() {
    out <- list(list(id = .self$.id,
                name = .self$.name,
                type = "range",
                datasourceId = .self$.id,
                datasourceGroup = .self$.id,
                defaultChartType="Blocks Track",
                annotation=NULL,
                minValue=NA,
                maxValue=NA,
                metadata=NULL))
      out
  },
#  parseMeasurement=function(msId) {
#     if (msId != id)
#       stop("invalid parsed measurement")
#     NULL
#   },
  .get_metadata = function(cur_hits, metadata) {
    return(NULL)
  },
  .get_values_from_hits = function(cur_hits, measurement) {
    return(NULL)
  }
)

