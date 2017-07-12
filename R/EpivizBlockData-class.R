#' Data container for interval data.
#'
#' Used to serve data for visualizations of genomic regions only. Wraps
#' \code{\link{GenomicRanges}} objects.
#'
#' @docType class
#' @seealso EpivizData
EpivizBlockData <- setRefClass("EpivizBlockData",
  contains="EpivizTrackData",
  fields=list(
    # step="numeric"
  ),
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
  get_default_chart_type = function() { "BlocksTrack" },
  get_measurements = function() {
    out <- list(
      EpivizMeasurement(
        id = .self$.id,
        name = .self$.name,
        type = "range",
        datasourceId = .self$.id,
        datasourceGroup = .self$.id,
        datasourceName = .self$.source_name,
        defaultChartType=.self$get_default_chart_type()
      )
    )
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
  },
  get_default_chart_type_html = function() {
    "epiviz-json-blocks-track"
  },
  .get_col_data = function(query) {
    return(NULL)
  },
  .get_sql_index_table_info = function(annotation=NULL) {
    if (is.null(annotation)) {
      annotation <- "NULL"
    }
    list(index_table="block_data_index",
      values=list(paste0(
        "'", .self$get_name(), "'", ",", # measurement_id
        "'", .self$get_name(), "'", ",", # measurement_name
        "'", .self$get_name(), "'", ",", # location
        "'", annotation,"'")
        )
    )
  }
)

