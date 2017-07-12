#' Data container for RangedSummarizedExperiment objects
#'
#' Used to serve general data (used in e.g., scatter plots and heatmaps). Wraps
#' \code{\link{RangedSummarizedExperiment}} objects. Numeric values obtained from
#' \code{assays} slot
#'
#' @docType class
#' @seealso EpivizData
#'
EpivizFeatureData <- setRefClass("EpivizFeatureData",
  contains="EpivizData",
  fields=list(.assay="ANY", .metadata="ANY"),
  methods=list(
    initialize=function(object = SummarizedExperiment(matrix(nr=0,nc=0),
                                                      rowRanges=GRanges()),
                        assay=1, metadata = NULL, ...) {
      .self$.assay <- assay
      .self$.metadata <- metadata
      callSuper(object=object, ...)
    },
    .check_class = function(object) { is(object, "RangedSummarizedExperiment") },
    .check_columns = function(columns) {
      all(columns %in% rownames(colData(.self$.object)))
    },
    .infer_columns = function() {
      rownames(colData(.self$.object))
    },
    .check_limits = function(ylim) {
      if (!is.matrix(ylim))
        return(FALSE)
      if (nrow(ylim) != 2)
        return(FALSE)
      if (ncol(ylim) != length(.self$.columns))
        return(FALSE)
      TRUE
    },
    .infer_nas = function() {
      mat <- SummarizedExperiment::assay(.self$.object, i=.self$.assay)
      col_index <- match(.self$.columns, rownames(colData(.self$.object)))
      na_mat <- is.na(mat[,col_index])
      if (!is.matrix(na_mat))
        na_mat <- cbind(na_mat)
      which(rowSums(na_mat)>0)
    },
    .infer_limits = function() {
      mat <- SummarizedExperiment::assay(.self$.object, i=.self$.assay)
      col_index <- match(.self$.columns, rownames(colData(.self$.object)))
      unname(sapply(col_index, function(i) range(pretty(range(mat[,i], na.rm=TRUE)))))
    }
  )
)

.valid.EpivizFeatureData.object <- function(x) {
  if(!is(x$.object, "RangedSummarizedExperiment"))
    return("'object' must be of class 'RangedSummarizedExperiment'")
  if(!is(rowRanges(x$.object), "GNCList"))
    return("'rowRanges(object)' must be of class 'GNCList'")
  NULL
}

.valid.EpivizFeatureData.ylim <- function(x) {
  if(!is(x$.ylim, "matrix"))
    return("'ylim' must be a matrix")
  if(nrow(x$.ylim) != 2)
    return("'ylim' must have two rows")
  if(ncol(x$.ylim) != length(x$.columns))
    return("'ylim' must have 'length(columns)' columns")
  NULL
}

.valid.EpivizFeatureData.assay <- function(x) {
  if (is.character(x$.assay)) {
    if(!(x$.assay %in% names(assays(x$.object))))
      return("'assay' not found in 'object'")
    return(NULL)
  }

  if (x$.assay > length(assays(x$.object)))
    return("'assay' not found in 'object'")
  NULL
}

.valid.EpivizFeatureData <- function(x) {
  c(.valid.EpivizFeatureData.object(x),
    .valid.EpivizFeatureData.ylim(x),
    .valid.EpivizFeatureData.assay(x))
}

S4Vectors::setValidity2("EpivizFeatureData", .valid.EpivizFeatureData)

EpivizFeatureData$methods(
  get_default_chart_type = function() { "ScatterPlot" },
  get_measurements=function() {
    out <- lapply(.self$.columns, function(cur_col) {
      m <- match(cur_col, .self$.columns)

      anno <- NULL
      if (ncol(colData(.self$.object)) > 0) {
        anno <- as.list(colData(.self$.object)[cur_col,,drop=FALSE])
        anno <- lapply(anno, as.character)
      }

      EpivizMeasurement(id=cur_col,
                        name=cur_col,
                        type="feature",
                        datasourceId=.self$.id,
                        datasourceGroup=.self$.id,
                        datasourceName=.self$.source_name,
                        defaultChartType=.self$get_default_chart_type(),
                        annotation=anno,
                        minValue=.self$.ylim[1,m],
                        maxValue=.self$.ylim[2,m],
                        metadata=.self$.metadata)
    })
    out
  },
  #     parseMeasurement=function(msId) {
#       column <- strsplit(msId, split="__")[[1]][2]
#       if(!.checkColumns(column)) {
#         stop("invalid parsed measurement")
#       }
#       column
#     },
    .get_metadata = function(cur_hits, cur_metadata) {
      if (length(.self$.metadata) < 1) {
          return(NULL)
      }

      if(any(!cur_metadata %in% .self$.metadata))
        stop("error getting metadata")

      if (length(cur_hits) == 0) {
        out <- lapply(cur_metadata, function (x) list())
        names(out) <- cur_metadata
        return(out)
      }
      out <- as.list(mcols(rowRanges(.self$.object))[cur_hits, cur_metadata])
      names(out) <- cur_metadata
      out
    },
  .get_values_from_hits = function(cur_hits, measurement, round=FALSE) {
    if (!measurement %in% .self$.columns) {
      stop("could not find measurement", measurement)
    }
    col_names <- colnames(.self$.object)
    m <- match(measurement, col_names)
    unname(assay(.self$.object, .self$.assay)[cur_hits, m])
  },
  get_default_chart_type_html = function() {
    "epiviz-json-scatter-plot"
  },
  get_metadata_columns = function() {
    return(.self$.metadata)
  }
)

