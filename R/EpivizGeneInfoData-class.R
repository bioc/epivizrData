#' Container for gene annotation data
#'
#' Used to serve data to gene annotation tracks. Wraps \code{\link{GenomicRanges}} objects.
#' Annotation obtained from columns \code{Gene} (gene symbols) and \code{Exons} (exon start and end locations).
#'
#' @docType class
#' @seealso EpivizData
#' @seealso register,OrganismDb
#'
EpivizGeneInfoData <- setRefClass("EpivizGeneInfoData",
  contains="EpivizTrackData",
  methods=list(
    initialize=function(...) {
      callSuper(...)
      .self$.columns <- NULL
    }
  )
)

.valid.EpivizGeneInfoData.ylim <- function(x) {
  if (!is.null(x$.ylim))
    return("'ylim' must be 'NULL'")
  NULL
}

.valid.EpivizGeneInfoData.metadata <- function(x) {
  mdata <- mcols(x$.object)
  nms <- names(mdata)
  requiredNames <- c("Gene","Exons")
  if (any(!requiredNames %in% nms))
    return("'metadata' must contain columns 'Gene' and 'Exons'")

  if (is(mdata$Gene, "Rle") && !is.character(runValue(mdata$Gene)))
    return("'Gene' must be a 'character' vector or Rle, or 'CharacterList'")
  if (!is(mdata$Gene, "Rle") && !(is.character(mdata$Gene) || is(mdata$Gene, "CharacterList")))
    return("'Gene' must be a 'character' vector or Rle, or 'CharacterList'")

  if (!is(mdata$Exons, "IRangesList"))
    return("'Exons' must be an 'IRangesList'")

  NULL
}

.valid.EpivizGeneInfoData <- function(x) {
  c(.valid.EpivizGeneInfoData.ylim(x),
    .valid.EpivizGeneInfoData.metadata(x))
}

S4Vectors::setValidity2("EpivizGeneInfoData", .valid.EpivizGeneInfoData)

EpivizGeneInfoData$methods(
  get_default_chart_type = function() { "GenesTrack" },
  get_measurements=function() {
    out <- list(EpivizMeasurement(
      id = .self$.id,
      name = .self$.name,
      type = "range",
      datasourceId = .self$.id,
      datasourceGroup = .self$.id,
      datasourceName = .self$.source_name,
      defaultChartType = .self$get_default_chart_type(),
      metadata=c("gene", "exon_starts","exon_ends")))
    out
  },
  get_rows=function(query, metadata) {
    out <- callSuper(query, metadata)
    if (length(.self$.cur_hits) == 0) {
      return(out)
    }

    out$values$strand <- as.character(strand(.self$.object)[.self$.cur_hits])
    out
  },
  .get_metadata=function(cur_hits, cur_metadata) {
    if (length(cur_hits) == 0) {
      out <- lapply(cur_metadata, function(x) list())
      names(out) <- cur_metadata
      return(out)
    }
    out <- vector("list", length(cur_metadata))
    names(out) <- cur_metadata
    for (col in cur_metadata) {
      cur_out <- switch(col,
                       gene=as.character(.self$.object$Gene[cur_hits]),
                       exon_starts=unname(lapply(start(.self$.object$Exons)[cur_hits], paste, collapse=",")),
                       exon_ends=unname(lapply(end(.self$.object$Exons)[cur_hits],paste,collapse=",")))
      out[[col]] <- cur_out
    }
    out
  },
  get_default_chart_type_html = function() {
    stop("Genes Track is currently not supported for polymer.")
  },
  .get_col_data = function(chr, start, end) {
    return(NULL)
  },
  .mysql_insert_index = function(db_name, annotation) {
    if (is.null(annotation) || missing(annotation)){
      annotation <- "NULL"
    }

    return(paste0(
      "INSERT INTO ", db_name, ".gene_data_index",
      " VALUES (",
      "'", .self$get_name(), "'", ",", # measurement_id
      "'", .self$get_name(), "'", ",", # measurement_name
      "'", .self$get_name(), "'", ",", # location
      "'", .self$get_name(), "'", ",", # column_name
      0, ",", # min
      0, ",", # max
      "'", annotation, "'",
      ")"))
  },
  .mysql_create_table = function(db_name) {
    return(paste0(
      "CREATE TABLE IF NOT EXISTS ", db_name, ".`", .self$get_name(), "` (
      `id` bigint(20) NOT NULL AUTO_INCREMENT,
      `chr` varchar(20) NOT NULL,
      `start` bigint(20) NOT NULL,
      `end` bigint(20) NOT NULL,
      `gene` varchar(255) DEFAULT NULL,
      `strand` varchar(20) DEFAULT NULL,
      `exon_starts` text,
      `exon_ends` text,
      PRIMARY KEY (`id`,`chr`,`start`),
      KEY `location_idx` (`start`,`end`)
    ) ENGINE=MyISAM DEFAULT CHARSET=latin1"))
  }
)
