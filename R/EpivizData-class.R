setClassUnion("EpivizDataMgrOrNULL", c("EpivizDataMgr", "NULL"))

#' Data container for epiviz data server
#'
#' @import methods
#' @import S4Vectors
#' @export
EpivizData <- setRefClass("EpivizData",
  contains="VIRTUAL",
  fields=list(
    .object="ANY",
    .mgr="EpivizDataMgrOrNULL",
    .id="character",
    .name="character",
    .source_name="character",
    .columns="ANY",
    .ylim="ANY",
    .cur_query="ANY",
    .cur_hits="ANY"
  ),
  methods=list(
    initialize = function(object = GNCList(GRanges()),
                          columns=NULL,
                          ylim=NULL, ...) {
      .self$.object <- object
      .self$.columns <- columns

      if (!.self$.check_columns(columns))
        stop("Invalid 'columns' argument")

      if (is.null(.self$.columns))
        .self$.columns <- .self$.infer_columns()

      naIndex <- .self$.infer_nas()
      if (length(naIndex)>0) {
        .self$.object <- .self$.object[-naIndex,]
      }

      if (!is.null(ylim)) {
        if (!.self$.check_limits(ylim))
          stop("invalid 'ylim' argument")
        .self$.ylim <- ylim
      } else {
        .self$.ylim <- .self$.infer_limits()
      }

      .self$.cur_query <- NULL
      .self$.cur_hits <- NULL
      .self$.mgr <- NULL
      callSuper(...)
    },
    .infer_nas = function() {
      integer()
    },
    .check_columns = function(columns) {
      is.null(columns)
    },
    .infer_columns = function() {
      NULL
    },
    .check_limits = function(ylim) {
      is.null(ylim)
    },
    .infer_limits = function() {
      NULL
    },
    .check_class = function(new_object) {
      stop(".check_class called on VIRTUAL object")
    },
    update = function(new_object, send_request=TRUE) {
      "Update underlying data object with new object"
      if (!.self$.check_class(new_object)) {
        stop("class of 'new_object' is not equal to class of current 'object'")
      }

      original_object <- .self$.object
      .self$.object <- new_object

      if (!is.null(.self$.columns)) {
        if (!.self$.check_columns(.self$.columns)) {
          .self$.object <- original_object
          stop("columns not found in 'new_object'")
        }
        .self$.ylim <- .self$.infer_limits()
      }

      .self$.object <- reorderIfNeeded(.self$.object)
      .self$.object <- coerceIfNeeded(.self$.object)

      na_index <- .self$.infer_nas()
      if (length(na_index) > 0) {
        .self$.object <- .self$.object[-na_index,]
      }

      if (send_request && !is.null(.self$.mgr))
        .self$.mgr$.clear_datasourceGroup_cache(.self)
      invisible()
    },
    # TODO: use accessor functions for this
    get_id = function() {
      "Get id provided by manager \\code{\\link{EpivizDataMgr-class}}"
      .self$.id
    },
    set_id = function(id) {
      "Set id, used by manager \\code{\\link{EpivizDataMgr-class}}"
      .self$.id <- id
      invisible()
    },
    # TODO: use accessor functions for this
    get_name = function() {
      "Get datasource name, usually set by manager \\code{\\link{EpivizDataMgr-class}}"
      .self$.name
    },
    set_name=function(name) {
      "Set datasource name, usually set by manager \\code{\\link{EpivizDataMgr-class}}"
      .self$.name <- name
      invisible()
    },
    # TODO: use accessor functions for this
    get_source_name = function() {
      "Get original datasource name provided by manager \\code{\\link{EpivizDataMgr-class}}"
      .self$.source_name
    },
    set_source_name = function(source_name) {
      "Set original datasource name, used by manager \\code{\\link{EpivizDataMgr-class}}"
      .self$.source_name <- source_name
      invisible()
    },

    set_limits = function(ylim) {
      "Set plotting limits for continuous data"
      if (!.self$.check_limits(ylim))
          stop("'invalid' limits argument")
      .self$.ylim <- ylim
    },
    get_measurements = function() {
      "Get description of measurements served by this object"
      stop("'get_measurements' called on virtual class object")
    },
    get_default_chart_type = function() {
      "Get name of default chart type for this data type"
      stop("'get_default_chart_type' called on virtual class object")
    },
    parse_measurement = function(ms_id=NULL) {
      "Parse a measurement description for data served by this object"
      stop("'parse_measurement' called on virtual class object")
    },
    get_metadata_columns = function() {
      "Get the metadata served by this object"
      stop("'get_metadata_columns' called on virtual class object")
    },
    set_mgr = function(mgr) {
      "Set data manager, \\code{\\link{EpivizDataMgr-class}}"
      if (!is(mgr, "EpivizDataMgr"))
        stop("'mgr' must be of class 'EpivizDataMgr'")

      .self$.mgr <- mgr
      invisible()
    },
    show = function() {
      "Print information about this object"
      cat(class(.self), "object", .self$.id, "\n")
      methods::show(.self$.object)
      cat("\n\tcolumns:", paste(.self$.columns, collapse=","),"\n")
      cat("\tlimits:\n")
      print(.self$.ylim)
    }
  )
)

#####
# validity
.valid.EpivizData.columns <- function(x) {
  if(!x$.check_columns(x$.columns))
    return("invalid 'columns' slot")
  NULL
}

.valid.EpivizData <- function(x) {
  c(.valid.EpivizData.columns(x))
}

S4Vectors::setValidity2("EpivizData", .valid.EpivizData)

#######
# get data
EpivizData$methods(
  .get_hits = function(query) {
    if (!is(query, "GRanges"))
      stop("'query' must be a GRanges object")
    if (length(query) != 1) {
      stop("'query' must be of length 1")
    }

    if (is.null(.self$.cur_query) ||
        !identical(unname(query), unname(.self$.cur_query)))
    {
      .self$.cur_query <- query
      olaps <- suppressWarnings(GenomicRanges::findOverlaps(query, .self$.object, select="all"))
      .self$.cur_hits <- subjectHits(olaps)

      if (length(.self$.cur_hits) == 0) {
        return(invisible())
      }

      if (!S4Vectors::isSorted(start(.self$.object)[.self$.cur_hits])) {
        stop("these should be ordered by now...")
      }
      # this is here to make insertion on JS cache work
      .self$.cur_hits <- seq(min(.self$.cur_hits), max(.self$.cur_hits))
    }
    invisible()
  },
  get_rows = function(query, metadata, useOffset = FALSE) {
    "Get genomic interval information overlapping query <\\code{\\link{GenomicRanges}}> region"
    if (is.null(query)) {
      .self$.cur_hits <- 1:length(.self$.object)
      .self$.cur_query <- NULL
    } else {
      .self$.get_hits(query)
    }

    if (length(.self$.cur_hits) == 0) {
      out <- list(globalStartIndex=NULL, useOffset=FALSE,
                  values=list(id=list(),
                   chr=list(),
                    start=list(),
                    end=list(),
                    metadata=.self$.get_metadata(.self$.cur_hits, metadata)))
    } else {
      if (!useOffset) {
        out <- list(globalStartIndex=.self$.cur_hits[1],
                  useOffset=FALSE,
                  values=list(
                    id=.self$.cur_hits,
                    chr=as.vector(seqnames(.self$.object)),
                    start=start(.self$.object)[.self$.cur_hits],
                    end=end(.self$.object)[.self$.cur_hits],
                    metadata=.self$.get_metadata(.self$.cur_hits, metadata)
                   )
                )
      } else {
        st <- start(.self$.object)[.self$.cur_hits]
        stDiff <- diff(st)
        end <- end(.self$.object)[.self$.cur_hits]
        endDiff <- diff(end)

        out <- list(globalStartIndex=.self$.cur_hits[1],
                    useOffset=TRUE,
                    values=list(
                      id=.self$.cur_hits,
                      chr=as.vector(seqnames(.self$.object)),
                      start=c(st[1], stDiff),
                      end=c(end[1], endDiff),
                      metadata=.self$.get_metadata(.self$.cur_hits, metadata)
                     )
                )
        }
    }
    # make sure we are wrapping lists correctly
    if (length(out$values) > 0 && length(out$values$id) == 1) {
      for (slotName in names(out$values)) {
        if (slotName != "metadata")
          out$values[[slotName]] <- list(out$values[[slotName]])
      }
    }
    return(out)
  },
  .get_values_from_hits = function(curHits, measurement, round) {
    numeric()
  },
  get_values=function(query, measurement, round=TRUE) {
    "Get measurement values for features overlapping query region <\\code{\\link{GenomicRanges}}"
    if (is.null(query)) {
      .self$.cur_hits <- 1:length(.self$.object)
      .self$.cur_query <- NULL
    } else {
      .self$.get_hits(query)
    }

    if (length(.self$.cur_hits) == 0) {
      out <- list(globalStartIndex=NULL, values=list())
    } else {
      out <- list(globalStartIndex=.self$.cur_hits[1],
                  values=.self$.get_values_from_hits(.self$.cur_hits, measurement, round=round))
      # check we are wrapping lists correctly
      if (length(out$values) == 1) {
        out$values <- list(out$values)
      }
    }
    return(out)
  },
  toJSON = function(chr=NULL, start=NULL, end=NULL) {
    "Convert data to JSON"

    if (is.null(start))
      start <- 1
    if (is.null(end))
      end <- .Machine$integer.max

    if (is.null(chr)) {
      query <- NULL
    } else {
      query <- GRanges(seqnames=chr, ranges=IRanges(start=start,end=end))
    }

    row_data <- .self$get_rows(query = query, metadata=c())
    col_data <- .self$.get_col_data(query)

    result <- list(rows=row_data, cols=col_data)
    data_json <- epivizrChart::json_writer(result)

    measurements <- .self$get_measurements()
    for (i in 1:length(measurements)) {
      measurements[[i]]@dataprovider <- "epivizr"
    }

    ms_list <- lapply(measurements, as.list)
    ms_json <- epivizrChart::json_writer(ms_list)

    return(list(measurements=ms_json, data=data_json))
  },
  .get_col_data = function(query) {
    ms_list <- .self$get_measurements()
    cols <- list()

    for (i in 1:length(ms_list)) {
      ms <- ms_list[[i]]
      values <- .self$get_values(query=query, measurement=ms@id)
      cols[[ms@id]] <- values
    }

    return(cols)
  },
  toMySQL = function(host=NULL, unix.socket=NULL,
    username, password, db_name, annotation=NULL, batch=50 ) {
    "Send EpivizData to a MySQL Database
    \\describe{
    \\item{host}{Hostname}
    \\item{unix.socket}{Unix Socket}
    \\item{username}{Username for MySQL database}
    \\item{password}{Password for MySQL database}
    \\item{db_name}{Name of MySQL database}
    \\item{annotation}{Annotation for index table}
    \\item{batch}{Batch size for data sent to the MySQL database at a time}
    }"
    connection <- DBI::dbConnect(drv=RMySQL::MySQL(), host=host,
      unix.socket=unix.socket, username=username, password=password)

    df <- as.data.frame(.self, stringsAsFactors=FALSE)

    create_table_query <- .self$.mysql_create_table(df, db_name)
    DBI::dbSendQuery(connection, create_table_query)

    index_queries <- .self$.mysql_insert_index(db_name, annotation)
    for (query in index_queries) {
      DBI::dbSendQuery(connection, query)
    }

    # wrap character columns in single quotes for SQL query
    filter <- sapply(colnames(df), function(colname) is.character(df[,colname]))
    df[,filter] <- apply(df[,filter], 2, function(col){ paste0("'", col, "'")})

    # batch queries
    insert_queries <- lapply(seq(1, nrow(df), batch),
      function(index, step, datasource) {
        # check if our step is outside the size of df
        # TODO: This only occurs on the last index (move outside)
        if ((nrow(df) - index) < step) {
          step <- (nrow(df) - index)
        }

        batch_values <- apply(df[index:(index+step),], 1, function(row) {
          paste0("(", paste0(row, collapse = ','), ")")
        })

        paste0("INSERT INTO ", db_name, ".`", datasource, "` ",
          "(", paste0(colnames(df),collapse = ', '), ") VALUES ", batch_values)
      }, step=(batch-1), datasource=.self$get_name())

    for (query in insert_queries) {
      DBI::dbSendQuery(connection, batch)
    }

    DBI::dbDisconnect(connection)

    invisible()
  },
  .mysql_create_table = function(df, db_name) {
    "Auxiliary method for toMySQL that returns a string representation of a table
    creation query for this EpivizData object
    \\describe{
    \\item{df}{The EpivizData object as a data frame (stringsAsFactors must be FALSE)}
    \\item{db_name}{The name of the SQL database}
    }"
    # filtering column names without chr, start, and end
    # (they are hardcoded in query below)
    filter <- c(-1,-2,-3)
    col_names <- colnames(df[filter])

    # SQL types
    sql_cols <- sapply(col_names, function(col_name) {
      if (is.character(df[,col_name])) {
        # the bytes we want allocated for this column in the table
        max <- max(nchar(df[,col_name]))

        paste0("`", col_name, "`", " varchar(", max,")")
      } else if (is.numeric(df[,col_name])){
        paste0("`", col_name, "`", " double")
      }
    })

    create_table_query <- paste0(
      "CREATE TABLE IF NOT EXISTS ", db_name, ".`", .self$get_name(), "` (
      `id` bigint(20) NOT NULL AUTO_INCREMENT,",
      "`chr` varchar(20) NOT NULL,",
      "`start` bigint(20) NOT NULL,",
      "`end` bigint(20) NOT NULL, ",
      paste0(sql_cols, sep=",", collapse=""),
      "PRIMARY KEY (`id`,`chr`,`start`),",
      "KEY `location_idx` (`start`,`end`)",
      ") ENGINE=MyISAM DEFAULT CHARSET=latin1"
    )

    if (nrow(df) > 1000000){
      chrs <- unique(df$chr)
      return(paste0(
        create_table_query, " ",
        "PARTITION BY LIST COLUMNS(chr) ",
        "SUBPARTITION BY HASH (start) ",
        "SUBPARTITIONS 10 ",
        "(", paste0("PARTITION ", chrs, " VALUES IN ('", chrs, "') ENGINE = MyISAM",
          collapse=",")))
    }

    create_table_query
  },
  .mysql_insert_index = function(db_name, ...) {
    "Auxiliary function for toMySQL that returns a string represention of
    an insert query for the EpivizData object
    \\describe{
    \\item{db_name}{The name of the MySQL database}
    \\item{...}{Annotation field is only passed in for GeneInfoData and BlockData}
    }"

    queries <- lapply(.self$get_measurements(), function(ms) {
      if (is.null(ms[[1]]@annotation)) {
          annotation <- "NULL"
      }

      paste0(
        "INSERT INTO ", db_name, ".bp_data_index", # make this a function
        " VALUES (",
        "'", .self$get_name(), "'", ",", # measurement_id
        "'", .self$get_name(), "'", ",", # measurement_name
        "'", .self$get_name(), "'", ",", # location
        "'", ms[[1]]@id, "'", ",", # column_name
        ms[[1]]@minValue, ",", # min
        ms[[1]]@maxValue, ",", # max
        0, ",", # window size
        "'", annotation,"'",
        ")")
    })

    queries
  }
)
