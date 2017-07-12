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
  toMySQL = function(host, unix.socket, user, pass, db_name, annotation=NULL) {
    driver <- RMySQL::MySQL()
    connection <- DBI::dbConnect(drv=driver, host=host,
      unix.socket=unix.socket, username=user, password=pass)

    # INSERT INDEX TO DB
    index_query <- .self$.insert_index_query(db_name, annotation)
    DBI::dbSendQuery(connection,index_query)

    # CREATE TABLE IN DB
    table_query <- .self$.create_table_query(db_name)
    DBI::dbSendQuery(connection, table_query)

    # INSERT DATA TO DB
    all_rows <- .self$get_rows(query=NULL, metadata=c("gene", "exon_starts","exon_ends"))
    values <- all_rows$values

    df <- data.frame(
      chr=values$chr,
      start=values$start,
      end=values$end,
      strand=values$strand,
      gene=values$metadata$gene,
      exon_starts=unlist(values$metadata$exon_starts, use.names=FALSE),
      exon_ends=unlist(values$metadata$exon_ends, use.names=FALSE),
      stringsAsFactors=FALSE
    )

    # BATCH QUERIES
    insert_queries <- lapply(seq(1, nrow(df), 50),
      function(index, step, datasource) {
        if ((nrow(df) - index) < step) {
          step <- (nrow(df) - index)
        }
        batch <- paste0("(",
           "'", df[index:(index+step),'chr'], "',",
           df[index:(index+step),'start'], ",",
           df[index:(index+step),'end'], ",",
           "'", df[index:(index+step),'strand'], "',",
           "'", df[index:(index+step),'gene'], "',",
           "'", df[index:(index+step),'exon_starts'],"',",
           "'",df[index:(index+step),'exon_ends'], "'",
           ")", collapse=",")

        paste0(
        "INSERT INTO ", db_name, ".`", datasource, "` ",
        "(chr, start, end, strand, gene, exon_starts, exon_ends) VALUES ", batch)
    }, step=49, datasource=.self$get_name())

    for (batch in insert_queries) {
      DBI::dbSendQuery(connection, batch)
    }

    DBI::dbDisconnect(connection)

    invisible()
  },
  .insert_index_query = function(db_name, annotation="NULL") {
    index <- "gene_data_index"
    min <- 0
    max <- 0
    datasource <- .self$get_name()

    index_query <- paste0(
      "REPLACE INTO ", db_name, ".", index,
      " VALUES (",
      "'", datasource, "'", ",",
      "'", datasource, "'", ",",
      "'", datasource, "'", ",",
      "'", datasource, "'", ",",
      min, ",",
      max, ",",
      "'", annotation, "'",
      ")")

    return(index_query)
  },
  .create_table_query = function(db_name) {
    datasource <- .self$get_name()
    return(paste0(
      "CREATE TABLE IF NOT EXISTS ", db_name, ".`", datasource, "` (
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
