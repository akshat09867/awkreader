utils::globalVariables(c(":="))

#' Fast Batch Combining of Flat Files via AWK
#'
#' @description A streamlined wrapper around \code{filtered.fread} designed to quickly
#' read, process, and combine multiple flat files into a single dataset without applying row filters.
#'
#' @param the.files A character vector of file paths to process. Non-existent files are automatically filtered out.
#' @param path.to.awk A character string specifying the path to the AWK binary. If \code{NULL} (default),
#' the function attempts to invoke a global system call to \code{"awk"}.
#' @param header A logical value indicating whether the target files contain a header row. Default is \code{TRUE}.
#' @param the.variables A character vector specifying which columns to retain. Use \code{"."} (default)
#' to retain all columns.
#' @param include.filename A logical value indicating whether to include a source file tracking column
#' in the returned dataset. Default is \code{TRUE}.
#' @param skip A numeric offset, a character regex pattern, or a structured list indicating lines to bypass.
#' If a list is used, it \strong{must follow dot notation}:
#' \itemize{
#'   \item \code{skip.metadata.rows}: An integer count or a character regex pattern used to identify where
#'   the metadata block ends.
#'   \item \code{skip.data.rows}: An integer specifying the number of data rows to explicitly skip after the header.
#' }
#' Default is \code{0}.
#' @param file.header A character string defining the column name for the tracked file origin.
#' Only utilized if \code{include.filename = TRUE}. Default is \code{"file"}.
#' @param num.files.per.batch An integer specifying how many files to aggregate per AWK system pipeline call.
#' Default is \code{1000}.
#' @param return.as A character string specifying the desired return object. Options are \code{"result"} (default),
#' \code{"code"} (returns raw generated AWK scripts), or \code{"all"} (returns both).
#' @param envir The environment context in which evaluation variables are evaluated. Default is \code{.GlobalEnv}.
#' @param show.warnings A logical value determining whether underlying \code{data.table::fread} messages
#' should be displayed or suppressed. Default is \code{FALSE}.
#' @param return.data.table A logical value indicating whether to return a \code{data.table} object
#' or a standard \code{data.frame}. Default is \code{TRUE}.
#' @param nrows An integer specifying the maximum total rows to parse out from the batch pipeline. Default is \code{Inf}.
#' @param drop A character or numeric index vector specifying columns that should be explicitly excluded from the final output.
#' @param ... Extra parameters forwarded to underlying internal setup routines inside \code{filtered.fread}.
#'
#' @return A \code{data.table} (or \code{data.frame}), or a character vector containing the raw shell commands,
#' depending on the value passed to \code{return.as}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Combine multiple log files rapidly without applying filters
#' all_logs <- combined.fread(
#'   the.files = c("log_jan.csv", "log_feb.csv"),
#'   skip = list(skip.metadata.rows = 2, skip.data.rows = 0)
#' )
#' }
combined.fread <- function(the.files, path.to.awk = NULL, header = TRUE, the.variables = ".", include.filename = TRUE, skip = 0, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...) {
  return(filtered.fread(the.files = the.files, path.to.awk = path.to.awk, the.filter = NULL, the.variables = the.variables, include.filename = include.filename, file.header = file.header, num.files.per.batch = num.files.per.batch, return.as = return.as, envir = envir, show.warnings = show.warnings, return.data.table = return.data.table, nrows = nrows, drop = drop))
}

#' Fast, Filtered Reading of Multiple Files via AWK
#'
#' @description Translates R-style filtering statements into highly efficient AWK commands
#' to process, filter, and select specific columns from multiple flat files simultaneously.
#' Batched outputs are fast-loaded and bound together into a single dataset.
#'
#' @param the.files A character vector of file paths to process. Non-existent files are automatically filtered out.
#' @param path.to.awk A character string specifying the path to the AWK binary. If \code{NULL} (default),
#' the function attempts to invoke a global system call to \code{"awk"}.
#' @param header A logical value indicating whether the target files contain a header row. Default is \code{TRUE}.
#' If \code{FALSE}, columns are auto-assigned as \code{V1}, \code{V2}, etc.
#' @param delim A character string specifying the column separator within the files. Default is \code{","}.
#' @param the.filter A character string or unquoted expression outlining the filtering logic to pass to AWK.
#' Supports implicit translation of basic math and symbolic calls. Default is \code{NULL} (no filtering).
#' @param the.variables A character vector specifying which columns to retain. Use \code{"."} (default)
#' to retain all columns.
#' @param include.filename A logical value indicating whether to include a source file tracking column
#' in the returned dataset. Default is \code{TRUE}.
#' @param skip A numeric offset, a character regex pattern, or a structured list indicating lines to bypass.
#' If a list is used, it \strong{must follow dot notation}:
#' \itemize{
#'   \item \code{skip.metadata.rows}: An integer count or a character regex pattern used to identify where
#'   the metadata block ends before hitting the core table.
#'   \item \code{skip.data.rows}: An integer specifying the number of data rows to explicitly skip after the header.
#' }
#' Default is \code{0}.
#' @param file.header A character string defining the column name for the tracked file origin.
#' Only utilized if \code{include.filename = TRUE}. Default is \code{"file"}.
#' @param num.files.per.batch An integer specifying how many files to aggregate per AWK system pipeline call.
#' Default is \code{1000}.
#' @param return.as A character string specifying the desired return object. Options are:
#' \itemize{
#'   \item \code{"result"} (default): Returns the compiled dataset.
#'   \item \code{"code"}: Bypasses compilation and returns a character vector of the raw generated AWK scripts.
#'   \item \code{"all"}: Returns a structured list containing both the compiled dataset and the underlying AWK statements.
#' }
#' @param envir The environment context in which evaluation characters or external metadata strings are parsed.
#' Default is \code{.GlobalEnv}.
#' @param and.symbol A character replacement flag for logical AND statements. Default is \code{"&"}.
#' @param or.symbol A character replacement flag for logical OR statements. Default is \code{"|"}.
#' @param in.symbol A character replacement flag for inclusion tests. Default is \code{"\%in\%"}.
#' @param nin.symbol A character replacement flag for exclusion tests. Default is \code{"\%nin\%"}.
#' @param show.warnings A logical value determining whether underlying \code{data.table::fread} shell messages
#' should be displayed or suppressed. Default is \code{FALSE}.
#' @param return.data.table A logical value indicating whether to return a \code{data.table} object
#' or a standard \code{data.frame}. Default is \code{TRUE}.
#' @param nrows An integer specifying the maximum total rows to parse out from the batch pipeline. Default is \code{Inf}.
#' @param drop A character or numeric index vector specifying columns that should be explicitly excluded from the final output.
#' @param ... Extra parameters forwarded to underlying internal setup routines.
#'
#' @return A \code{data.table} (or \code{data.frame}), or a character vector containing the raw shell commands,
#' depending on the value passed to \code{return.as}.
#'
#' @importFrom data.table fread rbindlist setnames setDF
#' @export
#'
#' @examples
#' \dontrun{
#' # Standard usage with a numeric filter and dot-notation row skipping
#' my_data <- filtered.fread(
#'   the.files = "diamonds.csv",
#'   the.filter = "price > 5000",
#'   skip = list(skip.metadata.rows = 0, skip.data.rows = 5)
#' )
#'
#' # Read data without headers, selecting specific auto-generated columns
#' raw_data <- filtered.fread(
#'   the.files = "sensor_logs.txt",
#'   header = FALSE,
#'   delim = "\t",
#'   the.variables = c("V1", "V3")
#' )
#' }
filtered.fread <- function(the.files, path.to.awk = NULL, file.pattern=NULL, recursive=FALSE, header = TRUE, delim = ",", the.filter = NULL, the.variables = ".", include.filename = TRUE, skip = 0, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, and.symbol = "&", or.symbol = "|", in.symbol = "%in%", nin.symbol = "%nin%", show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }

  lv.name <- "last.variable"
  value.code <- "code"
  value.all <- "all"

  if (!is.logical(return.data.table)) {
    return.data.table <- TRUE
  }
  the.files <- .resolve.files(the.files, file.pattern = file.pattern, recursive = recursive)
    total.files <- length(the.files)
  if (total.files == 0) {
    stop("No valid files were found matching the specified criteria.")
  }
  metadata.skip <- 0
  data.skip <- 0
  if (is.list(skip)) {
    if (!is.null(skip$skip.data.rows)) {
      data.skip <- skip$skip.data.rows
    }
    if (!is.null(skip$skip.metadata.rows)) {
      metadata.skip <- skip$skip.metadata.rows
      if (is.character(metadata.skip)) {
        preview.lines <- readLines(the.files[1], n = 100, warn = FALSE)
        match.index <- which(grepl(metadata.skip, preview.lines))[1]

        if (is.na(match.index)) {
          stop(sprintf("The skip pattern '%s' was not found in the file.", metadata.skip))
        }
        metadata.skip <- match.index - 1
      }
    }
  } else if (is.character(skip)) {
    preview.lines <- readLines(the.files[1], n = 100, warn = FALSE)
    match.index <- which(grepl(skip, preview.lines))[1]

    if (is.na(match.index)) {
      stop(sprintf("The skip pattern '%s' was not found in the file.", skip))
    }
    metadata.skip <- match.index - 1
  } else if (is.numeric(skip)) {
    metadata.skip <- skip
  }
  first.file.con <- file(the.files[1], "r")
  on.exit(close(first.file.con), add = TRUE)
  if (metadata.skip > 0) {
    readLines(first.file.con, n = metadata.skip)
  }
  header.line <- readLines(first.file.con, n = 1)

  if (header) {
    all.variables <- unlist(strsplit(header.line, split = delim, fixed = TRUE))
    all.variables <- gsub('^"|"$', "", all.variables)
    if (is.null(the.variables) | "." %in% the.variables) {
      the.variables <- all.variables
    }
    if (sum(the.variables %in% all.variables) == 0) {
      stop("No variables in the data were specified.")
    }
  } else {
    num_cols <- length(strsplit(header.line, delim, fixed = TRUE)[[1]])
    all.variables <- paste0("V", 1:num_cols)
    if (is.null(the.variables) || "." %in% the.variables) {
      the.variables <- all.variables
    }
    if (sum(the.variables %in% all.variables) == 0) {
      stop("No variables in the data were specified.")
    }
  }
  if (!is.null(drop)) {
    if (is.numeric(drop)) {
      drop <- all.variables[drop]
    }
    the.variables <- the.variables[!(the.variables %in% drop)]
  }

  if (length(the.variables) == 0) {
    stop("All variables were dropped.")
  }

  w <- which(all.variables %in% the.variables)
  column.names.awk <- paste(sprintf("$%d", w), collapse = ",")
  if (!is.numeric(num.files.per.batch)) {
    num.files.per.batch <- 1000
  }
  if (num.files.per.batch < 1) {
    num.files.per.batch <- 1000
  }

  string.filename <- ""
  if (include.filename == TRUE) {
    string.filename <- ",FILENAME"
  }

  num.batches <- ceiling(total.files / num.files.per.batch)
  if (is.null(path.to.awk) && .Platform$OS.type == "windows") {
    path.to.awk <- find.awk.binary()
  } else {
    path.to.awk <- "awk"
  }

  awk.filter <- translate.filtering.statement(the.filter = the.filter, the.variables = all.variables, envir = envir, and.symbol = and.symbol, or.symbol = or.symbol, in.symbol = in.symbol, nin.symbol = nin.symbol)
  if (header) {
    skip.limit <- metadata.skip + 1 + data.skip
  } else {
    skip.limit <- data.skip + metadata.skip
  }
  awk.script.content <- sprintf(
    'BEGIN { FS="%s"; OFS="," } FNR <= %s { next } { %s print %s%s }',
    delim, skip.limit, awk.filter, column.names.awk, string.filename
  )

  outputs <- execute.awk.stream(awk.script.content, the.files, value.code, all.variables[w], include.filename, num.batches, num.files.per.batch, path.to.awk, total.files, show.warnings, nrows, file.header, return.as)
  list.data <- outputs$list.data
  expanded.statements <- outputs$expanded.statements
  if (return.as == value.code) {
    res <- expanded.statements
  }

  if (return.as != value.code) {
    the.result <- rbindlist(l = list.data, fill = T)
    if (nrows < nrow(the.result)) {
      the.result <- the.result[1:nrows, ]
    }
    if (return.data.table == FALSE) {
      setDF(the.result)
    }

    if (return.as == value.all) {
      res <- list(
        result = the.result,
        code = expanded.statements
      )
    }

    if (return.as != value.all) {
      res <- the.result
    }
  }

  return(res)
}

#' Core Translation Pipeline for Filtering Expressions
#'
#' @description Orchestrates the parsing and conversion of R-style filtering strings
#' or logical statements into valid, executable AWK syntax expressions. Maps variables
#' to their positional column indices (e.g., \code{$1}, \code{$2}) and handles
#' operating-system-specific quote adjustments.
#'
#' @param the.filter A character string or unquoted expression containing the R filtering statement.
#' @param the.variables A character vector containing the full ordered variable names from the data header.
#' @param envir The environment context in which variable evaluations are evaluated. Default is \code{.GlobalEnv}.
#' @param and.symbol A character tracking key for logical AND operations. Default is \code{"&"}.
#' @param or.symbol A character tracking key for logical OR operations. Default is \code{"|"}.
#' @param in.symbol A character tracking key for membership matching operations. Default is \code{"\%in\%"}.
#' @param nin.symbol A character tracking key for excluded membership operations. Default is \code{"\%nin\%"}.
#' @param equation.symbols A character vector listing the recognized relational comparison operators.
#' Default is \code{c(">=", ">", "<=", "<", "!=", "==")}.

#' @return A character string representing the compiled logic statement formatted for direct injection into an AWK pipeline execution.
#' @keywords internal

translate.filtering.statement <- function(the.filter, the.variables, envir = .GlobalEnv, and.symbol = "&", or.symbol = "|", in.symbol = "%in%", nin.symbol = "%nin%", equation.symbols = c(">=", ">", "<=", "<", "!=", "==")) {
  if (is.null(the.filter)) {
    return("")
  }
  if (is.na(the.filter[1])) {
    return("")
  }
  if (the.filter[1] == "") {
    return("")
  }

  trimmed.filter <- trimws(the.filter)

  each.character <- strsplit(x = trimmed.filter, split = "")[[1]]
  num.characters <- length(each.character)

  w <- which(each.character %in% c(and.symbol, or.symbol))

  conjunctions <- each.character[each.character %in% c(and.symbol, or.symbol)]

  if (length(w) == 0) {
    begin <- 1
    end <- num.characters
  }
  if (length(w) > 0) {
    begin <- c(1, w + 1)
    end <- c(w - 1, num.characters)
  }

  num.pieces <- length(begin)
  num.conjunctions <- num.pieces - 1
  translated.pieces <- character(length = num.pieces)

  # equation.symbols.characters <- c("=", "!", "<", ">")
  # equation.symbols <- c(">=", "<=", "!=", "==", ">", "<")

  for (i in 1:num.pieces) {
    this.piece <- paste(each.character[begin[i]:end[i]], collapse = "")

    contains.in.symbol <- length(grep(pattern = in.symbol, x = this.piece, fixed = T)) > 0
    contains.nin.symbol <- length(grep(pattern = nin.symbol, x = this.piece, fixed = T)) > 0

    intermediate.piece <- this.piece

    if (contains.in.symbol == T) {
      intermediate.piece <- translate.in.statement(in.statement = this.piece, the.variables = the.variables, in.symbol = in.symbol, envir = envir)
    }
    if (contains.nin.symbol == T) {
      intermediate.piece <- translate.nin.statement(nin.statement = this.piece, the.variables = the.variables, nin.symbol = nin.symbol, in.symbol = in.symbol, envir = envir)
    }
    if (contains.in.symbol == F & contains.nin.symbol == F) {
      intermediate.piece <- translate.logical.statement(the.statement = this.piece, the.variables = the.variables, envir = envir)
    }

    translated.pieces[i] <- intermediate.piece
  }

  translated.conjunctions <- gsub(pattern = or.symbol, replacement = "||", x = gsub(pattern = and.symbol, replacement = "&&", x = each.character[w], fixed = TRUE), fixed = TRUE)

  inc.space <- ""

  if (num.conjunctions > 0) {
    inc.space <- " "
  }

  full.translation <- trimws(sprintf("if(%s%s)", paste(sprintf("%s %s%s", trimws(translated.pieces[1:(num.pieces - 1)]), translated.conjunctions, inc.space), collapse = ""), trimws(translated.pieces[num.pieces])))

  full.translation <- gsub(pattern = "'", replacement = '"', x = full.translation, fixed = T)

  for (i in 1:length(the.variables)) {
    full.translation <- gsub(pattern = the.variables[i], replacement = sprintf("$%d", i), x = full.translation)
  }

  full.translation <- trimws(full.translation)

  return(full.translation)
}

#' Parse and Translate Standard R Logical Operators
#'
#' @description Evaluates low-level logical connectors within a segment of text, converting
#' R's boolean layout terms (such as \code{&} or \code{|}) into corresponding AWK relational syntax blocks.
#'
#' @param the.statement A character string isolating a single logical operation block.
#' @param the.variables A character vector containing the recognized file variable headers.
#' @param envir The environment context in which the evaluation elements reside. Default is \code{.GlobalEnv}.
#' @return A processed character string containing translated logical characters compatible with AWK.
#' @keywords internal

translate.logical.statement <- function(the.statement, the.variables, envir = .GlobalEnv) {
  equation.symbols <- c(">=", "<=", "!=", "==", ">", "<")
  two.sides <- FALSE

  for (i in 1:length(equation.symbols)) {
    equation.pieces <- trimws(strsplit(x = the.statement, split = equation.symbols[i], fixed = TRUE)[[1]])

    if (length(equation.pieces) == 2) {
      two.sides <- TRUE
      the.symbol <- equation.symbols[i]
      break
    }
  }

  if (!two.sides) {
    return(the.statement)
  }

  col <- equation.pieces

  for (i in 1:length(equation.pieces)) {
    contains.column <- FALSE
    for (var in the.variables) {
      escaped.var <- gsub("([][\\\\.|(){}^$+*?-])", "\\\\\\1", var)
      if (grepl(pattern = paste0("\\b", escaped.var, "\\b"), x = equation.pieces[i])) {
        contains.column <- TRUE
        break
      }
    }

    if (!contains.column) {
      exists.in.envir <- tryCatch(
        expr = {
          parsed.expr <- parse(text = trimws(equation.pieces[i]))
          !is.null(eval(parsed.expr, envir = envir))
        },
        error = function(e) FALSE
      )

      if (exists.in.envir) {
        col[i] <- eval(expr = parse(text = trimws(equation.pieces[i])), envir = envir)
      }
    }
  }

  is.function.call <- grepl(pattern = "^[A-Za-z0-9_.]+\\s*\\(.*\\)$", x = trimws(col))

  has.quotes <- grepl("^['\"].*['\"]$", trimws(col))

  is.numeric.string <- !is.na(suppressWarnings(as.numeric(col)))
  to.quote <- (is.character(col) | is.factor(col)) & !is.function.call & !is.numeric.string & !has.quotes

  col[to.quote] <- sprintf('"%s"', col[to.quote])

  res <- trimws(sprintf("%s %s %s", trimws(col[1]), trimws(the.symbol), trimws(col[2])))
  split.pieces <- trimws(strsplit(res, trimws(the.symbol))[[1]])

  for (u in 1:length(split.pieces)) {
    for (v in 1:length(the.variables)) {
      escaped.var <- gsub("([][\\\\.|(){}^$+*?-])", "\\\\\\1", the.variables[v])
      split.pieces[u] <- gsub(pattern = paste0("\\b", escaped.var, "\\b"), replacement = sprintf("$%d", v), x = split.pieces[u])
    }
  }

  w <- which(split.pieces %in% sprintf('"$%s"', 1:length(the.variables)) | split.pieces %in% sprintf("'$%s'", 1:length(the.variables)))
  if (length(w) > 0) {
    split.pieces[w] <- gsub(pattern = "['\"]", replacement = "", x = split.pieces[w])
  }

  res <- trimws(paste(split.pieces, collapse = sprintf(" %s ", trimws(the.symbol))))
  return(res)
}

#' Translate R Vector Membership Filters to AWK Logic
#'
#' @description Parses explicit vector membership statements containing the \code{\%in\%} operator,
#' translating them into structurally matching compound matching loops or array validations in AWK syntax.
#'
#' @param in.statement A character string representing an isolated membership statement fragment.
#' @param the.variables A character vector matching data frame column names to map to column indexes.
#' @param nin.symbol A character structural definition string representing negative matches. Default is \code{"\%nin\%"}.
#' @param in.symbol A character structural definition string representing positive matches. Default is \code{"\%in\%"}.
#' @param envir The evaluation context environment frame. Default is \code{.GlobalEnv}.
#'
#' @return A character string containing the mapped structural subset layout for the AWK statement block.
#' @keywords internal

translate.in.statement <- function(in.statement, the.variables, nin.symbol = "%nin%", in.symbol = "%in%", envir = .GlobalEnv) {
  in.statement <- gsub(pattern = nin.symbol, replacement = in.symbol, x = in.statement, fixed = T)

  in.translation <- translate.in.statement.global(in.statement = in.statement, the.variables = the.variables, in.symbol = in.symbol, envir = envir)

  num.characters <- nchar(in.translation)
  first.character <- substring(text = in.translation, first = 1, last = 1)
  last.character <- substring(text = in.translation, first = num.characters, last = num.characters)

  already.parens <- first.character == "(" & last.character == ")"
  if (already.parens == T) {
    intermediate.piece <- sprintf("%s", in.translation)
  }
  if (already.parens == F) {
    intermediate.piece <- sprintf("(%s)", in.translation)
  }

  inner.expression <- substr(intermediate.piece, start = 2, stop = nchar(intermediate.piece) - 1)

  pieces <- strsplit(inner.expression, "||", fixed = T)[[1]]

  for (p in 1:length(pieces)) {
    pieces[p] <- trimws(pieces[p])

    pieces[p] <- translate.logical.statement(the.statement = pieces[p], the.variables = the.variables, envir = envir)

    split.piece <- strsplit(pieces[p], "==")[[1]]
    for (sp in 1:length(split.piece)) {
      split.piece[sp] <- trimws(split.piece[sp])
    }

    for (v in 1:length(the.variables)) {
      split.piece[1] <- if (split.piece[1] == the.variables[v]) sprintf("$%d", v) else split.piece[1]
    }

    pieces[p] <- paste(split.piece, collapse = "==")
  }
  translated.expression <- paste(pieces, collapse = "||")
  res <- paste0(paste0("(", translated.expression), ")")

  return(res)
}

#' Translate Negated R Vector Membership Filters to AWK Logic
#'
#' @description Special-case handler designed to invert membership matching statements containing
#' the \code{\%nin\%} operation rules, converting them cleanly into negated validation criteria strings for AWK.
#'
#' @param nin.statement A character string representing an isolated negative matching fragment.
#' @param the.variables A character vector identifying known variable column allocations.
#' @param nin.symbol The target string pattern matching an exclusion declaration. Default is \code{"\%nin\%"}.
#' @param in.symbol The target string pattern matching an inclusion declaration. Default is \code{"\%in\%"}.
#' @param envir The system environment lookup layer for evaluating vectors. Default is \code{.GlobalEnv}.
#'
#' @return An isolated, translated character block containing negated relational checks.
#' @keywords internal

translate.nin.statement <- function(nin.statement, the.variables, nin.symbol = "%nin%", in.symbol = "%in%", envir = .GlobalEnv) {
  in.statement <- gsub(pattern = nin.symbol, replacement = in.symbol, x = nin.statement, fixed = T)

  in.translation <- translate.in.statement.global(in.statement = in.statement, the.variables = the.variables, in.symbol = in.symbol, envir = envir)

  num.characters <- nchar(in.translation)
  first.character <- substring(text = in.translation, first = 1, last = 1)
  last.character <- substring(text = in.translation, first = num.characters, last = num.characters)

  already.parens <- first.character == "(" & last.character == ")"
  if (already.parens == T) {
    intermediate.piece <- sprintf("!%s", in.translation)
  }
  if (already.parens == F) {
    intermediate.piece <- sprintf("!(%s)", in.translation)
  }

  inner.expression <- substr(intermediate.piece, start = 3, stop = nchar(intermediate.piece) - 1)

  pieces <- strsplit(inner.expression, "||", fixed = T)[[1]]

  for (p in 1:length(pieces)) {
    pieces[p] <- trimws(pieces[p])

    pieces[p] <- translate.logical.statement(the.statement = pieces[p], the.variables = the.variables, envir = envir)

    split.piece <- strsplit(pieces[p], "==")[[1]]
    for (sp in 1:length(split.piece)) {
      split.piece[sp] <- trimws(split.piece[sp])
    }

    for (v in 1:length(the.variables)) {
      split.piece[1] <- if (split.piece[1] == the.variables[v]) sprintf("$%d", v) else split.piece[1]
    }

    pieces[p] <- paste(split.piece, collapse = "==")
  }
  translated.expression <- paste(pieces, collapse = "||")
  res <- paste0(paste0("!(", translated.expression), ")")

  return(res)
}

#' Pattern-Based Subsetting and Reading of Multiple Files via AWK
#'
#' @description Reads and aggregates multiple flat files concurrently, filtering rows
#' based on regular expression patterns processed directly via AWK before parsing the data into R.
#'
#' @param the.files A character vector of file paths to process. Non-existent files are automatically filtered out.
#' @param path.to.awk A character string specifying the path to the AWK binary. If \code{NULL} (default),
#' the function attempts to invoke a global system call to \code{"awk"}.
#' @param header A logical value indicating whether the target files contain a header row. Default is \code{TRUE}.
#' @param the.patterns A character vector containing the regular expression patterns to match against data rows. Default is \code{NULL}.
#' @param tf A logical value determining whether to include rows that match the patterns (\code{TRUE})
#' or exclude rows that match them (\code{FALSE}). Default is \code{TRUE}.
#' @param delim A character string specifying the column separator within the files. Default is \code{","}.
#' @param connectors A character string defining how multiple patterns should be combined logically.
#' Options are \code{"or"} (default) or \code{"and"}.
#' @param the.variables A character vector specifying which columns to retain. Use \code{"."} (default)
#' to retain all columns.
#' @param include.filename A logical value indicating whether to include a source file tracking column
#' in the returned dataset. Default is \code{TRUE}.
#' @param skip A numeric offset, a character regex pattern, or a structured list indicating lines to bypass.
#' If a list is used, it \strong{must follow dot notation}:
#' \itemize{
#'   \item \code{skip.metadata.rows}: An integer count or a character regex pattern used to identify where
#'   the metadata block ends.
#'   \item \code{skip.data.rows}: An integer specifying the number of data rows to explicitly skip after the header.
#' }
#' Default is \code{0}.
#' @param file.header A character string defining the column name for the tracked file origin.
#' Only utilized if \code{include.filename = TRUE}. Default is \code{"file"}.
#' @param num.files.per.batch An integer specifying how many files to aggregate per AWK system pipeline call.
#' Default is \code{1000}.
#' @param return.as A character string specifying the desired return object. Options are \code{"result"} (default),
#' \code{"code"}, or \code{"all"}.
#' @param envir The environment context in which evaluation characters are parsed. Default is \code{.GlobalEnv}.
#' @param show.warnings A logical value determining whether underlying terminal messages should be shown. Default is \code{FALSE}.
#' @param return.data.table A logical value indicating whether to return a \code{data.table} or standard \code{data.frame}. Default is \code{TRUE}.
#' @param nrows An integer specifying the maximum total rows to parse out. Default is \code{Inf}.
#' @param drop A character or numeric index vector specifying columns to explicitly exclude.
#' @param ... Extra parameters forwarded to internal setup routines.
#'
#' @return A \code{data.table} (or \code{data.frame}) containing rows satisfying the pattern configurations.
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract rows matching "Error" or "Critical" from across several log files
#' errors <- pattern.fread(
#'   the.files = c("server1.log", "server2.log"),
#'   the.patterns = c("Error", "Critical"),
#'   connectors = "or"
#' )
#' }
pattern.fread <- function(the.files, path.to.awk = NULL, header = TRUE, the.patterns = NULL, file.pattern=NULL, recursive=FALSE, tf = TRUE, delim = ",", connectors = "or", the.variables = ".", include.filename = TRUE, skip = 0, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }

  and.symbols <- c("&", "&&", "and")
  or.symbols <- c("|", "||", "or")

  negation.symbols <- c("!", "not", "false", "F", "0")

  lv.name <- "last.variable"
  value.code <- "code"
  value.all <- "all"

  if (!is.logical(return.data.table)) {
    return.data.table <- TRUE
  }
  the.files <- .resolve.files(the.files, file.pattern = file.pattern, recursive = recursive)
    total.files <- length(the.files)
    if (total.files == 0) {
      stop("No valid files were found matching the specified criteria.")
    }
  metadata.skip <- 0
  data.skip <- 0
  if (is.list(skip)) {
    if (!is.null(skip$skip.data.rows)) {
      data.skip <- skip$skip.data.rows
    }
    if (!is.null(skip$skip.metadata.rows)) {
      metadata.skip <- skip$skip.metadata.rows
      if (is.character(metadata.skip)) {
        preview.lines <- readLines(the.files[1], n = 100, warn = FALSE)
        match.index <- which(grepl(metadata.skip, preview.lines))[1]

        if (is.na(match.index)) {
          stop(sprintf("The skip pattern '%s' was not found in the file.", metadata.skip))
        }
        metadata.skip <- match.index - 1
      }
    }
  } else if (is.character(skip)) {
    preview.lines <- readLines(the.files[1], n = 100, warn = FALSE)
    match.index <- which(grepl(skip, preview.lines))[1]

    if (is.na(match.index)) {
      stop(sprintf("The skip pattern '%s' was not found in the file.", skip))
    }
    metadata.skip <- match.index - 1
  } else if (is.numeric(skip)) {
    metadata.skip <- skip
  }
  first.file.con <- file(the.files[1], "r")
  on.exit(close(first.file.con), add = TRUE)
  if (metadata.skip > 0) {
    readLines(first.file.con, n = metadata.skip)
  }
  header.line <- readLines(first.file.con, n = 1)

  if (header) {
    all.variables <- unlist(strsplit(header.line, split = delim, fixed = TRUE))
    all.variables <- gsub('^"|"$', "", all.variables)

    if (is.null(the.variables) | "." %in% the.variables) {
      the.variables <- all.variables
    }
    if (sum(the.variables %in% all.variables) == 0) {
      stop("No variables in the data were specified.  Double check that the names were spelled correctly.")
    }
  } else {
    num_cols <- length(strsplit(header.line, delim, fixed = TRUE)[[1]])
    all.variables <- paste0("V", 1:num_cols)
    if (is.null(the.variables) || "." %in% the.variables) {
      the.variables <- all.variables
    }
    if (sum(the.variables %in% all.variables) == 0) {
      stop("No variables in the data were specified.")
    }
  }
  if (!is.null(drop)) {
    if (is.numeric(drop)) {
      drop <- all.variables[drop]
    }
    the.variables <- the.variables[!(the.variables %in% drop)]
  }
  if (length(the.variables) == 0) {
    stop("All variables were dropped.")
  }

  if (!is.numeric(num.files.per.batch)) {
    num.files.per.batch <- 1000
  }
  if (num.files.per.batch < 1) {
    num.files.per.batch <- 1000
  }

  w <- which(all.variables %in% the.variables)

  column.names.awk <- paste(sprintf("$%d", w), collapse = ",")

  patterns.exist <- !is.null(the.patterns)
  if (patterns.exist == FALSE) {
    awk.pattern <- ""
  }
  if (patterns.exist == TRUE) {
    num.patterns <- length(the.patterns)

    will.negate <- rep.int(x = tolower(as.character(tf)) %in% negation.symbols, times = ceiling(num.patterns / length(tf)))[1:num.patterns]

    logical.symbols <- rep.int(x = "", times = num.patterns)
    logical.symbols[will.negate == TRUE] <- "!"

    logical.patterns <- trimws(sprintf("%s /%s/", logical.symbols, the.patterns))

    awk.pattern <- logical.patterns[1]

    the.connections <- ""

    if (num.patterns > 1) {
      raw.connections <- rep.int(x = connectors, times = (num.patterns - 1) / length(connectors))

      the.connections <- rep.int(x = " || ", times = num.patterns - 1)
      the.connections[raw.connections %in% and.symbols] <- " && "

      for (j in 2:num.patterns) {
        awk.pattern <- sprintf("%s %s %s", awk.pattern, the.connections[j - 1], logical.patterns[j])
      }
    }
  }

  string.filename <- ""
  if (include.filename == TRUE) {
    string.filename <- ",FILENAME"
  }

  list.data <- list()

  num.batches <- ceiling(total.files / num.files.per.batch)

  awk.statements <- character(length = num.batches)
  expanded.statements <- character(length = num.batches)

  if (header) {
    skip.limit <- data.skip + 1 + metadata.skip
  } else {
    skip.limit <- data.skip + metadata.skip
  }
  awk.script.content <- sprintf('BEGIN { FS="%s"; OFS="," } FNR <= %s { next } %s {print %s%s}', delim, skip.limit, awk.pattern, column.names.awk, string.filename)

  temp_script <- tempfile(fileext = ".awk")
  writeLines(awk.script.content, temp_script)
  on.exit(unlink(temp_script), add = TRUE)

  if (is.null(path.to.awk) && .Platform$OS.type == "windows") {
    path.to.awk <- find.awk.binary()
  } else {
    path.to.awk <- "awk"
  }

  for (i in 1:num.batches) {
    batch.files <- the.files[((i - 1) * num.files.per.batch + 1):min(total.files, i * num.files.per.batch)]
    pasted.file.names <- paste(shQuote(batch.files), collapse = " ")

    awk.statements[i] <- sprintf("%s -f %s %s", path.to.awk, shQuote(normalizePath(temp_script)), pasted.file.names)
    expanded.statements[i] <- sprintf("%s '%s' %s", path.to.awk, awk.script.content, pasted.file.names)
    if (return.as != value.code) {
      if (show.warnings == TRUE) {
        batch.data <- fread(cmd = awk.statements[i], fill = T, nrows = nrows, header = FALSE, sep = ",")
      }
      if (show.warnings != TRUE) {
        suppressWarnings(batch.data <- fread(cmd = awk.statements[i], fill = T, nrows = nrows, header = FALSE, sep = ","))
      }

      if (nrow(batch.data) > 0) {
        if (!include.filename) {
          setnames(batch.data, all.variables[w])
        } else {
          setnames(batch.data, c(all.variables[w], file.header))
        }
      }
      list.data[[i]] <- batch.data
    }
  }

  if (return.as == value.code) {
    res <- expanded.statements
  }
  if (return.as != value.code) {
    the.result <- rbindlist(l = list.data, fill = T)
    if (return.data.table == FALSE) {
      setDF(the.result)
    }

    if (nrows < nrow(the.result)) {
      the.result <- the.result[1:nrows, ]
    }
    if (return.as == value.all) {
      res <- list(result = the.result, code = expanded.statements)
    }
    if (return.as != value.all) {
      res <- the.result
    }
  }

  return(res)
}

#' Efficient Record and Row Counting via AWK
#'
#' @description Computes the total number of records matching specific logical filtering criteria
#' across multiple large files using AWK. This performs counting at the shell level, eliminating
#' the overhead of loading whole datasets into memory.
#'
#' @param the.files A character vector of file paths to scan. Non-existent files are automatically filtered out.
#' @param path.to.awk A character string specifying the path to the AWK binary. If \code{NULL} (default),
#' the function attempts to invoke a global system call to \code{"awk"}.
#' @param delim A character string specifying the column separator within the files. Default is \code{","}.
#' @param the.filter A character string or unquoted expression outlining the filtering logic to pass to AWK.
#' Default is \code{NULL} (counts all records matching layout rules).
#' @param the.variables A character vector specifying active evaluation columns. Default is \code{"."}.
#' @param include.filename A logical value indicating whether to retain tracking metrics grouped by individual files. Default is \code{TRUE}.
#' @param skip A numeric offset, a character regex pattern, or a structured list indicating lines to bypass.
#' If a list is used, it \strong{must follow dot notation}:
#' \itemize{
#'   \item \code{skip.metadata.rows}: An integer count or a character regex pattern used to identify where
#'   the metadata block ends.
#'   \item \code{skip.data.rows}: An integer specifying the number of data rows to explicitly skip after the header.
#' }
#' Default is \code{0}.
#' @param file.header A character string defining the tracking header index. Default is \code{"file"}.
#' @param num.files.per.batch An integer specifying how many files to aggregate per pipeline call. Default is \code{1000}.
#' @param return.as A character string specifying the desired return format. Options are \code{"result"} (default),
#' \code{"code"}, or \code{"all"}.
#' @param envir The environment context in which variables are parsed. Default is \code{.GlobalEnv}.
#' @param and.symbol A character replacement flag for logical AND statements. Default is \code{"&"}.
#' @param or.symbol A character replacement flag for logical OR statements. Default is \code{"|"}.
#' @param in.symbol A character replacement flag for inclusion tests. Default is \code{"\%in\%"}.
#' @param nin.symbol A character replacement flag for exclusion tests. Default is \code{"\%nin\%"}.
#' @param show.warnings A logical value determining whether terminal messages are displayed. Default is \code{FALSE}.
#' @param nrows An integer specifying the maximum total matching record sets to count. Default is \code{Inf}.
#' @param drop A character or numeric index vector specifying columns to exclude from mapping.
#' @param ... Extra parameters forwarded to underlying internal setup routines.
#'
#' @return A \code{data.table} summarizing record counts per file (or overall), or raw shell string arrays.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get matching transaction counts across massive datasets without importing rows
#' total_expensive_items <- record.count(
#'   the.files = dir(".", pattern = "sales_*.csv"),
#'   the.filter = "price > 10000"
#' )
#' }
record.count <- function(the.files, path.to.awk = NULL, delim = ",", the.filter = NULL,file.pattern=NULL, recursive=FALSE,
                         the.variables = ".", include.filename = TRUE, skip = 0, file.header = "file",
                         num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv,
                         and.symbol = "&", or.symbol = "|", in.symbol = "%in%",
                         nin.symbol = "%nin%", show.warnings = FALSE, nrows = Inf, drop = NULL, ...) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  the.files <- .resolve.files(the.files, file.pattern = file.pattern, recursive = recursive)
    total.files <- length(the.files)
  if (total.files == 0) {
    stop("No valid files were found matching the specified criteria.")
  }
  if (!is.numeric(num.files.per.batch) || num.files.per.batch < 1) {
    num.files.per.batch <- 1000
  }

  metadata.skip <- 0
  data.skip <- 0
  if (is.list(skip)) {
    if (!is.null(skip$skip.data.rows)) {
      data.skip <- skip$skip.data.rows
    }
    if (!is.null(skip$skip.metadata.rows)) {
      metadata.skip <- skip$skip.metadata.rows
      if (is.character(metadata.skip)) {
        preview.lines <- readLines(the.files[1], n = 100, warn = FALSE)
        match.index <- which(grepl(metadata.skip, preview.lines))[1]

        if (is.na(match.index)) {
          stop(sprintf("The skip pattern '%s' was not found in the file.", metadata.skip))
        }
        metadata.skip <- match.index - 1
      }
    }
  } else if (is.character(skip)) {
    preview.lines <- readLines(the.files[1], n = 100, warn = FALSE)
    match.index <- which(grepl(skip, preview.lines))[1]

    if (is.na(match.index)) {
      stop(sprintf("The skip pattern '%s' was not found in the file.", skip))
    }
    metadata.skip <- match.index - 1
  } else if (is.numeric(skip)) {
    metadata.skip <- skip
  }
  first.file.con <- file(the.files[1], "r")
  on.exit(close(first.file.con), add = TRUE)
  if (metadata.skip > 0) {
    readLines(first.file.con, n = metadata.skip)
  }
  header.line <- readLines(first.file.con, n = 1)

  all.variables <- unlist(strsplit(header.line, split = delim, fixed = TRUE))
  all.variables <- gsub('^"|"$', "", all.variables)

  awk.filter <- translate.filtering.statement(
    the.filter = the.filter, the.variables = all.variables, envir = envir,
    and.symbol = and.symbol, or.symbol = or.symbol, in.symbol = in.symbol,
    nin.symbol = nin.symbol
  )
  if (is.null(the.filter) || awk.filter == "") {
    awk.action <- "{count++}"
  } else {
    awk.action <- sprintf("{%s {count++}} ", awk.filter[[1]][1])
  }
  skip.limit <- data.skip + 1 + metadata.skip


  num.batches <- ceiling(total.files / num.files.per.batch)
  awk.statements <- character(length = num.batches)
  expanded.statements <- character(length = num.batches)
  list.data <- list()

  if (is.null(path.to.awk) && .Platform$OS.type == "windows") {
    path.to.awk <- find.awk.binary()
  } else {
    path.to.awk <- "awk"
  }
  awk.script.content <- sprintf(
    'BEGIN { FS="%s"; OFS="," } FNR==1 && NR>1 {print prev_file, count+0; count=0} FNR==1 {prev_file=FILENAME} FNR<=%s {next} %s END {if(prev_file) print prev_file, count}',
    delim, skip.limit, awk.action
  )
  temp.script <- tempfile(fileext = ".awk")
  writeLines(awk.script.content, con = temp.script)
  on.exit(unlink(temp.script), add = TRUE)
  for (i in 1:num.batches) {
    batch.files <- the.files[((i - 1) * num.files.per.batch + 1):min(total.files, i * num.files.per.batch)]
    pasted.file.names <- paste(shQuote(batch.files), collapse = " ")

    awk.statements[i] <- sprintf("%s -f %s %s", path.to.awk, shQuote(normalizePath(temp.script)), pasted.file.names)
    expanded.statements[i] <- sprintf("%s '%s' %s", path.to.awk, awk.script.content, pasted.file.names)
    if (return.as != "code") {
      if (show.warnings) {
        batch.data <- fread(cmd = awk.statements[i], fill = TRUE, nrows = nrows, header = FALSE, sep = ",")
      } else {
        suppressWarnings(batch.data <- fread(cmd = awk.statements[i], fill = TRUE, nrows = nrows, header = FALSE, sep = ","))
      }

      if (nrow(batch.data) > 0) {
        names(batch.data) <- c(file.header, "count")

        if (!include.filename) {
          batch.data[, (file.header) := NULL]
        }
      }
      list.data[[i]] <- batch.data
    }
  }

  if (return.as == "code") {
    return(expanded.statements)
  }

  final.result <- rbindlist(l = list.data, fill = TRUE)

  if (return.as == "all") {
    return(list(result = final.result, code = expanded.statements))
  }

  return(final.result)
}


#' Fast Stream Aggregation of Delimited Files via AWK
#'
#' High-performance aggregation and streaming of multiple tabular data files using an
#' optimized AWK engine pipeline. This function parses columns, groups multi-file inputs,
#' and calculates summarizations (like sum, mean, and standard deviation) natively in the shell
#' before pulling structured aggregates back into R. It safely evaluates scalar mathematical transformations
#' passed as strings (e.g., \code{"sqrt(col)"}).
#'
#' @param the.files A character vector containing paths to the targeted delimited text datasets.
#' @param value.code A internal character tracking string for code generation mappings. Defaults to \code{"data"}.
#' @param delim A character string defining the field separator delimiter within files. Defaults to \code{","}.
#' @param num.batches An integer defining the processing grouping structure. Defaults to \code{1}.
#' @param num.files.per.batch An integer specifying the maximum threshold of files targeted per concurrent process execution loop. Defaults to \code{1000}.
#' @param summarize.with A named list specifying operations and target columns. Names must match supported functions:
#' \code{"sum"}, \code{"mean"}, or \code{"sd"}. Elements can include raw columns or functional call strings like \code{"sqrt(price)"}. Defaults to \code{NULL}.
#' @param return.as A character string defining what data is returned to the environment. Options include \code{"result"} (aggregated dataset), \code{"code"} (generated AWK commands), or \code{"all"} (a combined list of both). Defaults to \code{"result"}.
#' @param group.by A character vector or list containing column names to act as the aggregation grouping dimensions. Defaults to \code{NULL}.
#' @param path.to.awk A character string declaring the binary location execution path. Defaults to \code{"awk"}.
#' @param file.header A character string tracking column identification titles when filenames are tracked. Defaults to \code{"file"}.
#' @param include.filename A logical value indicating whether the tracking origin file string column should append to final structures. Defaults to \code{FALSE}.
#' @param skip An integer, list, or character regex pattern. Controls metadata/line skipping configuration routines before row parsing evaluations trigger. Defaults to \code{0}.
#' @param show.warnings A logical value determining whether underlying structural data warnings should surface during operation execution cycles. Defaults to \code{TRUE}.
#' @param nrows A numeric ceiling threshold limiting output aggregation allocations. Defaults to \code{Inf}.
#' @param return.data.table A logical value. If \code{TRUE}, returns a \code{data.table} object; otherwise, reverts the format to a standard base \code{data.frame}. Defaults to \code{TRUE}.
#'
#' @return Depending on the configuration argument passed to \code{return.as}, returns either a
#' \code{data.table} (\code{data.frame} if \code{return.data.table = FALSE}), a character vector representing full
#' system executable command configurations, or a combined metadata list package object.
#'
#' @export
#'
#' @importFrom data.table rbindlist setDF
#'
#' @examples
#' \dontrun{
#' # Aggregate data over multiple diamond batch CSV extractions
#' agg_res <- aggregated.fread(
#'   the.files = "diamonds.csv",
#'   group.by = list("cut", "color"),
#'   summarize.with = list(
#'     mean = list("sqrt(price)", "carat"),
#'     sd = "depth"
#'   ),
#'   return.as = "all"
#' )
#' print(agg_res$result)
#' }
aggregated.fread <- function(the.files,
                             value.code = "data",
                             delim = ",",
                             file.pattern=NULL,
                             recursive=FALSE,
                             num.batches = 1,
                             num.files.per.batch = 1000,
                             summarize.with = NULL,
                             return.as = "result",
                             group.by = NULL,
                             path.to.awk = "awk",
                             file.header = "file",
                             include.filename = FALSE,
                             skip = 0,
                             show.warnings = TRUE,
                             nrows = Inf,
                             sample.size.median = -1,
                             return.data.table = TRUE) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }

  val.code.const <- "code"
  val.all.const <- "all"

  the.files <- .resolve.files(the.files, file.pattern = file.pattern, recursive = recursive)
    total.files <- length(the.files)
    if (total.files == 0) {
      stop("No valid files were found matching the specified criteria.")
    }

  if (!is.numeric(num.files.per.batch) || num.files.per.batch < 1) {
    num.files.per.batch <- 1000
  }

  metadata.skip <- 0
  data.skip <- 0

  if (is.list(skip)) {
    if (!is.null(skip$skip.data.rows)) {
      data.skip <- skip$skip.data.rows
    }
    if (!is.null(skip$skip.metadata.rows)) {
      metadata.skip <- skip$skip.metadata.rows
      if (is.character(metadata.skip)) {
        preview.lines <- readLines(the.files[1], n = 100, warn = FALSE)
        match.index <- which(grepl(metadata.skip, preview.lines))[1]

        if (is.na(match.index)) {
          stop(sprintf("The skip pattern '%s' was not found in the file.", metadata.skip))
        }
        metadata.skip <- match.index - 1
      }
    }
  } else if (is.character(skip)) {
    preview.lines <- readLines(the.files[1], n = 100, warn = FALSE)
    match.index <- which(grepl(skip, preview.lines))[1]

    if (is.na(match.index)) {
      stop(sprintf("The skip pattern '%s' was not found in the file.", skip))
    }
    metadata.skip <- match.index - 1
  } else if (is.numeric(skip)) {
    metadata.skip <- skip
  }

  first.file.con <- file(the.files[1], "r")
  on.exit(close(first.file.con), add = TRUE)
  if (metadata.skip > 0) {
    readLines(first.file.con, n = metadata.skip)
  }
  header.line <- readLines(first.file.con, n = 1)

  all.variables <- unlist(strsplit(header.line, split = delim, fixed = TRUE))
  all.variables <- gsub('^"|"$', "", all.variables)

  skip.limit <- data.skip + 1 + metadata.skip

  num.batches <- ceiling(total.files / num.files.per.batch)
  awk.statements <- character(length = num.batches)
  expanded.statements <- character(length = num.batches)
  list.data <- list()

  if (is.null(path.to.awk) || path.to.awk == "awk") {
    if (.Platform$OS.type == "windows" && exists("find.awk.binary", mode = "function")) {
      path.to.awk <- find.awk.binary()
    } else {
      path.to.awk <- "awk"
    }
  }

  if (is.null(group.by)) {
    group.awk <- '\"All_Data\"'
    header.names <- character()
  } else {
    if (is.list(group.by)) {
      group.by <- unlist(group.by)
    }
    group.idx <- which(all.variables %in% group.by)
    if (length(group.idx) == 0) {
      stop("At least one valid column must be specified in 'group.by', or leave it NULL to summarize the entire dataset.")
    }
    group.awk <- sprintf("$%d", group.idx)
    group.awk <- paste(group.awk, collapse = " FS ")
    header.names <- c(group.by)
  }

  awk.body.statements <- c(sprintf("seen_groups[%s] = 1;", group.awk))
  awk.end.pre.prints <- character()
  awk.end.prints <- character()
  needs.psquare <- FALSE

  if (is.list(summarize.with)) {
    for (func in names(summarize.with)) {
      cols <- summarize.with[[func]]

      func.clean <- tolower(trimws(func))
      if (func.clean %in% c("n", "count", "sample_size", "samplesize", "n_obs")) {
        func.clean <- "n"
      }

      if (is.null(cols) || (is.logical(cols) && cols) || (is.character(cols) && length(cols) == 1 && cols == "")) {
        cols <- "_group_count_"
      }

      for (col in cols) {
        col.trimmed <- trimws(col)

        if (col.trimmed == "_group_count_") {
          awk.body.statements <- c(awk.body.statements, sprintf("n_group[%s]++;", group.awk))
          awk.end.prints <- c(awk.end.prints, "n_group[i]")
          header.names <- c(header.names, "n")
          next
        }

        has.quotes <- grepl("^['\"].*['\"]$", col.trimmed)
        is.multi.var <- grepl(",", col.trimmed)

        if (is.multi.var) {
          args <- sapply(strsplit(col.trimmed, ",")[[1]], trimws)
          col1.raw <- args[1]
          col2.raw <- args[2]

          math.func1 <- FALSE
          math.func1.name <- NULL
          col1 <- col1.raw
          if (grepl("^[A-Za-z0-9_.]+\\s*\\(.*\\)$", col1.raw) && !has.quotes) {
            math.func1 <- TRUE
            matches <- regexec("^([A-Za-z0-9_.]+)\\s*\\(\\s*([A-Za-z0-9_.]+)\\s*\\)$", col1.raw)
            extracted <- regmatches(col1.raw, matches)[[1]]
            if (length(extracted) >= 3) {
              math.func1.name <- extracted[2]
              col1 <- extracted[3]
            }
          }

          math.func2 <- FALSE
          math.func2.name <- NULL
          col2 <- col2.raw
          if (grepl("^[A-Za-z0-9_.]+\\s*\\(.*\\)$", col2.raw) && !has.quotes) {
            math.func2 <- TRUE
            matches <- regexec("^([A-Za-z0-9_.]+)\\s*\\(\\s*([A-Za-z0-9_.]+)\\s*\\)$", col2.raw)
            extracted <- regmatches(col2.raw, matches)[[1]]
            if (length(extracted) >= 3) {
              math.func2.name <- extracted[2]
              col2 <- extracted[3]
            }
          }

          col1.idx <- which(all.variables == col1)
          col2.idx <- which(all.variables == col2)
          if (length(col1.idx) == 0) stop(sprintf("Column '%s' not found.", col1))
          if (length(col2.idx) == 0) stop(sprintf("Column '%s' not found.", col2))

          col1.awk <- sprintf("$%d", col1.idx)
          if (math.func1) col1.awk <- paste0(math.func1.name, "(", col1.awk, ")")

          col2.awk <- sprintf("$%d", col2.idx)
          if (math.func2) col2.awk <- paste0(math.func2.name, "(", col2.awk, ")")

          h1 <- if (math.func1) paste(math.func1.name, col1, sep = "_") else col1
          h2 <- if (math.func2) paste(math.func2.name, col2, sep = "_") else col2
          header.names <- c(header.names, paste(func.clean, h1, h2, sep = "_"))

          p1 <- if (math.func1) paste0(math.func1.name, "_", col1.idx) else col1.idx
          p2 <- if (math.func2) paste0(math.func2.name, "_", col2.idx) else col2.idx
          var.prefix <- paste(func.clean, p1, p2, sep = "_")

        } else {
          math.func <- FALSE
          math.func.name <- NULL
          col.name <- col.trimmed

          if (grepl("^[A-Za-z0-9_.]+\\s*\\(.*\\)$", col.trimmed) && !has.quotes) {
            math.func <- TRUE
            matches <- regexec("^([A-Za-z0-9_.]+)\\s*\\(\\s*([A-Za-z0-9_.]+)\\s*\\)$", col.trimmed)
            extracted <- regmatches(col.trimmed, matches)[[1]]
            if (length(extracted) >= 3) {
              math.func.name <- extracted[2]
              col.name <- extracted[3]
            }
          }

          col.idx <- which(all.variables == col.name)
          if (length(col.idx) == 0) stop(sprintf("Column '%s' not found.", col.name))

          col.awk <- sprintf("$%d", col.idx)
          if (math.func) {
            col.awk <- paste0(math.func.name, "(", col.awk, ")")
            header.names <- c(header.names, paste(func.clean, math.func.name, col.name, sep = "_"))
          } else {
            header.names <- c(header.names, paste(func.clean, col.name, sep = "_"))
          }
          var.prefix <- paste(func.clean, col.idx, sep = "_")
        }

        if (func.clean == "n") {
          awk.body.statements <- c(awk.body.statements, sprintf("count_%s[%s]++;", var.prefix, group.awk))
          awk.end.prints <- c(awk.end.prints, sprintf("count_%s[i]", var.prefix))

        } else if (func.clean == "min") {
          awk.body.statements <- c(awk.body.statements, sprintf("if (!(%s in min_%s) || %s < min_%s[%s]) min_%s[%s] = %s;", group.awk, var.prefix, col.awk, var.prefix, group.awk, var.prefix, group.awk, col.awk))
          awk.end.prints <- c(awk.end.prints, sprintf("min_%s[i]", var.prefix))

        } else if (func.clean == "max") {
          awk.body.statements <- c(awk.body.statements, sprintf("if (!(%s in max_%s) || %s > max_%s[%s]) max_%s[%s] = %s;", group.awk, var.prefix, col.awk, var.prefix, group.awk, var.prefix, group.awk, col.awk))
          awk.end.prints <- c(awk.end.prints, sprintf("max_%s[i]", var.prefix))

        } else if (func.clean == "sum") {
          awk.body.statements <- c(awk.body.statements, sprintf("sum_%s[%s] += %s;", var.prefix, group.awk, col.awk))
          awk.end.prints <- c(awk.end.prints, sprintf("sum_%s[i]", var.prefix))

        } else if (func.clean == "mean") {
          awk.body.statements <- c(awk.body.statements, sprintf("sum_%s[%s] += %s;", var.prefix, group.awk, col.awk), sprintf("count_%s[%s]++;", var.prefix, group.awk))
          awk.end.prints <- c(awk.end.prints, sprintf("(count_%s[i] > 0 ? (sum_%s[i] / count_%s[i]) : \"NA\")", var.prefix, var.prefix, var.prefix))

        } else if (func.clean == "sd") {
          awk.body.statements <- c(awk.body.statements, sprintf("sum_%s[%s] += %s;", var.prefix, group.awk, col.awk), sprintf("sumsq_%s[%s] += (%s * %s);", var.prefix, group.awk, col.awk, col.awk), sprintf("count_%s[%s]++;", var.prefix, group.awk))
          num.str <- sprintf("(sumsq_%s[i] - (sum_%s[i]*sum_%s[i])/count_%s[i])", var.prefix, var.prefix, var.prefix, var.prefix)
          safe_num.str <- sprintf("(%s < 0 ? 0 : %s)", num.str, num.str)
          sd.formula <- sprintf("(count_%s[i] > 1 ? sqrt(%s / (count_%s[i] - 1)) : \"NA\")", var.prefix, safe_num.str, var.prefix)
          awk.end.prints <- c(awk.end.prints, sd.formula)

        } else if (func.clean == "cor") {
          if (!is.multi.var) stop(sprintf("The '%s' function requires two comma-separated variables.", func))
          awk.body.statements <- c(awk.body.statements,
                                   sprintf("sum_x_%s[%s] += %s;", var.prefix, group.awk, col1.awk),
                                   sprintf("sum_y_%s[%s] += %s;", var.prefix, group.awk, col2.awk),
                                   sprintf("sum_x2_%s[%s] += (%s * %s);", var.prefix, group.awk, col1.awk, col1.awk),
                                   sprintf("sum_y2_%s[%s] += (%s * %s);", var.prefix, group.awk, col2.awk, col2.awk),
                                   sprintf("sum_xy_%s[%s] += (%s * %s);", var.prefix, group.awk, col1.awk, col2.awk),
                                   sprintf("count_%s[%s]++;", var.prefix, group.awk))

          num.str <- sprintf("(count_%s[i] * sum_xy_%s[i] - sum_x_%s[i] * sum_y_%s[i])", var.prefix, var.prefix, var.prefix, var.prefix)
          var_x <- sprintf("(count_%s[i] * sum_x2_%s[i] - (sum_x_%s[i] * sum_x_%s[i]))", var.prefix, var.prefix, var.prefix, var.prefix)
          var_y <- sprintf("(count_%s[i] * sum_y2_%s[i] - (sum_y_%s[i] * sum_y_%s[i]))", var.prefix, var.prefix, var.prefix, var.prefix)
          cor.formula <- sprintf("(count_%s[i] > 1 && %s > 0 && %s > 0 ? %s / sqrt(%s * %s) : \"NA\")", var.prefix, var_x, var_y, num.str, var_x, var_y)
          awk.end.prints <- c(awk.end.prints, cor.formula)

        } else if (func.clean == "median") {
          needs.psquare <- TRUE
          awk.body.statements <- c(
            awk.body.statements,
            sprintf("update_psquare(\"%s\", %s, %s, %d);", var.prefix, group.awk, col.awk, as.integer(sample.size.median))
          )

          pre.print <- sprintf("med_%s = get_psquare_median(\"%s\", i); ", var.prefix, var.prefix)
          awk.end.pre.prints <- c(awk.end.pre.prints, pre.print)
          awk.end.prints <- c(awk.end.prints, sprintf("med_%s", var.prefix))
        }
      }
    }
  }

  psquare.func <- "
  function update_psquare(v, g, x, cap,   key, cnt, i, j, k, d, d_sign, q_hat, tmp) {
    key = v SUBSEP g;
    if (cap > 0 && ps_cnt[key] >= cap) return;

    cnt = ++ps_cnt[key];
    if (cnt <= 5) {
      ps_init[key, cnt] = x + 0;
      if (cnt == 5) {
        for (i = 1; i <= 5; i++) {
          for (j = i + 1; j <= 5; j++) {
            if (ps_init[key, i] > ps_init[key, j]) {
              tmp = ps_init[key, i]; ps_init[key, i] = ps_init[key, j]; ps_init[key, j] = tmp;
            }
          }
        }
        for (i = 1; i <= 5; i++) {
          ps_q[key, i] = ps_init[key, i];
          ps_n[key, i] = i;
        }
        ps_np[key, 1] = 1; ps_np[key, 2] = 2; ps_np[key, 3] = 3; ps_np[key, 4] = 4; ps_np[key, 5] = 5;
      }
      return;
    }

    x = x + 0;
    if (x < ps_q[key, 1]) { ps_q[key, 1] = x; k = 1; }
    else if (x < ps_q[key, 2]) k = 1;
    else if (x < ps_q[key, 3]) k = 2;
    else if (x < ps_q[key, 4]) k = 3;
    else if (x < ps_q[key, 5]) k = 4;
    else { ps_q[key, 5] = x; k = 4; }

    for (i = k + 1; i <= 5; i++) ps_n[key, i]++;

    ps_np[key, 1] += 0;
    ps_np[key, 2] += 0.25;
    ps_np[key, 3] += 0.50;
    ps_np[key, 4] += 0.75;
    ps_np[key, 5] += 1.00;

    for (i = 2; i <= 4; i++) {
      d = ps_np[key, i] - ps_n[key, i];
      if ((d >= 1 && ps_n[key, i+1] - ps_n[key, i] > 1) || (d <= -1 && ps_n[key, i-1] - ps_n[key, i] < -1)) {
        d_sign = (d >= 0) ? 1 : -1;

        q_hat = ps_q[key, i] + (d_sign / (ps_n[key, i+1] - ps_n[key, i-1])) * ((ps_n[key, i] - ps_n[key, i-1] + d_sign) * (ps_q[key, i+1] - ps_q[key, i]) / (ps_n[key, i+1] - ps_n[key, i]) + (ps_n[key, i-1] + d_sign - ps_n[key, i]) * (ps_q[key, i] - ps_q[key, i-1]) / (ps_n[key, i] - ps_n[key, i-1]));

        if (ps_q[key, i-1] < q_hat && q_hat < ps_q[key, i+1]) {
          ps_q[key, i] = q_hat;
        } else {
          ps_q[key, i] = ps_q[key, i] + d_sign * (ps_q[key, i + d_sign] - ps_q[key, i]) / (ps_n[key, i + d_sign] - ps_n[key, i]);
        }
        ps_n[key, i] += d_sign;
      }
    }
  }

  function get_psquare_median(v, g,   key, cnt, i, j, tmp) {
    key = v SUBSEP g;
    cnt = ps_cnt[key];
    if (cnt == 0) return \"NA\";
    if (cnt < 5) {
      for (i = 1; i <= cnt; i++) {
        for (j = i + 1; j <= cnt; j++) {
          if (ps_init[key, i] > ps_init[key, j]) {
            tmp = ps_init[key, i]; ps_init[key, i] = ps_init[key, j]; ps_init[key, j] = tmp;
          }
        }
      }
      if (cnt % 2 == 1) return ps_init[key, int(cnt/2) + 1];
      else return (ps_init[key, cnt/2] + ps_init[key, cnt/2 + 1]) / 2.0;
    }
    return ps_q[key, 3];
  }
  "

  body.string <- paste(awk.body.statements, collapse = " ")
  pre.print.string <- paste(awk.end.pre.prints, collapse = " ")
  print.string <- paste(awk.end.prints, collapse = '","')

  print.statement <- if (is.null(group.by)) {
    paste("print", print.string)
  } else {
    paste("print i,", print.string)
  }

  awk.script.content <- sprintf(
    '%s BEGIN { FS="%s"; OFS="," } FNR <= %s { next } { %s } END { for(i in seen_groups) { %s %s } }',
    if (needs.psquare) psquare.func else "", delim, skip.limit, body.string, pre.print.string, print.statement
  )

  outputs <- execute.awk.stream(
    awk.script.content = awk.script.content,
    the.files = the.files,
    value.code = value.code,
    header.names = header.names,
    include.filename = include.filename,
    num.batches = num.batches,
    num.files.per.batch = num.files.per.batch,
    path.to.awk = path.to.awk,
    total.files = total.files,
    show.warnings = show.warnings,
    nrows = nrows,
    file.header = file.header,
    return.as = return.as
  )

  list.data <- outputs$list.data
  expanded.statements <- outputs$expanded.statements

  if (return.as == val.code.const) {
    res <- expanded.statements
  } else {
    the.result <- data.table::rbindlist(l = list.data, fill = TRUE)
    if (nrows < nrow(the.result)) {
      the.result <- the.result[1:nrows, ]
    }
    if (!return.data.table) {
      data.table::setDF(the.result)
    }

    if (return.as == val.all.const) {
      res <- list(
        result = the.result,
        code = expanded.statements
      )
    } else {
      res <- the.result
    }
  }

  return(res)
}
