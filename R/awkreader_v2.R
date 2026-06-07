# Most inputs are the same as in filtered.fread()
combined.fread <- function(the.files, path.to.awk = NULL, the.variables = ".", include.filename = TRUE, skip = 0, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...) {
  return(filtered.fread(the.files = the.files, path.to.awk = path.to.awk, the.filter = NULL, the.variables = the.variables, include.filename = include.filename, file.header = file.header, num.files.per.batch = num.files.per.batch, return.as = return.as, envir = envir, show.warnings = show.warnings, return.data.table = return.data.table, nrows = nrows, drop = drop))
}

#' @import data.table
#' @export
filtered.fread <- function(the.files, path.to.awk = NULL, delim = ",", the.filter = NULL, the.variables = ".", include.filename = TRUE, skip = 0, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, and.symbol = "&", or.symbol = "|", in.symbol = "%in%", nin.symbol = "%nin%", show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }

  lv.name <- "last.variable"
  value.code <- "code"
  value.all <- "all"

  if (!is.logical(return.data.table)) {
    return.data.table <- TRUE
  }
  the.files <- path.expand(the.files)
  the.files <- the.files[file.exists(the.files)]

  total.files <- length(the.files)
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
  if (metadata.skip > 0) {
    readLines(first.file.con, n = metadata.skip)
  }
  header.line <- readLines(first.file.con, n = 1)
  close(first.file.con)
  all.variables <- unlist(strsplit(header.line, split = delim, fixed = TRUE))
  all.variables <- gsub('^"|"$', "", all.variables)
  if (is.null(the.variables) | "." %in% the.variables) {
    the.variables <- all.variables
  }
  if (sum(the.variables %in% all.variables) == 0) {
    stop("No variables in the data were specified.")
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


  string.filename <- ""
  if (include.filename == TRUE) {
    string.filename <- ",FILENAME"
  }

  list.data <- list()

  num.batches <- ceiling(total.files / num.files.per.batch)

  awk.statements <- character(length = num.batches)

  # If path to awk isn't provided, awk can be added to system path (Windows), or may already be on the path (Mac)
  if (is.null(path.to.awk)) {
    path.to.awk <- "awk"
  }
  # Otherwise, telling the function where to find awk installed would work. Like so:
  # path.to.awk = 'C:/"Program Files (X86)"/GnuWin32/bin/awk' #My installed awk is here. Note the double quotes around paths with spaces

  # Using Windows double-quoting if shell uses cmd.exe, else using single-quoting
  # OS = sessionInfo()$running  #To see the OS, but currently looking for the CMD.EXE executable in shell.type
  shell.type <- Sys.getenv("R.SHELL")
  if (!nzchar(shell.type)) {
    shell.type <- Sys.getenv("COMSPEC")
  }

  use.windows <- grepl("cmd.exe", tolower(shell.type), fixed = TRUE)

  awk.filter <- translate.filtering.statement(the.filter = the.filter, the.variables = all.variables, envir = envir, and.symbol = and.symbol, or.symbol = or.symbol, in.symbol = in.symbol, nin.symbol = nin.symbol, use.windows = use.windows)
  skip.limit <- metadata.skip + 1 + data.skip
  if (use.windows) {
    string.placeholder <- '"%s"'
    statement.to.fill <- '%s -F "%s" -v OFS="," "FNR <= %s { next }{%s print %s%s}" %s'
  } else {
    string.placeholder <- "'%s'"
    statement.to.fill <- "%s -F '%s' -v OFS=',' 'FNR <= %s { next }{%s print %s%s}' %s"
  }
  for (i in 1:num.batches) {
    pasted.file.names <- paste(sprintf(string.placeholder, the.files[((i - 1) * num.files.per.batch + 1):min(total.files, i * num.files.per.batch)]), collapse = " ")
    awk.statements[i] <- sprintf(statement.to.fill, path.to.awk, delim, skip.limit, awk.filter, column.names.awk, string.filename, pasted.file.names)
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
    res <- awk.statements
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
      res <- list(result = the.result, code = awk.statements)
    }
    if (return.as != value.all) {
      res <- the.result
    }
  }

  return(res)
}

translate.filtering.statement <- function(the.filter, the.variables, envir = .GlobalEnv, and.symbol = "&", or.symbol = "|", in.symbol = "%in%", nin.symbol = "%nin%", equation.symbols = c(">=", ">", "<=", "<", "!=", "=="), use.windows = FALSE) {
  if (is.null(the.filter)) {
    return("")
  }
  if (is.na(the.filter[1])) {
    return("")
  }
  if (the.filter[1] == "") {
    return("")
  }

  if (use.windows == TRUE) {
    quotation.escape <- '\\"'
  }
  if (use.windows == FALSE) {
    quotation.escape <- '\"'
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

  equation.symbols.characters <- c("=", "!", "<", ">")
  equation.symbols <- c(">=", "<=", "!=", "==", ">", "<")

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


  full.translation <- gsub(pattern = '"', replacement = quotation.escape, x = full.translation, fixed = T)
  full.translation <- gsub(pattern = "'", replacement = quotation.escape, x = full.translation, fixed = T)

  for (i in 1:length(the.variables)) {
    full.translation <- gsub(pattern = the.variables[i], replacement = sprintf("$%d", i), x = full.translation)
  }

  full.translation <- trimws(full.translation)

  return(full.translation)
}

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

  ending.values <- equation.pieces

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
        ending.values[i] <- eval(expr = parse(text = trimws(equation.pieces[i])), envir = envir)
      }
    }
  }

  is.math.function <- grepl(pattern = "^(log|mean|min|max|sum|exp|sqrt|abs|round)\\s*\\(", x = ending.values, ignore.case = TRUE)
  is.numeric.string <- !is.na(suppressWarnings(as.numeric(ending.values)))
  to.quote <- (is.character(ending.values) | is.factor(ending.values)) & !is.math.function & !is.numeric.string

  ending.values[to.quote] <- sprintf('"%s"', ending.values[to.quote])

  res <- trimws(sprintf("%s %s %s", trimws(ending.values[1]), trimws(the.symbol), trimws(ending.values[2])))
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

pattern.fread <- function(the.files, the.patterns = NULL, tf = TRUE, path.to.awk = NULL, delim = ",", connectors = "or", the.variables = ".", include.filename = TRUE, skip = 0, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...) {
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
  the.files <- path.expand(the.files)
  the.files <- the.files[file.exists(the.files)]

  total.files <- length(the.files)

  if (total.files == 0) {
    stop("No existing files were found.")
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
  if (metadata.skip > 0) {
    readLines(first.file.con, n = metadata.skip)
  }
  header.line <- readLines(first.file.con, n = 1)
  close(first.file.con)
  all.variables <- unlist(strsplit(header.line, split = delim, fixed = TRUE))
  all.variables <- gsub('^"|"$', "", all.variables)

  if (is.null(the.variables) | "." %in% the.variables) {
    the.variables <- all.variables
  }
  if (sum(the.variables %in% all.variables) == 0) {
    stop("No variables in the data were specified.  Double check that the names were spelled correctly.")
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

  shell.type <- Sys.getenv("R.SHELL")
  if (!nzchar(shell.type)) {
    shell.type <- Sys.getenv("COMSPEC")
  }

  if (grepl("cmd.exe", tolower(shell.type), fixed = TRUE)) {
    use.windows <- TRUE
  } else {
    use.windows <- FALSE
  }
  skip.limit <- data.skip + 1 + metadata.skip
  if (use.windows) {
    string.placeholder <- '"%s"'
    statement.to.fill <- '%s -F "%s" -v OFS="," "FNR <=%s { next } %s {print %s%s}" %s'
  } else {
    string.placeholder <- "'%s'"
    statement.to.fill <- "%s -F '%s' -v OFS=',' 'FNR <= %s { next } %s {print %s%s}' %s"
  }

  if (is.null(path.to.awk)) {
    path.to.awk <- "awk"
  }

  for (i in 1:num.batches) {
    pasted.file.names <- paste(sprintf(string.placeholder, the.files[((i - 1) * num.files.per.batch + 1):min(total.files, i * num.files.per.batch)]), collapse = " ")

    awk.statements[i] <- sprintf(statement.to.fill, path.to.awk, delim, skip.limit, awk.pattern, column.names.awk, string.filename, pasted.file.names)

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
    res <- awk.statements
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
      res <- list(result = the.result, code = awk.statements)
    }
    if (return.as != value.all) {
      res <- the.result
    }
  }

  return(res)
}

#' @import data.table
#' @export
record.count <- function(the.files, path.to.awk = NULL, delim = ",", the.filter = NULL,
                         the.variables = ".", include.filename = TRUE, skip = 0, file.header = "file",
                         num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv,
                         and.symbol = "&", or.symbol = "|", in.symbol = "%in%",
                         nin.symbol = "%nin%", show.warnings = FALSE, nrows = Inf, drop = NULL, ...) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  the.files <- path.expand(the.files)
  the.files <- the.files[file.exists(the.files)]
  total.files <- length(the.files)

  if (total.files == 0) {
    stop("No existing files were found.")
  }

  if (!is.numeric(num.files.per.batch) || num.files.per.batch < 1) {
    num.files.per.batch <- 1000
  }

  shell.type <- Sys.getenv("R.SHELL")
  if (!nzchar(shell.type)) {
    shell.type <- Sys.getenv("COMSPEC")
  }
  use.windows <- grepl("cmd.exe", tolower(shell.type), fixed = TRUE)
  metadata.skip <- 0
  data.skip <- 0
  if (is.list(skip)) {
    if (!is.null(skip$skip.data.rows)) {
      data.skip <- skip$skip.data.rows
      print(data.skip)
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
  if (metadata.skip > 0) {
    readLines(first.file.con, n = metadata.skip)
  }
  header.line <- readLines(first.file.con, n = 1)
  close(first.file.con)

  all.variables <- unlist(strsplit(header.line, split = delim, fixed = TRUE))
  all.variables <- gsub('^"|"$', "", all.variables)

  awk.filter <- translate.filtering.statement(
    the.filter = the.filter, the.variables = all.variables, envir = envir,
    and.symbol = and.symbol, or.symbol = or.symbol, in.symbol = in.symbol,
    nin.symbol = nin.symbol, use.windows = use.windows
  )
  if (is.null(the.filter) || awk.filter == "") {
    awk.action <- "{count++}"
  } else {
    awk.action <- sprintf("{%s {count++}} ", awk.filter[[1]][1])
  }
  skip.limit <- data.skip + 1 + metadata.skip
  print(skip.limit)
  if (use.windows) {
    string.placeholder <- '"%s"'
    statement.to.fill <- '%s -F "%s" -v OFS="," "FNR==1 && NR>1 {print prev_file, count+0; count=0} FNR==1 {prev_file=FILENAME} FNR<=%s {next} %s END {if(prev_file) print prev_file, count}" %s'
  } else {
    string.placeholder <- "'%s'"
    statement.to.fill <- "%s -F '%s' -v OFS=',' 'FNR==1 && NR>1 {print prev_file, count+0; count=0} FNR==1 {prev_file=FILENAME} FNR<=%s {next} %s  END {if(prev_file) print prev_file, count}' %s"
  }

  num.batches <- ceiling(total.files / num.files.per.batch)
  awk.statements <- character(length = num.batches)
  list.data <- list()

  if (is.null(path.to.awk)) {
    path.to.awk <- "awk"
  }

  for (i in 1:num.batches) {
    file.subset <- the.files[((i - 1) * num.files.per.batch + 1):min(total.files, i * num.files.per.batch)]
    pasted.file.names <- paste(sprintf(string.placeholder, file.subset), collapse = " ")

    awk.statements[i] <- sprintf(statement.to.fill, path.to.awk, delim, skip.limit, awk.action, pasted.file.names)

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
    return(awk.statements)
  }

  final.result <- rbindlist(l = list.data, fill = TRUE)

  if (return.as == "all") {
    return(list(result = final.result, code = awk.statements))
  }

  return(final.result)
}
