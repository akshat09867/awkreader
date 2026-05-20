# Most inputs are the same as in filtered.fread()

combined.fread <- function(the.files, path.to.awk = NULL, the.variables = ".", include.filename = TRUE, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...) {
  return(filtered.fread(the.files = the.files, path.to.awk = path.to.awk, the.filter = NULL, the.variables = the.variables, include.filename = include.filename, file.header = file.header, num.files.per.batch = num.files.per.batch, return.as = return.as, envir = envir, show.warnings = show.warnings, return.data.table = return.data.table, nrows = nrows, drop = drop))
}

#' @import data.table
#' @export
filtered.fread <- function(the.files, path.to.awk = NULL, delim = ",", the.filter = NULL, the.variables = ".", include.filename = TRUE, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, and.symbol = "&", or.symbol = "|", in.symbol = "%in%", nin.symbol = "%nin%", show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...) {
  require(data.table)

  lv.name <- "last.variable"
  value.code <- "code"
  value.all <- "all"

  if (!is.logical(return.data.table)) {
    return.data.table <- TRUE
  }

  the.files <- the.files[file.exists(the.files)]

  total.files <- length(the.files)

  header.statement <- sprintf("header.dt <- fread(input = '%s', nrows = 1)", the.files[1])
  eval(expr = parse(text = header.statement))


  all.variables <- names(get("header.dt"))

  if (is.null(the.variables) | "." %in% the.variables) {
    the.variables <- all.variables
  }
  if (sum(the.variables %in% names(header.dt)) == 0) {
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
  shell.type <- Sys.getenv("R_SHELL")
  if (!nzchar(shell.type)) {
    shell.type <- Sys.getenv("COMSPEC")
  }

  use.windows <- grepl("cmd.exe", tolower(shell.type), fixed = TRUE)

  awk.filter <- translate.filtering.statement(the.filter = the.filter, the.variables = all.variables, envir = envir, and.symbol = and.symbol, or.symbol = or.symbol, in.symbol = in.symbol, nin.symbol = nin.symbol, use.windows = use.windows)

  if (use.windows) {
    string.placeholder <- '"%s"'
    statement.to.fill <- '%s -F "%s" "FNR < 2 { next }{%s print %s%s}" %s'
  } else {
    string.placeholder <- "'%s'"
    statement.to.fill <- "%s -F '%s' 'FNR < 2 { next }{%s print %s%s}' %s"
  }

  for (i in 1:num.batches) {
    pasted.file.names <- paste(sprintf(string.placeholder, the.files[((i - 1) * num.files.per.batch + 1):min(total.files, i * num.files.per.batch)]), collapse = " ")
    awk.statements[i] <- sprintf(statement.to.fill, path.to.awk, delim, awk.filter, column.names.awk, string.filename, pasted.file.names)


    if (return.as != value.code) {
      if (show.warnings == TRUE) {
        batch.data <- fread(cmd = awk.statements[i], fill = T, nrows = nrows)
      }
      if (show.warnings != TRUE) {
        suppressWarnings(batch.data <- fread(cmd = awk.statements[i], fill = T, nrows = nrows))
      }

      if (nrow(batch.data) > 0) {
        if (include.filename == FALSE) {
          names(batch.data) <- names(header.dt)[w]
        }
        if (include.filename == TRUE) {
          nc <- ncol(batch.data)
          nv <- length(w)
          if (nc > 1 + nv) {
            split.cols <- names(batch.data)[(nv + 1):nc]
            batch.data[, eval(lv.name) := get(split.cols[1])]

            for (j in 2:(nc - nv)) {
              batch.data[, eval(lv.name) := sprintf("%s %s", get(lv.name), get(split.cols[j]))]
            }

            batch.data[, (split.cols) := NULL]
          }
          names(batch.data) <- c(names(header.dt)[w], file.header)
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

  ending.values <- equation.pieces

  for (i in 1:length(equation.pieces)) {
    exists.in.envir <- tryCatch(expr = !is.null(eval(parse(text = trimws(equation.pieces[i])))), error = function(e) {
      is.null(e)
    }, finally = "hello")
    if (exists.in.envir == TRUE) {
      ending.values[i] <- eval(expr = parse(text = trimws(equation.pieces[i])), envir = envir)
    }
  }

  if (is.character(ending.values) | is.factor(ending.values)) {
    ending.values <- sprintf("'%s'", ending.values)
  }

  if (length(ending.values) == 2) {
    res <- trimws(sprintf("%s %s %s", trimws(ending.values[1]), trimws(the.symbol), trimws(ending.values[2])))
  }


  split.pieces <- trimws(strsplit(res, trimws(the.symbol))[[1]])

  for (u in 1:length(split.pieces)) {
    for (v in 1:length(the.variables)) {
      split.pieces[u] <- gsub(pattern = the.variables[v], replacement = sprintf("$%d", v), x = split.pieces[u])
    }
  }

  w <- which(split.pieces %in% sprintf("'$%s'", 1:length(the.variables)))
  split.pieces[w] <- gsub(pattern = "'", replacement = "", x = split.pieces[w])

  res <- trimws(paste(split.pieces, collapse = sprintf(" %s ", trimws(the.symbol))))

  return(res)
}

translate.in.statement.global <- function(in.statement, the.variables, in.symbol = "%in%", envir = .GlobalEnv) {
  all.characters <- strsplit(x = trimws(x = in.statement), split = "")[[1]]

  num.characters <- length(all.characters)
  num.leading.parens <- min(which(all.characters != "(")) - 1
  num.trailing.parens <- min(which(all.characters[num.characters:1] != ")")) - 1

  reduced.statement <- substring(text = trimws(x = in.statement), first = num.leading.parens + 1, last = num.characters - num.trailing.parens)

  the.pieces <- trimws(x = strsplit(x = reduced.statement, split = in.symbol)[[1]], which = "both")

  the.pieces <- gsub(pattern = "\'", replacement = "'", x = the.pieces, fixed = T)
  the.pieces <- gsub(pattern = '\"', replacement = "'", x = the.pieces, fixed = T)

  chars.the.pieces <- strsplit(x = the.pieces, split = "")

  chars.leading.parens <- lapply(X = chars.the.pieces, FUN = function(x) {
    sum(x == "(")
  })

  chars.trailing.parens <- lapply(X = chars.the.pieces, FUN = function(x) {
    sum(x == ")")
  })

  parens <- data.frame(leading = as.numeric(chars.leading.parens), trailing = as.numeric(chars.trailing.parens))

  for (i in 1:nrow(parens)) {
    if (parens$trailing[i] < parens$leading[i]) {
      the.pieces[[i]] <- sprintf("%s%s", trimws(the.pieces[[i]]), rep.int(x = ")", times = parens$leading[i] - parens$trailing[i]))
    }
  }

  rhs.values <- eval(expr = parse(text = the.pieces[2]), envir = envir)

  main.translation <- paste(sprintf("%s == '%s'", the.pieces[1], rhs.values), collapse = " || ")

  translated.statement <- sprintf("(%s)", main.translation)

  return(translated.statement)
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


pattern.fread <- function(the.files, the.patterns = NULL, tf = TRUE, path.to.awk = NULL, delim = ",", connectors = "or", the.variables = ".", include.filename = TRUE, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...) {
  require(data.table)

  and.symbols <- c("&", "&&", "and")
  or.symbols <- c("|", "||", "or")

  negation.symbols <- c("!", "not", "false", "F", "0")

  lv.name <- "last.variable"
  value.code <- "code"
  value.all <- "all"

  if (!is.logical(return.data.table)) {
    return.data.table <- TRUE
  }

  the.files <- the.files[file.exists(the.files)]

  total.files <- length(the.files)

  if (total.files == 0) {
    stop("No existing files were found.")
  }

  header.statement <- sprintf("header.dt <- fread(input = '%s', nrows = 1)", the.files[1])
  eval(expr = parse(text = header.statement))

  if (is.null(the.variables) | "." %in% the.variables) {
    the.variables <- names(header.dt)
  }
  if (sum(the.variables %in% names(header.dt)) == 0) {
    stop("No variables in the data were specified.  Double check that the names were spelled correctly.")
  }

  if (!is.null(drop)) {
    if (is.numeric(drop)) {
      drop <- names(header.dt)[drop]
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

  w <- which(names(header.dt) %in% the.variables)

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

  shell.type <- Sys.getenv("R_SHELL")
  if (!nzchar(shell.type)) {
    shell.type <- Sys.getenv("COMSPEC")
  }

  if (grepl("cmd.exe", tolower(shell.type), fixed = TRUE)) {
    use.windows <- TRUE
  } else {
    use.windows <- FALSE
  }

  if (use.windows) {
    string.placeholder <- '"%s"'
    statement.to.fill <- '%s -F "%s" "FNR < 2 { next } %s {print %s%s}" %s'
  } else {
    string.placeholder <- "'%s'"
    statement.to.fill <- "%s -F '%s' 'FNR < 2 { next } %s {print %s%s}' %s"
  }

  if (is.null(path.to.awk)) {
    path.to.awk <- "awk"
  }

  for (i in 1:num.batches) {
    pasted.file.names <- paste(sprintf(string.placeholder, the.files[((i - 1) * num.files.per.batch + 1):min(total.files, i * num.files.per.batch)]), collapse = " ")

    awk.statements[i] <- sprintf(statement.to.fill, path.to.awk, delim, awk.pattern, column.names.awk, string.filename, pasted.file.names)

    if (return.as != value.code) {
      if (show.warnings == TRUE) {
        batch.data <- fread(cmd = awk.statements[i], fill = T, nrows = nrows)
      }
      if (show.warnings != TRUE) {
        suppressWarnings(batch.data <- fread(cmd = awk.statements[i], fill = T, nrows = nrows))
      }

      if (nrow(batch.data) > 0) {
        if (include.filename == FALSE) {
          names(batch.data) <- names(header.dt)[w]
        }

        if (include.filename == TRUE) {
          nc <- ncol(batch.data)
          nv <- length(w)
          if (nc > 1 + nv) {
            split.cols <- names(batch.data)[(nv + 1):nc]
            batch.data[, eval(lv.name) := get(split.cols[1])]

            for (j in 2:(nc - nv)) {
              batch.data[, eval(lv.name) := sprintf("%s %s", get(lv.name), get(split.cols[j]))]
            }

            batch.data[, (split.cols) := NULL]
          }
          names(batch.data) <- c(names(header.dt)[w], file.header)
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
