#' Global Scope Evaluation Framework for Membership Vectors
#'
#' @description Evaluates membership variables that point directly to larger active environment structures
#' (like global character vectors) rather than internal row variables, building optimized inline conditional maps.
#'
#' @param in.statement A character expression fragment mapping lookup requirements.
#' @param the.variables A character vector matching the positional target mapping structure.
#' @param in.symbol The specific string structure pattern to catch.
#' @param envir The active R runtime environment containing the target variable content array. Default is \code{.GlobalEnv}.
#'
#' @return A resolved logical text mapping line compatible with text extraction shell commands.
#' @keywords internal
translate.in.statement.global <- function(in.statement, the.variables, in.symbol, envir = .GlobalEnv) {
  pieces <- trimws(strsplit(in.statement, in.symbol)[[1]])
  left.side <- pieces[1]
  right.side <- pieces[2]

  target.vector <- eval(parse(text = right.side), envir = envir)

  is.numeric.vector <- is.numeric(target.vector)
  if (!is.numeric.vector) {
    target.vector <- sprintf('"%s"', target.vector)
  }

  awk.var <- left.side
  for (v in 1:length(the.variables)) {
    escaped.var <- gsub("([][\\\\.|(){}^$+*?-])", "\\\\\\1", the.variables[v])
    if (grepl(pattern = paste0("\\b", escaped.var, "\\b"), x = left.side)) {
      awk.var <- sprintf("$%d", v)
      break
    }
  }
  if (in.symbol == "%in%") {
    awk.conditions <- paste(awk.var, "==", target.vector)
    compiled.awk <- paste0("(", paste(awk.conditions, collapse = " || "), ")")
  } else if (in.symbol == "%nin%") {
    awk.conditions <- paste(awk.var, "!=", target.vector)
    compiled.awk <- paste0("(", paste(awk.conditions, collapse = " && "), ")")
  }

  return(compiled.awk)
}


check.awk.availability <- function() {
  awk_path <- Sys.which("awk")
  if (awk_path == "") {
    stop(
      "AWK interpreter not found on your system PATH.\n",
      "Please install AWK or ensure it is accessible.\n",
      "- Windows users: Install Rtools or add Git Bash to your system PATH.\n",
      "- Mac/Linux users: Install via your default system package manager.",
      call. = FALSE
    )
  }
  return(awk_path)
}


#' Intelligent Auto-Detection for System AWK Binary
#'
#' @description An internal helper that searches the system environment and common
#' Windows installation paths (Rtools, Git Bash) to find a working AWK interpreter.
#'
#' @return A character string representing the absolute path to the AWK binary.
#' @keywords internal
find.awk.binary <- function() {
  system_path <- Sys.which("awk")
  if (system_path != "") {
    return(unname(system_path))
  }
  if (.Platform$OS.type == "windows") {
    potential_dirs <- c(
      list.files("C:/", pattern = "^rtools[0-9]*", full.names = TRUE),
      "C:/Rtools",
      "C:/Program Files/Git",
      "C:/Program Files (x86)/Git"
    )
    possible_bin_paths <- c(
      paste0(potential_dirs, "/usr/bin/awk.exe"),
      paste0(potential_dirs, "/bin/awk.exe")
    )
    valid_paths <- possible_bin_paths[file.exists(possible_bin_paths)]
    if (length(valid_paths) > 0) {
      return(normalizePath(valid_paths[1]))
    }
  }
  stop(
    "AWK interpreter could not be automatically detected on your system PATH.\n",
    "Please ensure Rtools or Git is installed, or supply an explicit path.",
    call. = FALSE
  )
}

#' Execute AWK Script over Batches of Files (Internal Engine)
#'
#' An internal helper function that splits file processing into batches, constructs
#' systemic shell commands to invoke AWK, and streams the processed text strings
#' back into R via \code{data.table::fread}.
#'
#' @param awk.script.content A character string containing the raw body of the AWK script logic.
#' @param the.files A character vector of normalized paths to the target files.
#' @param value.code A character string indicating the identifier for code-only return mode.
#' @param header.names A character vector of column names to assign to the resulting dataset.
#' @param include.filename A logical value indicating whether to append the source filename tracking column.
#' @param num.batches An integer specifying the total number of batches to chunk files into.
#' @param num.files.per.batch An integer specifying the maximum number of files processed per AWK execution window.
#' @param path.to.awk A character string designating the system path or command name for the AWK binary.
#' @param total.files An integer tracking the total count of valid files to process.
#' @param show.warnings A logical value. If \code{FALSE}, wraps the internal engine reading in \code{suppressWarnings}.
#' @param nrows A numeric value restricting the maximum number of rows to read per batch chunk.
#' @param file.header A character string establishing the column header name for file origin logging.
#' @param return.as A character string controlling the return type format (\code{"result"}, \code{"code"}, or \code{"all"}).
#'
#' @return A named list containing two elements:
#' \item{list.data}{A list of data tables containing parsed chunk outputs.}
#' \item{expanded.statements}{A character vector containing the raw shell strings passed to the system command pipeline.}
#'
#' @importFrom data.table fread setnames
#' @keywords internal
#' Execute AWK Script over Batches of Files (Internal Engine)
#'
#' An internal helper function that splits file processing into batches, constructs
#' systemic shell commands to invoke AWK, and streams the processed text strings
#' back into R via \code{data.table::fread}.
#'
#' @param awk.script.content A character string containing the raw body of the AWK script logic.
#' @param the.files A character vector of normalized paths to the target files.
#' @param value.code A character string indicating the identifier for code-only return mode.
#' @param header.names A character vector of column names to assign to the resulting dataset.
#' @param include.filename A logical value indicating whether to append the source filename tracking column.
#' @param num.batches An integer specifying the total number of batches to chunk files into.
#' @param num.files.per.batch An integer specifying the maximum number of files processed per AWK execution window.
#' @param path.to.awk A character string designating the system path or command name for the AWK binary.
#' @param total.files An integer tracking the total count of valid files to process.
#' @param show.warnings A logical value. If \code{FALSE}, wraps the internal engine reading in \code{suppressWarnings}.
#' @param nrows A numeric value restricting the maximum number of rows to read per batch chunk.
#' @param file.header A character string establishing the column header name for file origin logging.
#' @param return.as A character string controlling the return type format (\code{"result"}, \code{"code"}, or \code{"all"}).
#'
#' @return A named list containing two elements:
#' \item{list.data}{A list of data tables containing parsed chunk outputs.}
#' \item{expanded.statements}{A character vector containing the raw shell strings passed to the system command pipeline.}
#'
#' @importFrom data.table fread setnames
#' @keywords internal
#' Execute AWK Script over Batches of Files (Internal Engine)
#'
#' An internal helper function that splits file processing into batches, constructs
#' systemic shell commands to invoke AWK, and streams the processed text strings
#' back into R via \code{data.table::fread}.
#'
#' @param awk.script.content A character string containing the raw body of the AWK script logic.
#' @param the.files A character vector of normalized paths to the target files.
#' @param value.code A character string indicating the identifier for code-only return mode.
#' @param header.names A character vector of column names to assign to the resulting dataset.
#' @param include.filename A logical value indicating whether to append the source filename tracking column.
#' @param num.batches An integer specifying the total number of batches to chunk files into.
#' @param num.files.per.batch An integer specifying the maximum number of files processed per AWK execution window.
#' @param path.to.awk A character string designating the system path or command name for the AWK binary.
#' @param total.files An integer tracking the total count of valid files to process.
#' @param show.warnings A logical value. If \code{FALSE}, wraps the internal engine reading in \code{suppressWarnings}.
#' @param nrows A numeric value restricting the maximum number of rows to read per batch chunk.
#' @param file.header A character string establishing the column header name for file origin logging.
#' @param return.as A character string controlling the return type format (\code{"result"}, \code{"code"}, or \code{"all"}).
#'
#' @return A named list containing two elements:
#' \item{list.data}{A list of data tables containing parsed chunk outputs.}
#' \item{expanded.statements}{A character vector containing the raw shell strings passed to the system command pipeline.}
#'
#' @importFrom data.table fread setnames
#' @keywords internal
execute.awk.stream <- function(awk.script.content, the.files, value.code, header.names, include.filename,
                                num.batches, num.files.per.batch, path.to.awk, total.files, show.warnings,
                                nrows, file.header, return.as) {

  if (is.null(path.to.awk) || length(path.to.awk) == 0 || !nzchar(path.to.awk) || !file.exists(path.to.awk)) {
    stop(sprintf(
      "AWK binary not found or invalid ('%s'). Run find.awk.binary()/check.awk.availability() first.",
      if (is.null(path.to.awk) || length(path.to.awk) == 0) "NULL" else path.to.awk
    ), call. = FALSE)
  }

  is.windows <- .Platform$OS.type == "windows"

  # fread(cmd=) uses shell() on Windows, which prefers R_SHELL/SHELL over
  # COMSPEC if either is set. Force COMSPEC (cmd.exe) for this call, since
  # the quoting below is built specifically for cmd.exe. See ?shell.
  if (is.windows) {
    old.shell  <- Sys.getenv("SHELL",   unset = NA)
    old.rshell <- Sys.getenv("R_SHELL", unset = NA)
    Sys.unsetenv("SHELL")
    Sys.unsetenv("R_SHELL")
    on.exit({
      if (!is.na(old.shell))  Sys.setenv(SHELL = old.shell)
      if (!is.na(old.rshell)) Sys.setenv(R_SHELL = old.rshell)
    }, add = TRUE)
  }

  awk.statements <- character(length = num.batches)
  expanded.statements <- character(length = num.batches)
  list.data <- list()

  temp.script <- tempfile(fileext = ".awk")
  writeLines(awk.script.content, con = temp.script)
  on.exit(unlink(temp.script), add = TRUE)

  norm.awk.path    <- normalizePath(path.to.awk, mustWork = TRUE)
  norm.temp.script <- normalizePath(temp.script,  mustWork = TRUE)

  if (is.windows) {
    safe.awk.path    <- shQuote(norm.awk.path, type = "cmd")
    safe.temp.script <- shQuote(norm.temp.script, type = "cmd")
  } else {
    safe.awk.path    <- shQuote(norm.awk.path)
    safe.temp.script <- shQuote(norm.temp.script)
  }

  for (i in seq_len(num.batches)) {
    batch.files <- the.files[((i - 1) * num.files.per.batch + 1):min(total.files, i * num.files.per.batch)]
    norm.batch.files <- normalizePath(batch.files, mustWork = TRUE)

    safe.batch.files <- if (is.windows) {
      shQuote(norm.batch.files, type = "cmd")
    } else {
      shQuote(norm.batch.files)
    }
    pasted.file.names <- paste(safe.batch.files, collapse = " ")

    inner.cmd <- sprintf("%s -f %s %s", safe.awk.path, safe.temp.script, pasted.file.names)

    awk.statements[i] <- if (is.windows) shQuote(inner.cmd, type = "cmd2") else inner.cmd

    expanded.statements[i] <- sprintf("%s -f '%s' %s", norm.awk.path, awk.script.content,
                                       paste(norm.batch.files, collapse = " "))

    if (return.as != value.code) {
      run.fread <- function() {
        fread(cmd = awk.statements[i], fill = TRUE, nrows = nrows, header = FALSE, sep = ",")
      }
      batch.data <- tryCatch(
        if (show.warnings) run.fread() else suppressWarnings(run.fread()),
        error = function(e) {
          stop(sprintf("AWK command failed.\nCommand: %s\nOriginal error: %s",
                        awk.statements[i], conditionMessage(e)), call. = FALSE)
        }
      )
      if (nrow(batch.data) > 0) {
        if (!include.filename) {
          setnames(batch.data, header.names)
        } else {
          setnames(batch.data, c(header.names, file.header))
        }
      }
      list.data[[i]] <- batch.data
    }
  }

  return(list(list.data = list.data, expanded.statements = expanded.statements))
}

#' Resolve and Filter Target File Paths
#'
#' Internal helper to resolve directory paths, glob patterns, and file vectors
#' into a clean, deduplicated vector of validated file paths.
#'
#' @param the.files Character vector. File paths, directory paths, or glob patterns.
#' @param file.pattern Optional character string to filter file names when directories
#'   are processed. Accepts simple extensions (e.g., \code{"csv"} or \code{".csv"}),
#'   wildcards (e.g., \code{"*.csv"}), or regular expressions (e.g., \code{"\\.csv$"}).
#'   Default is \code{NULL}.
#' @param recursive Logical. Should directory searches recurse into subdirectories?
#'   Default is \code{FALSE}.
#'
#' @return A character vector of unique, normalized, verified existing file paths.
#'
#' @importFrom utils glob2rx
#' @noRd
.resolve.files <- function(the.files, file.pattern = NULL, recursive = FALSE) {
  expanded.files <- Sys.glob(the.files)

  if (length(expanded.files) == 0) {
    stop("No existing files or directories matched 'the.files'.")
  }

  if (!is.null(file.pattern) && nzchar(file.pattern)) {
    if (grepl("\\*", file.pattern)) {
      file.pattern <- glob2rx(file.pattern)
    } else if (!grepl("[][!^$+|()]", file.pattern)) {
      clean_ext <- gsub("^\\.", "", file.pattern)
      file.pattern <- sprintf("\\.%s$", clean_ext)
    }
  }

  is_dir <- dir.exists(expanded.files)
  dir.paths <- expanded.files[is_dir]
  file.paths <- expanded.files[!is_dir]

  discovered.files <- character(0)
  if (length(dir.paths) > 0) {
    discovered.files <- unlist(lapply(dir.paths, function(d) {
      list.files(
        path = d,
        pattern = file.pattern,
        full.names = TRUE,
        recursive = recursive,
        no.. = TRUE
      )
    }))
  }

  if (length(file.paths) > 0 && !is.null(file.pattern) && nzchar(file.pattern)) {
    file.paths <- file.paths[grepl(file.pattern, basename(file.paths))]
  }

  all.files <- c(file.paths, discovered.files)
  all.files <- normalizePath(all.files, mustWork = FALSE)
  the.files <- unique(all.files[file.exists(all.files) & !dir.exists(all.files)])

  if (length(the.files) == 0) {
    stop("No valid files were found matching the specified criteria.")
  }

  return(the.files)
}
