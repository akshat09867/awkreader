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
