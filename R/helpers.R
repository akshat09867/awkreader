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
