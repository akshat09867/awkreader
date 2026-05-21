library(microbenchmark)
library(data.table)

# 1. Setup: Create a 10,000 row mock file
tmp_file <- tempfile(fileext = ".csv")
header <- "user,item,rating"
rows <- paste(1:10000, "item_id", 5, sep = ",")
writeLines(c(header, rows), tmp_file)

# Old way: data.table overhead
old_method <- function(file) {
  header.dt <- fread(input = file, nrows = 1)
  all.vars <- names(header.dt)
  return(all.vars)
}

# New way: base R connection
new_method <- function(file) {
  con <- file(file, "r")
  header_line <- readLines(con, n = 1)
  close(con)
  all.vars <- unlist(strsplit(header_line, split = ",", fixed = TRUE))
  return(all.vars)
}

# 3. Run the benchmark
results <- microbenchmark(
  old = old_method(tmp_file),
  new = new_method(tmp_file),
  times = 100
)

print(results)
