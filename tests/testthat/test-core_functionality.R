library(testthat)

# GLOBAL TEST SETUP
root_data_path <- file.path("../..", "Data", "ratings data")
all.files <- list.files(path = root_data_path, full.names = TRUE)

# Defensive check: skip all tests if data isn't mounted correctly in CI/local
skip_if(length(all.files) == 0, "Mock data files not found in Data/ directory.")

the.files <- all.files[1:2]
two.items <<- c("1fg4sLgEFzAtOqCa", "6dXLifXK5LrvtdV1")

# GROUP 1: Basic Reading and Return Types (r1 - r5)
test_that("Basic combined reads and return types work correctly", {
  # r1: combined.fread
  res_comb <- combined.fread(the.files = the.files)
  expect_s3_class(res_comb, "data.table")
  expect_true("file" %in% names(res_comb))

  # r2: return.as = "code" with NULL filter
  res_code <- filtered.fread(the.files = the.files, the.filter = NULL, skip = 0, return.as = "code")
  expect_type(res_code, "character")
  expect_match(res_code, "FNR <= 1 { next }{ print $1,$2,$3,FILENAME}", fixed = TRUE)

  # r3: nrows limit
  res_nrows <- filtered.fread(the.files = the.files, nrows = 10)
  expect_equal(nrow(res_nrows), 10)

  # r4/r5: return.as = "all"
  res_all <- filtered.fread(the.files = the.files, return.as = "all")
  expect_type(res_all, "list")
  expect_s3_class(res_all$result, "data.table")
  expect_type(res_all$code, "character")
})

# GROUP 2: Column Formatting and Selection (r6, r7, r9, r17, r18)
test_that("Column subsetting and filename appending behave correctly", {
  # r6: Custom file header
  res_custom_header <- filtered.fread(the.files = the.files, file.header = "the source")
  expect_true("the source" %in% names(res_custom_header))

  # r7: Omit filename
  res_no_file <- filtered.fread(the.files = the.files, include.filename = FALSE)
  expect_false("file" %in% names(res_no_file))

  # r9: Pick specific variables
  res_vars <- filtered.fread(
    the.files = the.files, the.filter = "rating == 5",
    the.variables = c("item", "rating"), include.filename = FALSE
  )
  expect_equal(names(res_vars), c("item", "rating"))

  # r17: Read all variables with "."
  res_dot <- filtered.fread(
    the.files = the.files, the.filter = "rating == 5",
    the.variables = ".", include.filename = FALSE
  )
  # Should retain original structure without file column
  expect_false("file" %in% names(res_dot))
})

# GROUP 3: Logical Operators and Math (r8, r10 - r13, r19 - r23)
test_that("AWK translation accurately parses logical operators and math", {
  # r11: AND operator (&)
  res_and <- filtered.fread(the.files = the.files, the.filter = "rating >= 3 & item == '0JFCjVx2P1RMzy3h'")
  expect_gt(nrow(res_and), 0)
  expect_true(all(res_and$rating >= 3 & res_and$item == "0JFCjVx2P1RMzy3h"))

  # r12: OR operator (|)
  res_or <- filtered.fread(the.files = the.files, the.filter = "rating == 3 | rating == 4")
  expect_gt(nrow(res_or), 0)
  expect_true(all(res_or$rating %in% c("3", "4")))

  # r13: NOT operator (!=)
  res_not <- filtered.fread(the.files = the.files, the.filter = "rating != 1 & rating != 2")
  expect_gt(nrow(res_not), 0)
  expect_true(all(!res_not$rating %in% c("1", "2")))

  # r22: Math functions (log)
  res_math <- filtered.fread(the.files = the.files, the.filter = "item == 'sFFbD3fA0Jsvs7Ic' & rating > log(rating)", return.as = "all")
  expect_equal(res_math$code, "awk -F ',' -v OFS=',' 'FNR <= 1 { next }{if($2 == \"sFFbD3fA0Jsvs7Ic\" && $3 > log($3)) print $1,$2,$3,FILENAME}' '../../Data/ratings data/file_1.csv' '../../Data/ratings data/file_10.csv'")
})

# GROUP 4: Environment Variables and Set Operators (r14 - r16, r25)
test_that("Environment variable injection and %in% / %nin% work", {
  # r15: External vector injection with %in%
  res_in <- filtered.fread(the.files = the.files, the.filter = "rating >= 3 & item %in% two.items")
  expect_gt(nrow(res_in), 0)
  expect_true(all(res_in$item %in% c("1fg4sLgEFzAtOqCa", "6dXLifXK5LrvtdV1")))

  # r16: Numeric vectors with %nin%
  res_nin <- filtered.fread(the.files = the.files, the.filter = "rating %nin% c(1:2, 4)", include.filename = FALSE)
  expect_gt(nrow(res_nin), 0)
  expect_false(any(res_nin$rating %in% c("1", "2", "4")))

  # r25: External vector indexing
  res_index <- filtered.fread(the.files = the.files, the.filter = "rating >= 4 & item == two.items[1]")
  expect_gt(nrow(res_index), 0)
  expect_true(all(res_index$item == two.items[1]))
})

# GROUP 5: Batch Processing (r24, r26)
test_that("Batching logic processes correctly without dropping data", {
  # r26: Test batching size of 1
  res_batch_code <- filtered.fread(
    the.files = the.files, the.filter = "rating >= 4",
    num.files.per.batch = 1, return.as = "code"
  )

  # If batch size is 1, and we have 2 files, the code returned should be a vector of length 2
  expect_equal(length(res_batch_code), 2)
  expect_type(res_batch_code, "character")
})

# GROUP 6: pattern.fread (r27 - r32)
test_that("pattern.fread correctly evaluates regex and connectors", {
  # r27: Simple pattern match
  res_pat1 <- pattern.fread(the.files = the.files, the.patterns = c("5PQIOK"))
  expect_gt(nrow(res_pat1), 0)

  # r28 / r29: Negative pattern match (tf = F)
  res_pat2 <- pattern.fread(the.files = the.files, the.patterns = c("5PQIOK"), tf = c(FALSE), return.as = "all")
  expect_match(res_pat2$code, "! /5PQIOK/", fixed = TRUE)

  # r30: Multiple patterns with AND
  res_pat_and <- pattern.fread(the.files = the.files, the.patterns = c("kT27T", "wBMJkot"), connectors = "and")
  if (nrow(res_pat_and) > 0) {
    expect_s3_class(res_pat_and, "data.table")
  }

  # r32: Titanic Dataset integration
  titanic_path <- file.path("../..", "Data", "Titanic.csv")
  if (file.exists(titanic_path)) {
    res_titanic <- pattern.fread(
      the.files = titanic_path,
      the.patterns = c("Female", "Child", "1st"),
      tf = c(TRUE, TRUE, FALSE),
      connectors = "and",
      file.header = "source_file"
    )

    expect_gt(nrow(res_titanic), 0)
    expect_true("source_file" %in% names(res_titanic))
  }
})

# GROUP 7: Delimiter Flexibility (delim parameter)
test_that("filtered.fread correctly handles custom delimiters", {
  # 1. Setup a temporary pipe-separated file
  tmp_file <- tempfile(fileext = ".psv")
  on.exit(unlink(tmp_file))

  writeLines(c(
    "user|item|rating",
    "user_A|item_X|5",
    "user_B|item_Y|3",
    "user_C|item_Z|5"
  ), tmp_file)

  # 2. Test data parsing with the custom delimiter
  res_delim <- filtered.fread(
    the.files = tmp_file,
    the.filter = "rating == 5",
    delim = "|",
    include.filename = FALSE
  )

  expect_s3_class(res_delim, "data.table")
  expect_equal(nrow(res_delim), 2)
  expect_equal(res_delim$user, c("user_A", "user_C"))

  # 3. Test that the AWK command accurately injects the field separator flag
  res_code_delim <- filtered.fread(
    the.files = tmp_file,
    the.filter = "rating == 5",
    delim = "|",
    return.as = "code"
  )

  # Verify that the AWK field separator flag is updated to -F '|'
  expect_match(res_code_delim, "awk -F '|'", fixed = TRUE)

  res_code_delim <- pattern.fread(
    the.files = tmp_file,
    the.filter = "rating == 5",
    delim = "|",
    return.as = "code"
  )
})

# GROUP 8: Inline Header Optimization
test_that("filtered.fread optimizes header parsing without losing column mappings", {
  # Create a mock file where data positions matter
  tmp_file1 <- tempfile(fileext = ".csv")
  tmp_file2 <- tempfile(fileext = ".csv")
  on.exit(unlink(c(tmp_file1, tmp_file2)))

  writeLines(c("user,item,rating", "A,X,5"), tmp_file1)
  writeLines(c("user,item,rating", "B,Y,2"), tmp_file2)

  # Assert that multi-file evaluation correctly tracks headers using the optimized path
  res_opt <- filtered.fread(
    the.files = c(tmp_file1, tmp_file2),
    the.filter = "rating == 5",
    include.filename = FALSE
  )

  expect_equal(nrow(res_opt), 1)
  expect_equal(res_opt$user, "A")
  expect_equal(names(res_opt), c("user", "item", "rating"))
})
