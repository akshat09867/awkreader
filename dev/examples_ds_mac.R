# Set working directory to the source file's location.

# rm(list=ls())

# setwd("c://users/jliv/downloads")
source("R/awkreader_v2.R")

## Constants


all.files <- list.files(path = "Data/ratings data", full.names = T)

the.files <- all.files[1:2]



the.filter <- NULL
the.variables <- "."
include.filename <- T
file.header <- "file"
envir <- .GlobalEnv
and.symbol <- "&"
or.symbol <- "|"
in.symbol <- "%in%"
nin.symbol <- "%nin%"
equation.symbols <- c(">", ">=", "<", "<=", "!=", "==")
num.files.per.batch <- 1000
tf <- TRUE
return.as <- "result"
show.warnings <- FALSE
nrows <- Inf
return.data.table <- TRUE
drop <- NULL
path.to.awk <- NULL
## Variables for pattern.fread
negations <- FALSE
connectors <- "or"


## Read in a combined data set from all of the files.  Note that this includes a column called file to provide the source of each row.
r1 <- combined.fread(
  path.to.awk = path.to.awk,
  the.files = the.files
)
print(head(r1))
# Equivalently
r2 <- filtered.fread(
  the.files = the.files, the.filter = NULL,
  path.to.awk = path.to.awk
)
print(head(r2))
# Read in only the first 10 rows
r3 <- filtered.fread(
  the.files = the.files, nrows = 10,
  path.to.awk = path.to.awk
)
print(head(r3))


# Note that combined.fread is a wrapper of filtered.fread that specifically sets the.filter = NULL.

## Only show the code for reading the files by setting return.as = "code"
r4 <- filtered.fread(
  the.files = the.files, return.as = "code",
  path.to.awk = path.to.awk
)
print(head(r4))
## Show both the code and the results by setting return.as = "all"
r5 <- filtered.fread(
  the.files = the.files, return.as = "all",
  path.to.awk = path.to.awk
)
print(head(r5))


## Change the name of the file column to any name you want:
r6 <- filtered.fread(
  the.files = the.files, file.header = "the source",
  path.to.awk = path.to.awk
)
print(head(r6))

## Don't include the name of the file
r7 <- filtered.fread(
  the.files = the.files, include.filename = F,
  path.to.awk = path.to.awk
)
print(head(r7))

## Write filtering language in R's syntax
the.filter <- "rating == 5"
r8 <- filtered.fread(
  the.files = the.files, the.filter = the.filter,
  path.to.awk = path.to.awk
)
print(head(r8))

## Pick the variables
r9 <- filtered.fread(
  the.files = the.files, the.filter = the.filter,
  the.variables = c("user", "rating"), include.filename = F,
  path.to.awk = path.to.awk
)
print(head(r9))


## Demonstrates the & operator.
r10 <- filtered.fread(
  the.files = the.files, the.filter = "item == '0JFCjVx2P1RMzy3h'",
  path.to.awk = path.to.awk,
  return.as = "all",
)
print(head(r10))
# Note that the outside quotes should not match the inside quotes.
the.filter <- "rating >= 3 & item == '0JFCjVx2P1RMzy3h'"

r11 <- filtered.fread(
  the.files = the.files, the.filter = the.filter,
  path.to.awk = path.to.awk, return.as = "all"
)
print(head(r11))

## Demonstrates the | operator
r12 <- filtered.fread(
  the.files = the.files, the.filter = "rating == 3 | rating == 4",
  path.to.awk = path.to.awk, return.as = "all"
)
print(head(r12))

## Demonstrates the %in% operator
r13 <- filtered.fread(
  the.files = the.files, the.filter = 'item %in% c("0JFCjVx2P1RMzy3h", "1fg4sLgEFzAtOqCa")', return.as = "all",
  path.to.awk = path.to.awk,
)
print(head(r13))

## Demonstrates the %in% operator
r14 <- filtered.fread(
  the.files = the.files, the.filter = 'item %in% c("0JFCjVx2P1RMzy3h", "1fg4sLgEFzAtOqCa")', return.as = "all",
  path.to.awk = path.to.awk,
)
print(head(r14))

## Demonstrates the %in% operator with other variables defined in R.
r15 <- filtered.fread(
  the.files = the.files, the.filter = 'rating >= 5 & item =="0JFCjVx2P1RMzy3h" | item == "1fg4sLgEFzAtOqCa" ',
  path.to.awk = path.to.awk,
)
print(head(r15))

## Demonstrates the %in% operator with other variables defined in R.
r16 <- filtered.fread(
  the.files = the.files, the.filter = 'rating >= 5 & item %in% c("0JFCjVx2P1RMzy3h","1fg4sLgEFzAtOqCa")',
  path.to.awk = path.to.awk,
)
print(head(r16))

## Pass filtering statements involving other variables defined in R.
two.items <- c("0kG80toKp2msfAut", "0JFCjVx2P1RMzy3h", "1fg4sLgEFzAtOqCa")
r17 <- filtered.fread(
  the.files = the.files, the.filter = "rating >= 3 & item %in% two.items", return.as = "all",
  path.to.awk = path.to.awk,
)
print(head(r17))

## Demonstrates the %nin% operator
the.filter <- "rating %nin% c(1, 4)"
r18 <- filtered.fread(
  the.files = the.files, the.filter = the.filter,
  path.to.awk = path.to.awk, return.as = "all"
)
print(head(r18))
## Read all variables with "."
r19 <- filtered.fread(
  the.files = the.files, the.filter = "rating >= 3 & item %in% two.items", the.variables = ".", include.filename = F,
  path.to.awk = path.to.awk,
)
print(head(r19))

## Read the data in batches of size 100 and then combine.
# Note that the batches are only required if the length of the AWK coding statement is too long.
# Note that show.warnings = F will use suppressWarnings() to remove warning statements.
# Any batch of files with no cases matching the.filter's inclusion criteria would otherwise generate a warning message.

r20 <- filtered.fread(
  the.files = all.files, the.filter = "rating >= 4 & item %in% two.items", include.filename = T, num.files.per.batch = 100, show.warnings = F,
  path.to.awk = path.to.awk
)
print(head(r20))

# Note that these are using variables in R to define the.filter.  The program will locate these variables and translate to their values (e.g. "6opmPfANUHJH121e") for using in reading and filtering the data.
# filtered.fread(the.files = the.files, the.filter = "rating >= 4 & item == two.items[1]",
#               path.to.awk=path.to.awk)
# FAILED: STATEMENT CAN BE TOO LONG ON WINDOWS CMD. HERE LIMIT IS ~150 FILES, MUST USE num.files.per.batch


r21 <- filtered.fread(
  the.files = all.files, the.filter = "rating >= 4 & item %in% two.items",
  include.filename = T, num.files.per.batch = 100, show.warnings = F, return.as = "all",
  path.to.awk = path.to.awk
)
print(head(r21))

########## Testing pattern.fread

## Matching any row with a pattern

r22 <- pattern.fread(
  the.files = the.files, the.patterns = c("fg4sLg", "Vx2P"),
  path.to.awk = path.to.awk, return.as = "all"
)
print(head(r22))

r23 <- pattern.fread(the.files = the.files, the.patterns = c("0JFCjVx"), tf = c(F), return.as = "result")
print(r23)

r24 <- pattern.fread(the.files = the.files, the.patterns = c("0JFCjVx", "Bji5PQ"), tf = c(F, T), return.as = "result")
print(r24)

r25 <- pattern.fread(the.files = the.files, the.patterns = c("JFCjVx2", "RMz"), connectors = "and", return.as = "result")
print(r25)

r26 <- pattern.fread(the.files = the.files, the.patterns = c("JFCjVx2", "D4wo3"), connectors = c("or"), include.filename = F, show.warnings = F)
print(r26)

r27 <- pattern.fread(the.files = "Data/Titanic.csv", the.patterns = c("Female", "Child", "1st"), tf = c(T, T, F), connectors = "and")
print(r27)
