# Set working directory to the source file's location.
source("R/awkreader_v2.R")

# rm(list = ls())
## Constants

all.files <- list.files(path = "Data/ratings data", full.names = T)
the.files <- all.files[1:2]

path.to.awk <- NULL
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

## Variables for pattern.fread
negations <- FALSE
connectors <- "or"
two.items <- c("1fg4sLgEFzAtOqCa", "6dXLifXK5LrvtdV1")

print(length(the.files))
print(the.files)
## Read in a combined data set from all of the files.  Note that this includes a column called file to provide the source of each row.
r1 <- combined.fread(the.files = the.files)
print(head(r1))
## Only show the code for reading the files by setting return.as = "code"
r2 <- filtered.fread(the.files = the.files, the.filter = NULL, return.as = "code")
print(head(r2))
# Read in only the first 10 rows
r3 <- filtered.fread(the.files = the.files, nrows = 10)
print(head(r3))
# Note that combined.fread is a wrapper of filtered.fread that specifically sets the.filter = NULL.

## Show both the code and the results by setting return.as = "all"
r4 <- filtered.fread(the.files = the.files, return.as = "all")
print(head(r4))

## Show both the code and the results by setting return.as = "all"
r5 <- filtered.fread(the.files = the.files, return.as = "all")
print(head(r5))

## Change the name of the file column to any name you want:
r6 <- filtered.fread(the.files = the.files, file.header = "the source")
print(head(r6))
## Don't include the name of the file
r7 <- filtered.fread(the.files = the.files, include.filename = F)
print(head(r7))
## Write filtering language in R's syntax
r8 <- filtered.fread(the.files = the.files, the.filter = "rating == 5", return.as = "all")
print(head(r8))
# Pick the variables
r9 <- filtered.fread(the.files = the.files, the.filter = "rating == 5", the.variables = c("item", "rating"), include.filename = F)
print(head(r9))

r10 <- filtered.fread(the.files = the.files, the.filter = "item == 'nJQPOMSy5GLvp7vG'", return.as = "all")
print(head(r10))
# Demonstrates the & operator.
# Note that the outside quotes should not match the inside quotes.
r11 <- filtered.fread(the.files = the.files, the.filter = 'rating >= 3 & item == "0JFCjVx2P1RMzy3h"', return.as = "all")
print(head(r11))

# Demonstrates the | operator
r12 <- filtered.fread(the.files = the.files, the.filter = "rating == 3 | rating == 4", include.filename = F)
print(head(r12))
# Demonstrates the ! operator
r13 <- filtered.fread(the.files = the.files, the.filter = "rating != 1 & rating != 2 & rating != 3 & rating != 4")
print(head(r13))

# Demonstrates the %in% operator
r14 <- filtered.fread(the.files = the.files, the.filter = 'rating == 5 & item %in% c("1fg4sLgEFzAtOqCa", "6dXLifXK5LrvtdV1")', return.as = "all")
print(head(r14))
# Pass filtering statements involving other variables defined in R.
r15 <- filtered.fread(the.files = the.files, the.filter = "rating >= 3 & item %in% two.items", return.as = "all")
print(head(r15))
# Demonstrates the %nin% operator
r16 <- filtered.fread(the.files = the.files, the.filter = "rating %nin% c(1:2, 4)", return.as = "all", include.filename = F)
print(head(r16))
# Read all variables with "."
r17 <- filtered.fread(the.files = the.files, the.filter = "rating >= 3 & item %in% two.items", the.variables = ".", include.filename = F)
print(head(r17))

# Read only a subset of the variables
r18 <- filtered.fread(the.files = the.files, the.filter = "rating >= 3 & item %in% two.items", the.variables = c("user", "rating"), include.filename = F)
print(head(r18))

r19 <- filtered.fread(the.files = the.files, the.filter = "user < item & rating == 4", return.as = "all", include.filename = F)
print(head(r19))
r20 <- filtered.fread(the.files = the.files, the.filter = "user > item & rating == 4", return.as = "all", include.filename = F)
print(head(r20))
r21 <- filtered.fread(the.files = the.files, the.filter = "rating == 3 & rating == rating & item == item", return.as = "all", include.filename = F)
print(head(r21))
r22 <- filtered.fread(the.files = the.files, the.filter = "item == 'sFFbD3fA0Jsvs7Ic' & rating > log(rating)", return.as = "all", include.filename = F)
print(head(r22))

r23 <- filtered.fread(the.files = the.files, the.filter = "rating == 4", return.as = "all", include.filename = F)
print(head(r23))

# Read the data in batches of size 100 and then combine.  Note that the batches are only required if the length of the AWK coding statement is too long.  Note that show.warnings = F will use suppressWarnings() to remove warning statements.  Any batch of files with no cases matching the.filter's inclusion criteria would otherwise generate a warning message.

r24 <- filtered.fread(the.files = all.files, the.filter = "rating >= 4 & item %in% two.items", include.filename = T, num.files.per.batch = 100, show.warnings = F)
print(head(r24))

# Note that these are using variables in R to define the.filter.  The program will locate these variables and translate to their values (e.g. "6opmPfANUHJH121e") for using in reading and filtering the data.
r25 <- filtered.fread(the.files = all.files, the.filter = "rating >= 4 & item == two.items[1]")
print(head(r25))

r26 <- filtered.fread(the.files = the.files, the.filter = "rating >= 4 & item %in% two.items", include.filename = T, num.files.per.batch = 1, show.warnings = F, return.as = "code")
print(head(r26))

########## Testing pattern.fread

## Matching any row with a pattern

r27 <- pattern.fread(the.files = the.files, the.patterns = c("5PQIOK"))
print(head(r27))

r28 <- pattern.fread(the.files = the.files, the.patterns = c("5PQIOK"), tf = c(F), return.as = "result")
print(head(r28))
r29 <- pattern.fread(the.files = the.files, the.patterns = c("5PQIOK"), tf = c(F), return.as = "code")
print(head(r29))
r30 <- pattern.fread(the.files = the.files, the.patterns = c("kT27T", "wBMJkot"), connectors = "and", return.as = "result")
print(head(r30))

r31 <- pattern.fread(the.files = the.files, the.patterns = c("8XKJD4", "0JFCj"), connectors = c("or"), include.filename = T, skip = 0, show.warnings = F)
print(r31)
r32 <- pattern.fread(the.files = "Data/Titanic.csv", the.patterns = c("Female", "Child", "1st"), tf = c(T, T, F), connectors = "and", file.header = "source_file")
print(head(r32))


r_count1 <- record.count(the.files = the.files, the.filter = "user > item & rating == 4", return.as = "all", include.filename = F, skip = 0)
print(r_count1)

r_count2 <- record.count(the.files = the.files, the.filter = "rating == 3 | rating == 4", include.filename = T, return.as = "all")
print(r_count2)

# r_count3 <- record.count(the.files = c("diamonds.csv", "diamonds.csv"), the.filter = "price > 1000")
# print(r_count3)

# tt <- fread("diamonds.csv")
# print(tt[price > 1000, .N])
