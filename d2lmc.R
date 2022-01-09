#d2lmc is a function that takes a plain text files (txt) with multiple choice
#questions and converts them to CSV files in a format compatible with BrightSpace
#quiz format.

d2lmc <- function(a, b="exam_mc.csv") {
  fl2 <- file(a, open = "r")
  lns <- readLines(fl2)
  mat2 <- matrix(ncol = 3, nrow = length(lns)+length(str_which(lns, "^[1-9]")))
  i = 1
  for (ln in lns) {
    if(grepl("^[1-9]",ln)){
      mat2[i,] <- c("NewQuestion", "MC", "")
      i = i+1
      a1 <- str_remove(str_split_fixed(ln,"\\.",2)[2], " ")
      mat2[i,] <- c("QuestionText",
                    str_remove(str_split_fixed(ln,"\\.",2)[2], " "), "")
      print(mat2[i,]) } else {
        #if correct answer i.e. has *
        if(grepl(pattern = "\\*$",ln)){
          mat2[i,] <- c("Option","100",
                        str_remove(str_remove(
                          str_split_fixed(ln,"\\.",2)[2], " "),
                          "\\*")) } else {
                            mat2[i,] <- c("Option","0",
                                          str_remove(str_split_fixed(ln,"\\.",2)[2], " "))
                          }
      }
    i=i+1
  }
  mat3 <<- mat2
  write.table(mat2, file = b, col.names = F, row.names = F, sep = ",")
}
