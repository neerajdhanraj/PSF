convert_datatype <- function(data) {
  if (is.vector(data))
    series = as.data.table(data)

  else if (is.list(data))
    series = data.table(data = unlist(data))

  else if (is.ts(data))
    series = data.table(data = data)

  else if (is.matrix(data) | is.data.frame(data)) {

    if (nrow(data) > 1 & ncol(data) == 1)
      series = data.table(data = data[,1])

    else if (nrow(data) == 1 & ncol(data) > 1) {
      series = data.table(data = t(data[1,]))
      if (is.data.frame(data))
        setnames(series, "data.1", "data")
    }

    else {
      warning("Matrix with multiple univariate series, using only the first column!")
      series = data.table(data = data[,1])
    }

  }

  return(series)
}
