df_to_array <- function(df, indexes) {
  indexes_names <- names(indexes)
  if (ncol(df) != length(indexes) + 1) {
    stop("df must have ", length(indexes) + 1, " columns\n",
      call. = FALSE
    )
  }
  for (i in indexes_names) {
    df[, i] <- factor(as.character(df[[i]]), levels = indexes[[i]])
  }
  set_sizes <- sapply(indexes, length)
  names_num <- names(df)[sapply(df, is.numeric)]
  df <- df[do.call("order", df[rev(indexes_names)]), ]
  a <- array(df[[names_num]], dim = set_sizes, dimnames = indexes)
  # if (length(dim(a)) == 1) {
  #   a <- as.vector(a)
  #   names(a) <- indexes[[1]]
  # }
  # if (length(dim(a)) == 2) {
  #   a <- as.matrix(a)
  # }
  a
}

create_array <- function(value = 1, indexes) {
  set_sizes <- sapply(indexes, function(x) length(x))
  a <- array(value, dim = set_sizes, dimnames = indexes)
  # if (length(dim(a)) == 1) {
  #   a <- as.vector(a)
  #   names(a) <- indexes[[1]]
  # }
  # if (length(dim(a)) == 2) {
  #   a <- as.matrix(a)
  # }
  a
}

array_to_df <- function(array){
  array %>%
    as.table() %>%
    as.data.frame() %>%
    dplyr::rename(value = Freq)
}
