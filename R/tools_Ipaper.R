listk <- function(...) {
  # get variable names from input expressions
  cols <- as.list(substitute(list(...)))[-1]
  vars <- names(cols)
  Id_noname <- if (is.null(vars)) seq_along(cols) else which(vars == "")

  if (length(Id_noname) > 0) {
    vars[Id_noname] <- sapply(cols[Id_noname], deparse)
  }
  # ifelse(is.null(vars), Id_noname <- seq_along(cols), Id_noname <- which(vars == ""))
  x <- setNames(list(...), vars)
  return(x)
}

# ' @importFrom utils modifyList
modifyList <- function(x, val, keep.null = FALSE) {
  # stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  vnames <- names(val)
  vnames <- vnames[nzchar(vnames)]
  if (keep.null) {
    for (v in vnames) {
      x[v] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) {
        list(modifyList(x[[v]], val[[v]], keep.null = keep.null))
      } else {
        val[v]
      }
    }
  } else {
    for (v in vnames) {
      x[[v]] <- if (v %in% xnames && is.list(x[[v]]) &&
        is.list(val[[v]])) {
        modifyList(x[[v]], val[[v]], keep.null = keep.null)
      } else {
        val[[v]]
      }
    }
  }
  x
}

rm_empty <- function(x) {
  if (is.list(x)) {
    x[!sapply(x, is_empty)]
  } else {
    x[!is.na(x)]
  }
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


print2 <- function(..., max.level = NA) {
  l <- listk(...)
  str(l, max.level = max.level)
  # .tmp <- print(str(l, max.level = max.level))
  invisible()
}
