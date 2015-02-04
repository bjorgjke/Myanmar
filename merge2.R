merge2 <- function (x, y, by = intersect(names(x), names(y)), by.x = by, 
                    by.y = by, all = FALSE, all.x = all, all.y = all, sort = TRUE, 
                    suffixes = c(".x", ".y"), ...) 
{
  fix.by <- function(by, df) {
    if (is.null(by)) 
      by <- numeric(0)
    by <- as.vector(by)
    nc <- ncol(df)
    if (is.character(by)) 
      by <- match(by, c("row.names", names(df))) - 1L
    else if (is.numeric(by)) {
      if (any(by < 0) || any(by > nc)) 
        stop("'by' must match numbers of columns")
    }
    else if (is.logical(by)) {
      if (length(by) != nc) 
        stop("'by' must match number of columns")
      by <- seq_along(by)[by]
    }
    else stop("'by' must specify column(s) as numbers, names or logical")
    if (any(is.na(by))) 
      stop("'by' must specify valid column(s)")
    unique(by)
  }
  nx <- nrow(x <- as.data.frame(x))
  ny <- nrow(y <- as.data.frame(y))
  by.x <- fix.by(by.x, x)
  by.y <- fix.by(by.y, y)
  if ((l.b <- length(by.x)) != length(by.y)) 
    stop("'by.x' and 'by.y' specify different numbers of columns")
  if (l.b == 0) {
    nm <- nm.x <- names(x)
    nm.y <- names(y)
    names(x) <- paste(nm.x, suffixes[1], sep = "")
    names(y) <- paste(nm.y, suffixes[2], sep = "")
    ij <- expand.grid(seq_len(nx), seq_len(ny))
    res <- cbind(x[ij[, 1], , drop = FALSE], y[ij[, 2], , 
                                               drop = FALSE])
  }
  else {
    if (any(by.x == 0)) {
      x <- cbind(Row.names = I(row.names(x)), x)
      by.x <- by.x + 1L
    }
    if (any(by.y == 0)) {
      y <- cbind(Row.names = I(row.names(y)), y)
      by.y <- by.y + 1L
    }
    row.names(x) <- NULL
    row.names(y) <- NULL
    if (l.b == 1L) {
      bx <- x[, by.x]
      if (is.factor(bx)) 
        bx <- as.character(bx)
      by <- y[, by.y]
      if (is.factor(by)) 
        by <- as.character(by)
    }
    else {
      bx <- x[, by.x, drop = FALSE]
      by <- y[, by.y, drop = FALSE]
      names(bx) <- names(by) <- paste("V", seq_len(ncol(bx)), 
                                      sep = "")
      bz <- do.call("paste", c(rbind(bx, by), sep = "\r"))
      bx <- bz[seq_len(nx)]
      by <- bz[nx + seq_len(ny)]
    }
    comm <- match(bx, by, 0)
    bxy <- bx[comm > 0]
    xinds <- match(bx, bxy, 0)
    yinds <- match(by, bxy, 0)
    if (nx > 0 && ny > 0) 
      m <- .Internal(merge(xinds, yinds, all.x, all.y))
    else m <- list(xi = integer(0), yi = integer(0), x.alone = seq_len(nx), 
                   y.alone = seq_len(ny))
    nm <- nm.x <- names(x)[-by.x]
    nm.by <- names(x)[by.x]
    nm.y <- names(y)[-by.y]
    ncx <- ncol(x)
    if (all.x) 
      all.x <- (nxx <- length(m$x.alone)) > 0
    if (all.y) 
      all.y <- (nyy <- length(m$y.alone)) > 0
    lxy <- length(m$xi)
    nm.x <- paste(nm.x, suffixes[1], sep = "")
    x <- x[c(m$xi, if (all.x) m$x.alone), c(by.x, seq_len(ncx)[-by.x]), 
           drop = FALSE]
    names(x) <- c(nm.by, nm.x)
    if (all.y) {
      ya <- y[m$y.alone, by.y, drop = FALSE]
      names(ya) <- nm.by
      ya <- cbind(ya, x[rep.int(NA_integer_, nyy), nm.x, 
                        drop = FALSE])
      x <- rbind(x, ya)
    }
    nm.y <- paste(nm.y, suffixes[2], sep = "")
    y <- y[c(m$yi, if (all.x) rep.int(1L, nxx), if (all.y) m$y.alone), 
           -by.y, drop = FALSE]
    if (all.x) 
      for (i in seq_along(y)) is.na(y[[i]]) <- (lxy + 1L):(lxy + 
                                                             nxx)
    names(y) <- nm.y
    res <- cbind(x, y)
    if (sort) 
      res <- res[if (all.x || all.y) 
        do.call("order", x[, seq_len(l.b), drop = FALSE])
        else sort.list(bx[m$xi]), , drop = FALSE]
  }
  attr(res, "row.names") <- .set_row_names(nrow(res))
  res
}
