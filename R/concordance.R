concordance <- function(x, y) {

  n <- length(x)
  stopifnot(length(y) == n)
  if (n == 0L) return(integer(0L))
  if (n == 1L) return(0L)
  if (n <= 130) concordance_naive(x, y)
  concordance_fenwick(x, y)

}

concordance_naive <- function(x, y) {
  dx <- sign(outer(x, x, `-`))
  dy <- sign(outer(y, y, `-`))
  .rowSums(dx * dy, nrow(dx), ncol(dx))
}

concordance_fenwick <- function(x, y) {

  n <- length(x)

  # 1) order by x (stable), keep x-ordered copies
  ord <- order(x, y, method = "radix")
  xo <- x[ord]
  yo <- y[ord]

  # 2) compute group runs for equal x (use rle to avoid building big lists)
  if (anyDuplicated(xo)) {
    rle_x <- rle(xo)
    group_lengths <- rle_x$lengths
    group_starts <- cumsum(c(1L, head(group_lengths, -1L)))
    group_ends   <- cumsum(group_lengths)
    G <- length(group_lengths)  # number of groups
  } else {
    # fast path
    rle_x <- rle(xo)
    group_lengths <- rep.int(1L, n)
    group_starts <- 1:n
    group_ends   <- 1:n
    G <- n  # number of groups
  }

  # 3) compress y to ranks 1..m (strict ordering of unique y's)
  uniq_y <- sort(unique(yo))
  ry <- match(yo, uniq_y)      # integer vector 1..m
  m <- length(uniq_y)

  # allocate bit and result vectors (integers)
  bit <- integer(m)            # 1-indexed BIT (positions 1..m)
  less_right    <- integer(n)
  greater_right <- integer(n)
  # originally there were separate vectors, but we can reuse some memory
  # less_left     <- integer(n)
  # greater_left  <- integer(n)

  # precompute the results of bitwAnd so we can just do lookup
  # lowbits <- (1:m) - ((1:m) & ((1:m) - 1L))
  # lowbits <- (1:m) - bitwAnd((1:m), ((1:m) - 1L))
  #lowbits <- vapply(1:m, \(i) bitwAnd(i, -i), 0L)# -> lowbits
  lowbits <- bitwAnd(1:m, -1:-m)

  # Helper: inline bit_sum and bit_add are implemented as loops below
  # -------- Right sweep (groups processed from right to left) ----------
  total_seen <- 0L
  for (g in G:1L) {
    i1 <- group_starts[g]
    i2 <- group_ends[g]
    # query each index in this group (do NOT add them yet)
    for (i in i1:i2) {
      r <- ry[i]
      # less_right: bit_sum(r-1)
      j <- r - 1L
      s_less <- 0L
      while (j > 0L) {
        s_less <- s_less + bit[j]
        j <- j - lowbits[j] #bitwAnd(j, -j)
      }
      # leq_count: bit_sum(r)
      j <- r
      s_leq <- 0L
      while (j > 0L) {
        s_leq <- s_leq + bit[j]
        j <- j - lowbits[j] #bitwAnd(j, -j)
      }
      less_right[i] <- s_less
      greater_right[i] <- total_seen - s_leq
    }
    # now add this entire group into the BIT (so earlier groups see them)
    for (i in i1:i2) {
      r <- ry[i]
      j <- r
      while (j <= m) {
        bit[j] <- bit[j] + 1L
        j <- j + lowbits[j] #bitwAnd(j, -j)
      }
      total_seen <- total_seen + 1L
    }
  }
  out_xorder <- (greater_right - less_right)  # partial result
  # reset for left sweep, these are now less_left and greater_left
  less_right[] <- 0L
  greater_right[] <- 0L


  # -------- Left sweep (groups processed from left to right) ----------
  bit[] <- 0L   # reset BIT in-place (no new allocation)
  total_seen <- 0L
  for (g in 1L:G) {
    i1 <- group_starts[g]
    i2 <- group_ends[g]
    # query each index in this group (do NOT add them yet)
    for (i in i1:i2) {
      r <- ry[i]
      # less_left: bit_sum(r-1)
      j <- r - 1L
      s_less <- 0L
      while (j > 0L) {
        s_less <- s_less + bit[j]
        j <- j - lowbits[j] #bitwAnd(j, -j)
      }
      # leq_count: bit_sum(r)
      j <- r
      s_leq <- 0L
      while (j > 0L) {
        s_leq <- s_leq + bit[j]
        j <- j - lowbits[j] #bitwAnd(j, -j)
      }
      # less_left[i] <- s_less
      # greater_left[i] <- total_seen - s_leq
      less_right[i] <- s_less
      greater_right[i] <- total_seen - s_leq
    }
    # now add this group's ranks to BIT
    for (i in i1:i2) {
      r <- ry[i]
      j <- r
      while (j <= m) {
        bit[j] <- bit[j] + 1L
        j <- j + lowbits[j] #bitwAnd(j, -j)
      }
      total_seen <- total_seen + 1L
    }
  }

  out_xorder <- out_xorder + (less_right - greater_right)
  # reuse some memory
  less_right[] <- 0L
  less_right[ord] <- out_xorder
  return(less_right)

  # combine contributions (in x-sorted order), then restore original order
  # out_xorder <- (greater_right - less_right) + (less_left - greater_left)
  # out <- integer(n)
  # out[ord] <- out_xorder
  # out
}
