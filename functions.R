

maxFreqN <- function(x, na, nmode) {
  if (na == 1) {
    ux  <- na.omit(unique(x))
    tt <- data.frame(table(ux))
    return(as.character(tt[which.max(tt$Freq), 1:nmode]))
  }
  tt <- data.frame(table(x))
  return(as.character(tt[which.max(tt$Freq), 1:nmode]))
}



mode <- function(x) {
        ux  <- na.omit(unique(x))
        if(length(ux) == 0) {
                return("")
        } else {
                tab <- tabulate(match(x, ux)); ux[tab == max(tab)]
        }
}
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

#
# Return an array `i` of indexes into `target`, parallel to array `probe`.
# For each index `j` in `target`, probe[i[j]] is nearest to target[j].
#
nearest <- function(probe, target, ends=c(-Inf,Inf)) {
  #
  # Both `probe` and `target` must be vectors of numbers in ascending order.
  #
  glb <- function(u, v) {
    n <- length(v)
    z <- c(v, u)
    j <- i <- order(z)
    j[j > n] <- -1
    k <- cummax(j)
    return (k[i > n])
  }
  y <- c(ends[1], target, ends[2])
  
  i.lower <- glb(probe, y)
  i.upper <- length(y) + 1 - rev(glb(rev(-probe), rev(-y)))
  y.lower <- y[i.lower]
  y.upper <- y[i.upper]
  lower.nearest <- probe - y.lower < y.upper - probe
  i <- ifelse(lower.nearest, i.lower, i.upper) - 1
  i[i < 1 | i > length(target)] <- NA
  return (i)
}

#
# # Graphical illustration.
# #
# set.seed(17)
# x <- sort(round(runif(8), 3))
# y <- sort(round(runif(12), 1))
# i <- nearest(x, y)
# plot(c(0,1), c(3/4,9/4), type="n", bty="n", yaxt="n", xlab="Values", ylab="")
# abline(v = (y[-1] + y[-length(y)])/2, col="Gray", lty=3)
# invisible(apply(rbind(x, y[i]), 2, function(a) arrows(a[1], 1, a[2], 2, length=0.15)))
# points(x, rep(1, length(x)), pch=21, bg="Blue")
# points(y, rep(2, length(y)), pch=21, bg="Red", cex=sqrt(table(y)[as.character(y)]))
# text(c(1,1), c(1,2), c("x","y"), pos=4)
# # #
# # # Timing.
# # #
# x <- runif(1e6)
# y <- runif(1e6)
# system.time({
#   x <- sort(x); y <- sort(y)
#   nearest(x,y)
# })
fixSog <- function(xf, qtile) {
  vslLoaded      <- fread(xf)
  # we remove fishing speed of mroe than 6 knots
  vslFish   <- vslLoaded[fish == 1, ]
  vslRest   <- vslLoaded[is.na(fish), ]
  vslRest[, fish := 0]
  # I need to find the outliers of sog in fishing
  quantiles <-
    quantile(vslFish$sog,
             probs = c(1 - qtile, qtile),
             na.rm = T)
  if ((1.5 * quantiles[2]) >= 6) {
    vslFish    <- vslFish[sog >= 1.5 * quantiles[2], fish := 0]
    vslLoaded  <- rbind(vslFish, vslRest)
    fwrite(vslLoaded, paste0('clean/', as.character(xf)))
  }
  return(vslLoaded)
}
copyUPtoNA <- function(DT, field) {
  pos <- DT[,.I[is.na(field)]]
  idx <- unname(unlist(lapply(split(pos, cumsum(c(1, diff(pos) != 1))),
                              function(x) rep(x[length(x)] + 1, length(x)))))
  return(DT[pos,field] <- DT[idx,field]) 
}
