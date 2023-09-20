### Exercice 1

## (a)
w_vec <- function(n, m = 1, p = 0.5)
    atan(cumsum(c(0, (m * p * rbinom(n - 1, m, p) - m * p * p))))

## (b)
w_for <- function(n, m = 1, p =  0.5) {
    w <- rbinom(n - 1, m, p)

    res <- numeric(n)
    res[[1]] <- 0
    for (k in seq_along(w))
        res[[k + 1]] <- res[[k]] + m * p * w[[k]] - m * p * p

    atan(res)
}

## Graphiques!
vals <- list(list(m = 1, p = 5e-1),
             list(m = 1, p = 1e-2),
             list(m = 10, p = 1e-4),
             list(m = 10, p = 1e-3))

set.seed(666)
trajs <- lapply(vals, \(v) with(v, list(x = w_vec(10000, m = m, p = p),
                                        m = m, p = p)))

par(mfrow = c(2, 2))
for (traj in trajs)
    with(traj, plot(x,
                    type = "l", xlab =  "t", ylab = "X(t)",
                    main = paste0("m = ", m, ", p = ", p)))


### Exercice 2

ex2 <- function(x, αs = seq(0, 0.99, 0.01)) {
    cutoffs <- quantile(x, αs)
    sapply(cutoffs, \(cutoff) mean(Filter(\(x) x > cutoff, x)))
}

## Graphiques

par(mfrow = c(2, 2))
for (traj in trajs)
    with(traj, plot(seq(0, 0.99, 0.01), ex2(x),
                    type = "l", xlab =  "α", ylab = "T(X)",
                    main = paste0("m = ", m, ", p = ", p)))

### Exercice 3

cheaprng <- function(..., v1 = 1) {

    x <- as.numeric(list(...))
    n <- length(x)
    v <- seq(v1, by =  1, length =  n)

    for (k in 1:1000)
        x <- ((((k %% 2) + 1) * tcrossprod(x)) %*% v) %% 8191

    (as.numeric(crossprod(x)) %% 100) / 100
}
