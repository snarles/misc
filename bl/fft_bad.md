Limitations of FFT
========================================================

Just a function taking values for $x > 0$.
We want to obtain its Fourier transform

$$
F(\omega) = \int_0^\infty g(x) \exp(-i\omega x) dx
$$


```r
g <- function(x) exp(-x^2) * (x > 0)
xs <- 1:500/100
plot(xs, g(xs), type = "l")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

Method 1: Integration (using 100 points)


```r
slow_fourier <- function(f, w, len = 10, res = 100) {
  xs <- 0:(len * res)/res
  sum(f(xs) * exp(-1i * w * xs))/res
}
ws <- 0:40/20 * 2 * pi
F_slow <- sapply(ws, function(w) slow_fourier(g, w))
plot(ws, Re(F_slow), type = "l",
     ylab = expression(F(omega)), xlab = expression(omega))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Looks reasonable...

Method 2: Fast Fourier transform

We have

$$
\sqrt{n} F(\omega_j) = \sum_{k=1}^n g(x_k) \exp(-i x_k \omega_j)
$$
where
$$
x_k = \frac{k-1}{\sqrt{n}}
$$
$$
\omega_j = 2 \pi \frac{j-1}{\sqrt{n}}
$$


```r
fft_fourier <- function(f, n = 10) {
  xs <- ((1:n) - 1)/sqrt(n)
  ws <- 2 * pi * xs
  list(Fw = fft(f(xs))/sqrt(n), w = ws)
}
plot(ws, Re(F_slow), type = "l",
     ylab = expression(F(omega)), xlab = expression(omega))
res <- fft_fourier(g, 4)
lines(res$w, Re(res$Fw), col = "red")
res <- fft_fourier(g, 25)
lines(res$w, Re(res$Fw), col = "red")
res <- fft_fourier(g, 100)
lines(res$w, Re(res$Fw), col = "red")
res <- fft_fourier(g, 400)
lines(res$w, Re(res$Fw), col = "red")
res <- fft_fourier(g, 1600)
lines(res$w, Re(res$Fw), col = "red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 
