Testing Fourier Stuff
========================================================

##Installing FFTW
```
http://mazamascience.com/WorkingWithData/?p=1439
wget http://www.fftw.org/fftw-3.3.3.tar.gz
tar -xzf fftw-3.3.3.tar.gz
cd fftw-3.3.3
./configure --enable-shared
make
sudo make install
```


```r
library(fftw)
```

Slow algorithm


```r
f <- function(x) {
  exp(-x^2)
}

slow_fourier <- function(f, w, len = 10, res = 10, div = 1, shift = 0) {
  xs <- -(len * res):(len * res)/res
  sum(f(xs/div - shift) * exp(-1i * w/div * (xs - div * shift))/(res * div))
}

slow_fourier(f, 2.2, 10, 10)
```

```
## [1] 0.5285409+0i
```

```r
slow_fourier(f, 2.2, 10, 10, 2)
```

```
## [1] 0.5285409-0i
```

```r
slow_fourier(f, 2.2, 10, 20)
```

```
## [1] 0.5285409-0i
```

```r
slow_fourier(f, 2.2, 20, 20)
```

```
## [1] 0.5285409-0i
```

```r
slow_fourier(f, 2.2, 50, 20, 8)
```

```
## [1] 0.5285409-0i
```

```r
slow_fourier(f, 2.2, 50, 20, 8, .5)
```

```
## [1] 0.5285409+0i
```

```r
slow_fourier(f, 2.199115, 50, 30, 8, .5)
```

```
## [1] 0.5290556+0i
```

FFT algorithm

```r
fourier <- function(f, w0, res = 1000000, shift = 0, div = 1) {
  xs <- ((1:res) - 1)/sqrt(res)
  w <- floor(w0 * sqrt(res)/(2 * pi)/div)
  z <- fft(f(xs/div - shift)) * exp(2i*pi*div*shift*((1:res) -1)/sqrt(res))
  z[w]/sqrt(res)/div
}

fourier(f, 1.1)
```

```
## [1] 0.6578134-0.4497168i
```

```r
fourier(f, 1.1, 2000000, shift= 10, div = 1)
```

```
## [1] 1.31486-0i
```

```r
fourier(f, 1.1, 2000000, shift= 20, div = 2)
```

```
## [1] 1.321235-0i
```

FFT with defaults

```r
fourier <- function(f, w0, res = 1000000, len = 10) {
  div <- sqrt(res)/len
  shift <- 1/2 * len
  xs <- ((1:res) - 1)/sqrt(res)
  w <- floor(w0 * sqrt(res)/(2 * pi)/div)
  z <- fft(f(xs/div - shift)) * exp(2i*pi*div*shift*((1:res) -1)/sqrt(res))
  z[w]/sqrt(res)/div
}

fourier(f, 1.1, len = 30)
```

```
## [1] 1.487215+0i
```

```r
fourier(f, 1.1, len = 20)
```

```
## [1] 1.605875+0i
```

```r
fourier(f, 1.1, len = 100)
```

```
## [1] 1.376719-0i
```

