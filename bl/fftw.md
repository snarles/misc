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
  exp(-x^2) * (x > 0)
}

slow_fourier <- function(f, w, len = 10, res = 10, div = 1, shift = 0) {
  xs <- -(len * res):(len * res)/res
  sum(f(xs/div - shift) * exp(-1i * w/div * (xs - div * shift))/(res * div))
}

slow_fourier(f, 2.2, 10, 10)
```

```
## [1] 0.2142705-0.52437i
```

```r
slow_fourier(f, 2.2, 10, 10, 2)
```

```
## [1] 0.2392705-0.5257481i
```

```r
slow_fourier(f, 2.2, 10, 20)
```

```
## [1] 0.2392705-0.5257481i
```

```r
slow_fourier(f, 2.2, 20, 20)
```

```
## [1] 0.2392705-0.5257481i
```

```r
slow_fourier(f, 2.2, 50, 20, 8)
```

```
## [1] 0.2611455-0.5261995i
```

```r
slow_fourier(f, 2.2, 50, 20, 8, .5)
```

```
## [1] 0.2611455-0.5261995i
```

```r
slow_fourier(f, 2.199115, 50, 30, 8, .5)
```

```
## [1] 0.2624445-0.5262732i
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
## [1] 0.6568134-0.4497168i
```

```r
fourier(f, 1.1, 2000000)
```

```
## [1] 0.6570766-0.4496343i
```

```r
fourier(f, 1.1, 2000000, div = 1)
```

```
## [1] 0.6570766-0.4496343i
```

```r
fourier(f, 1.1, 2000000, shift= 1, div = 1)
```

```
## [1] 0.6572276-0.4496343i
```

```r
fourier(f, 1.1, 2000000, shift= 2, div = 2)
```

```
## [1] 0.6607425-0.4473604i
```

