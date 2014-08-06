\begin{frame}{Greeks}
\item Derivatives of the call price with respect to coefficients
\item Delta: $\Delta(t) = \frac{\partial c_t}{\partial P_t} = \Phi(d_1) + P_t \frac{\partial \Phi(d_1)}{\partial P_t} - Ke^{-r(T-t)}\frac{\partial \Phi(d_2)}{\partial P_t} = \Phi(d_1)$
\item Gamma: $\Gamma(t) = \frac{\partial^2 c_t}{\partial P_t^2} = \frac{1}{P_t \sigma\sqrt{T-t}} \Phi’(d_1)\geq 0$, convex increasing function of $P_t$
\item Theta: $\Theta(t) = \frac{\partial c}{\partial t} = \frac{\sigma P_t}{2\sqrt{T-t}} \Phi’(d_1) - rKe^{-r(T-t)}\Phi(d_2) \leq 0$\\
call option price is an increasing function of time to maturity
\item Vega: $\nu(t) = \frac{\partial c}{\partial \sigma} = P_t \Phi’(d_1) \sqrt{T-t} \geq 0$, BS price $\uparrow$ with $\sigma$
\end{frame}


\begin{frame}{Implied Volatility}
\item How do we estimate $\sigma$? Stock price vs options price
\item Equate $c_t^{BS}$ to $\c_t^M$, the actual price, and solve;\\
Because of call-put parity, can equate $p_t^{BS}$ to $p_t^M$ as well.\\
Non-linear equation, solved via iteration.
\item Moneyness $(K/P)$; if $P_t>K$ in the money\\
$P_t=K$ on the money\\
$P_t < K$ out of the money\\
As per Black-Scholes, $\sigma$ does not vary with $K$ and $T$;\\
But empirical data suggests otherwise
\item Time series of implied volatility surfaces (discuss in class)
\end{frame}
