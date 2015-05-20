---
title: "Predicting new rows via matrix completion"
output:
  html_document:
    mathjax: default
---

## Introduction

Suppose $R_{m \times n}$ is a partially observed matrix:
we only observe $R_{ij}$ for $(i, j) \in \Omega$.

Here is one way to predict the missing entries.
Consider the following matrix completion problem (Mazumder et al 2010).
$$
\text{minimize}_Z \frac{1}{2}\sum_{(i,j) \in \Omega} (R_{ij} - Z_{ij})^2 + \lambda ||Z||_\star 
$$
After solving this optimization problem, use $Z_{ij}$ as a prediction of the missing entries $R_{ij}$.

Consider the solution $Z$ of this optimization problem.
If $r = rank(Z)$, then also $Z = UV^T$, where $U$, $V$ are the solutions to
$$
\text{minimize}_{U_{m\times r}, V_{n \times r}} \frac{1}{2} \sum_{(i, j) \in \Omega} (R_{ij} - (UV^T)_{ij})^2 + \frac{\lambda}{2}(||U||^2_F + ||V||^2_F)
$$

## Semi-supervised learning

Can we interpret matrix completion as a form of semi-supervised learning?
In semi-supervised learning, we have observed covariates $x_1, ... , x_m \in \mathbb{R}^{n-1}$ and *partially* observed responses $y_1, ..., y_{m_0}$ where $m_0 < m$.
Let $X_{m \times (n-1)}$ denote the matrix of covariates and $Y = (y_1, ... , y_m)$ denote the full set of observed and unobserved responses.

The "supervised" approach would be to fit a model to the fully observed pairs,
$$ Y_i \approx X_i \beta + \beta_0 $$
for $i = 1,..., m_0$,
and then predict the unobserved responses as
$\hat{Y}_i = X_i \beta + \beta_0$ for $i = m_0 + 1, ... , m$.

Now consider using matrix completion to solve the problem.
Define the matrix $$R_{m \times n} = [X | Y]$$
Here we have observed $R_{ij}$ for all $j = 1, ... , n-1$ and for all $j = n$, $i = 1, ..., m_0$.
The problem of predicting $y_{m_0 + 1},..., y_m$ is equivalent to predicting the missing elements $R_{m_0 + 1, n}, ..., R_{m, n}$.

Hence we would think of matrix completion as a method for learning a predictive model, which can be used to predict $Y$ given $X$.
However, it is not perfectly straightforward to interpret matrix completion as a predictive model like regression.
In regression, the model gives a *prediction rule* for labelling a new observation $x_*$.
In matrix completion, if we wanted to predit $y_*$ for a new observation $x_*$, we could do so by extending the matrix $R$ by one row and re-running matrix completion.
However, this process does not seem to be interpretable as a "prediction rule."
In the following, we argue that there *is* a way to interpret matrix completion in terms of a prediction rule: our proposed rule gives different results than re-running matrix completion on an extended matrix $R$.



