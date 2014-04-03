---
subject: Lecture Notes
title: MAST90053 Experimental Mathematics
author: "Lecturer: Dr Andrea Bedini"
date: 2014, Semester 1, Week 5
bibliography: bibliography.bib
header-includes:
    \usepackage{algorithm}
    \usepackage{algorithmic}
    \renewcommand{\algorithmicrequire}{\textbf{Input:}}
    \renewcommand{\algorithmicensure}{\textbf{Output:}}
    \def\C{\mathbb{C}}
    \def\R{\mathbb{R}}
    \def\N{\mathbb{N}}
    \def\Z{\mathbb{Z}}
    \def\Q{\mathbb{Q}}
    \def\Li{\operatorname{Li}}
---

The quest for $\pi$
===================

Throughout history it has been important to calculate accurate values of $\pi$. In ancient times just a few digits were known, but in more modern times computing billions of digits has become a fascination, not to say obsession, of many computational mathematicians. The first rigorous technique for approximating $\pi$ was found by Archimedes (ca. 250 BC), who used a scheme based on inscribed and circumscribed polygons to obtain the bounds
$$
3\frac{10}{71} < \pi < 3\frac{1}{7}.
$$
Archimedes’ scheme is the first algorithm which is capable to produce an arbitrarily accurate value of $\pi$, and it can be stated as simply iterating the following recursion,
$$
\begin{aligned}
a_{n+1} &= \frac{2a_n b_n}{a_n+b_n},\\
b_{n+1} &= \sqrt{a_{n+1}b_n},
\end{aligned}
$$
with the initial condition $a_0=2\sqrt{3}$, $b_0=3$.

\exercise Check that Archimedes’ scheme converges to $\pi$, and estimate the rate of convergence.

There are also remarkable series approximations to $\pi$, such as the following one by Ramanujan,
$$
\frac{1}{\pi} = \frac{2\sqrt{2}}{9801}\sum_{k=0}^{\infty} \frac{(4k)!(1103+26390k)}{k!^4 396^{4k}},
$$
for which each term produces eight additional correct digits. A series which produces an additional fourteen correct digits was found by David and Gregory Chudnovsky,
$$
\frac{1}{\pi} = 12\sum_{k=0}^{\infty} \frac{(-1)^k (6k)!(13591409+545140134k)}{(3k)!k!^3640320^{3k+3/2}}.
$$

While the formulas above are efficient, the convergence to the correct value of $\pi$ is linear: the number of terms one needs to compute increases linearly with the desired accuracy. In the past thirty years (much) faster algorithms have become available. Nowadays there are recursions known, which converge quadratically, cubically and even quartically for which, at each iteration step, the number of correct digits approximately doubles, triples or quadruples @borwein2004mathematics @hirschhorn2005representations.

These ``classical'' formulas for $\pi$ share the property that to compute the correct $n$th digit, one needs to compute the first $n-1$ digits, and moreover, perform the calculations in the same high precision. Is it possible to find a formula for the $n$th digit of $\pi$ without having to know the first $n-1$ digits? That such a formula might exist is argued in the next section.

Individual digit algorithm for $\log2$
======================================

Consider the following well known formula for $\log 2$.
$$
\log 2 = \sum_{k=1}^{\infty} \frac{1}{k\,2^k}.
$$
This expression allows us to compute isolated digits in the *binary* expansion of $\log 2$. This works as follows. To compute the first few binary digits beginning at position $d+1$ is equivalent to computing the fractional part of $2^d \log 2$, denoted by $\{2^d \log 2\}$. It thus follows that

$$
\begin{aligned}
\{2^d \log 2\} &= \left\{ \left\{ \sum_{k=1}^d \frac{2^{d-k}}{k} \right\} + \left\{\sum_{k=d+1}^{\infty} \frac{1}{k\,2^{k-d}}\right\}\right\}\nonumber\\
&= \left\{ \left\{ \sum_{k=1}^d \frac{2^{d-k} \bmod k}{k} \right\} + \left\{\sum_{k=d+1}^{\infty} \frac{1}{k\,2^{k-d}}\right\}\right\},
\end{aligned}
$$

where we can insert “$\bmod k$" in the first summand as we are only
interested in the fractional part of the quotient after dividing by $k$.
A key point is now that $2^{d-k} \bmod k$ can be computed very rapidly.

\begin{algorithm}
\caption{Binary algorithm for modular exponentiation}
\begin{algorithmic}
\REQUIRE $b, e, m$
\ENSURE $r = b^e \bmod m$
\STATE $r \leftarrow 1$
\STATE $b \leftarrow b \bmod m$
\WHILE{$e > 0$}
    \IF{$e \bmod 2 = 1$}
        \STATE $r \leftarrow r \cdot b \bmod m$
    \ENDIF
    \STATE $e \leftarrow e >> 1$
    \STATE $b \leftarrow (b \cdot b) \bmod m$
\ENDWHILE
\end{algorithmic}
\end{algorithm}

The above algorithm requires only $O(\log_2 e)$ operations and the only needs the memory to store the intermediate result which, being computed $\bmod m$, doesn't require more memory than $m$. `Mathematica` implementats the same algorithm in the function `PowerMod[]`.

To efficiently compute the first $n$ binary digits of $\log 2$  starting at position $(d+1)$th, we need to do the following:

- Compute the first sum while discarding any integer part.

- Evaluate the second sum including only the terms needed to obtain $n$ accurate binary digits.

- Add the two summations, discarding any integer part. The resulting fraction gives the first $n$ digits of the binary expansion of $\log 2$ beginning at position $d+1$.

BBP formulas
============

With the above in mind, one may wonder whether there exists individual digit algorithms for binary expansion of other mathematical constants such as $\pi$. Bailey, Borwein and Plouffe undertook a search for similar formulas of the form
$$
\alpha = \sum_{k=0}^{\infty} \frac{p(k)}{q(k) b^k}.
$$
with $p$ and $q$ integer polynomials and $b$ the `base' of the formula.

They found the following formula for $\pi$
$$
\pi = \sum_{k=0}^\infty \frac{1}{16^{k}}
\left( \frac{4}{8k+1} - \frac{2}{8k+4} - \frac{1}{8k+5} - \frac{1}{8k+6}
\right),
$$

\exercise Using the trial formula
$$
\pi = \sum_{k=0}^{\infty} \frac{1}{b^k} \sum_{j=1}^n \frac{a_j}{(nk+j)^s},
$$
use PSLQ to find the above BBP formula for $\pi$.

Knowing the result, the above formula can be proved by elementary manipulations. First note that
\begin{multline*}
\sum_{k=0}^{\infty} \frac{1}{16^k(8k+j)} = 2^{j/2} \int_0^{1/\sqrt{2}} \sum_{k=0}^{\infty} x^{j-1+8k}\, \d x \\
= 2^{j/2} \int_0^{1/\sqrt{2}} \frac{x^{j-1}}{1-x^8}\,\d x = 16\int_0^{1} \frac{y^{j-1}}{16-y^8}\,\d y,
\end{multline*}
where the last equality follows from subsituting $y=\sqrt{2} x$. It then follows that the right hand side is equal to
$$
\pi = 16 \int_0^{1} \frac{4-2y^3-y^4-y^5}{16-y^8} \, \d y,
$$
where the last equality follows from cancelling common factors in the numerator and denominator. The final integral can be further simplified, but already in this form `Mathematica` can find the anti-derivative of the integrand in a simple form:
$$
2\log(2-y^2) - 2 \log(y^2-2y+2) - 4 \arctan(1-y) + C.
$$
It is now easy to show that the integral equals $\pi$.

\exercise
Prove that the BBP formula for $\pi$ is the special case $r=0$ of
\begin{multline*}
\pi=\sum_{k=0}^\infty \frac{1}{16^{k}} \left( \frac{4+8r}{8k+1} - \frac{8r}{8k+2} - \frac{4r}{8k+3} - \frac{2+8r}{8k+4} \right.\\
\left. - \frac{1+2r}{8k+5} - \frac{1+2r}{8k+6} + \frac{r}{8k+7}\right),
\end{multline*}

where $r$ can be any real or complex number.

\exercise Compute BBP formulas for $\pi^2$ and $\pi\sqrt{3}$, both in base $64=2^6$ and base $729=3^6$. There exists a BBP formula for the Catalan number $C$ in base $4096=2^{12}$, can you find it?

\exercise Find a formula for $\pi^2$ with the non-integer base $\phi^5$, where $\phi$ is the Golden Ratio. Hint: look for an expression for $\pi^2\phi^5$ where the numbers $a_j$ in its BBP expansion of the form
$$
\pi^2\phi^5 = \sum_{k=0}^{\infty} \frac{1}{\phi^{5k}} \sum_{j=1}^n \frac{a_j}{(nk+j)^s}
$$
can be multiples of powers of $\phi$.

Digit extractor
---------------

Let $\alpha$ be a number given in the presentation
$$
\alpha = \sum_{k=0}^{\infty} \frac{1}{b^k} \sum_{j=1}^n \frac{a_j}{(nk+j)^s},
$$
To compute the first few digits of $\alpha$, starting at position $d$ we need to implement the algorithm described in the previous section for each $j$th term in the above formula.

\exercise Program a digit extractor for $\alpha$ in `Mathematica`. Input should be the position $d$ at which you want to compute the digits of $\alpha$, the base $b$, and the list of coefficients $a_j$ $j=1,\ldots,n$ in the BBP expansion of $\alpha$. So your program should start as

```Mathematica
DigitExtractor[d_,b_,s_,a_List] := Block[
    {n = Length[a], ...},
    ...
]
```

Make use of the `Mathematica` commands `FractionalPart, PowerMod`, and `RealDigits`.

This program computes the digits of $\pi$ starting at position $d=1000$ by issuing
```Mathematica
DigitExtractor[1000, 16, 1, {4,0,0,-2,-1,-1,0,0}]
```
which gives the output `{3, 4, 9, 15, 1, 12, 0, 9, 11, 0}`. You can check with the command
```Mathematica
RealDigits[Pi, 16, 10, -1000]
```
that these are indeed the correct first eight digits in the hexadecimal expansion of $\pi$.

# References
\setlength{\parindent}{0cm}
