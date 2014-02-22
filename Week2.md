---
subject: Lecture Notes
title: MAST90053 Experimental Mathematics
author: "Lecturer: Dr Andrea Bedini"
date: 2014, Semester 1, Week 2
header-includes:
    \usepackage{algorithmic}
    \def\RR{\mathbb{R}}
    \def\ZZ{\mathbb{Z}}
    \def\nint{\operatorname{nint}}
---


Interger relation detection
===========================

Given a real vector $x=(x_1,x_2,\dotsc,x_n) \in \RR^n$, an integer relation for $x$ is a non-zero vector of integers $m = (m_1, m_2,\dotsc,m_n)$, such that $m^t x = m_1 x_1 + m_2 x_2 + \dotsb + m_n x_n = 0$. An integer dectection algorithm is a computational scheme that can recover the vector of integers $m$ (if it exists) or produce bounds within which no integer relation exists.

Although the integer relation problem is often regarded to be a relatively "new" problem, it is really a rather old problem. The problem of finding integer relations for two numbers $(x_1, x_2)$ can be solved by applying the Euclidean algorithm to $x_1, x_2$, or, equivalently, by computing the continued fraction expansion of the real number $x_1/x_2$.

Integer relation detection methods are employed very often in experimental math applications to recognize a mathematical constant whose numerical value can be computed to at least moderately high precision, and also to discover relations between a set of computed numerical values.

The PSLQ algorithm
==================

At the present time, the best known integer relation algorithm is the PSLQ algorithm[^PSLQ] of Helaman Ferguson. The PSLQ algorithm, together with related lattice reduction schemes, was recently named one of ten "algorithms of the century" by the publication Computing in Science and Engineering[^TOP10]. In addition to possessing good numerical stability, PSLQ is guaranteed to find a relation in a polynomially bounded number of iterations. The name "PSLQ" derives from its usage of a partial sum of squares vector and a LQ (lower trapezoidal-orthogonal) matrix factorization.

[^PSLQ]: Helaman R.P. Ferguson, and David H. Bailey. "A polynomial time, numerically stable integer relation algorithm." (1991). [URL](http://crd-legacy.lbl.gov/~dhbailey/dhbpapers/pslq.pdf)
    Helaman R.P. Ferguson, David H. Bailey, and Steve Arno. "Analysis of PSLQ, an integer relation finding algorithm." Mathematics of Computation of the American Mathematical Society 68.225 (1999): 351-369. [URL](http://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-00995-3/S0025-5718-99-00995-3.pdf)

[^TOP10]: D. H. Bailey. Integer relation detection. Computing in Science and Engineering, 2(1):24–28, 2000.

Let $x \in \RR$ be a nonzero $n$-tuple $x=(x_1,x_2,\dotsc,x_n)$. Define the partial sum of squares, $s_j$, for $x$ as 
$$
s_k = \sqrt{x_k^2 + \dotsc + x_n^2}
.
$$
Naturally we can assume that $x$ is a unit vector, so that $s_1 = |x| = 1$.
Define the lower trapezoidal $n \times (n-1)$ matrix $H_x$ with elements
$$
h_{ij}= \begin{cases}
0 & i<j \\
\frac{s_{i+1}}{s_i} & i=j \\
-\frac{x_i x_j}{s_j s_{j+1}} & i>j.
\end{cases}
$$

\example For $n=4$, the matrix $H_x$ is given by
$$
H_x = \begin{pmatrix}
 \frac{s_2}{s_1} & 0 & 0 \\
 -\frac{x_2 x_1}{s_1 s_2}  & \frac{s_3}{s_2} & 0 \\
 -\frac{x_3 x_1}{s_1 s_2} & -\frac{x_3 x_2}{s_2 s_3} & \frac{s_4}{s_3} \\
 -\frac{x_4 x_1}{s_1 s_2} & -\frac{x_4 x_2}{s_2 s_3} & -\frac{x_4 x_3}{s_3 s_4}
\end{pmatrix}
$$

\lemma
Let $x$ be a unit vector and $H_x$ the lower trapezoidal matrix defined above. Then:

1. Each column of $H_x$ is orthogonal to $x$, i.e. $xH_x=0$.
2. The columns of $H_x$ form an orthonormal basis, i.e. $H_x^t H_x = I_{n-1}$
3. $|H_x| = \sqrt{n-1}$

\lemma Let $P_x$ be the $n \times n$ matrix given by $P_x = H_x H_x^t$. Then $P_x$ satistfies the following:

1. $P_x^t = P_x$
2. $P_x = I_{n} - x^t x$
3. $P_x^2 = P_x$
4. $|P_x| = \sqrt{n-1}$
5. $P_x m^t = m^t$ for any relation $m$ for $x$.

<!-- PSLQ operates by constructing a series of matrices $A_k$, such that the entries of the vector $x_k$ = $A^{-1}_k x$ steadily decrease in size. At any given iteration, the largest and smallest entries of $x_k$ usually normally differ by no more than two or three orders of magnitude. When a relation is detected by the algorithm, the smallest entry of the $y_k$ vector abruptly decreases to roughly the "epsilon" of the working precision (i.e., $10^{-p}$, where $p$ is the precision level in digits), and the desired relation is given by the corresponding column of $A^{−1}$. -->

We will try to find an invertible $n \times n$ lower triangular _integer_ matrix $D$ such that a column of $D H_x$, say the $j$th one, has zeros everywhere except in the $j$th entry. Because we know that
$$
0 = x H_x = x D^{-1} D H_x,
$$
then the $j$th entry of the vector $x D^{-1}$ is zero too. Up to an overall factor $D^{-1}$ is also an integer matrix and therefore the above implies an integer relation for $x$ whose entries are given by the $j$th column of $D^{-1}$.

If the matrix $D$ were allowed to have real, rather than integer, entries, the above problem would be solved by reducing $H$ in the row canonical form.

\lemma (Row canonical form). Let $H = (h_{ij})$ be a $n \times n-1$ lower trapezoidal matrix of with $h_{ii} \neq 0$. Define an associated $n \times n$ lower triangular matrix $D = (d_{ij})$ recursively as follows. For fixed $i$, decrement $j$ from $n$ to 1, setting
$$
d_{ij} = \begin{cases}
0 & j>i, \\
1/h_{jj} & j=i < n, \\
1 & j = i = n, \\
- \sum_{j \leq k < i} d_{ik} h_{kj} / h_{jj} & j < i.
\end{cases}
$$
Then $DH$ is in the row canonical form
$$
DH = 
\begin{pmatrix}
1 &        &   \\
  & \ddots &   \\
  &        & 1 \\
0 & \cdots & 0 
\end{pmatrix}.
$$

If we want $D$ to have integer coefficients the best we can do to reduce $H$ in the Hermite normal form.

\definition (Hermite normal form). Let $H = (h_{ij})$ be a $n \times n-1$ lower trapezoidal matrix of with $h_{ii} \neq 0$. Define an associated $n \times n$ lower triangular matrix $D = (d_{ij})$ recursively as follows. For fixed $i$, decrement $j$ from $n$ to 1, setting
$$
d_{ij} = \begin{cases}
0 & j > i, \\
1 & j = i, \\
- \nint\left(\sum_{j \leq k < i} d_{ik} h_{kj} / h_{jj} \right) & j < i.
\end{cases}
$$
Then we will say $DH$ is the _Hermite reduction_ of $H$ and that $D$ is the _reducing matrix_ of $H$. The function $\nint$ denotes the nearest integer function, e.g., $\nint(t) = \lfloor t+1/2\rfloor$.

Due to the $\nint$ function the off-diagonal elements of $DH$ are not zero but fortunately we can prove that they are not too big either.

\lemma The entries of the Hermite reduced matrix $H' = (h'_{ij}) = DH$ satisfy the inequality
$$
|h'_{ij}| \leq \frac12 |h'_{ii}| \equiv \frac12 |h_{ij}|.
$$
This follows from the definitions of the $\nint$ function, Hermite reduction and the fact that $|t - \nint(t)| \leq 1/2$.


Now we will describe the exchange step. Fix $1\leq r\leq n-1$ and
consider the $2\times 2$ submatrix $$\begin{pmatrix}
a & 0\\
b & c
\end{pmatrix} \equiv
\begin{pmatrix}
h_{rr} & 0 \\
h_{r+1,r} & h_{r+1,r+1}
\end{pmatrix}.$$ The exchange step of the PSLQ algorithm is to change
this submatrix into the matrix

\begin{equation}
\begin{pmatrix}
d & 0\\
ab/d & -ac/d
\end{pmatrix}
=
\begin{pmatrix}
0 & 1\\
1 & 0
\end{pmatrix}
\begin{pmatrix}
a & 0\\
b & c
\end{pmatrix} 
=
\begin{pmatrix}
b/d & -c/d\\
c/d & b/d
\end{pmatrix}, 
\end{equation}
, [exchange] where $d=\sqrt{b^2+c^2}$. As can be seen in , such an
exchange can be achieved by premultiplying $H$ with a permutation
operator and postmultiplying with an orthogonal matrix.

## Exchange
Given an arbitrary lower trapezoidal
$n\times(n-1)$ matrix $H=(h_{ij})$, and a fixed integer $r$,
$1\leq e\leq n-1$, define the $(n-1)\times(n-1)$ orthogonal matrix
$O_r\in O(n-1)$ as follows. If $r=n-1$ set $O_{n-1}=\mathbb{I}_{n-1}$.
Otherwise, for $r<n-1$, set $a=h_{rr}$, $b=h_{r+1,r}$, $c=h_{r+1,r+1}$
and $d=\sqrt{b^2+c^2}$. The entries of the matrix $O_r=(o_{ij})$ are
given by,

$$\begin{aligned}
o_{ii} &=1 \;\text{for}\; i\neq r,r+1,\\
\begin{pmatrix}
o_{rr} & o_{r,r+1}\\
o_{r+1,r} & o_{r+1,r+1}
\end{pmatrix} 
&=
\begin{pmatrix}
b/d & -c/d\\
c/d & b/d
\end{pmatrix},\\
o_{ij}&=0\;\text{otherwise}\end{aligned}$$

Also define the $n\times n$ permutation matrix $P_r$ to be the matrix
formed by exchanging rows $r$ and $r+1$ of the $n\times n$ identity
matrix $\mathbb{I}_n$.

One iteration of PSLQ
---------------------

Given a constant $\gamma> 2\sqrt{3}$, and three matrices $H$, $A$, $B$,
where $H$ is a $n\times(n-1)$ lower trapezoidal matrix and $A$ and $B$
are $n\times n$ integral matrices with $B=A^{-1}$, an iteration of the
PSLQ algorithm consists of the following three steps:

1.  Replace $H$ by $DH$

2.  Choose an integer $r$, $1\leq r\leq n-1$ such that
    $\gamma^r |h_{rr}| \geq \gamma^i|h_{ii}|$ for all $i$,
    $1\leq i\leq n-1$.

3.  Replace $H$ by $P_r H O_r$, $A$ by $P_rDA$ and $B$ by $BD^{-1} P_r$.

Fix $\gamma>2/\sqrt{3}$ and set $\delta^2=3/4-1/\gamma^2$. Suppose some
integral linear combination of the entries of $x\in\mathbb{R}^n$ is zero, so
that $x$ has an integer relation. Let $M\geq 1$ be the least norm of any
such relation $m$. Normalise $y=x/\| x\|$ so that $\|y\|=1$ and iterate
PSLQ beginning with the following set of three matrices: $H=H_y$,
$A=B=\mathbb{I}_n$. Then

1.  A relation for $x$ will appear as a column of $B$ after fewer than
    $\frac{2\gamma}{\delta^2} n^2(n+1)\log(Mn^2)$ iterations of PSLQ.

2.  The norm of such a relation for $x$ appearing as a column of $B$ is
    no greater than $\sqrt{n} \| H\| \|BHH^t \| M$, where
    $\|H\| = \sqrt{\sum_{i,j=1}^n h_{ij}^2}$.

3.  If after a number of iterations of PSLQ no relation has yet appeared
    in a column of $B$, then there are no relations of norm less than
    the bound $1/\|H\|$.

Computer implementation
-----------------------

Assignment
----------

Program the PSLQ algorithm in Mathematica.

```
PSLQ[n_] := PSLQ[n] = Module[{},
    a = b = IdentityMatrix[n];
    ...
];
```

<span>99</span> David H. Bailey and Helaman R.P. Ferguson, *A polynomial
time, numerically stable integer relation algorithm*, Supercomputing
Research Center Technical Report SRC-TR-92-066; RNR Technical Report
RNR-91-032, 1–14. Helaman R.P. Ferguson, David H. Bailey and Steve Arno,
*Analysis of PSLQ, an integer relation finding algorithm*, Mathematics
of Computation **68** (1999), 351–369.
