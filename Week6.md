---
subject: Lecture Notes
title: MAST90053 Experimental Mathematics
author: "Lecturer: Dr Andrea Bedini"
date: 2014, Semester 1, Week 6
bibliography: bibliography.bib
header-includes:
    \def\C{\mathbb{C}}
    \def\R{\mathbb{R}}
    \def\N{\mathbb{N}}
    \def\Z{\mathbb{Z}}
    \def\Q{\mathbb{Q}}
    \def\Im{\operatorname{Im}}
    \def\Re{\operatorname{Re}}
    \def\Li{\operatorname{Li}}
---

More on BBP formulas
====================

This lecture, largely based on @borwein2004finding, is devoted to some of the theory behind BBP formulas of the form
$$
\alpha = P(s,b,n,A),
$$
where
$$
P(s,b,n,A) = \sum_{k\geq0} \frac{1}{b^k} \sum_{j=1}^n \frac{a_j}{(nk+j)^s},
$$
and $A=(a_1,\dotsc,a_n) \in \Z^n$. We will also write
$$
P(s,b,n,A) = \sum_{j=1}^n a_j L(s,b,n,j)
$$
and call $L(s,b,n,j)$ _BBP generators_.

If $s=1$, we will call representations of the above form *logarithmic* because for $s=1$ resembles the expansion of $-\log(1-x)$:
$$
- \log(1-x) = \sum_{k\geq 1} \frac{x^k}{k}
$$
By the same token, for general $s$ it is called *polylogarithmic* due to the similarity of with the polylogarithm function $\Li_s(z)$ defined by 
$$
\Li_s(x) = \sum_{k\geq 1} \frac{x^k}{k^s}.
$$

\noindent
The two formulas we saw last week are of this form:
$$
\log 2 = \sum_{k\geq 1} \frac{1}{k 2^k} = \frac{1}{2} P(1,2,1,(1)),
$$
or, more generally,
$$
\log\left(1-\frac{1}{b^m}\right) = - \sum_{k\geq 1} \frac{1}{k b^{mk}} = - b^{-m} P(1,b^m,1,(1)),
$$
and
$$
\begin{aligned}
\pi &= \sum_{k=0}^\infty \frac{1}{16^{k}}
\left( \frac{4}{8k+1} - \frac{2}{8k+4} - \frac{1}{8k+5} - \frac{1}{8k+6}
\right) \\
&= P(1,16,8,(4,0,0,-2,-1,-1,0,0))
\end{aligned}
$$

The existence of a BBP formula to the base 2 for $\log 2$ already implies the existence of a base 2 formula for many other logarithms. For instance, a formula for $\log 3$ can be obtained by the following reasoning:
$$
\begin{aligned}
\log 3 &= 2 \log 2 + \log\left(1 - \frac{1}{4}\right) = 2 \sum_{k\geq 1} \frac{1}{k 2^k} - \sum_{k\geq 1} \frac{1}{k 4^k} \\
&= \frac{1}{2} \sum_{k\geq 0} \frac{1}{4^k} \left( \frac{2}{2k+1} + \frac{1}{2k+2} \right) - \frac{1}{4} \sum_{k\geq 0} \frac{1}{4^k} \left( \frac{2}{2k +2} \right) \\
&= \sum_{k \geq 0} \frac{1}{4^k} \left( \frac{1}{2k+1} \right) \\
&= P(1,4,2,(1,0))
\end{aligned}
$$
Where we made use of the fact that, given $m \in \N$, a BBP formula to the base $b$ can be rewritten as a BBP formula to the base $b^m$ by taking terms $m$ at a time:
$$
\sum_{k \geq 0} \frac{p(k)}{q(k)} b^{-k} = \sum_{k \geq 0} \left( \sum_{j=0}^{m-1} \frac{p(mk+j)}{b^j q(mk+j)} \right) b^{-mk}
$$ 

By combining the formulas for $\log(1-2^{-m})$, we can obtain a BBP formula to the base 2 for $\log q$ for any integer $q$ that can be written as
$$
q = \frac{(2^{a_1}-1)(2^{a_2}-1)\dotsm(2^{a_h}-1)}{(2^{b_1}-1)(2^{b_2}-1)\dotsm(2^{b_j}-1)}
.
$$

Machin-type BBP formulas
------------------------

The original Machin’s formula (1706) is the following identity:
$$
\pi/4 = 4\arctan(1/5) - \arctan(1/239).
$$
Machin used this formula to compute 100 digits of $\pi$. A _Machin-type formula_ for $\pi$ is a formula expressing $\pi$ as a $\Z$-linear combination of arctangents. In 2002, Yasumasa Kanada announced the record computation of 1.24 trillion decimal digits of $\pi$, using the identities
$$
\begin{multlined}[t][0.9\textwidth]
    \pi = 48\arctan(1/49) + 128 \arctan(1/57) \\
    \shoveright{- 20 \arctan(1/239) + 48 \arctan(1/110443)} \\
    \shoveleft{\pi = 176\arctan(1/57) + 28 \arctan(1/239)} \\
    - 48 \arctan(1/682) + 96 \arctan(1/12943)
\end{multlined}
$$

One way to understand these indentities is to observe that
$$
\arctan(y/x) = \Im \log(x + i y)
.
$$
Machin's formula is therefore a consequence of the factorisation $(2+2i) = (5+i)^4(230+i)$. Similarly, Kanada's identities can be verified by observing that the products
$$
(49 + i)^{48}(57+i)^{128}(239+i)^{-20}(110443+i)^{48}
$$
and
$$
(57 + i)^{176}(239+i)^{28}(682+i)^{-48}(12943+i)^{96}
$$
both yield negative rational numbers.

Machin-type BBP generators
--------------------------

Combining the log expansion with the above observation we define the _Machin-type BBP generators_ to be BBP generators of the form
$$
\begin{aligned}
\arctan(-b^{-m}) &= \Im \log(1-\i b^{-m})
= -b^{-m} \sum_{k \geq 0} \frac{(-1)^k}{2k+1} b^{-2mk}\\
&= b^{-3m} P(1,b^{4m},4,(-b^{2m},0,1,0)).
\end{aligned}
$$
where we assume that $b\geq 2$ and not a proper power. Setting $x = \pm 2^{-m}$ in the series expansion for $\arctan(x/(1+x))$ yields a binary BBP formula which is distinct from the generators above. Thus, when $b = 2$ we use additional generators (called _Aurifeuillian_) of the form
$$
\arctan\left(\frac{1}{1 \pm 2^m}\right) = \Im\log(1 \pm (1+i)2^{-m}).
$$

\exercise Express the above generators in terms of Bailey's generators $P(s,b,n,A)$.

\noindent
Note that the three generators $\arctan(-b^{-m})$ and $\arctan(1/(1 \pm 2^m)$ are not independent because
$$
\Im \log \left(1+(1+i)2^{-m}\right) = \Im \log \left( 1 - i 2^{1-2m} \right) - \Im \log \left( 1 - (1+i)2^{-m}\right).
$$

Finding Machin-type BBP arctangent formulas
-------------------------------------------

Using the formulas from the previous section, a BBP formula for $\pi/4$
follows almost immediately:
$$
\pi/4 = -\arctan(-1) = 2^{-4} P(1,2^{4},8,[8,8,4,0,-2,-2,-1,0]),
$$
which is the case $r = -1/4$ of the formula
$$
\begin{multlined}[t][0.9\textwidth]
\pi=\sum_{k=0}^\infty \frac{1}{16^{k}} \left( \frac{4+8r}{8k+1} - \frac{8r}{8k+2} - \frac{4r}{8k+3} - \frac{2+8r}{8k+4} \right.\\
\left. - \frac{1+2r}{8k+5} - \frac{1+2r}{8k+6} + \frac{r}{8k+7}\right),
\end{multlined}
$$
seen last week.

Other binary Machin-type BBP formulas for $\pi/4$ can be found by looking for products of the form
$$
z = \prod_j (2^{a_j} - i)^{n_j} \prod_j (2^{b_j} - 1 - i)^{m_j} = \alpha(1 + i)
$$
for some $\alpha \in \Q$. The first factor corresponds to the Machin-type generators with $b=2$ and the second factor to the Aurifeuillian generators.

A hand search for additional formulas soon reveals that
$$
\begin{aligned}
(2-i) (3-i) &= 5-5i \\
(2-i)^2 (7+i) &= 25-25i \\
(3-i)^2 (7-i) &= 50-50i
\end{aligned}
$$
corresponding to the formulas
$$
\begin{aligned}
\pi/4 &= \arctan(1/2) + \arctan(1/3) \\
\pi/4 &= 2\arctan(1/2) - \arctan(1/7) \\
\pi/4 &= 2\arctan(1/2) + \arctan(1/7) \\
\end{aligned}
$$

Carl Størmer proved in 1897 that these, together with Machin's formula, are the only four non-trivial integral solutions to
$$
m \arctan(1/u) + n \arctan(1/v) = k\pi/4
$$

Similarly, one can look for binary Machin-type BBP formulas for arctangents with arguments different from $1/(1 \pm 2^m)$

\exercise
Prove that $\arctan(1/6)=\arctan(1/5)-\arctan(1/31)$.

Non-binary Machin-type arctangent formula for $\pi$
---------------------------------------------------

Here we investigate the possibility of a non-binary formula of for $\pi$ of  Machin-type (there are no Aurifeuillian generators for $b > 2$). We first need some preliminary definitions.

\definition Given fixed $b>1$, we say that a prime $p$ is a _primitive prime factor_ of $b^m-1$ if $p$ divides $b^m-1$ but does not divide any $b^n-1$ for $n < m$. In other words, $m$ is the least integer such that $p$ divides $b^m-1$.

\theorem Bang (1886). The only cases where $b^m-1$ has no primitive prime factors are when $b=2$, $m=6$, (therefore $b^m-1=3^27$); and when $b=2^N-1$, $N\in\N$, $m=2$, (therefore $b^m-1=2^{N+1}(2^{N-1}-1)$).

Bang’s Theorem can be used as an exclusion criterium for binary arctangent Machin-type formulas for $\pi$:

\theorem Given $b>2$ and not a proper power, there is no $\Q$-linear $b$-ary Machin-type BBP arctangent formula for $\pi$.

If $\pi$ were to have a $\Q$-linear Machin-type BBP arctangent formula,
it would be of the form
$$
n\pi = \sum_{m=1}^M n_m \Im \log(b^m - i),
$$
where $n \in \N$, $n_m \in \Z$, and $M \geq 1$, $n_M \neq 0$. This implies that
$$
\prod_{m=1}^M (b^m - i)^{n_m} = \alpha e^{n i \pi} \in \Q
$$
for some $\alpha \in \Q$, $\alpha \neq 0$. For any $b > 2$ and not a proper power, it follows from Bang’s Theorem that $b^{4M}-1$ has a primitive prime factor, say $p$. Furthermore, $p$ must be odd since $p=2$ can only be a primitive prime factor of $b^m-1$ when $b$ is odd and $m=1$. Since $p$ is a primitive prime factor of it does not divide $b^{2M}-1$, and hence must divide $b^{2M}+1=(b^M-i)(b^M+i)$. Now $p$ cannot divide both $(b^M-i)$ and $(b^M+i)$ since this would give the contradiction that $p$ divides $(b^M-i)-(b^M+i)=2i$. It follows that $p$ factors as $p=\frak{p}\bar{\frak{p}}$ over $\Z[i]$, where $\frak{p}$ and $\bar{\frak{p}}$ are conjugate primes in $\Z[i]$, and with exactly one of $\frak{p}$, $\bar{\frak{p}}$ dividing $b^M-i$. Furthermore, for $m<M$ neither $\frak{p}$ nor $\bar{\frak{p}}$ can divide $b^m-i$ since this would imply that $p$ divides $b^{4m}-1$ with $m<M$, contradicting the fact that $p$ is a primitive prime factor of $b^{4M}-1$. So, we conclude that the left hand side of is divisible by exactly one of $\frak{p}$, $\bar{\frak{p}}$ but not by the other, while any non-zero number in $\Q$ if divisible by either $\frak{p}$ or $\bar{\frak{p}}$ is also divisible by the other. Hence we arrive at a contradiction.


# References
\setlength{\parindent}{0cm}
