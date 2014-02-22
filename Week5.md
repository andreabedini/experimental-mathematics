BBP formulas
============

Throughout history it has been important to calculate accurate values of
$\pi$. In ancient times just a few digits were known, but in more modern
times computing billions of digits has become a fascination, not to say
obsession, of many computational mathematicians. The first rigorous
technique for approximating $\pi$ was found by Archimedes (ca. 250 BC),
who used a scheme based on inscribed and circumscribed polygons to
obtain the bounds $$3\tfrac{10}{71} < \pi < 3\tfrac{1}{7}.$$ Archimedes’
scheme is the first algorithm which is capable to produce an arbitrarily
accurate value of $\pi$, and it can be stated as simply iterating the
following recursion,

$$\begin{aligned}
a_{n+1} &= \frac{2a_n b_n}{a_n+b_n},\\
b_{n+1} &= \sqrt{a_{n+1}b_n},\end{aligned}$$

with the initial condition $a_0=2\sqrt{3}$, $b_0=3$.

Check that Archimedes’ scheme converges to $\pi$, and estimate the rate
of convergence.

There are also remarkable series approximations to $\pi$, such as the
following one by Ramanujan,
$$\frac{1}{\pi} = \frac{2\sqrt{2}}{9801}\sum_{k=0}^{\infty} \frac{(4k)!(1103+26390k)}{k!^4 396^{4k}},$$
for which each term produces eight additional correct digits. A series
which produces an additional fourteen correct digits was found by David
and Gregory Chudnovsky,
$$\frac{1}{\pi} = 12\sum_{k=0}^{\infty} \frac{(-1)^k (6k)!(13591409+545140134k)}{(3k)!k!^3640320^{3k+3/2}}.$$

While the formulas above are efficient, the convergence to the correct
value of $\pi$ is linear: the number of terms one needs to compute
increases linearly with the desired accuracy. In the past thirty years
(much) faster algorithms have become available. Nowadays there are
recursions known, which converge quadratically, cubically and even
quartically for which, at each iteration step, the number of correct
digits approximately doubles, triples or quadruples @BB1 [@H05].

These “classical" formulas for $\pi$ share the property that to compute
the correct $n$th digit, one needs to compute the first $n-1$ digits,
and moreover, perform the calculations in the same high precision. Is it
possible to find a formula for the $n$th digit of $\pi$ without having
to know the first $n-1$ digits? That such a formula might exist is
argued in the next section.

Individual digit algorithm for $\boldsymbol{\log}\, \boldsymbol{2}$ {#se:digit_alg}
-------------------------------------------------------------------

Consider the following well known formula for $\log 2$,
$$\log 2 = \sum_{k=1}^{\infty} \frac{1}{k\,2^k}.$$ This expression
allows us to compute isolated digits in the *binary* expansion of
$\log 2$. This works as follows. To compute the first few binary digits
beginning at position $d+1$ is equivalent to computing the fractional
part of $2^d \log 2$, denoted by $\{2^d \log 2\}$. It thus follows that

$$\begin{aligned}
\{2^d \log 2\} &= \left\{ \left\{ \sum_{k=1}^d \frac{2^{d-k}}{k} \right\} + \left\{\sum_{k=d+1}^{\infty} \frac{1}{k\,2^{k-d}}\right\}\right\}\nonumber\\
&= \left\{ \left\{ \sum_{k=1}^d \frac{2^{d-k} \bmod k}{k} \right\} + \left\{\sum_{k=d+1}^{\infty} \frac{1}{k\,2^{k-d}}\right\}\right\},
\label{log2}\end{aligned}$$

where we can insert “$\bmod k$" in the first summand as we are only
interested in the fractional part of the quotient after dividing by $k$.
A key point is now that $2^{d-k} \bmod k$ can be computed very rapidly,
see below. To efficiently compute the $(d+1)$th binary digit of $\log 2$
we thus need to do the following:

-   Compute each numerator of the first sum of .

-   Divide each numerator by $k$ and sum the terms while discarding any
    integer parts.

-   Evaluate the second sum–only a few terms are needed as it converges
    rapidly.

-   Add the two summations, discarding any integer part. The resulting
    fraction gives the first few digits of the binary expansion of
    $\log 2$ beginning at position $d+1$.

Binary algorithm for exponentiation
-----------------------------------

Expressions of the form $b^n \mod k$ can be computed very efficiently by
means of the binary algorithm for exponentiation. This ancient algorithm
is based on the observation that exponentiation can be performed
efficiently using a factorisation based on the binary expansion of $n$.
For example, we can write $3^{13}$ as
$(((3^2)^2)^2)\cdot ((3^2)^2)\cdot 3$ producing the result in only six
multiplications instead of twelve. As we need the end result only
$\bmod\, k$ we can compute the intermediate results $\bmod\, k$ which
significantly reduces the precision needed to perform the computation.
The `Mathematica` implementation of exponentiation $\bmod\, k$ is
`PowerMod[]`.

BBP formulas
------------

With the above in mind, one may wonder whether there exists individual
digit algorithms for binary expansion of other mathematical constants
such as $\pi$. Bailey, Borwein and Plouffe undertook a search for
formulas of the form for other constants, i.e., for any constant
$\alpha$ can we find integer polynomials $p$ and $q$ such that
$$\alpha = \sum_{k=0}^{\infty} \frac{p(k)}{q(k)2^k}.$$ Of course we do
not need to be restricted to base 2, and for general base $b$ can try to
find the following BBP representation for $\alpha$, = ~k=0~^^ .

[ass:PiBBP] Using the trial formula
$$\pi = \sum_{k=0}^{\infty} \frac{1}{b^k} \sum_{j=1}^n \frac{a_j}{(nk+j)^s},$$
find a BBP formula for $\pi$ in base 16 using PSLQ. Prove your formula.

[Answer to Exercise [ass:PiBBP]] For $b=16$, $s=1$ and $n=8$, we use
PSQL,

$$\begin{aligned}
&\texttt{s[j\_]:=Sum[16\^{}(-k)/(8k+j),\{k,0,100\}];}\\
&\texttt{PSLQ[Join[\{N[Pi,100]\},Table[s[j],\{j,1,8\}]]]}\end{aligned}$$

to find that =~k=0~^^ ( - - - ). [PiBBP] In fact, this formula is the
special case $r=0$ of

$$\begin{gathered}
\pi=\sum_{k=0}^\infty \frac{1}{16^{k}} \left( \frac{4+8r}{8k+1} - \frac{8r}{8k+2} - \frac{4r}{8k+3} - \frac{2+8r}{8k+4} \right.\\
\left. - \frac{1+2r}{8k+5} - \frac{1+2r}{8k+6} + \frac{r}{8k+7}\right),
\label{Pi_r}\end{gathered}$$

where $r$ can be any real or complex number.

Knowing the result, equation  can be proved by elementary manipulations.
First note that

$$\begin{gathered}
\sum_{k=0}^{\infty} \frac{1}{16^k(8k+j)} = 2^{j/2} \int_0^{1/\sqrt{2}} \sum_{k=0}^{\infty} x^{j-1+8k}\, \d x \\
= 2^{j/2} \int_0^{1/\sqrt{2}} \frac{x^{j-1}}{1-x^8}\,\d x = 16\int_0^{1} \frac{y^{j-1}}{16-y^8}\,\d y,\end{gathered}$$

where the last equality follows from subsituting $y=\sqrt{2} x$. It then
follows that the right hand side of is equal to = 16~0~^1^ y = 16~0~^1^
y, [PiBBPint] where the last equality follows from cancelling common
factors in the numerator and denominator. The final integral can be
further simplified, but already in this form `Mathematica` can find the
anti-derivative of the integrand in a simple form:
$$-2\left(2\arctan(1-y) + \log |2-2y+y^2|-\log |2-y^2|\right) +C.$$
(Prove that the derivative of this is indeed equal to the integrand in
). It is now easy to show that the integral equals $\pi$.

[pi2BBP] Compute BBP formulas for $\pi^2$ and $\pi\sqrt{3}$, both in
base $64=2^6$ and base $729=3^6$. There exists a BBP formula for the
Catalan number $C$ in base $4096=2^{12}$, can you find it?

[Answer to Exercise [pi2BBP]] In the the notation of the next section:

$$\begin{aligned}
\pi^2 &= \frac{9}{8} P(2,64,6,[16,-24,-8,-6,1,0])\\
&= \frac{2}{27} P(2,729,12,[243,-405,-81,-27,-72,-9,-9,-5,1,0]),\\
\pi\sqrt{3} &= \frac{1}{4} P(1,64,6,[20,6,-1,-3,-1,0])\\
&= \frac{1}{9} P(1,729,12,[81,-54,0,-9,0,-12,-3,-2,0,-1,0,0]),\\\end{aligned}$$

[$\pi$ and the Golden Ratio] Find a formula for $\pi^2$ with the
non-integer base $\phi^5$, where $\phi$ is the Golden Ratio. Hint: look
for an expression for $\pi^2\phi^5$ where the numbers $a_j$ in its BBP
expansion of the form
$$\pi^2\phi^5 = \sum_{k=0}^{\infty} \frac{1}{\phi^{5k}} \sum_{j=1}^n \frac{a_j}{(nk+j)^s}$$
can be multiples of powers of $\phi$.

Digit extractor
---------------

Let $\alpha$ be a number given in the presentation = ~k=0~^^ ~j=1~^n^ .
[alphaBBP] To compute the first few digits of $\alpha$, starting at
position $d$ we need to implement the algorithm described in Section
[se:digit~a~lg] for each $j$th term in . After splitting
$\{b^{d-1} \alpha\}$ into two sums, it is efficient to compute the first
sum using the following algorithm for computing
$S_1=\sum_{k=0}^{d-1} f_k$,

$$\begin{aligned}
&\texttt{S1=0};\\
&\texttt{Do[S1=S1+f[[k]],\{k,0,d-1\}];}\end{aligned}$$

For the second sum the command `Sum[]` can be used, and a small number
of terms suffices to obtain a good approximation.

[`DigitExtractor`] Program a digit extractor for $\alpha$ in
`Mathematica`. Input should be the position $d$ at which you want to
compute the digits of $\alpha$, the base $b$, and the list of
coefficients $a_j$ $j=1,\ldots,n$ in the BBP expansion of $\alpha$. So
your program should start as
$$\texttt{DigitExtractor[d\_,b\_,a\_List] := Module[$\ldots$}$$ Make use
of the `Mathematica` commands `FractionalPart[], PowerMod[]` and
`RealDigits[]`.

A simple notebook `DigitExtractor.nb` is available from the subject
homepage,

<http://www.ms.unimelb.edu.au/~s620445/DigitExtractor.nb>

and @AW96 contains a faster and slightly more complicated code. This
program computes, for example, the digits of $\pi$ starting at position
$d=1000$ by issuing
$$\texttt{DigitExtractor[1000,16,\{4,0,0,-2,-1,-1,0,0\}]},$$ which gives
the output {3,2,4,3,15,1,12,0,9}. You can check with the command
$$\texttt{RealDigits[N[Pi,1300],16][[1,Range[1001,1008]]]},$$ that these
are indeed the correct first eight digits in the hexadecimal expansion
of $\pi$.

Machin-type BBP formulas for arctangents
========================================

This section, largely based on @BBG, is devoted to some of the theory
behind BBP formulas of the form = P(s,b,n,A), [BBP1] where
P(s,b,n,A)=~k=0~^^ ~j=1~^n^ , [BBP~P~def] and
$A=[a_1,\ldots,a_n]\in\Z^n$. We will also write
$$P(s,b,n,A) = \sum_{j=1}^n a_j L(s,b,n,j),$$ and call $L(s,b,n,j)$ *BBP
generators*.

If $s=1$, we will call representations of the form *logarithmic* because
for $s=1$ resembles the expansion of $-\log(1-x)$: -(1-x) = ~k=1~^^.
[logseries] By the same token, for general $s$ a BBP representation of
the form is called *polylogarithmic* due to the similarity of with the
polylogarithm $\Li_s(z)$ defined by
$$\Li_s(z) = \sum_{k=1}^\infty \frac{z^k}{k^s}.$$

Machin’s formula and Gravé’s problem
------------------------------------

Machin’s formula (1706) is the following identity /4 = 4(1/5) -(1/239),
[Machin] which was used by Machin to compute 100 digits of $\pi$. In the
following we will call a formula which expresses $\pi$ a s general
$\Z$-linear combination of arctangents a *Machin-type* formula for
$\pi$. Machin’s formula is a particular solution to Gravé’s problem,
which comprises that there are only four non-trivial solutions to
$$m\arctan(1/u) + n\arctan(1/v)=k\pi/4,$$ namely Machin’s formula and

$$\begin{aligned}
&\pi/4 = \arctan(1/2) +\arctan(1/3),\quad \;\;(\text{Euler},\; 1738)\nonumber\\
&\pi/4 = 2\arctan(1/2) -\arctan(1/7),\quad (\text{Hermann},\; 1706)
\label{Grave}\\
&\pi/4 = 2\arctan(1/3) +\arctan(1/7),\quad (\text{Hutton},\; 1776).\nonumber\end{aligned}$$

Gravé’s problem was settled by Størmer in 1897.

While at first somewhat mysterious, Machin’s formula is a simple
consequence of the following observations: (y/x) = (x+iy) , [arctan] and
$$(5+\i)^4(239+\i)^{-1} = 2+2\i.$$ Because $\pi/4=\arctan(1)$, these two
observations immediately imply .

[ex:Grave] Using properties of $\Z[\i]$, the Gaussian integers, prove
the equalities .

Machin-type BBP generators
--------------------------

Combining with we define *Machin-type BBP generators* to be BBP
generators of the form

$$\begin{aligned}
\arctan(-b^{-m}) &= \Im \log(1-\i b^{-m}) = -b^{-m} \sum_{k=0}^{\infty} \frac{(-1)^k}{2k+1} b^{-2mk},\nonumber\\
&= b^{-3m} P(1,b^{4m},4,[-b^{2m},0,1,0]).
\label{MachinBBPb>2}\end{aligned}$$

In this definition we have assumed $b\geq 2$, and $b\neq c^n$ for
$c,n\in\N$ and $n>1$. A binary BBP generator is obtained from expanding
$\arctan(x/(1+x))$ and setting $x=\pm2^{-m}$. Thus, for $b=2$ we use the
additional generators

$$\begin{aligned}
\arctan\left(\frac{1}{1-2^m}\right) = \Im\log(1-(1+\i)2^{-m}).\end{aligned}$$

[arctanBBPbin] Let $P(s,b,n,A)$ be as in . Then,

$$\begin{gathered}
\Im\log(1-(1+\i)2^{-m}) = \\2^{-7m+3} P(1,2^{8m-4},8,[-2^{6m-3},-2^{5m-2},-2^{4m-2},0,2^{2m-1},2^{m},1,0]).\end{gathered}$$

Prove and Lemma [arctanBBPbin].

The Machin-type BBP generators defined above are based on arctangents.
The **logarithmic** Machin-type generators are (1-b^-m^) = -b^-m^
~k=0~^^ b^-mk^ = -b^-m^P(1,b^m^,1,[1]), [BBPloggenb]

$$\begin{gathered}
\Re \log(1-(1+\i)2^{-m}) = 2^{-8m+4} P(1,2^{8m-4},8,[-2^{7m-4},0, \\
2^{5m-3},2^{4m-2},2^{3m-2},0,-2^{m-1},-1]).
\label{BBPloggen2}\end{gathered}$$

Both arctangent and logarithmic binary generators may be derived from
extracting imgaginary and real parts from

$$\begin{aligned}
\log(1-(1+\i)2^{-m}) &= -\sum_{r=1}^8 (1+\i)^r \sum_{k=0}^{\infty} \frac{(1+\i)^{8k}}{8k+r} 2^{-8mk} \\
&= -\sum_{r=1}^8 (1+\i)^r \sum_{k=0}^{\infty} \frac{1}{8k+r} 2^{(4-8m)k} .\end{aligned}$$

Finding Machin-type BBP arctangent formulas
-------------------------------------------

Using the formulas from the previous section, a BBP formula for $\pi/4$
follows almost immediately:
$$\pi/4 = -\arctan(-1) = 2^{-4}P(1,2^{4},8,[8,8,4,0,-2,-2,-1,0]),$$
which is the case $r=-1/4$ from . Other binary Machin-type BBP formulas
for $\pi/4$ can be found by searching for products of the form z=~j~
(2^m~j~^-i)^k~j~^ ~j~ (2^m~j~^-1-i)^l~j~^, [binaryPi/4] or z=~j~
(2^m~j~^+1+i)^k~j~^ ~j~ (2^m~j~^-1-i)^l~j~^, [binaryPi/4b] which equal
$\alpha(1+\i)$ for some $\alpha\in\R$. Note that and are not independent
due to
$$\Im \log(1+(1+\i)2^{-m}) = \Im\log(1-\i2^{1-2m}) - \Im\log(1-(1+\i)2^{-m}).$$
The first factor in corresponds to the generator with $b=2$ and the
second factor to the generator in Lemma [arctanBBPbin]. These give
precisely the solutions to Gravé’s problem, see Exercise [ex:Grave].

Similarly, one can look for binary Machin-type BBP formulas for
arctangents with arguments different from $1/(1\pm2^m)$

Prove that $$\arctan(1/6)=\arctan(1/5)-\arctan(1/31).$$

Non-binary Machin-type arctangent formula for $\boldsymbol{\pi}$
----------------------------------------------------------------

We investigate the possibility of a non-binary formula of for $\pi$ of
arctangent Machin-type. We first need some preliminary definitions.

Given fixed $b>1$, we say that a prime $p$ is a **primitive prime
factor** of $b^m-1$ if $m$ is the least integer such that $p$ divides
$b^m-1$.

[Bang (1886)] [Bang] The only cases where $b^m-1$ has no primitive prime
factors are when $b=2$, $m=6$, $b^m-1=3^27$; and when $b=2^N-1$,
$N\in\N$, $m=2$, $b^m-1=2^{N+1}(2^{N-1}-1)$.

Bang’s Theorem can be used as an exclusion criterium for binary
arctangent Machin-type formulas for $\pi$:

Given $b>2$ and not a proper power, there is no $\Q$-linear $b$-ary
Machin-type BBP arctangent formula for $\pi$.

If $\pi$ were to have a $\Q$-linear Machin-type BBP arctangent formula,
it would be of the form $$n\pi = \sum_{m=1}^M n_m \Im \log(b^m-\i),$$ or
~m=1~^M^ (b^m^-i)^n~m~^ ^ni^ = ^^. [prodpi] For any $b>2$, it follows
from Bang’s Theorem that $b^{4M}-1$ has a primitive prime factor, say
$p$. Furthermore, $p$ must be odd since $p=2$ can only be a primitive
prime factor of $b^m-1$ when $b$ is odd and $m=1$. Since $p$ is a
primitive prime factor, it does not divide $b^{2M}-1$, and hence must
divide $b^{2M}+1=(b^M-\i)(b^M+\i)$. Now $p$ cannot divide both
$(b^M-\i)$ and $(b^M+\i)$ since this would give the contradiction that
$p$ divides $(b^M-\i)-(b^M+\i)=2\i$. It follows that $p$ factors as
$p=\frak{p}\bar{\frak{p}}$ over $\Z[\i]$, where $\frak{p}$ and
$\bar{\frak{p}}$ are conjugate primes in $\Z[\i]$, and with exactly one
of $\frak{p}$, $\bar{\frak{p}}$ dividing $b^M-\i$. Furthermore, for
$m<M$ neither $\frak{p}$ nor $\bar{\frak{p}}$ can divide $b^m-\i$ since
this would imply that $p$ divides $b^{4m}-1$ with $m<M$, contradicting
the fact that $p$ is a primitive prime factor of $b^{4M}-1$. So, we
conclude that the left hand side of is divisible by exactly one of
$\frak{p}$, $\bar{\frak{p}}$ but not by the other, while any number in
$\Q^{\times}$ if divisible by either $\frak{p}$ or $\bar{\frak{p}}$ is
also divisible by the other. Hence we arrive at a contradiction.
