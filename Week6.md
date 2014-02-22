Guessing integer sequences
==========================

So far we have learned how to guess a floating point numbers in terms of
known mathematical constants, and how to experimentally derive new
identities for known constants, such as the BBP formulas. In this
section we will study some problems where one would like to know a
“closed form" formula for an integer sequence $(a_n)_{n\geq 0}$. Much of
the material in the following is taken from @K99 [@Z05].

A valuable online resource is the *The On-Line Encyclopedia of Integer
Sequences* at

<http://www.research.att.com/~njas/sequences/>

This database contains many sequences, and information about sequences,
that have occurred in the mathematics literature. If you enter a
sequence that is stored in the OEIS, you will receive information about
your sequence. If your sequence does not already occur in the database,
then a software package called `Superseeker` tries to identify the
sequence. We will discuss a particular class of sequences that can be
identified by `Superseeker`, or more precisely, by the freely available
`Mathematica` package `rate.m` (Rate! is German for Guess!) from
Christian Krattenthaler, see

<http://www.mat.univie.ac.at/~kratt/rate/rate.html>

A copy is available also from the subject webpage

<http://www.ms.unimelb.edu.au/~s620445/>

Another powerful software tool is the Maple package `gfun`.

Consider the sequence $(a_n)_{n\geq 1}$,
$$a_n=1,2,6,24,120,720,5040,\ldots$$ If you haven’t already recognised
this sequence, you might try to recognise the sequence $(b_n)_{n\geq 1}$
of ratios $b_n=a_{n+1}/a_n$, $$b_n=2,3,4,5,6,7,\ldots$$ Guessing that
$b_n=n+1$, we discover that $a_{n+1}=(n+1)a_n$, and thus that $a_n=n!$.

Consider the sequence $$a_n=1,2,5,14,42,132,429,1430,\ldots$$ This may
also look familiar, but let’s consider again the sequence of ratios
$$b_n = 2,\frac52,\frac{14}{5},3,\frac{22}{7},\frac{13}{4},\frac{10}{3},\ldots$$
which you may be able to recognise as
$$b_n = \frac{2+4n}{2+n},\quad n=1,2,\ldots$$ It follows that the
original sequence were the Catalan numbers
$$a_n = \frac{1}{n+1} \binom{2n}{n}.$$

What makes these examples easy is that the ratios $b_n$ are simple
rational expressions of the form b~n~ = . [ratio] Let’s look at another
example:

[ASMex] Consider the sequence a~n~ = 1,2,7,42,429,7436,218348,10850216,
911835460,…[ASMan] These numbers count $n\times n$ **alternating sign
matrices**, which we will encounter in detail below. Trying again to
recognise the sequence of ratios $b_n=a_{n+1}/a_n$
$$b_n = 2, \frac{7}{2}, 6, \frac{143}{14}, \frac{52}{3}, \frac{323}{11}, \frac{646}{13},\ldots,$$
by fitting it to the form does not lead to success. $$$$

In trying to find a formula for $a_n$ in Example [ASMex] one might
consider looking at the prime factors of $a_n$. The `Mathematica`
command $$\texttt{FactorInteger[\{1,2,7,42,429,7436,218348\}]}$$ gives
the following output

$$\begin{aligned}
&\texttt{\{\{\{1,1\}\},}\\
&\texttt{\{\{2,1\}\},}\\
&\texttt{\{\{7,1\}\},}\\
&\texttt{\{\{2,1\},\{3,1\},\{7,1\}\},}\\
&\texttt{\{\{3,1\},\{11,1\},\{13,1\}\},}\\
&\texttt{\{\{2,2\},\{11,1\},\{13,2\}\},}\\
&\texttt{\{\{2,2\},\{13,2\},\{17,1\},\{19,1\}\}\},}\end{aligned}$$

where $\{p,m\}$ means that the prime factor $p$ occurs with multiplicity
$m$. Obviously there is some structure here, but which one? Instead of
looking at the ratios $b_n$ we might look at the ratios of the ratios,
$c_n=b_{n+1}/b_n$,
$$c_n = \frac{7}{4}, \frac{12}{7}, \frac{143}{84}, \frac{56}{33}, \frac{969}{572}, \frac{22}{13}, \frac{115}{68}\ldots$$
While this is still not of the form , it turns out that $c_n$ is a ratio
of two quadratics in $n$: c~n~ = . [ASMcn]

Find the parameters in , and show that c~n~ = .

Unfolding the ratio we finally get
$$b_n = \frac{\binom{3n+1}{2n+1}}{\binom{2n-1}{n-1}},$$ which in turn
implies
$$a_n = \prod_{j=1}^n \frac{\binom{3j+1}{2j+1}}{\binom{2j-1}{j-1}},$$ or
equivalently $$a_n = \prod_{j=0}^{n-1} \frac{(3j+1)!}{(n+j)!}.
\label{ASMsimple}$$

The package `rate.m` automises the procedure above, by trying to fit
simple rational forms to ratios, ratios of ratios and so on. After
loading the package with

$$\begin{aligned}
&\texttt{SetDirectory["$\ldots$"];}\\
&\texttt{<<rate.m;}\end{aligned}$$

The command $$\texttt{Rate[1,2,7,42,429,7436,218348,10850216]},$$ or
$$\texttt{Apply[Rate[\{1,2,7,42,429,7436,218348,10850216\}]]},$$
produces an answer which is equivalent to .

Alternating sign matrices
=========================

The sequence counts the number of alternating sign matrices (ASMs), a
generalisation of permutation matrices. ASM occurred naturally in the
study of a deformation of Dodgson’s condensation algorithm to compute
determinants. A nice review on ASMS is @Bressoud. ASMs are matrices with

​(i) entries taken from $\{-1,0,1\}$,\
(ii) row- and column-sums are equal to $1$,\
(iii) in each row and each column, the non-zero entries alternate in
sign.

There are seven $3\times 3$ ASMs

$$\begin{aligned}
&\begin{pmatrix}
1 & 0 & 0 \\
0 & 1 & 0 \\
0 & 0 & 1
\end{pmatrix},\quad
\begin{pmatrix}
0 & 1 & 0 \\
1 & 0 & 0 \\
0 & 0 & 1
\end{pmatrix},\quad
\begin{pmatrix}
1 & 0 & 0 \\
0 & 0 & 1 \\
0 & 1 & 0
\end{pmatrix},\quad
\begin{pmatrix}
0 & 1 & 0 \\
0 & 0 & 1 \\
1 & 0 & 0
\end{pmatrix},\\
&\begin{pmatrix}
0 & 0 & 1 \\
1 & 0 & 0 \\
0 & 1 & 0
\end{pmatrix},\quad
\begin{pmatrix}
0 & 0 & 1 \\
0 & 1 & 0 \\
1 & 0 & 0
\end{pmatrix},\quad
\begin{pmatrix}
0 & 1 & 0 \\
1 & -1 & 1 \\
0 & 1 & 0
\end{pmatrix}\end{aligned}$$

The algorithm of Reverend Charles Ludwig Dodgson (aka Lewis Carroll)
goes as follows:\
Set
$$A_{i,j}^{(0)}=1,\quad A_{i,j}^{(1)}=a_{ij},\quad 1\leq i,j\leq n.$$
Iterate for $2\leq k \leq n$ and $1 \leq i,j\leq n-k+1$ the following:
A~a,b~^(k)^ = ( A^(k-1)^~a,b~ A^(k-1)^~a+1,b+1~ - A^(k-1)^~a,b+1~
A^(k-1)^~a+1,b~ ). [Dodg] The final output will be
$\det A = A^{(n)}_{1,1}$. Robbins and Rumsey generalised by replacing
$-1$ by $\lambda$: A~a,b~^(k)^ = ( A^(k-1)^~a,b~ A^(k-1)^~a+1,b+1~
+A^(k-1)^~a,b+1~ A^(k-1)^~a+1,b~ ). [RR] The $\lambda$-determinant
$\det_\lambda A$ is now defined as the output $A_{1,1}^{(n)}$ of the
recursion . Surprisingly, this turns out to be a (Laurent) polynomial in
the entries. Furthermore, the expansion of the $\lambda$-determinant
generalising
$$\det A = \sum_{\pi\in S_n} (-1)^{\pi} \prod_{i=1}^n a_{i,\pi(i)},$$
becomes
$$\det {}_{\lambda} A = \sum_{B\in \mathcal{A}(n)} \lambda^{I(B)}(1+\lambda^{-1})^{N(B)} \prod_{1\leq i,j\leq n}^n a_{ij}^{B_{ij}},$$
where $\mathcal{A}(n)$ is the set of alternating sign matrices, $I(B)$
is the so-called inversion number and $N(B)$ is the number of $(-1)s$ in
the matrix $B$, see @Bressoud.

Enumeration
-----------

One of the first questions to be answered is how many ASMs there are. In
order to enumerate ASMs efficiently, we first transform them by taking
partial column sums for each column yielding, e.g., $$\begin{pmatrix}
0 & 0 & 1 & 0 & 0 \\
0 & 0 & 0 & 1 & 0 \\
1 & 0 & 0 & -1 & 1 \\
0 & 1 & -1 & 1 & 0\\
0 & 0 & 1 & 0 & 0
\end{pmatrix},
\quad \rightarrow\quad
\begin{pmatrix}
0 & 0 & 1 & 0 & 0 \\
0 & 0 & 1 & 1 & 0 \\
1 & 0 & 1 & 0 & 1 \\
1 & 1 & 0 & 1 & 1\\
1 & 1 & 1 & 1 & 1
\end{pmatrix}$$ Because of the alternating condition, the new matrix has
only $0$’s and $1$’s, and because each columns starts and ends with a
$1$, the bottom row has all ones. Next we record for each row the
location of the $1$’s. In the example above we get
$$\begin{array}{ccccccccc}
&&&& 3 &&&&\\
&&& 3 & & 4 &&&\\
&& 1 && 3 && 5 &&\\
& 1 && 2 && 4 && 5 &\\
1 && 2 && 3 && 4 && 5
\end{array}$$ These triangles are called **monotone triangles**, and are
defined by the properties that entries in each row are strictly
increasing, and every entry is weakly between the two entries right
below it.

Monotone triangles are easy to enumerate using a **recurrence** scheme.
Let us consider the slightly more general problem of finding the number
$F(a_1,\ldots,a_n)$ of monotone triangles whose bottom row is
$a_1,\ldots, a_n$. Given the bottom row, it follows from the conditions
above that the second-row from the bottom, let’s call it
$b_1,\ldots,b_{n-1}$, has to satisfy the following restrictions:

$$\begin{aligned}
&a_1\leq b_1 \leq a_2 \leq b_2 \leq \ldots \leq b_{n-1} \leq a_n,
\label{bCond1}\\
&b_1 < b_2 < \ldots b_{n-1}.
\label{bCond2}\end{aligned}$$

Furthermore, the number $F(a_1,\ldots,a_n)$ is recursively given by
F(a~1~,…,a~n~) = ~(b~1~,…,b~n-1~)~ F(b~1~,…,b~n-1~), [ASMF] where the
summation is over all $b$’s that satisfy and .

The `Mathematica` notebook `ASMblist.nb`, available from the subject
homepage, generates all possible configurations of
$b=(b_1,\ldots,b_{n-1})$ given an input $a=(a_1,\ldots,a_n)$. Implement
and compute $F(1,2,\ldots,n)$, the number of $n\times n$ ASMs. Compare
your answer with .

[Horizontally symmetric ASMs] Horizontally symmetric ASMs (HSASMs) of
size $(2n+1)\times (2n+1)$ are ASMs which are symmetric with respect to
reflection in the horizontal symmetry axis. There is, for example, only
one HSASM of size 3. Show that HSASMs of size $(2n+1)\times (2n+1)$ are
enumerated by $F(1,3,5,\ldots,2n+1)$. Compute the number of HSASMs and
derive a closed form formula.

Discrete Hirota equation
------------------------

A not well understood appearance of ASM and related numbers is the
following. Consider again the Robbins-Rumsey recurrence and set
$\lambda=1$. Let us furthermore use the change of variables
$$n=k+a+b,\qquad m=a+b+2k,$$ and write $f_{n,m,k} = A^{(k)}_{a,b}$. Then
becomes
$$f_{n,m,k}f_{n-2,m-2,k-2} = f_{n-1,m-2,k-1}f_{n-1,m,k-1} + f_{n-2,m-1,k-1}f_{n,m-1,k-1},$$
which is the discrete Hirota equation. We arrive at the discrete
Boussinesq equation by imposing the periodicity $f_{n,m,k}=f_{n,m,k-1}$,
f~n,m~f~n-2,m-2~ = f~n-1,m-2~f~n-1,m~ + f~n-2,m-1~f~n,m-1~. [Bouss]

Solve using `Mathematica` with the initial conditions
$$f_{-1,m}=1,\quad f_{0,m}=1,\quad f_{n,m\leq 2n-1}=0.$$ Try to
recognise sequences of the form $f_{n,2n+s}$ for $s=0,1,\ldots$. Can you
find a general formula for the full solution $f_{n,m}$?
