---
subject: Lecture Notes
title: MAST90053 Experimental Mathematics
author: "Lecturer: Dr Andrea Bedini"
date: 2014, Semester 1, Week 1
header-includes:
    \usepackage{tikz}
    \def\d{\mathrm{d}}
bibliography: bibliography.bib
---

Introduction
============

This course will give you the tools that many modern researchers use to develop _new mathematics_. It might appear that all mathematical theorems stem from a logical chain of arguments, as they are often presented in textbooks. More often then not, mathematicians formulate significant conjectures after playing around with a problem and a formal proof of the newly born theorem is only needed at a later stage.

This way of making progress was already used by Gauss, but in more modern times computers have played a large role in solving problems that would otherwise be intractable. This course aims to develop students’ mathematical inventiveness and imagination in mathematical experimentation using modern tools such as symbolic manipulation packages. You can find much of the material used in this course in the books @borwein2004mathematics @borwein2004experimentation @petkovvsek1996.

Learning `Mathematica`
----------------------

In this subject you will be required to write programs in `Mathematica`. Nevertheless this subject is not a programming course, and you will have to develop (basic) programming skills yourself with practice, either at home or during our computer lab sessions. Fortunately learning how to use `Mathematica`is very easy, and you will have plenty of time to get familiar with the software and its language.

`Mathematica` comes with an extensive documentation centre which you can access within the program or online. I encourage you to turn to the documentation centre whenever you need to work out how to do something in `Mathematica` or what a particular function does.

If needed, the book _An Introduction to Modern Mathematical Computing With Mathematica(R)_ by @borwein2012introduction suits perfectly the requirements of this subject.

A simple sum
============

Problem: determine the final digit $\ell_n$ of $\sigma_n = \sum_{k=1}^n\ k$.

Not knowing where to start, we can run a "computer experiment" and compute the first few terms. Here are the first 100 terms of $\sigma_n$ with the final digit highlighted:

\begin{alignat*}{11}
\mathbf{1},&& \mathbf{3},&& \mathbf{6},&& 1\mathbf{0},&& 1\mathbf{5},&& 2\mathbf{1},&& 2\mathbf{8},&& 3\mathbf{6},&& 4\mathbf{5},&& 5\mathbf{5} \\
6\mathbf{6},&& 7\mathbf{8},&& 9\mathbf{1},&& 10\mathbf{5},&& 12\mathbf{0},&& 13\mathbf{6},&& 15\mathbf{3},&& 17\mathbf{1},&& 19\mathbf{0},&& 21\mathbf{0} \\
23\mathbf{1},&& 25\mathbf{3},&& 27\mathbf{6},&& 30\mathbf{0},&& 32\mathbf{5},&& 35\mathbf{1},&& 37\mathbf{8},&& 40\mathbf{6},&& 43\mathbf{5},&& 46\mathbf{5} \\
49\mathbf{6},&& 52\mathbf{8},&& 56\mathbf{1},&& 59\mathbf{5},&& 63\mathbf{0},&& 66\mathbf{6},&& 70\mathbf{3},&& 74\mathbf{1},&& 78\mathbf{0},&& 82\mathbf{0} \\
86\mathbf{1},&& 90\mathbf{3},&& 94\mathbf{6},&& 99\mathbf{0},&& 103\mathbf{5},&& 108\mathbf{1},&& 112\mathbf{8},&& 117\mathbf{6},&& 122\mathbf{5},&& 127\mathbf{5} \\
132\mathbf{6},&& 137\mathbf{8},&& 143\mathbf{1},&& 148\mathbf{5},&& 154\mathbf{0},&& 159\mathbf{6},&& 165\mathbf{3},&& 171\mathbf{1},&& 177\mathbf{0},&& 183\mathbf{0} \\
189\mathbf{1},&& 195\mathbf{3},&& 201\mathbf{6},&& 208\mathbf{0},&& 214\mathbf{5},&& 221\mathbf{1},&& 227\mathbf{8},&& 234\mathbf{6},&& 241\mathbf{5},&& 248\mathbf{5} \\
255\mathbf{6},&& 262\mathbf{8},&& 270\mathbf{1},&& 277\mathbf{5},&& 285\mathbf{0},&& 292\mathbf{6},&& 300\mathbf{3},&& 308\mathbf{1},&& 316\mathbf{0},&& 324\mathbf{0} \\
332\mathbf{1},&& 340\mathbf{3},&& 348\mathbf{6},&& 357\mathbf{0},&& 365\mathbf{5},&& 374\mathbf{1},&& 382\mathbf{8},&& 391\mathbf{6},&& 400\mathbf{5},&& 409\mathbf{5} \\
418\mathbf{6},&&\ 427\mathbf{8},&&\ 437\mathbf{1},&&\ 446\mathbf{5},&&\ 456\mathbf{0},&&\ 465\mathbf{6},&&\ 475\mathbf{3},&&\ 485\mathbf{1},&&\ 495\mathbf{0},&&\ 505\mathbf{0},&&\ \ldots \\
\end{alignat*}

It is clear that the pattern repeats every 20 terms. With this insight is trivial to prove that indeed $\ell_{n+20} = \ell_n$.

\begin{align*}
\sigma_{n + 20} &= \sigma_n + \sum_{k=n+1}^{n+20}\ k
= \sigma_n + 20 n + \sum_{k=1}^{20}\ k \\
\intertext{Therefore}
\ell_{n + 20} &= \ell_n + 20 n + \sum_{k=1}^{20}\ k \mod{10} = \ell_n
\end{align*}

The arithmetic-geometric mean
=============================

This exercise was carried out by Gauss in 1799 (without the help of
computers but using numerical tables). Consider the arithmetic-geometric mean iteration:

$$
a_{n+1} = \frac{1}{2}(a_n+b_n)
\quad
b_{n+1} = \sqrt{a_n b_n}
$$

with $a_0=1$, $b_0=\sqrt{2}$.

\begin{exercise}
Show that the series $(a_n)$ and $(b_n)$ have the same common limit.
\end{exercise}

Use the following code to implement this recursion in `Mathematica`:

```Mathematica
Clear[a,b];
a[0]=N[1,200];
b[0]=N[Sqrt[2],200];
a[n_]:=a[n]=(a[n-1]+b[n-1])/2;
b[n_]:=b[n]=Sqrt[a[n-1]b[n-1]];
```

Make sure you understand the code above.

* What do `N[1,200]` and `N[Sqrt[2],200]` mean? Can't we simply write `1` and `Sqrt[2]`?

* Why are we writing `a[n_]:=a[n]=...` rather than `a[n_]:=...`? What about simply `a[n_]=...`

* Type `a[n]` into `Mathematica` and watch it hit the recursion limit. What is happening? How can it be prevented?

Now consider the following integral

$$
\frac{2}{\pi} \int_0^1 \frac{\d t}{\sqrt{1-t^4}},
$$

and use the numerical integration command

```Mathematica
NIntegrate[...,{WorkingPrecision->200,
    AccuracyGoal->100}]
```

to evaluate this integral to sufficient accuracy. Show that the answer
is within numerical precision equal to the inverse of the limit of
$a_n$ and $b_n$.

\begin{exercise}
Prove that the above integral is indeed the inverse of the limits of $a_n$ and $b_n$.
\end{exercise}

Lines in the plane
------------------

This exercise is borrowed from @Graham1994Concrete. How many slices of pizza can a person obtain by making n straight cuts with a pizza knife? Or, more academically: What is the maximum number $L_n$ of regions defined by $n$ lines in the plane? It is easy to see that

$$
L_0 = 1, \qquad L_1=2, \qquad L_2=4.
$$

Before you read on, formulate a prediction for $L_n$ based on these three cases.

\begin{marginfigure}[6cm]
\begin{tikzpicture}
    \begin{scope}
    \draw (30:-1.5) -- (30:1.5) (120:0.75) node {1} (120:-0.75) node {2} ;
    \end{scope}
    \begin{scope}[yshift=-3cm]
    \draw (30:-1.5) -- (30:1.5) (150:0.75) node {1} (150:-0.75) node {3};
    \draw (110:-1.25) -- (110:1.25) (70:-0.75) node {4} (70:0.75) node {2};
    \end{scope}
    \begin{scope}[yshift=-6cm]
    \draw (30:-1.5) -- (30:1.5) (150:0.75) node {1} (150:-0.75) node {3};
    \draw (110:-1.25) -- (110:1.25) (70:-0.75) node {4} (70:0.75) node {2};
    \draw[red]  (0,-0.5) +(-30:-1.25) -- +(-30:1.25);
    \end{scope}
\end{tikzpicture}
\end{marginfigure}

When we add the third line, we find that it can split at most three of the old regions, no matter how we have placed the frst two lines. Therefore $L_3=4+3=7$. The generalization follows easily: the $n$th line generates $k$ new regions if and only if it splits $k$ old regions. It splits $k$ old regions if and only if it crosses the previous lines in $k-1$ different places. Hence, to obtain the maximum number of regions, the $n$th line must cross the previous $n-1$ lines, which can be achieved by choosing it in such a way that it is not parallel to any of the previous lines. We thus get the recursion

$$
\begin{aligned}
L_0 &= 1\\
L_n &= L_{n-1} + n.
\end{aligned}
$$

To program this recursion in `Mathematica` we use the following code:

```Mathematica
Clear[L];
L[0]=1;
L[n_]:=L[n-1]+n;
Table[L[n],{n,1,10}]
```

Can you now guess a closed form formula for $L_n$? Use the _The On-Line Encyclopedia of Integer Sequences_ (https://oeis.org) to find out more.



[Visualisation] Pascal’s triangle has elements $\binom{m}{n}$:
$$\begin{array}{ccccccccc}
&&&& 1 &&&& \\
&&&1 && 1 &&&\\
&& 1 && 2 && 1 &&\\
& 1 && 3 && 3 && 1 &\\
1 && 4 && 6 && 4 && 1
\end{array}$$ What is the structure of the distribution of even and odd
numbers in this triangle? To visualise this we plot a point if
$\binom{m}{n} \equiv 1 \bmod 2$ and none otherwise. Write a little
program in `Mathematica` that does this. Commands that are useful are
`Binomial[m,n], Mod[x,2]` and `ListPlot[]`.

[Proof of Exercise 1.] Gauss’ discovery had a profound impact on
mathematics, and was the birth of a revolution in analysis. First note
that a~n+1~^2^ - b~n+1~^2^ = ()^2^. [eq:a2] It follows from this that
the two series have the same limit. Let $a_0=a$, $b_0=b$ and
$$M(a,b) = \lim_{n\rightarrow\infty} a_n = \lim_{n\rightarrow\infty} b_n.$$

The limit $M(a,b)$ has the following properties:

[lem:Mab]

$$\begin{aligned}
M(\lambda a, \lambda b) &=\lambda M(a,b)\nonumber\\
M(\frac{a+b}{2},\sqrt{ab}) &= M(a,b),\nonumber\\
M(1,x) &= \left(\frac{1+x}{2}\right) M(1,\frac{2\sqrt{x}}{1+x})
\label{eq:functionalM}\end{aligned}$$

[Proof of Lemma [lem:Mab]] The first line in Lemma [lem:Mab], the
homogeneity of $M(a,b)$, follows directly from the homogeneity of the
recursion . The second line follows from the fact that
$M(a_{n+1},b_{n+1}) =  M(a_n,b_n)$, and the third line follows from the
first two by choosing $\lambda = (a+b)/2$ and $x=b/a$.

Let I(x) = ~0~^/2^ . [eq:Idef] $I(x)$ is called the **complete elliptic
integral of the first kind**. Note that for $x=\sqrt{2}$, and using the
substitution $t=\sin\theta$, we have I() = ~0~^1^ , the integral
appearing in Gauss’ original observation. We will now show that, more
generally, = I(x). [eq:M=1/I] The idea behind the proof of is to show
that $I(x)$ satisfies . We will then argue that it is the unique
analytic solution to that equation.

First we rewrite $I(x)$ using the variable substitution $t=x\tan\theta$.
Using the following properties,
$$\frac{\d\theta}{\d t} = x^{-1}\cos^2\theta,\qquad \cos^2\theta = \frac{x^2}{x^2+t^2},\qquad \sin^2\theta = \frac{t^2}{x^2+t^2},$$
we find that
$$I(x) = \frac{2}{\pi} \int_0^{\infty} \frac{\d t}{\sqrt{(x^2+t^2)(1+t^2)}}.$$
Now we want use the variable substitution $s=\frac12 (t - \frac{x}{t})$,
or t^2^ = 2st +x (t-s)^2^ = s^2^ +x. [eq:t2s] We do not want to solve
for $t$ as this introduces square roots which are complicated to deal
with for symbolic packages. Moreover, for more general problems we need
higher order polynomials which cannot be solved, and we have to rely on
other methods. One elementary way to execute the change of variables
using `Mathematica` is to substitute $2st+x$ for $t^2$ in
$(x^2+t^2)(1+t^2)$, then expand and do the substitution again:

$$\begin{aligned}
&\texttt{Expand[(x\^{}2+t\^{}2)(1+t\^{}2) /.\,t\^{}2 $\rightarrow$ 2\,s\,t+x]};\\
&\texttt{Factor[\% /.\,t\^{}2 $\rightarrow$ 2\,s\,t+x]}\end{aligned}$$

This gives $(2st+x)(1+4s^2+2x+x^2)$ which is equal to
$t^2(4s^2+(1+x)^2)$.

Experiment with the `Mathematica` commands `Eliminate[]` and
`Resultant[]` for more sophisticated ways of incorporating such a change
of variables.

We also need to compute the Jacobian $|\d t/\d s|$ and this follows from
differentiating the first equation in with respect to $s$:
$$2t\frac{\d t}{\d s} = 2t+2s\frac{\d t}{\d s},$$ hence
$$\left|\frac{\d t}{\d s} \right| = \left|\frac{t}{t-s}\right| = \frac{|t|}{\sqrt{x+s^2}}$$
Combining everything, we thus find

$$\begin{aligned}
I(x) &= \frac{2}{\pi} \int_0^{\infty} \frac{\d t}{\sqrt{(x^2+t^2)(1+t^2)}}\\
&=\frac{2}{\pi} \int_{-\infty}^{\infty} \left|\frac{\d t}{\d s}\right|\frac{\d s}{\sqrt{t^2((1+x)^2+4s^2)}}\\
&= \frac{2}{\pi} \int_{-\infty}^{\infty} \frac{\d s}{2\sqrt{(x+s^2)((\frac{1+x}{2})^2+s^2)}}\\
&= \frac{2}{1+x} \frac{2}{\pi} \int_{0}^{\infty} \frac{\d s}{2\sqrt{(\frac{4x}{(1+x)^2}+s^2)(1+s^2)}}\\
&= \frac{2}{1+x} I\left(\frac{2\sqrt{x}}{1+x}\right).\end{aligned}$$

Now we would like to argue that is the unique solution to . Regarding
$x$ as a complex variable, we consider the function
$$f(z)=\frac{1}{M(1,\sqrt{1-z})}.$$ The function $f(z)$ satisfies the
recursion (1+z)f(z^2^) = f(1-(1-z)^2^/(1+z)^2^), [eq:frec] and assuming
$f(z)$ is analytic around the origin we can write f(z) = ~n0~ f~n~ z^n^,
[eq:analytic] in some open interval around $z=0$, and this expansion is
unique. Plugging into the recursion and solving for the first few
coefficients of $f_n$ we experimentally see that this indeed gives a
solution. Now we show that $I(\sqrt{1-z})$ is of this form.

First we expand $(1-z \sin^2(\theta))^{-1/2}$ around $z=0$ up to 10th
order,

$$\texttt{Series[(1-z Sin[t])\^{}(-1/2),\{z,0,10\}]},$$

from which we can recognise that
$$(1-z \sin^2(\theta))^{-1/2} = \sum_{n=0}^\infty \binom{2n}{n} \left(\frac{z\sin^2 \theta}{4}\right)^n.$$
Then we integrate term by term using,

$$\texttt{(2/Pi)Integrate[Sin[t]\^{}(2n),\{t,0,Pi/2\}]},$$

and find that
$$I(\sqrt{1-z}) = \sum_{n=0}^{\infty} \binom{2n}{n}^2 \left(\frac{z}{16}\right)^{n}.$$
Substituting back $z=1-x^2$ we obtain
$$I(x) = {}_2F_1(\tfrac12,\tfrac12;1;1-x^2),$$ where ${}_2F_1(a,b;c;z)$
is the **hypergeometric series** defined by ~2~F~1~(a,b;c;z) = ~n=0~^^
z^n^, [eq:hypergdef] and where $(a)_0=1$ and for $n$ a positive integer
$$(a)_n = a(a+1)\cdots (a+n-1),$$ so that $(1)_n=n!$. Hence we see that
$I(\sqrt{1-z})$ is of the form .

Finally, for $x=\sqrt{2}$ we have that
$$M(1,\sqrt{2}) = \frac{1}{I(\sqrt{2})} = {}_2F_1(\tfrac12,\tfrac12;1;-1)^{-1} = \frac{\sqrt{\pi} \Gamma(3/4)}{2\Gamma(5/4)} = 1.198140234\ldots$$

Hypergeometric series will be reappearing later in this course. The
functional equation is a particular case of the more general quadratic
Gaussian transformation formula ~2~F~1~(a,b;2b;) =(1+z)^2a^
~2~F~1~(a,a-b+12;b+12;z^2^).

The result is in fact used in the implementation of the `Mathematica`
function `ArithmeticGeometricMean[a,b]`, see the help browser of
`Mathematica`.

# Practice

Exercise
--------

Compute $A^{10^6}$ and $A^{-7}$ for
$$
A=\begin{pmatrix}
3/2 & 1/2\\
-1/2 & 1/2
\end{pmatrix}.
$$

Use `Mathematica` to evaluate $A^n$ for various integers $n$:
```Mathematica
A={{3/2,1/2},{-1/2,1/2}}
Table[MatrixForm[MatrixPower[A,n]],{n,1,6}]
```
and deduce a formula for $A^n$. Prove this formula.

Exercise
--------

Consider the recursion

$$\begin{aligned}
a_0&=x,\\
a_{n+1} &= \frac12(a_n^2+y^2).\end{aligned}$$

Try computing a few iterations of this recursion for various values of
$x$ and $y$, and use the command `ListPointPlot3D[]` to generate a
scatter plot. Based on the scatter plot, describe the shape of the
domain in the $x-y$ plane for which the recursion converges. Prove your
result.

`Mathematica` code producing this plot:

```Mathematica
Clear[a, x, y];
a[x_, y_, 0] = x; 
a[x_, y_, n_] := a[x, y, n] = (1/2)*(a[x, y, n - 1]^2 + y^2);
ListPointPlot3D[Flatten[
    Table[{x, y, a[x, y, n]}, {n, 1, 4}, {x, -2, 2, .1}, {y, -2, 2, .1}]
    , 2]]
```

![](test.pdf)

Exercise
--------

@borwein2004mathematics, p. 212.

Let

$$
A = \begin{pmatrix}
 2 & -1 &  0 \\
-1 &  2 & -1 \\
 0 & -1 &  2 \\
\end{pmatrix}
$$

Show that any matrix $B$ that commutes with $A$ must be a quadratic polynomial in $A$. _Hint_: Consider the explicit form of a quadratic in $A$ and the information implicit in $AB - BA = 0$. This yields a set of linear equations that you can solve using `Mathematica`.

Exercise
--------

@borwein2004mathematics, p. 300.

Evaluate the sum $\sum_{k=0}^{n-1} F_{n-k} 10^k$, where $F_n$ is the $n$-th Fibonacci number. _Hint_: Experiment numerically, find a closed form and verify it. 

Exercise
--------

@borwein2004mathematics, p. 213.

This example appeared in Ramanujan's lost notebook. For $t \geq 0$ and $a > 0$, evaluate:

$$
\mathcal I(a,t) = \negthickspace \int_{-t}^{\infty} \frac{a^x}{\Gamma(x + 1)}\ \d x\, + \negthickspace \int_0^{\infty} \negthickspace \frac{e^{-ax} x^{t-1}}{\pi^2 + \log^2 x} \left( \cos(\pi t) - \frac{\sin(\pi t)}{\pi} \log x \right) \d x
$$

_Hint_: Differentiate $\mathcal I$ with respect to $t$ to show that $\mathcal I(a, t)$ is a function of only $a$. Then observe the derivatives of $\mathcal I$ with respect to $a$, and identify the function up to a multiplicative constant. Plotting the function might help too. Finally evaluate $\mathcal I(0,0)$ to obtain the constant.

This is effectively an exercise in controlling `Mathematica` evaluation. You can control how `Mathematica` evaluates an expression using the functions `Hold`, `ReleaseHold` and `Evaluate` (and others, see the documentation).

A useful trick is the following:

```Mathematica
A = Integrate[terrible_function_of_t] // Hold;
Block[{Integrate}, D[A, t] // ReleaseHold]
```

`Block[{Integrate}, ...]` temporarily hides the definition of `Integrate` deferring the evaluation of the integral after the one of the derivative `D[A,t]`.

# References
