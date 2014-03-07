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


8 Activities in Experimental Mathematics
----------------------------------------

Without claiming to give a exact definition of experimental mathematics, by this name we mean the methodology of doing mathematics that includes the use of computations for:

1. Gaining insight and intuition
2. Discovering new patterns and relationships
3. Using graphical visualisations to suggest underlying mathematical principles
4. Testing and especially falsifying conjectures
5. Exploring a possible result to see if it is worth a formal proof
6. Suggesting approaches for a formal proof
7. Replacing lengthy hand derivations with computer-based derivations
8. Confirming analytically derived results


Learning `Mathematica`
----------------------

In this subject you will be required to write programs in `Mathematica`. Nevertheless this subject is not a programming course, and you will have to develop (basic) programming skills yourself with practice, either at home or during our computer lab sessions. Fortunately learning how to use `Mathematica` is not hard, and you will have plenty of time to get familiar with the software and its language.

`Mathematica` comes with an extensive documentation centre which you can access within the program or online. I encourage you to turn to the documentation centre whenever you need to work out how to do something in `Mathematica` or what a particular function does.

If needed, the book _An Introduction to Modern Mathematical Computing With Mathematica(R)_ by @borwein2012introduction suits perfectly the requirements of this subject.


A simple sum
============

Let's consider the problem of determining the final digit $\ell_n$ of $\sigma_n = \sum_{k=1}^n\ k$.

Of course $\sigma_n$ has the well-known simple expression $\sigma_n = n(n+1)/2$ but it is not evident how this expression can lead us to the answer. We can run a "computer experiment" and compute the first few terms. Here are the first 100 terms of $\sigma_n$:

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

\noindent
It is _evident_ that the pattern repeats every 20 terms. This is an insight we could not attain from the analytical expression. With this insight is trivial to prove that indeed $\ell_{n+20} = \ell_n$. We have

\begin{align*}
\sigma_{n + 20} &= \sigma_n + \sum_{k=n+1}^{n+20}\ k
= \sigma_n + 20 n + \sum_{k=1}^{20}\ k
. \\
\intertext{Therefore}
\ell_{n + 20} &= \ell_n + \left( 20 n + \sum_{k=1}^{20}\ k \right) \mod{10} = \ell_n
.
\end{align*}

The arithmetic-geometric mean
=============================

This exercise was carried out by Gauss in 1799 without the help of
computers but using numerical tables. Gauss noticed that the reciprocal of the integral

$$
\frac{2}{\pi} \int_0^1 \frac{\d t}{\sqrt{1-t^4}}
$$

agreed numerically with the limit of the arithmetic-geometric mean iteration:

$$
a_{n+1} = \frac{1}{2}(a_n+b_n)
\quad
b_{n+1} = \sqrt{a_n b_n}
$$

with initial values $a_0=1$, $b_0=\sqrt{2}$. 

Exercise
--------

Compute the limit of the above iteration. Try to implement the iteration in `Mathematica` by yourself or use the following code:

```Mathematica
Clear[a,b];
a[0]=N[1,200];
b[0]=N[Sqrt[2],200];
a[n_]:=a[n]=(a[n-1]+b[n-1])/2;
b[n_]:=b[n]=Sqrt[a[n-1]b[n-1]];
```

Make sure you understand the code above:

* What do `N[1,200]` and `N[Sqrt[2],200]` mean? Why we don't simply write `1` and `Sqrt[2]`?

* Why are we writing `a[n_]:=a[n]=...` rather than `a[n_]:=...`? What about simply `a[n_]=...`?

* Type `a[n]` into `Mathematica` and watch it hit the recursion limit. What is happening? How can it be prevented?

Exercise
--------

Evaluate numerically the above integral and show that it is (within numerical precision) equal to the inverse of the limit of $a_n$ and $b_n$.

You can increase the number of digits computed using the options `WorkingPrecision` and `AccuracyGoal`:

```Mathematica
NIntegrate[...,WorkingPrecision->200,
    AccuracyGoal->100]
```


Lines in the plane
==================

This exercise is borrowed from @Graham1994Concrete. How many slices of pizza can a person obtain by making $n$ straight cuts with a pizza knife? Or, more academically: What is the maximum number $L_n$ of regions defined by $n$ lines in the plane? It is easy to see that

$$
L_0 = 1, \qquad L_1=2, \qquad L_2=4.
$$

Before you read on, formulate a prediction for $L_n$ based on these three cases.

\begin{marginfigure}[3cm]
\centering
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
\bigskip
\caption{How many slices of pizza can a person obtain by making n straight cuts with a pizza knife?}
\end{marginfigure}

When we add the third line, we find that it can split at most three of the old regions, no matter how we have placed the frst two lines. Therefore $L_3=4+3=7$. The generalization follows easily: the $n$th line generates $k$ new regions if and only if it splits $k$ old regions. It splits $k$ old regions if and only if it crosses the previous lines in $k-1$ different places. Hence, to obtain the maximum number of regions, the $n$th line must cross the previous $n-1$ lines, which can be achieved by choosing it in such a way that it is not parallel to any of the previous lines. We thus get the recursion

$$
\begin{aligned}
L_0 &= 1\\
L_n &= L_{n-1} + n.
\end{aligned}
$$

Exercise
--------

Program this recursion in `Mathematica`. Guess a closed form formula for $L_n$. You can use the _The On-Line Encyclopedia of Integer Sequences_^[http://oeis.org] to find out more.


Other exercises
===============

Exercise
--------

Pascal’s triangle has elements $\binom{m}{n}$:

$$
\begin{array}{ccccccccc}
&&&& 1 &&&& \\
&&&1 && 1 &&&\\
&& 1 && 2 && 1 &&\\
& 1 && 3 && 3 && 1 &\\
1 && 4 && 6 && 4 && 1
\end{array}
$$

What is the structure of the distribution of even and odd numbers in this triangle? To visualise this we plot a point if $\binom{m}{n} \equiv 1 \bmod 2$ and none otherwise. Write a little program in `Mathematica` that does this. Commands that are useful are `Binomial[m,n], Mod[x,2]` and `ListPlot[]`.

Exercise
--------

@borwein2004mathematics, p. 89. Consider the recursion

$$
\begin{aligned}
a_0 &= x,\\
a_{n+1} &= \frac12(a_n^2+y^2).
\end{aligned}
$$

Try computing a few iterations of this recursion for various values of
$x$ and $y$, and use the command `ListPointPlot3D[]` to generate a
scatter plot. Based on the scatter plot, describe the shape of the
domain in the $(x,y)$ plane for which the recursion converges. Prove your
result.

\begin{marginfigure}
\centering
\includegraphics[width=\textwidth]{test}
\caption{Visualisation of $a_n$ for $n = 1, \dotsc, 4$ and various values of $x$ and $y$.}
\end{marginfigure}

You can use this code to produce a plot:

```Mathematica
Clear[a,x,y];
a[x_,y_,0]=x;
a[x_,y_,n_]:=a[x,y,n]=1/2(a[x,y,n-1]^2+y^2);
Flatten[Table[{x,y,a[x,y,n]},
{n,4},{x,-2,2,.1},{y,-2,2,.1}],2]//ListPointPlot3D
```

Exercise
--------

@borwein2004mathematics, p. 90. Compute $A^{10^6}$ and $A^{-7}$ for

$$
A=\begin{pmatrix}
3/2 & 1/2\\
-1/2 & 1/2
\end{pmatrix}.
$$

Use `Mathematica` to evaluate $A^n$ for various integers $n$ and deduce a formula for $A^n$. Prove this formula.


Exercise
--------

@borwein2004mathematics, p. 212. Let

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

@borwein2004mathematics, p. 300. Evaluate the sum $\sum_{k=0}^{n-1} F_{n-k} 10^k$, where $F_n$ is the $n$-th Fibonacci number. _Hint_: Experiment numerically, find a closed form and verify it. 

Exercise
--------

@borwein2004mathematics, p. 213. This example appeared in Ramanujan's lost notebook. For $t \geq 0$ and $a > 0$, evaluate:

$$
\mathcal I(a,t) = \negthickspace \int_{-t}^{\infty} \frac{a^x}{\Gamma(x + 1)}\ \d x\, + \negthickspace \int_0^{\infty} \negthickspace \frac{e^{-ax} x^{t-1}}{\pi^2 + \log^2 x} \left( \cos(\pi t) - \frac{\sin(\pi t)}{\pi} \log x \right) \d x
$$

_Hint_: Differentiate $\mathcal I$ with respect to $t$ to show that $\mathcal I(a, t)$ is a function of only $a$. Then observe the derivatives of $\mathcal I$ with respect to $a$, and identify the function up to a multiplicative constant. Plotting the function might help too. Finally evaluate $\mathcal I(0,0)$ to obtain the constant.

This is a (difficult) exercise in controlling `Mathematica` evaluation. You can control how `Mathematica` evaluates an expression using the functions `Hold`, `ReleaseHold` and `Evaluate` (and others, see the documentation).

A useful trick is the following:

```Mathematica
A = Integrate[terrible_function_of_t] // Hold;
Block[{Integrate}, D[A, t] // ReleaseHold]
```

`Block[{Integrate}, ...]` temporarily hides the definition of `Integrate` deferring the evaluation of the integral after the one of the derivative `D[A,t]`.

# References
