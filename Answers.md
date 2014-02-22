Definite Integrals
==================

Evaluate
$$\int_0^1 \int_0^1 \frac{1}{(x+y)\sqrt{(1-x)(1-y)}} \,\d x \,\d y.$$

This double integral coverges slowly numerically. It helps to carry out
one integral symbolically to obtain ~0~^1^ x, [eq:4Catalan] which
converges rapidly numerically. The answer can be identified with PSQL to
be $4G$ where $G$ is Catalan’s constant. After the variable substitution
$x=\sin t$, `Mathematica` can do the integral symbolically.

Evaluate
$$z=\int_0^1 \int_0^1 \frac{2(1-x)}{(1-xy)\log xy} \,\d x \,\d y.$$
Using `NIntegrate` with `WorkingPrecision \rightarrow 20` gives enough
precision to identify `N[z,16]` as $-2\gamma$ with PSLQ. Here $\gamma$
is the Euler-Mascheroni constant. This can be proved by expanding the
integrand around $x=0$ and interchange summation and integration:
$$z=\int_0^1 \int_0^1 \frac{2}{\log xy} \,\d x\,\d y + \sum_{k=1}^\infty \int_0^1 \int_0^1 \frac{2(-1+y)x^ky^{k-1}}{\log xy} \,\d x\,\d y,$$
which is an integral `Mathematica` can do symbolically to give

$$\begin{aligned}
z&= -2-2\sum_{k=1}^\infty \left(\frac{1}{k+1} -\log(1+1/k)\right) \\
&=  -2-2\sum_{k=1}^\infty \left(\frac{1}{k+1} -\log(k+1) + \log k \right)\\
&= -2-2 \lim_{n\rightarrow\infty}\left[\left(\sum_{k=1}^n \frac{1}{k+1}\right) -\log(n+1)\right]\\
&= -2 \lim_{n\rightarrow\infty}\left[\left(\sum_{k=1}^n \frac{1}{k}\right) -\log(n)\right] = -2\gamma.\end{aligned}$$

Evaluate
$$z=\int_0^1 \int_0^1 \frac{2(1-x)}{(1+xy)\log xy} \,\d x \,\d y.$$ This
integral is proportional to the “alternating Euler-Mascheroni constant"
$$z=2\log\left(\frac{\pi}{4}\right) = -2\sum_{k=0}^\infty (-1)^{k-1} \left(\frac1k - \log(1+1/k)\right).$$

Punctured staircase polygons
============================

$$\begin{aligned}
b_0 &= -1.8663241194351920050196247849947754021367053683188,\\
b_1 &= 7.9059563392740772434859105475473124673846546852395, \\
b_2 &= -46.5085162436681253033290351151924311221699873945957, \\
b_3 &= 281.2147291484328551535505262022980358985518683128643,\\
b_4 &= -1614.7343647206986620185285959093380353946075352711515.\end{aligned}$$

$$\begin{aligned}
d_0 &= 8.6201818620079949610759352141292522535449252848716,\\
d_1 &= -468.7223887466847260085039772682780912865053123648977,\\
d_2 &= 14385.2652080798262785986339172974916220192481677735609,\\
d_3 &= -327706.0206247312324630064168765731929525683228274017821,\\
d_4 &= 6166772.8106951798037106345891052649114413423406374104270.\end{aligned}$$

$$\begin{aligned}
c_0 &= 1.55210340048879105374176365435382538663929668478847,\\
c_1 &= -26.95934441862764488183047318936221452933927035205910,\\
c_2 &= 229.69965348690927263825571439908372423145409912308926,\\
c_3 &= -1883.07448232622508062053076266876882359755690440878975,\\
c_4 &= 15155.59894720951295625524489434236009747737705173205070.\end{aligned}$$

Guttmann and Jensen have identified the $b_i$ and $d_i$ in terms of
known constants. The numbers $c_i$ however are still “unknown".

Find the constants $b_i$ and $d_i$ using PSLQ. Bonus: Try to identify
any of the constants $c_i$.

These constants are of the form $b_i \sqrt{3}\pi^{3/2}= m/n$,
$d_i \pi^{3/2}= m/n$.
