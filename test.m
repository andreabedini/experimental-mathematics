out = (
Clear[a, x, y]; a[x_, y_, 0] = x; 
a[x_, y_, n_] := 
a[x, y, n] = (1/2)*(a[x, y, n - 1]^2 + y^2);
ListPointPlot3D[Flatten[Table[{x, y, a[x, y, n]}, {n, 1, 4}, {x, -2, 2, .1}, {y, -2, 2, .1}], 2]]
)
Export["test.pdf", out]