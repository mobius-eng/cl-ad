# CL-AD

[Automatic (algorithmic) differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation)
library for Common Lisp. It is based on
[Scheme library](https://github.com/qobi/R6RS-AD) but is implemented
in a more generic way using CLOS.

## What is "automatic (algorithmic) differentiation (AD)"?

AD rerers to the method of calculating numerical derivatives
analytically, but not symbolically. A general idea is to exploit the
chain rule. For this the whole arithmetic of the language is replaced
with slightly more generic arithmetic operations: instead of operating
on numbers only, these operations can accept *dual numbers* or *tapes*
and produce the derivative of the operation as well as the main
result.

In essence, *dual numbers* are the extension of numbers with
*perturbations*, their "derivatives". Consider function `f(x)`. If we
know it's derivative `df(x)`, then given a dual number `[z dz]`, the
result of application of `f(x)` to it is a dual number
`[f(z) df(z)*dz]`. This result can become an input to the next
function `g(x)`, producing `[g(f(z)) dg(z)*df(z)*dz]`. And so on. To
begin the process, all we need is to set `dz=1`. This is called
forward derivative.

TODO: reverse derivative.

## Current status

Forward derivatives - complete, beta-state
Reverse derivatives - TODO

## Future

- Extend it to symbolic data
- Implement full Jacobian calculation

## Examples


## License

Copyright © 2016 Alexey Cherkaev (mobius-eng)

Distributed under
[Lesser GNU Public License 3](http://www.gnu.org/licenses/lgpl-3.0.en.html)
or (at your option) any later version.
