# Hints
There are several ways to handle variable bindings, with none clearly superior to the others.
I have listed them in rough order of popularity
* Capture Avoiding Substitution, include with some hint code.
* Higher Order Abstract Syntax, include with some hint code.
* [De Bruijn](https://en.wikipedia.org/wiki/De_Bruijn_index). Mark will provide code hints if anyone really wants them.
* Locally nameless, a combination of Capture Avoiding Substitution and De Bruijn. The library [unbound-generics](https://github.com/lambdageek/unbound-generics) takes this approach.
* Abstract Binding Trees, described in "Practical Foundations for Programming Languages".  If you figure out why this is good please tell Mark.

There are many more.  If you are feeling adventurous, try to come up with your own.
