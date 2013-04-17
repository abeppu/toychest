# Introduction to toychest

## Amb

At the time of writing, I'm still new to clojure. I wanted to return to some tidbits I had seen years ago in SICP, and try to get in touch with whatever lispy enlightenment I may have brushed against there. I quickly realized that the continuation-oriented amb in SICP wasn't really clojure-y, and that lazy collections offered a more idiomatic way to do something similar.

The entry point I've set up is amb-let, which takes a bindings form, a requirements form, and a body. Perhaps I should just give an example : 

    (amb-let [a (amb (range 1 10))
              b (amb (range a 10))
	      c (amb (range b 10))]
             [[#{a} (> a 2)]
 	      [#{a b c} (= (+ (* a a) (* b b)) (* c c))]]
	     [a b c]) ;; => '([3 4 5])
	      
The whole `amb-let` produces a sequence whose member values (if any) are evaluations of the body using `amb-let`-bound variables which satisfy the requirements given. 

Note that within the requirements form, each requirement is itself a vector with two entries : a set of variables introduced in the binding section, and an expression which uses those variables. This structure looks a bit cumbersome, but this seemed like a straight forward, explicit way to allow amb-let to apply requirements (and thus prune the search space) as soon as possible.

Note also that only a subset of clojure's normal destructuring works in amb-let currently. This could be expanded, but the principle issue is that amb-let needs to keep track of which variables it has defined in order to apply requirements as soon as the variables upon which they depend are defined.

Lastly, note that while `amb-let` can handle `amb`s over streams, if you aren't careful you could produce a backtracking search problem which rabbit-hole's off into infinity and never returns.

For more example usages, see the tests!