# Introduction to toychest

## Amb

At the time of writing, I'm still new to clojure. I wanted to return to some tidbits I had seen years ago in SICP, and try to get in touch with whatever lispy enlightenment I may have brushed against there. I quickly realized that the continuation-oriented amb in SICP wasn't really clojure-y, and that lazy collections offered a more idiomatic way to do something similar.

The entry point I've set up is amb-let, which takes a bindings form, possibly containing multiple interleaved requirements introduced by :where, and a body. Perhaps I should just give an example : 

    (amb-let [a (amb (range 1 10))
              :where (> a 2)
              b (amb (range a 10))
	      c (amb (range b 10))
 	      :where (= (+ (* a a)
 	                   (* b b))
 		        (* c c))]	    
	     [a b c]) ;; => '([3 4 5])
	      
The whole `amb-let` produces a sequence whose member values (if any) are evaluations of the body using `amb-let`-bound variables which satisfy the requirements given. 


Note that after each let-binding there can be at most one `where` clause, and it can only use the variables introduced so far. If you need to satisfy multiple requirements after establishing a variable, just and them together. Order is important here; you could hypothetically always put your requirements in a single `:where` clause after the the last binding, but this would create sub-optimal performance. At its heart, amb-let is doing backtracking search, and pruning off whole branches on the basis of identifying a partial assignment as being unsalvageable, can speed the search up substantially.

Note also that only a subset of clojure's normal destructuring works in amb-let currently. This could be expanded.

Lastly, note that while `amb-let` can handle `amb`s over streams, if you aren't careful you could produce a backtracking search problem which rabbit-hole's off into infinity and never returns.

For more example usages, see the tests!