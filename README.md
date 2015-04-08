# Kripke

Kripke a Clojure library for manipulating sequences of similar-enough structures.

## Installation

Add the following dependency to `project.clj`:

![Clojars Project](https://clojars.org/kripke/latest-version.svg)

Next, load the `kripke` namespace:

```clojure
(require '[kripke :refer :all])
```

## Basic usage

A typical workflow is as follows:

1. Start with a prototype structure
2. Abstract the prototype into a frame and a sequence of substitution maps
3. Optionally, manipulate the frame
4. Construct a sequence of structures

Here's a simple example. Let us construct three maps: {:key 1}, {:key 2}, and {:key 3}. We begin by constructing a prototype using the function `alt` (for 'alternatives'):

```clojure
(def prototype {:key (alt 1 2 3)})
;=> {:key #<kripke$alt$...>}
```

Next, we abstract the frame and the substitution maps:

```clojure
(def frame+smaps (abstract prototype))
;=> [{:key abstract__11051}
;    ({abstract__11051 1} {abstract__11051 2} {abstract__11051 3})]
```

The frame resembles the prototype but contains symbols for every 'choice' that needs to be made. The possible values for these choices are encoded in the substitution maps.

We can use the function `model` to put everything back together:

```clojure
(apply model frame+smaps)
;=> ({:key 1} {:key 2} {:key 3})
```

## Changing frames

One advantage of working with substitution maps is that the frame can be changed after abstracting a prototype:

```clojure
(let [[frame smaps] frame+smaps
      new-frame (clojure.set/map-invert frame)]
  (model new-frame smaps))
;=> ({1 :key} {2 :key} {3 :key})
```

## More possibilites

From here on, we'll use `make`, a built-in shorthand for modeling a prototype. `(make x)` is short for `(apply model (abstract x))`.

It's possible to include `alt` in many places in your prototype. Moreover, `alt` can be used recursively or without arguments. Observe:

```clojure
(make (alt 1 2 [(alt 3 4 (alt 5 6) (alt))]))
;=> (1 2 [3] [4] [5] [6])
```

Notice that `alt` can function as a filter when used without arguments:

```clojure
(make #{[(alt)]})
;=> ()
```

## Tables

`alt` is useful, but sometimes it's desirable to add more constraints. Tables allow you to correlate choices made in different locations in a prototype.

```clojure
(make {(tab :t 1 2 3) (tab :t 'a 'b 'c)})
;=> ({1 a} {2 b} {3 c})
```

As you can see, tables are named. Here's an example with two tables:

```clojure
(make {(tab :x 1 2 3) (tab :x 'a 'b 'c)
       (tab :y "Aardvark" "Zebra") (tab :y :wine :beer)})
;=> ({1 a "Aardvark" :wine}
;    {1 a "Zebra" :beer}
;    {2 b "Aardvark" :wine}
;    {2 b "Zebra" :beer}
;    {3 c "Aardvark" :wine}
;    {3 c "Zebra" :beer})
```

## Store and load

If you want more power, you can use `store` and `load` to smuggle data from one position in a prototype to another. Just make sure `store` is called before `load`, going by depth-first order.

```clojure
(make [(alt (store [:a (alt 1 2 3 4)] :even))
       (alt (retr [a :a] (if (even? a) a (alt))))])
;=> ([:even 2] [:even 4])
```

## Summarize

Given a collection of structures, `summarize` returns a frame and a sequence of substitution maps that represent the same information.

```clojure
(def form '([1 2 [3 #{4}]] [:a 2 [3 (4)]]))

(summarize form)
;=> [[summarize__14972 2 [3 summarize__14973]]
;    ({summarize__14972 1 summarize__14973 #{4}}
;     {summarize__14972 :a summarize__14973 (4)})]

(= form (apply model (summarize form)))
;=> true
```

## License

Copyright © 2015 Jonas De Vuyst

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
