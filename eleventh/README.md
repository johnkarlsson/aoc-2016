# Takeaways from Day 11

## Monad transformers

I used the priority queue from
[fingertree-0.1.1.0](https://hackage.haskell.org/package/fingertree-0.1.1.0) to
implement [A*](https://en.wikipedia.org/wiki/A*_search_algorithm). Simple intro
at [Monoids and Finger
Trees](http://apfelmus.nfshost.com/articles/monoid-fingertree.html).

While working on the function signature for `minView` (a regular _pop_
operation) inside the `State` Monad, I stumbled on the need to pattern match or
otherwise having to deal with `Maybe` explicitly. However, the `StateT` Monad
has a matching type signature:

```haskell
minView :: Ord k => PQueue k v -> Maybe (v, PQueue k v)
```

```haskell
StateT :: (s -> m (a, s)) -> StateT s a
```

which allows you to define the pop operation as

```haskell
pop :: StateT Frontier Maybe Node
pop = StateT . Q.minView
```

or, as was needed in this case, a pop operation that popped the next unseen
node:

```haskell
pop :: Explored -> StateT Frontier Maybe Node
pop seen = do
    n@(h, _) <- StateT Q.minView
    if h `S.member` seen then pop seen else return n
```

Note that when the first bind operation is a `Nothing`, the computation can be
aborted early. Another example of this is in the search function:

```haskell
search :: Explored -> StateT Frontier Maybe Node
search seen = do
    n <- pop seen
    if isGoal n
       then return n
       else do
           expand n
           search (S.insert h seen)
```
