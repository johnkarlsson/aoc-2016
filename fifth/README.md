# Takeaways Day 5
## MVars

In Day 5's challenge you needed to find an **8 character passcode** from a
**stream** with the following properties:

- It is costly to calculate
- It does not return the answers in the correct order
- It may return multiple answers for each character position (in which case only the first one should be used)

This was implemented using `replicateM 8 newEmptyMVar` and then writing to
those values using `tryPutMVar`. This way, the search can continue by ignoring
the fact that the `MVar` has already been written to (hence the _try_).
Printing the characters in order is just a simple `forM`:

```haskell
digits <- replicateM 8 newEmptyMVar
forkIO (search digits)
forM_  digits (putStr <=< takeMVar)
```
