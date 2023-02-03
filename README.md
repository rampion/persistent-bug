Investigating a potential persistent bug with `<-.` on nullable fields.

```
$ cabal run
…
people that match filters: []
[Person {personName = "John Doe", personAge = Just 35},Person {personName = "Jane Doe", personAge = Nothing}]
people that match filters: [PersonAge <-. [Nothing,Just 35]]
[Person {personName = "John Doe", personAge = Just 35},Person {personName = "Jane Doe", personAge = Nothing}]
people that match filters: [PersonAge <-. [Just 35]]
[Person {personName = "John Doe", personAge = Just 35}]
people that match filters: [PersonAge <-. [Nothing]]
[]
```

I would have expected `Person {personName = "Jane Doe", personAge = Nothing}` to
match filters `[PersonAge <-. [Nothing]]`.
