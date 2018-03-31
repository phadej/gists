---
title: "SO: What's the motivation to use a phantom type?"
author: Oleg Grenrus
---

### The question

Don Stewart's Haskell in the Large's presentation mentioned Phantom Types:

```hs
data Ratio n = Ratio Double
1.234 :: Ratio D3

data Ask ccy = Ask Double
Ask 1.5123 :: Ask GBP
```

What's the motivation to use a phantom type?

### The answer

To answer the "what's the motivation to use a phantom type". There is two points:

- to make invalid states inrepresentable
- carry some of the information on the type level

Read rest the rest on [StackOverflow](http://stackoverflow.com/questions/28247543/motivation-behind-phantom-types/28250226#28250226)

