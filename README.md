# Donatron: A library to make accepting donations easy!

An example that shows how to use `Cats' EitherT` in Scala.
It also explains how to use the power of `EitherT's` "right-bias" for conditional code execution.

The idea is to make following code work:

```scala

def donate(request: Request): F[Response] =
  checkForValidInts(request)
    .flatMap(checkForMinimumLength)
    .flatMap(acceptDonations)
    .flatMap(logReply)
    .merge
    .flatMap(logAndReturnResponse)
```

The rules for the execution of `donate` function are as follows:

* `checkForValidInts` is the entry point
* `checkForMinimumLength`, `acceptDonations` & `logReply` should only be executed if previous step was successful.
* `logAndReturnResponse` should be executed regardless of all the previous steps.
* Only exception to `logAndReturnResponse` is `acceptDonations` which can raise an error.