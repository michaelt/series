ListM
=====

*`FreeT` and (`pipes`) `Producer` replacement with a simple build/foldr optimization model following the well understood model of Data.List*

The standard FreeT module is irremediably slow and lacks crucial combinators.
In particular it does not develop the important case in which the functor -- e.g (a, _),
here `Of a _` -- generates a List like structure. Though the `ListM` type here is an
optimize `FreeT`, and thus can take any functor the aim is to represent *effectful sequences* of various sorts. In particular the Pipes Producer concept is to be represented, togther with an equivalent of the important but dog-slow `FreeT (Producer a m) m r`

The first optimization imposed is to replace it with a type `ListM` that makes a suitable quotient; this will lead to some correctness subtleties not yet resolved, but the procedure is familiar.

The next is to develop an optimization infrastructure in terms of a corresponding Church encode type systematically following the model of `Data.List`.

In some respects we follow the model of `ertes` experimental `fuse` package, though this attempts precedes that one temporally.

