ListM
=====

This package attempts to provide a replacement for `FreeT` in the `free` package and perhaps an optimization of the `Producer` type in `pipes` (= `FreeT ((,)a) m r`). The idea is impose a simple build/foldr optimization model following the well understood model of `Data.List`. At the moment, it is using an easier-to-implement variant in which the church encoded version of the `ListM`: 

    type Fold_ f m r = forall r'
                       .  (f r' -> r') 
                       -> (m r' -> r')
                       -> (r -> r') 
                       -> r'

is wrapped in a newtype to keep everything from falling apart. In particular the
compiler is apt to lose the rank-2 character of the type unless it is wrapped. On the other hand, it may be that the newtype constructor is impeding some compiler optimizations. 

The standard `FreeT` module is irremediably slow and lacks crucial combinators.
In particular it does not develop the important case in which the functor -- e.g `(a, _)`,
here `Of a _` -- generates a list-like structure. Though the `ListM f m a` type here aspires to be an optimized `FreeT f m a` -- and thus can take any functor f -- the aim is to represent *effectful sequences* of various sorts. In particular the `pipes` `Producer` concept (= `FreeT ((,) a) m r`) is to be represented, togther with an equivalent of the important but catastrophically slow `FreeT (Producer a m) m r`. 

The first optimization imposed, however, is to replace `FreeT` with a datatype `ListM` that requires a suitable quotient to be seen as isomorphic to `FreeT` ; this will lead to some correctness subtleties not yet resolved, but the procedure is familiar.

The next is to develop an optimization infrastructure in terms of a corresponding Church encoded type, systematically following the model of `Data.List`.

In some respects we follow the model of `ertes` experimental [`fuse` package](http://hub.darcs.net/ertes/fuse), which may hold more interest.

