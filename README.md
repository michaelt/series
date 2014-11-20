series
======

The standard `FreeT` module is irremediably slow and lacks
crucial combinators. In particular it does not develop the
important case in which the functor -- e.g `(a, _)`, here
`Of a _` -- generates a list-like structure. Though the
`Series f m a` type here aspires to be an optimized
`FreeT f m a` -- and thus can take any functor f -- the aim is to
represent *effectful sequences* of various sorts, such as the
`Producer` type in `pipes` (= `FreeT ((,) a) m r` =
`Series (Of a) m r`)

In some respects we follow the model of `ertes`'s experimental
[`fuse` package](http://hub.darcs.net/ertes/fuse), which may hold
more interest; in particular the device of calling the strict
pair `Of a b` is found there; his `FreeT` type is called `List`.

The first optimization is in the datatype `Series`: it requires a
suitable quotient to be seen as isomorphic to `FreeT`; this will
lead to some correctness subtleties not yet resolved, but the
procedure is familiar.

The next is to develop an optimization infrastructure in terms of
a corresponding Church encoded type. Ideally this will follow the
model of `Data.List`. At the moment, it is using two
easier-to-implement variants in which a church encoded version of
`Series`, i.e.:

    type Fold_ f m r = forall r'
                       .  (f r' -> r') 
                       -> (m r' -> r')
                       -> (r -> r') 
                       -> r'

is used, both raw and wrapped in a newtype. (`ertes` and the
Church-encoded module of the `free` package use an inexplicably
more complex type.) At the moment, then, all functions are
basically of the form `build . f-church . fold` so that we can
eliminate `fold . build` in the style of the
`stream . unstream  = id` rule in `vector`.






