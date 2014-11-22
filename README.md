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

The first optimization is in the datatype `Series`:

    data Series f m r = Construct (f (Series f m r))
                      | Wrap (m (Series f m r))
                      | Done r

It requires a suitable quotient to be seen as isomorphic to
`FreeT`. This will lead to some correctness subtleties not yet
resolved, but the procedure is familiar.

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

or rather

    newtype Fold f m r = Fold (getFold :: Fold_ f m r)

is used. (`ertes` and the Church-encoded module of the `free`
package use an inexplicably more complex type.) At the moment,
then, all functions are basically of the form
`build . churched-definition . fold` so that we can eliminate
`fold . build` in the style of the `stream . unstream  = id` rule
in `vector`.

Of the two principal fusion operations,

    buildSeries :: Fold f m r -> Series f m r 
    buildSeries = \(Fold phi) -> phi Construct Wrap Done

and

    foldSeries ::  (Functor f, Monad m) => Series f m r -> Fold f m r

the latter is a flipped and wrapped variant of Atkey's

    effectfulFold :: (Functor f, Monad m) =>
       (m x -> x) -> (r -> x) -> (f x -> x) -> Series f m r -> x
       

(modulo the 'Done' constructor, which implicitly restricts the
available class of Functors.) See
http://bentnib.org/posts/2012-01-06-streams.html and the (distressingly
technical) associated paper. The examples of implementing
functions by way of `effectfulFold` are extremely surprising and
illuminating and are emulated here, where I can figure it out.

Some benchmarks on more and less complex compositions of
functions can be seen here
http://michaelt.github.io/bench/seriesbench.html Those marked
'seriesbuildfoldr' are for functions defined through the fusion
framework; those marked 'seriesnaive' are just ordinary recursive
definitions using the Series datatype.

It is interesting that the present framework is always faster
than Data.List, and more reliable than that for vector and
Data.List, but these cases are perhaps somewhat stylized. I am
also surprised so far that newtype wrapping makes the fusion
rules infinitely more reliable.
