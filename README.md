series
======

This library defines a simple newtype

    newtype Folding f m a = Folding {getFolding::
         forall r. (f r -> r) -> (m r -> r) -> a -> r}

which wraps and generalizes the unwrapped type GHC uses to 
optimize `Data.List`

    forall r . (a -> r -> r) -> r -> r

which is equivalent to 

    Folding ((,) a) Identity ()

This library defines a `Prelude` of functions on `Folding` especially
`Folding (Of a) m r` and proposes a number of bytestring and other
such things.  The hope is to employ this type for a fairly
straightforward optimization of a number of types of the `ListT` 
and `Producer` sort, using the almost-correct equivalences

     Producer a m r ~ Folding ((,) a) m r
     FreeT f m r  ~ Folding f m r
     Series f m r ~ Folding f m r
     
and a number of potential others, e.g. `LogicT`, `Conduit.Source`, etc 
which are equivalent to `Folding ((,) a) m ()`. The `Series` type 
defined here is an attempt at an optimized `FreeT` aimed
at improving the pipes usage `FreeT (Producer a m) m r` and
the like. (Some decisions have been made homogeneous with 
`ertes`'s similarly motivated [`fuse` package](http://hub.darcs.net/ertes/fuse), 
which calls it `FreeT` replacement `List`; it may be more interesting.)

In each of the `Prelude`s included here, operations with types like

     f_producer :: Producer a m r -> Producer b m z
     f_freet :: FreeT f m r -> FreeT g m s
     f_series :: Series (Of a) m r -> Series (Of b) m r
     f_list :: [a] -> [a]

are implemented as

     buildProducer . f_folding . foldProducer
     buildFreeT . f_folding . foldFreeT
     buildSeries . f_folding . foldSeries
     buildList . f_folding . foldList
     

where `f_folding` is the appropriate function of `Folding`s. The different
`Prelude` s thus differ mostly by find-and-replace. Functions that enter or
exit space of 'serial' types use only one of the fusion operators. 

In each case the principal (only) "fusion" rule is of the form

     buildProducer (foldProducer phi) = phi
     buildFreeT (foldFreeT phi) = phi
     buildSeries (foldSeries phi) = phi
     buildList (foldList phi) = phi  
     
It may be that the resulting implementations are better at making 
it past the impediments of `criterion`, but some benchmarks on 
more and less complex compositions of functions `f.g.h`, with and without
defintions via `Folding`, can be seen here:

![ ](http://michaelt.github.io/images/seriesbench.png)

The rest of the report is
[here](http://michaelt.github.io/bench/seriesbench.html). Lines
marked 'f.g.h/FOLDING' bench compositions of functions defined
through the fusion framework described above; those marked
'f.g.h' bench compositions of functions given ordinary
definitions using the constructors or as they are exported by 
suitable libraries. Functions from `Data.Vector.Unboxed` are 
marked 'vector' and are included for comparison.

The benchmarks are pure and thus use `Folding (Of a) Identity ()`, 
`Series (Of a) Identity ()` and `[a]`. It is interesting that for these benchmarks, the
present fusion framework is *always* faster than Data.List. It is
also more reliable than both vector and Data.List (though vector
is of course much faster where fusion succeeds.) But these cases
are perhaps somewhat stylized, and in my experience `criterion` is a bit 
cruel to anything that requires specialization and other optimization. 
I am also surprised so far that newtype wrapping makes the fusion 
rules more reliable.
