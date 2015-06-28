module FRP.Grapefruit.Signal.Incremental.Set (

    -- * Diffs
    Diff (Diff),
    insertion,
    deletion,
    elementInsertion,
    elementDeletion,

    -- * Construction
    empty,
    singleton,

    -- * Conversion
    -- fromSeqs,
    -- fromAscSeqs,
    -- fromDistinctAscSeqs,
    toSeqs,
    toAscSeqs,

    -- * Combination
    union,
    difference,
    intersection,

    -- * Queries
    null,
    size,
    member,
    staticMember,
    notMember,
    staticNotMember,
    -- isSubsetOf,
    -- also staticIsSubsetOf?
    -- isProperSubsetOf,
    -- also staticIsProperSubsetOf?

    -- * Filtering
    -- filter,
    -- staticFilter,
    -- partition,
    -- staticPartition,
    -- split,
    -- staticSplit,
    -- splitMember,
    -- staticSplitMember,

    -- * Mapping
    -- map,
    -- staticMap,
    -- mapMonotonic,
    -- staticMapMonotonic

) where

    -- Prelude
    import Prelude hiding (null, filter, map)

    -- Control
    import Control.Applicative as Applicative ((<$>), (<*>))

    -- Data
    import           Data.Semigroup as Semigroup
    import           Data.Monoid    as Monoid
    import qualified Data.List      as List
    import           Data.Set       as Set       (Set)
    import qualified Data.Set       as Set
    import           Data.Sequence  as Seq       (Seq)
    import qualified Data.Sequence  as Seq
    import           Data.Map       as Map       (Map)
    import qualified Data.Map       as Map

    -- FRP.Grapefruit
    import           FRP.Grapefruit.Signal.Segmented            as SSignal
    import           FRP.Grapefruit.Signal.Incremental          as ISignal    hiding (combine, map)
    import qualified FRP.Grapefruit.Signal.Incremental          as ISignal
    import qualified FRP.Grapefruit.Signal.Incremental.Sequence as SeqISignal

    {-FIXME:
        There are several occurences of set union, difference and intersection which involve a set
        which is being changed, for example, in patch and combinationDiff. This results in time
        linear in the changed set which is very bad. Maybe, we should replace those occurences with
        alternative implementations of union, difference and intersection which work by iterating
        through the elements of the diff sets.
    -}

    -- * Diffs
    instance (Ord el) => Incremental (Set el) where

        data Diff (Set el) = Diff (Map el Bool)

        patch set diff = (set `Set.difference` revDiffMap False) `Set.union` revDiffMap True where

            revDiffMap = reverseDiffMap diff

        type ValidationState (Set el) = ()

        validationInit _ = ()

        validationStep _ _ = Just ()

    instance (Ord el) => Semigroup (Diff (Set el)) where

        (<>) = mappend

    instance (Ord el) => Monoid (Diff (Set el)) where

        mempty = Diff Map.empty

        Diff diffMap1 `mappend` Diff diffMap2 = Diff (diffMap2 `Map.union` diffMap1)
        {-
            Mind the order of Map.union arguments. Map.union is left-biased and the insertions and
            deletions of the second diff must win.
        -}

    insertion :: (Ord el) => Set el -> Diff (Set el)
    insertion = containednessChange True

    deletion :: (Ord el) => Set el -> Diff (Set el)
    deletion = containednessChange False

    containednessChange :: (Ord el) => Bool -> Set el -> Diff (Set el)
    containednessChange containedness set = Diff $
                                            Map.fromList [(el,containedness) | el <- Set.toList set]

    elementInsertion :: el -> Diff (Set el)
    elementInsertion el = Diff $ Map.singleton el True

    elementDeletion :: el -> Diff (Set el)
    elementDeletion el = Diff $ Map.singleton el False

    -- Applying diffSet only to the diff and memoizing the result improves efficiency.
    reverseDiffMap :: (Ord el) => Diff (Set el) -> (Bool -> Set el)
    reverseDiffMap (Diff diffMap) = \containedness -> if containedness then insertionSet
                                                                       else deletionSet where

        (insertionMap,deletionMap) = Map.partition id diffMap

        insertionSet               = Map.keysSet insertionMap

        deletionSet                = Map.keysSet deletionMap

    -- * Construction
    empty :: (Ord el) => ISignal era (Set el)
    empty = ISignal.const Set.empty

    singleton :: (Ord el) => SSignal era el -> ISignal era (Set el)
    singleton = ISignal.map start step . ISignal.monolithicFromSSignal where

        start (Monolithic init)   = (Set.singleton init,init)

        step (Replacement el') el = (Diff (Map.fromList [(el,False),(el',True)]),el')

    -- * Conversion
    -- fromSeqs :: (Ord el) => ISignal era (Seq el) -> ISignal era (Set el)

    -- fromAscSeqs

    -- fromDistinctAscSeqs

    toSeqs :: (Ord el) => ISignal era (Set el) -> ISignal era (Seq el)
    toSeqs = toAscSeqs

    toAscSeqs :: (Ord el) => ISignal era (Set el) -> ISignal era (Seq el)
    toAscSeqs = ISignal.map start step where

        start init = ((Seq.fromList . Set.toAscList) init,init)

        step       = toAscSeqsStep

    toAscSeqsStep :: (Ord el) => Diff (Set el) -> Set el -> (Diff (Seq el),Set el)
    toAscSeqsStep (Diff diffMap) set = (mconcat seqDiffs,last sets) where

        (seqDiffs,nextSets)       = unzip $ zipWith atomicStep (Map.toList diffMap) sets

        sets                      = set : nextSets

        atomicStep (el,False) set = case Set.splitMember el set of
                                        (_,False,_)    -> (mempty,set)
                                        (ltSet,True,_) -> (,) (SeqISignal.elementDeletion $
                                                               Set.size ltSet)
                                                              (Set.delete el set)
        atomicStep (el,True)  set = case Set.splitMember el set of
                                        (ltSet,False,_) -> (,) (SeqISignal.elementInsertion
                                                                    (Set.size ltSet)
                                                                    el)
                                                               (Set.insert el set)
                                        (_,True,_)      -> (mempty,set)

    -- * Combination
    union :: (Ord el) => ISignal era (Set el) -> ISignal era (Set el) -> ISignal era (Set el)
    union = combine True True True Set.union

    difference :: (Ord el) => ISignal era (Set el) -> ISignal era (Set el) -> ISignal era (Set el)
    difference = combine False True False Set.difference

    intersection :: (Ord el) => ISignal era (Set el) -> ISignal era (Set el) -> ISignal era (Set el)
    intersection = combine False False False Set.intersection

    {-|
        Pointwise combination of two incremental set signals.

        Let us assume a function @modify :: Bool -> Set el -> Set el@ where @modify False@ is the
        identity function and @modify True@ is the complement function. This function is not
        implementable since the complement of a finite set is usually infinite. However, we use this
        function for defining the behavior of @combine@.

        Now choose @mod1@, @mod2@, @mod'@ and @setComb@ such that the following holds:
        @
        setComb set1 set2 = modify mod' (modify mod1 set1 `Set.intersection` modify mod2 set2)
        @
        Then @combine mod1 mod2 mod' setComb@ is the pointwise application of @setComb@.
    -}
    combine :: (Ord el)
            => Bool
            -> Bool
            -> Bool
            -> (Set el -> Set el -> Set el)
            -> (ISignal era (Set el) -> ISignal era (Set el) -> ISignal era (Set el))
    combine mod1 mod2 mod' setComb = ISignal.combine start step1 step2 where

        start init1 init2       = (setComb init1 init2,(init1,init2))

        step1 diff1 (set1,set2) = (,) (combinationDiff mod1 mod2 mod' diff1 set1 set2)
                                      (patch set1 diff1,set2)

        step2 diff2 (set1,set2) = (,) (combinationDiff mod2 mod1 mod' diff2 set2 set1)
                                      (set1,patch set2 diff2)

    combinationDiff :: (Ord el)
                    => Bool -> Bool -> Bool -> Diff (Set el) -> Set el -> Set el -> Diff (Set el)
    combinationDiff diffMod otherMod mod' diff diffSet otherSet = Diff diffMap' where

        diffMap'    = Map.unions [consMap containedness (revDiffMap' containedness) |
                                  containedness <- [False,True]]

        revDiffMap' = flip reduce otherSet . reverseDiffMap diff . (/= diffMod) . (/= mod')

        reduce      = if otherMod then Set.difference else Set.intersection

        consMap val = Map.fromAscList . List.map (flip (,) val) . Set.toAscList

    -- * Queries
    null :: (Ord el) => ISignal era (Set el) -> SSignal era Bool
    null = fmap (== 0) . size

    size :: (Ord el) => ISignal era (Set el) -> SSignal era Int
    size = fmap Set.size . ISignal.toSSignal

    member :: (Ord el) => SSignal era el -> ISignal era (Set el) -> SSignal era Bool
    member els sets = Set.member <$> els <*> ISignal.toSSignal sets

    staticMember :: (Ord el) => el -> ISignal era (Set el) -> SSignal era Bool
    staticMember el = monolithicToSSignal . ISignal.map start step where

        start init                    = let

                                            contained = Set.member el init

                                        in (Monolithic contained,contained)

        step (Diff diffMap) contained = let

                                            contained' = case Map.lookup el diffMap of
                                                             Nothing            -> contained
                                                             Just containedness -> containedness

                                        in (Replacement contained',contained')

    notMember :: (Ord el) => SSignal era el -> ISignal era (Set el) -> SSignal era Bool
    notMember = (fmap not .) . member

    staticNotMember :: (Ord el) => el -> ISignal era (Set el) -> SSignal era Bool
    staticNotMember = (fmap not .) . staticMember

