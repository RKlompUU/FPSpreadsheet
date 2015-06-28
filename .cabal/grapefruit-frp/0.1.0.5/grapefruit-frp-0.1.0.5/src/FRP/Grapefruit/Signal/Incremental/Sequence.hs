module FRP.Grapefruit.Signal.Incremental.Sequence (

    -- * Diffs
    Diff (Diff),
    AtomicDiff (Insertion, Deletion, Shift, Update),
    insertion,
    deletion,
    shift,
    update,
    elementInsertion,
    elementDeletion,
    elementShift,
    elementUpdate,

    -- * Construction
    empty,
    singleton,
    (<|),
    (|>),

    -- * Combination
    (><),

    -- * Queries
    FRP.Grapefruit.Signal.Incremental.Sequence.null,
    FRP.Grapefruit.Signal.Incremental.Sequence.length,

    -- * Transformations
    map,
    staticMap,
    filter,
    staticFilter,
    reverse

) where

    -- Prelude
    import           Prelude hiding (filter, foldl, foldr, length, map, null, reverse, sum)
    import qualified Prelude

    -- Data
    import           Data.Semigroup as Semigroup
    import           Data.Monoid    as Monoid
    import           Data.Foldable  as Foldable
    import           Data.Sequence  as Seq       (Seq)
    import qualified Data.Sequence  as Seq

    -- Internal
    import           Internal.Signal.Incremental.Sequence.AtomicDiff as AtomicDiff
                                                                        hiding (atomicPatch,
                                                                                reverse)
    import qualified Internal.Signal.Incremental.Sequence.AtomicDiff as AtomicDiff
    import           Internal.Signal.Incremental.Sequence.Selection  as SeqSel
                                                                        hiding (atomicPatch)
    import qualified Internal.Signal.Incremental.Sequence.Selection  as SeqSel

    -- FRP.Grapefruit
    import           FRP.Grapefruit.Signal.Segmented   as SSignal
    import           FRP.Grapefruit.Signal.Incremental as ISignal hiding (map)
    import qualified FRP.Grapefruit.Signal.Incremental as ISignal

    -- * Diffs
    instance Incremental (Seq el) where

        data Diff (Seq el) = Diff (Seq (AtomicDiff el))

        patch seq (Diff atomicDiffs) = foldl atomicPatch seq atomicDiffs

        type ValidationState (Seq el) = Int

        validationInit initSeq = Seq.length initSeq

        validationStep (Diff atomicDiffs) len = foldl consComp (Just len) atomicDiffs where

            consComp maybeLen atomicDiff = maybeLen >>= atomicValidationStep atomicDiff

    atomicValidationStep :: AtomicDiff el -> Int -> Maybe Int
    atomicValidationStep atomicDiff len | isOk      = Just (len + lengthDelta atomicDiff)
                                        | otherwise = Nothing where

        isOk                 = case atomicDiff of
                                   Insertion idx els -> idx >= 0 && idx <= len
                                   Deletion idx cnt  -> intervalIsOk idx cnt
                                   Shift from cnt to -> intervalIsOk from cnt && intervalIsOk to cnt
                                   Update idx els    -> intervalIsOk idx (Seq.length els)

        intervalIsOk idx cnt = idx >= 0 && cnt >= 0 && idx + cnt <= len

    instance Semigroup (Diff (Seq el)) where

        (<>) = mappend

    instance Monoid (Diff (Seq el)) where

        mempty = Diff Seq.empty

        Diff atomicDiffs1 `mappend` Diff atomicDiffs2 = Diff (atomicDiffs1 `mappend` atomicDiffs2)

    insertion :: Int -> Seq el -> Diff (Seq el)
    insertion idx els = fromAtomicDiff (Insertion idx els)

    deletion :: Int -> Int -> Diff (Seq el)
    deletion idx cnt = fromAtomicDiff (Deletion idx cnt)

    shift :: Int -> Int -> Int -> Diff (Seq el)
    shift from cnt to = fromAtomicDiff (Shift from cnt to)

    update :: Int -> Seq el -> Diff (Seq el)
    update idx els = fromAtomicDiff (Update idx els)

    fromAtomicDiff :: AtomicDiff el -> Diff (Seq el)
    fromAtomicDiff = Diff . Seq.singleton

    elementInsertion :: Int -> el -> Diff (Seq el)
    elementInsertion idx el = insertion idx (Seq.singleton el)

    elementDeletion :: Int -> Diff (Seq el)
    elementDeletion idx = deletion idx 1

    elementShift :: Int -> Int -> Diff (Seq el)
    elementShift from to = shift from 1 to

    elementUpdate :: Int -> el -> Diff (Seq el)
    elementUpdate idx el = update idx (Seq.singleton el)

    atomicPatch :: Seq el -> AtomicDiff el -> Seq el
    atomicPatch = AtomicDiff.atomicPatch id Seq.splitAt mappend

    diffLengthDelta :: Diff (Seq el) -> Int
    diffLengthDelta (Diff atomicDiffs) = sum (fmap lengthDelta atomicDiffs)

    fromAtomicStep :: (AtomicDiff el -> state -> (AtomicDiff el',state))
                   -> (Diff (Seq el) -> state -> (Diff (Seq el'),state))
    fromAtomicStep atomicStep (Diff atomicDiffs) state = (Diff atomicDiffs',state') where

        (atomicDiffs',state')                    = foldl consComp nilComp atomicDiffs

        nilComp                                  = (Seq.empty,state)

        consComp (atomicDiffs',state) atomicDiff = let

                                                       (atomicDiff',state') = atomicStep atomicDiff
                                                                                         state

                                                   in (atomicDiffs' Seq.|> atomicDiff',state')

    -- * Construction
    empty :: ISignal era (Seq a)
    empty = ISignal.const Seq.empty

    singleton :: SSignal era el -> ISignal era (Seq el)
    singleton = ISignal.map start step . ISignal.monolithicFromSSignal where

        start (Monolithic init) = (Seq.singleton init,())

        step (Replacement el) _ = (Diff (Seq.singleton (Update 0 (Seq.singleton el))),())

    (<|) :: SSignal era el -> ISignal era (Seq el) -> ISignal era (Seq el)
    heads <| tails = singleton heads >< tails

    (|>) :: ISignal era (Seq el) -> SSignal era el -> ISignal era (Seq el)
    inits |> lasts = inits >< singleton lasts

    -- * Combination
    (><) :: ISignal era (Seq el) -> ISignal era (Seq el) -> ISignal era (Seq el)
    (><) = ISignal.combine start (fromAtomicStep atomicStep1) (fromAtomicStep atomicStep2) where

        start init1 init2            = (init1 `mappend` init2,Seq.length init1)

        atomicStep1 atomicDiff1 len1 = (atomicDiff1,len1 + lengthDelta atomicDiff1)

        atomicStep2 atomicDiff2 len1 = (AtomicDiff.relocate len1 atomicDiff2,len1)

    -- * Queries
    null :: ISignal era (Seq el)-> SSignal era Bool
    null = fmap (== 0) . FRP.Grapefruit.Signal.Incremental.Sequence.length

    length :: ISignal era (Seq el) -> SSignal era Int
    length = ISignal.monolithicToSSignal . ISignal.map start step where

        start init    = let

                            lenInit = Seq.length init

                        in (Monolithic lenInit,lenInit)

        step diff len = let

                            len' = len + diffLengthDelta diff

                        in (Replacement len',len')

    -- equals :: ISignal era (Seq el) -> ISignal era (Seq el) -> SSignal era Bool

    -- compare :: ISignal era (Seq el) -> ISignal era (Seq el) -> SSignal era Ordering

    -- * Indexing
    -- index :: ISignal era (Seq el) -> SSignal era Int -> SSignal era el

    -- take :: SSignal era Int -> ISignal era (Seq el) -> ISignal era (Seq el)

    -- drop :: SSignal era Int -> ISignal era (Seq el) -> ISignal era (Seq el)

    -- splitAt :: SSignal era Int
    --         -> ISignal era (Seq el)
    --         -> (ISignal era (Seq el),ISignal era (Seq el))

    -- * Transformations
    -- not in Data.Sequence (but fmap is)
    map :: SSignal era (el -> el') -> ISignal era (Seq el) -> ISignal era (Seq el')
    map = ISignal.combine start funStep (fromAtomicStep atomicSeqStep) . monolithicFromSSignal where

        start (Monolithic initFun) initSeq    = (fmap initFun initSeq,(initFun,initSeq))

        funStep (Replacement fun) (_,seq)     = (,) (Diff (Seq.singleton (Update 0 (fmap fun seq))))
                                                    (fun,seq)

        atomicSeqStep atomicSeqDiff (fun,seq) = (,) (fmap fun atomicSeqDiff)
                                                    (fun,atomicPatch seq atomicSeqDiff)

    staticMap :: (el -> el') -> ISignal era (Seq el) -> ISignal era (Seq el')
    staticMap fun = ISignal.map start (fromAtomicStep atomicStep) where

        start init              = (fmap fun init,())

        atomicStep atomicDiff _ = (fmap fun atomicDiff,())

    -- not in Data.Sequence
    filter :: SSignal era (el -> Bool) -> ISignal era (Seq el) -> ISignal era (Seq el)
    filter = ISignal.combine start prdStep seqStep . ISignal.monolithicFromSSignal where

        start (Monolithic initPrd) initSeq  = (,) (filterSeq initPrd initSeq)
                                                  (initPrd,initSeq,SeqSel.fromSeq initPrd initSeq)

        prdStep (Replacement prd) (_,seq,_) = (,) (Diff $
                                                   Seq.fromList [Deletion  0 (Seq.length seq),
                                                                 Insertion 0 (filterSeq prd seq)])
                                                  (prd,seq,SeqSel.fromSeq prd seq)

        seqStep seqDiff (prd,seq,seqSel)    = let

                                                  (seqDiff',seqSel') = selectionStep prd
                                                                                     seqDiff
                                                                                     seqSel

                                              in (seqDiff',(prd,patch seq seqDiff,seqSel'))

    staticFilter :: (el -> Bool) -> ISignal era (Seq el) -> ISignal era (Seq el)
    staticFilter prd = ISignal.map start (selectionStep prd) where

        start initSeq = (filterSeq prd initSeq,SeqSel.fromSeq prd initSeq)

    filterSeq :: (el -> Bool) -> Seq el -> Seq el
    filterSeq prd = Seq.fromList . Prelude.filter prd . toList

    selectionStep :: (el -> Bool) -> Diff (Seq el) -> SeqSel -> (Diff (Seq el),SeqSel)
    selectionStep prd = fromAtomicStep (unsafeAtomicSelectionStep prd) . breakUpdates where

        breakUpdates (Diff atomicDiffs) = Diff (atomicDiffs >>= breakUpdate)

        breakUpdate (Update idx els)    = Seq.fromList $
                                          [Deletion idx (Seq.length els),Insertion idx els]
        breakUpdate atomicDiff          = Seq.singleton atomicDiff

    unsafeAtomicSelectionStep :: (el -> Bool) -> AtomicDiff el -> SeqSel -> (AtomicDiff el,SeqSel)
    unsafeAtomicSelectionStep prd atomicDiff seqSel = (atomicDiff',seqSel') where

        atomicDiff' = case atomicDiff of
                          Insertion idx els -> Insertion (selectionIndex seqSel idx)
                                                         (filterSeq prd els)
                          Deletion idx cnt  -> uncurry Deletion (selectionInterval seqSel idx cnt)
                          Shift from cnt to -> uncurry Shift (selectionInterval seqSel from cnt) $
                                               selectionIndex seqSel' to
                          Update idx els    -> error "grapefruit-frp: internal error"

        seqSel'     = SeqSel.atomicPatch prd seqSel atomicDiff

    reverse :: ISignal era (Seq el) -> ISignal era (Seq el)
    reverse = ISignal.map start (fromAtomicStep atomicStep) where

        start init                = (Seq.reverse init,Seq.length init)

        atomicStep atomicDiff len = (AtomicDiff.reverse len atomicDiff,len + lengthDelta atomicDiff)

    -- not in Data.Sequence
    sort :: (Ord el) => ISignal era (Seq el) -> ISignal era (Seq el)
    sort = staticSortBy compare

    -- not in Data.Sequence
    sortBy :: SSignal era (el -> el -> Ordering) -> ISignal era (Seq el) -> ISignal era (Seq el)
    sortBy = error "ISignal.sortBy not yet implemented"

    -- not in Data.Sequence
    staticSortBy :: (el -> el -> Ordering) -> ISignal era (Seq el) -> ISignal era (Seq el)
    staticSortBy = error "ISignal.staticSortBy not yet implemented"
