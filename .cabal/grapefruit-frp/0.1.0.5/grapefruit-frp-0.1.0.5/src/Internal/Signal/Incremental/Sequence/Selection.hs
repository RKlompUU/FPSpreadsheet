module Internal.Signal.Incremental.Sequence.Selection (

    SeqSel,
    fromSeq,
    splitAt,
    selectionIndex,
    selectionInterval,
    atomicPatch

) where

    -- Prelude
    import Prelude hiding (splitAt)

    -- Data
    import Data.Monoid     as Monoid
    import Data.Foldable   as Foldable
    import Data.FingerTree as FingerTree
    import Data.Sequence   as Seq        (Seq)

    -- Internal
    import           Internal.Signal.Incremental.Sequence.AtomicDiff as AtomicDiff
                                                                        hiding (atomicPatch)
    import qualified Internal.Signal.Incremental.Sequence.AtomicDiff as AtomicDiff

    -- * Pairs of original length and filtered length
    newtype Lengths = Lengths (Sum Int,Sum Int) deriving (Monoid)

    lengths :: Int -> Int -> Lengths
    lengths origLen selLen = Lengths (Sum origLen,Sum selLen)

    originalLength :: Lengths -> Int
    originalLength (Lengths (Sum origLen,_)) = origLen

    selectionLength :: Lengths -> Int
    selectionLength (Lengths (_,Sum selLen)) = selLen

    -- * Blocks of multiple bad elements and one good element
    newtype Block = Block Int

    instance Measured Lengths Block where

        measure (Block badCnt) = lengths (succ badCnt) 1

    -- * Sequence selections
    data SeqSel  = SeqSel (FingerTree Lengths Block) Int

    instance Monoid SeqSel where

        mempty = SeqSel FingerTree.empty 0

        SeqSel blocks1 end1 `mappend` seqSel2 = SeqSel (blocks1 >< others) end' where

            SeqSel others end' = adjustFront end1 seqSel2

    fromSeq :: (el -> Bool) -> Seq el -> SeqSel
    fromSeq prd = fromList . toList where

        fromList list = case break prd list of
                            (bads,[])         -> SeqSel FingerTree.empty (length bads)
                            (bads,ok : list') -> cons (length bads) (fromList list')

    cons :: Int -> SeqSel -> SeqSel
    cons badCnt (SeqSel blocks end) = SeqSel (Block badCnt <| blocks) end

    adjustFront :: Int -> SeqSel -> SeqSel
    adjustFront delta (SeqSel blocks end) = case viewl blocks of
                                                EmptyL          -> SeqSel empty (end + delta)
                                                block :< blocks -> SeqSel (adjBlock block <| blocks)
                                                                          end
                                            where

        adjBlock (Block badCnt) = Block (badCnt + delta)

    splitAt :: Int -> SeqSel -> (SeqSel,SeqSel)
    splitAt idx (SeqSel blocks end) = (SeqSel blocks1 end1,seqSel2) where

        (blocks1,others) = FingerTree.split ((<= idx) . originalLength) blocks

        end1             = idx - originalLength (measure blocks1)

        seqSel2          = adjustFront (negate end1) (SeqSel others end)

    selectionIndex :: SeqSel -> Int -> Int
    selectionIndex (SeqSel blocks _) idx = selectionLength $
                                           measure $
                                           FingerTree.takeUntil ((<= idx) . originalLength) blocks

    selectionInterval :: SeqSel -> Int -> Int -> (Int,Int)
    selectionInterval seqSel idx cnt = (selIdx,selectionIndex seqSel (idx + cnt) - selIdx) where

        selIdx = selectionIndex seqSel idx

    atomicPatch :: (el -> Bool) -> SeqSel -> AtomicDiff el -> SeqSel
    atomicPatch prd = AtomicDiff.atomicPatch (fromSeq prd) splitAt mappend

