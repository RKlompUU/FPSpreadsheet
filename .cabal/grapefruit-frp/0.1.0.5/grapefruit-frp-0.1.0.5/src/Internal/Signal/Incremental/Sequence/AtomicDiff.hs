module Internal.Signal.Incremental.Sequence.AtomicDiff (

    AtomicDiff (Insertion, Deletion, Shift, Update),
    atomicPatch,
    lengthDelta,
    relocate,
    reverse

) where

    -- Prelude
    import Prelude hiding (length, reverse)

    -- Data
    import           Data.Sequence as Seq (Seq)
    import qualified Data.Sequence as Seq

    data AtomicDiff el = Insertion Int (Seq el)
                       | Deletion Int Int
                       | Shift Int Int Int
                       | Update Int (Seq el)

    instance Functor AtomicDiff where

        fmap fun (Insertion idx els) = Insertion idx (fmap fun els)
        fmap _   (Deletion idx cnt)  = Deletion idx cnt
        fmap _   (Shift from cnt to) = Shift from cnt to
        fmap fun (Update idx els)    = Update idx (fmap fun els)

    atomicPatch :: (Seq el -> stuff)
          -> (Int -> stuff -> (stuff,stuff))
          -> (stuff -> stuff -> stuff)
          -> (stuff -> AtomicDiff el -> stuff)
    atomicPatch fromSeq splitAt (><) = actualPatch where

        actualPatch stuff (Insertion idx els) = insert idx (fromSeq els) stuff
        actualPatch stuff (Deletion idx cnt)  = take idx stuff >< drop (idx + cnt) stuff
        actualPatch stuff (Shift from cnt to) = let

                                                  (front,middleAndBack) = splitAt from stuff

                                                  (middle,back)         = splitAt cnt middleAndBack

                                                in insert to middle (front >< back)
        actualPatch stuff (Update idx els)    = flip actualPatch (Insertion idx els) $
                                                flip actualPatch (Deletion idx (Seq.length els)) $
                                                stuff

        insert idx middle stuff               = let

                                                  (front,back) = splitAt idx stuff

                                                in front >< (middle >< back)

        take                                  = (fst .) . splitAt

        drop                                  = (snd .) . splitAt

    lengthDelta :: AtomicDiff el -> Int
    lengthDelta (Insertion _ els) = Seq.length els
    lengthDelta (Deletion _ cnt)  = negate cnt
    lengthDelta (Shift _ _ _)     = 0
    lengthDelta (Update _ _)      = 0

    relocate :: Int -> AtomicDiff el -> AtomicDiff el
    relocate offset (Insertion idx els) = Insertion (idx + offset) els
    relocate offset (Deletion idx cnt)  = Deletion (idx + offset) cnt
    relocate  offset (Shift from cnt to) = Shift (from + offset) cnt (to + offset)
    relocate offset (Update idx els)    = Update (idx + offset) els

    reverse :: Int -> AtomicDiff el -> AtomicDiff el
    reverse len (Insertion idx els) = Insertion (len - idx) (Seq.reverse els)
    reverse len (Deletion idx cnt)  = Deletion (len - idx - cnt) cnt
    reverse len (Shift from cnt to) = Shift (len - from - cnt) cnt (len - to - cnt)
    reverse len (Update idx els)    = Update (len - idx - Seq.length els) (Seq.reverse els)
