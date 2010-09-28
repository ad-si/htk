-- | A MatchChannel is a guarded channel which allows arbitrary match functions
-- to be applied.  The intended application is for regular-expression
-- channels, where there is no better way of indexing, since
-- regular-expression matching is done by a black-box routine.
module Events.MatchChannel(
   Match(..), -- type of a guard.
   MatchChannel, -- type of a channel
   newMatchChannel, -- IO (MatchChannel v)
   ) where

import Events.GuardedEvents
import Events.GuardedChannels
import Events.RefQueue


newMatchChannel :: IO (MatchChannel v)
newMatchChannel =
   newMatchChannelPrim (error "Match.1")

newMatchChannelPrim :: v -> IO (MatchChannel v)
-- The arguments to newMatchChannelPrim are not looked at, but
-- help us to avoid overloading woes.
newMatchChannelPrim (_::v) =
   newGuardedChannel (error "newMatch1" :: (GQ (GuardQueue v) v))
      (error "newMatch2" :: (VQ (ValueQueue v)))

type MatchChannel v = GuardedChannel (Match v) v

-- -------------------------------------------------------------------
-- The Match type
-- -------------------------------------------------------------------

newtype Match v = Match (v -> Bool)
   -- return a and the transformed value if successful.

instance Guard (Match v) where
   nullGuard = Match (const True)
   andGuard (Match testFn1) (Match testFn2) =
      Match (\ v -> (testFn2 v) && (testFn1 v))
      -- We do testFn2 first because the definition of |>
      -- means that testFn1 is almost always nullGuard.


-- -------------------------------------------------------------------
-- The ValueQueue
-- -------------------------------------------------------------------

newtype ValueQueue v vD = ValueQueue (RefQueue (v,vD))

instance HasEmpty (ValueQueue v) where
   newEmpty =
      do
         queue <- newRefQueue
         return (ValueQueue queue)

instance HasAdd (ValueQueue v) v where
   add (ValueQueue queue1) v vD =
      do
         (queue2,invalidate) <- pushRefQueue queue1 (v,vD)
         return (ValueQueue queue2,invalidate)

instance HasRemove (ValueQueue v) (Match v) v where
   remove (ValueQueue queue1) (Match testFn) =
      do
         (searchResult,queue2) <- searchRefQueue queue1
            (\ (v,vD) -> testFn v)
         return (
            fmap
               (\ ((v,vD),backtrack) ->
                  (v,vD,
                     do
                        queue0 <- backtrack
                        return (ValueQueue queue0)
                     )
                  )
               searchResult,
            ValueQueue queue2
            )

-- -------------------------------------------------------------------
-- The GuardQueue
-- -------------------------------------------------------------------

newtype GuardQueue v gD = GuardQueue (RefQueue (Match v,gD))

instance HasEmpty (GuardQueue v) where
   newEmpty =
      do
         queue <- newRefQueue
         return (GuardQueue queue)

instance HasAdd (GuardQueue v) (Match v) where
   add (GuardQueue queue1) g gD =
      do
         (queue2,invalidate) <- pushRefQueue queue1 (g,gD)
         return (GuardQueue queue2,invalidate)

instance HasRemove (GuardQueue v) v (Match v) where
   remove (GuardQueue queue1) v  =
      do
         (searchResult,queue2) <- searchRefQueue queue1
            (\ (Match testFn,gD) -> testFn v )
         return (
            fmap
               (\ ((g,gD),backtrack) ->
                  (g,gD,
                     do
                        queue0 <- backtrack
                        return (GuardQueue queue0)
                     )
                  )
               searchResult,
            GuardQueue queue2
            )

