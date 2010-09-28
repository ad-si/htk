-- Test for sync/rec on the same channel.
module Main(main) where

import Util.Computation (done)
import Events.Events
import Events.GuardedEvents
import Events.Channels
import Events.NullGuard
import Events.EqGuard
import Events.RegexChannel

import Events.Spawn

main =
   do
      ch <- newChannel
      test "Channel" (send ch 1) (receive ch)
      ch <- newNullGuardedChannel
      test "NullGuard" (send ch 1) (toEvent (listen ch))
      ch <- newEqGuardedChannel
      test "EqGuard" (send ch (1::Int,"foo"))
         (toEvent (listen ch |> Eq (1::Int)))
      ch <- newRegexChannel
      test "Regex" (sendString ch "hello") (matchEvent "hello" ch)
      putStrLn "Test successful"

test :: String -> Event a -> Event b -> IO ()
test name sendEvt recvEvt =
   do
      let both = sendEvt >>> done +> recvEvt >>> done
      putStrLn ("Testing "++name)
      putStrLn "S before"
      sync(noWait(sendEvt))
      sync(both)
      putStrLn "R before"
      sync(noWait(recvEvt))
      sync both
      putStrLn "S after"
      sync(noWait both)
      sync sendEvt
      putStrLn "R after"
      sync(noWait both)
      sync recvEvt
      putStrLn "both-both"
      sync(noWait both)
      sync both
      putStrLn "done"

