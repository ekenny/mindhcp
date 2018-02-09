module ClientState where

import States
import Message


{-
  getAction function is basically the state table.  
  for a given state and message type, it returns 
  an action to perform.  That action function will
  return the new state.
-}
getAction::State->Message->(Message->Packet->IO State)

getAction Init _ = request 

getAction InitReboot _ = request

getAction Rebooting DhcpAck = bind
getAction Rebooting DhcpNack = startOver
getAction Rebooting _ = warn Rebooting  -- shouldn't happen

getAction Selecting DhcpOffer = collectOffer
getAction Selecting _ = warn Selecting -- shouldn't happen, but drop packet if it does

getAction Requesting DhcpOffer = discardOffer Requesting
getAction Requesting DhcpAck = bind
getAction Requesting DhcpNack = startOver

getAction Bound DhcpTimer1 = renew
getAction Bound _ = nothing Bound

getAction Renewing DhcpTimer2 = rebind
getAction Renewing DhcpNack = haltNetwork
getAction Renewing DhcpAck = bind
getAction Renewing _ = warn Renewing -- really shouldn't happen

getAction Rebinding DhcpAck = bind
getAction Rebinding DhcpNack = haltNetwork
getAction Rebinding _ = warn Rebinding  -- shouldn't happen

getAction _ End = haltNetwork 

-- no other combinations should occur, and would be an error
getAction state msg = reportStateError state

reportStateError::State -> Message -> Packet -> IO State
reportStateError state msg _ =
  do
    putStrLn("Error: Unexpected message (" ++ show msg ++ ") while " ++ show state)
    return state

request::Message -> Packet -> IO State
request _ _ =  
  do
    putStrLn "requesting new ip address"
    return Selecting
    
warn::State -> Message -> Packet -> IO State
warn state message _ = 
    do
      putStrLn("received invalid message (" ++ show message ++ ") while " ++ show state)
      return state

bind::Message->Packet->IO State
bind message _ = 
    do
      putStrLn("binding ip address")
      return Bound

rebind::Message->Packet->IO State
rebind message _ = 
  do
    putStrLn "rebinding"
    return Bound


discardOffer::State->Message->Packet->IO State
discardOffer state _ _ =
    do
      putStrLn("discarding offer received while accepting other offer")
      return state

collectOffer::Message->Packet->IO State
collectOffer _ _ =
    do
      putStrLn("collecting offer, accepting ip address offer")  -- todo: hanging onto the offer for later binding
      return Requesting
      

renew::Message->Packet->IO State
renew _ _ =
    do
      putStrLn("Renewing lease...")
      return Renewing

nothing::State->Message->Packet->IO State
nothing state msg _ =
    do
      putStrLn("doing nothing for message " ++ show msg ++ " while " ++ show state)
      return state

startOver::Message->Packet->IO State
startOver _ _ = 
  do
    putStrLn("starting over")
    return Init
    
haltNetwork::Message->Packet->IO State
haltNetwork msg _ = 
  do
    putStrLn("halting network in response to " ++ show msg ++ "")
    return Halted



-- THIS IS STILL UNDER CONSTRUCTION.  A LOT OF WORK STILL DO DO.
