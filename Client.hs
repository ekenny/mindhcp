module Client where

import IO
import Message
import ClientState
import States


doDhcp::State->IO()
doDhcp Halted = return ()
doDhcp state = 
  do
    putStrLn("client state: " ++ show state)
    (newMsg, packet) <- waitForMsg
    newState <- (getAction state newMsg) newMsg packet
    doDhcp newState


waitForMsg::IO (Message, Packet)
waitForMsg = 
  do putStrLn("What msg? [a]ck [o]ffer [n]ack [t]imer1 t[i]mer2 [e]nd:")
     getInput

getInput::IO (Message, Packet)
getInput = 
  do msg <- getLine
     return (getEvent msg)
                     
testPacket = Packet 0 0 0 0 0 0 0 0 0 0 0 "test" "file" 0 [Option 0 1 [0]]

getEvent::String -> (Message, Packet)
getEvent "a" = (DhcpAck, testPacket)
getEvent "o" = (DhcpOffer, testPacket)
getEvent "n" = (DhcpNack, testPacket)
getEvent "e" = (End, testPacket)
getEvent "t" = (DhcpTimer1, testPacket)
getEvent "i" = (DhcpTimer2, testPacket)
getEvent  _  = (End, testPacket)

