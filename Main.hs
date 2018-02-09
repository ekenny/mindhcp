
import IO
import States
import Message
import ClientState
import Client

main::IO()
main = 
  do
    doDhcp Init
    return ()
  
  