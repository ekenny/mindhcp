module Message where

import Data.Word

type Tag = Word8


data Message = DhcpAck | DhcpNack | DhcpDiscover | DhcpOffer | DhcpRequest
                 | DhcpDecline | DhcpRelease | DhcpInform | DhcpTimer1
                 | DhcpTimer2 | Reboot | End | NoMessage deriving (Eq,Show)

type TaggedMessage = (Tag, Message)

getTag::TaggedMessage->Tag
getTag msg = fst msg

getMessage::TaggedMessage->Message
getMessage msg = snd msg


taggedMessages = 
    [(5, DhcpAck), (0, DhcpNack), (0, DhcpDiscover),
     (0, DhcpOffer), (0, DhcpRequest), (0, DhcpDecline),
     (0, DhcpRelease), (0, DhcpInform), (0, DhcpTimer1),
     (0, DhcpTimer2), (0, Reboot), (0, End), (0, NoMessage)]

tag4Message::Message->TaggedMessage
tag4Message m = head (filter (\x-> snd(x) == m) taggedMessages)


msg4Tag::Tag->TaggedMessage
msg4Tag t = head (filter (\x-> fst(x) == t) taggedMessages)


type OptionTag = Word8

data Option = Option { tag :: OptionTag, len :: Word8, val :: [Word8]}

data Packet = Packet { msgOp :: Word8,
	msgHtype  :: Word8,
	msgHlen   :: Word8,
	msgHops   :: Word8,
	msgXid    :: Word8,
	msgSecs   :: Word16,
	msgFlags  :: Word16,
	msgCiaddr :: Word32,
	msgYiaddr :: Word32,
	msgGiaddr :: Word32,
	msgChaddr :: Word32,
	msgSname  :: String,
	msgFile   :: String,
	msgCookie :: Word32,
        msgOptions:: [Option] }


nilPacket = Packet 0 0 0 0 0 0 0 0 0 0 0 "" "" 0 [Option 0 1 [0]]


