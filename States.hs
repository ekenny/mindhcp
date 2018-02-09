module States where


data State = Init | InitReboot | Rebooting | Selecting | Requesting | Rebinding | Bound | Renewing | Halted deriving (Eq,Show)




