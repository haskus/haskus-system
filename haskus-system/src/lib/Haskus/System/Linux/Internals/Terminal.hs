{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Terminal (TTY)
--
-- FIXME: some of the IOCTLs numbers are arch specific. Currently we only
-- support x86-64. See ``arch/ARCH/include/uapi/asm/ioctls.h``
module Haskus.System.Linux.Internals.Terminal
   ( TermConfig (..)
   , TermConfigExt (..)
   , KTermConfig
   , ControlChar (..)
   , InputFlag (..)
   , InputFlags
   , OutputFlag (..)
   , OutputFlags
   , ofCRDLY
   , ofCR3
   , ofTABDLY
   , ofTAB3
   , ofXTABS
   , BaudRate (..)
   , ControlSize (..)
   , ControlFlag (..)
   , ControlFlags
   , getInputBaudRate
   , setInputBaudRate
   , getOutputBaudRate
   , setOutputBaudRate
   , LocalFlag (..)
   , LocalFlags
   , FlowControl (..)
   , FlushArg (..)
   , FlushOption (..)
   , WindowSize (..)
   -- * Get and set terminal attributes
   , ttyGetConfig
   , ttySetConfig
   , ttySetConfigDrain
   , ttySetConfigFlush
   , ttyGetConfigExt
   , ttySetConfigExt
   , ttySetConfigDrainExt
   , ttySetConfigFlushExt
   -- * Lock/unlock terminal attributes
   , ttyGetConfigLock
   , ttySetConfigLock
   -- * Get/Set window size
   , ttyGetWindowSize
   , ttySetWindowSize
   -- * Sending a break (drain)
   , ttyDrain
   , ttyDrainDuration
   , ttyDrainStart
   , ttyDrainStop
   -- * Software flow control
   , ttyFlowControl
   -- * Buffer count and flushing
   , ttyFlush
   , ttyGetInputSize
   , ttyGetOutputSize
   -- * Faking input
   , ttyFakeInput
   , ttyGetConsoleDevice
   -- * Console
   , ttyRedirectConsole
   -- * Controlling terminal
   , ttySetControllingTerminal
   , ttyRemoveControllingTerminal
   -- * Process group and session ID
   , ttyGetSessionId
   , ttyGetForegroundProcessGroupId
   , ttySetForegroundProcessGroupId
   -- * Exclusive mode
   , ttyEnableExclusive
   , ttyIsExclusive
   , ttyDisableExclusive
   -- * Line discipline
   , ttyGetLineDiscipline
   , ttySetLineDiscipline
   -- * Pseudoterminals
   , ttyGetPtyNumber
   , ttyGeneratePtySignal
   -- ** Packet mode
   , PacketControlFlag (..)
   , PacketControlFlags
   , ttySetPacketMode
   , ttyGetPacketMode
   -- ** Locking
   , ttyGetPtyLock
   , ttySetPtyLock
   -- * Modem control
   , ttyGetModemBits
   , ttySetModemBits
   , ttyClearSomeModemBits
   , ttySetSomeModemBits
   , ttyModemWait
   -- * Making a line as local
   , ttyGetSoftwareCarrierFlag
   , ttySetSoftwareCarrierFlag
   -- * Non-blocking IO
   , ttySetNonBlockingMode
   -- * Asynchronous mode
   , ttySetAsyncMode
   )
where

import Haskus.Binary.Enum
import Haskus.Binary.BitField
import Haskus.Binary.BitSet as BitSet
import Haskus.Binary.Vector
import Haskus.Number.Word
import Haskus.Number.Int
import Haskus.Binary.Bits
import Haskus.Binary.Storable
import Haskus.Utils.Flow
import Haskus.Utils.Types.Generics
import Haskus.System.Linux.Internals.Ioctl
import Haskus.System.Linux.Ioctl
import Haskus.System.Linux.Handle
import Haskus.System.Linux.ErrorCode
import Haskus.System.Linux.Process

-- =============================================================
--    From linux/include/uapi/asm-generic/termbits.h
-- =============================================================

-- | Terminal configuration
--
-- struct termios
data TermConfig = TermConfig
   { termInputFlags     :: InputFlags      -- ^ Input mode flags
   , termOutputFlags    :: OutputFlags     -- ^ Output mode flags
   , termControlFlags   :: ControlFlags    -- ^ Control mode flags
   , termLocalFlags     :: LocalFlags      -- ^ Local mode flags
   , termLineDiscipline :: Word8           -- ^ Line discipline
   , termControlChars   :: Vector 19 Word8 -- ^ Control characters
   }
   deriving (Show,Generic,Storable)

-- | Terminal extended configuration
--
-- struct termios2
data TermConfigExt = TermConfigEx
   { term2InputFlags     :: InputFlags      -- ^ Input mode flags
   , term2OutputFlags    :: OutputFlags     -- ^ Output mode flags
   , term2ControlFlags   :: ControlFlags    -- ^ Control mode flags
   , term2LocalFlags     :: LocalFlags      -- ^ Local mode flags
   , term2LineDiscipline :: Word8           -- ^ Line discipline
   , term2ControlChars   :: Vector 19 Word8 -- ^ Control characters
   , term2InputSpeed     :: Word32          -- ^ Input speed
   , term2OutputSpeed    :: Word32          -- ^ Output speed
   }
   deriving (Show,Generic,Storable)

-- | struct ktermios
type KTermConfig = TermConfigExt

-- | Control characters
data ControlChar
   = CCIntr
   | CCQuit
   | CCErase
   | CCKill
   | CCEOF
   | CCTime
   | CCMin
   | CCSwitch
   | CCStart
   | CCStop
   | CCSuspend
   | CCEOL
   | CCReprint
   | CCDiscard
   | CCWordErase
   | CCNextLine
   | CCEOL2
   deriving (Show,Eq,Enum,CEnum)

-- | Input flags
data InputFlag
   = IFIgnoreBreak
   | IFBreakInt
   | IFIgnorePar
   | IFParMrk
   | IFInpck
   | IFIStrip
   | IFInlCR
   | IFIgnoreCR
   | IFICRNL
   | IFIUCLC
   | IFIXOf
   | IFIXAny
   | IFIXOff
   | IFIMaxBel
   | IFUtf8
   deriving (Show,Eq,Enum,BitOffset)

type InputFlags = BitSet Word32 InputFlag

-- | Output flags
data OutputFlag
   = OFPost
   | OFLCUC
   | OFONLCR
   | OFCRNL
   | OFNoCR
   | OFNLRET
   | OFFill
   | OFDel
   | OFNLDLY
   | OFCR1
   | OFCR2
   | OFTAB1
   | OFTAB2
   | OFBSDLY
   | OFVTDLY
   | OFFFDLY
   deriving (Show,Eq,Enum,BitOffset)

type OutputFlags = BitSet Word32 OutputFlag

-- Some output flags are made of several raw output flags
ofCRDLY,ofCR3,ofTABDLY,ofTAB3,ofXTABS :: OutputFlags

ofCRDLY  = BitSet.fromList [OFCR1,OFCR2]
ofCR3    = ofCRDLY
ofTABDLY = BitSet.fromList [OFTAB1,OFTAB2]
ofTAB3   = ofTABDLY
ofXTABS  = ofTABDLY

data BaudRate
   = Baud0   -- ^ Hang-up
   | Baud50
   | Baud75
   | Baud110
   | Baud134
   | Baud150
   | Baud200
   | Baud300
   | Baud600
   | Baud1200
   | Baud1800
   | Baud2400
   | Baud4800
   | Baud9600
   | Baud19200
   | Baud38400
   | Baud57600
   | Baud115200
   | Baud230400
   | Baud460800
   | Baud500000
   | Baud576000
   | Baud921600
   | Baud1000000
   | Baud1152000
   | Baud1500000
   | Baud2000000
   | Baud2500000
   | Baud3000000
   | Baud3500000
   | Baud4000000
   deriving (Show,Eq,Enum)

data ControlSize
   = CS5
   | CS6
   | CS7
   | CS8
   deriving (Show,Eq,Enum,CEnum)

data ControlFlag
   = CFStopB
   | CFRead
   | CFParenB
   | CFParodd
   | CFHupCl
   | CFLocal
   deriving (Show,Eq,Enum,BitOffset)

type ControlFlags = BitFields Word32
            '[ BitField 1 "CRTSCTS"  Bool          -- Flow control
             , BitField 1 "CMSPAR"   Bool          -- Mark or space (stick) parity
             , BitField 1 "dummy"    Word8
             , BitField 1 "ibaud ex" Bool          -- Input baud extra bit
             , BitField 8 "dummy2"   Word8
             , BitField 4 "ibaud"    Word8         -- Input baud

             , BitField 3 "dummy3"   Word8
             , BitField 1 "obaud ex" Bool          -- Output baud extra bit
             , BitField 6 "CFLAGS"   (BitSet Word8 ControlFlag)
             , BitField 2 "CSIZE"    (EnumField Word8 ControlSize)
             , BitField 4 "obaud"    Word8         -- Output baud
             ]

getInputBaudRate :: ControlFlags -> BaudRate
getInputBaudRate cf = toEnum $ fromIntegral $ if extractField @"ibaud ex" cf
   then 16 + extractField @"ibaud" cf
   else extractField @"ibaud" cf

getOutputBaudRate :: ControlFlags -> BaudRate
getOutputBaudRate cf = toEnum $ fromIntegral $ if extractField @"obaud ex" cf
   then 16 + extractField @"obaud" cf
   else extractField @"obaud" cf

setInputBaudRate :: BaudRate -> ControlFlags -> ControlFlags
setInputBaudRate br cf = cf
      |> updateField @"ibaud ex" (testBit x 4)
      |> updateField @"ibaud"    (x .&. 0x0F)
   where
      x = fromIntegral (fromEnum br)

setOutputBaudRate :: BaudRate -> ControlFlags -> ControlFlags
setOutputBaudRate br cf = cf
      |> updateField @"obaud ex" (testBit x 4)
      |> updateField @"obaud"    (x .&. 0x0F)
   where
      x = fromIntegral (fromEnum br)

-- | Local flags
data LocalFlag
   = LFISig
   | LFICanon
   | LFXCase
   | LFEcho
   | LFEchoE
   | LFEchoNL
   | LFNoFlush
   | LFToStop
   | LFEchoCtl
   | LFEchoPrt
   | LFEchoKe
   | LFFlushO
   | LFDummy
   | LFPending
   | LFIExtend
   | LFExtProc
   deriving (Show,Eq,Enum,BitOffset)


type LocalFlags = BitSet Word32 LocalFlag


-- Flow control
--
-- TCOOFF, TCOON, TCIOFF, TCION
data FlowControl
   = SuspendOutput -- Suspends output
   | ResumeOutput  -- Restart suspended output
   | SuspendInput  -- transmits a STOP character, which stops the terminal device from transmitting data to the system
   | ResumeInput   -- transmits a START character, which starts the terminal device transmitting data to the system
   deriving (Show,Eq,Enum,CEnum)

-- Flush parameter
--
-- TCIFLUSH, TCOFLUSH, TCIOFLUSH
data FlushArg
   = FlushNotRead -- TCIFLUSH
   | FlushNotSent -- TCOFLUSH
   | FlushBoth    -- TCIOFLUSH
   deriving (Show,Eq,Enum,CEnum)

-- tcsetattr uses these
data FlushOption
   = NoFlush                 -- TCSANOW
   | DrainOutput             -- TCSADRAIN
   | DrainOutputDiscardInput -- TCSAFLUSH
   deriving (Show,Eq,Enum,CEnum)

-- =============================================================
--    From linux/include/uapi/asm-generic/termios.h
-- =============================================================

data WindowSize = WindowSize
   { windowRows    :: {-# UNPACK #-} !Word16
   , windowColumns :: {-# UNPACK #-} !Word16
   , windowPixelX  :: {-# UNPACK #-} !Word16
   , windowPixelY  :: {-# UNPACK #-} !Word16
   }
   deriving (Show,Eq,Generic,Storable)

-- TODO: struct termio, modem lines

-- =============================================================
--    From linux/include/uapi/asm-generic/ioctls.h
-- =============================================================

-- | Get serial port settings
--
-- TCGETS
ttyGetConfig :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m TermConfig
ttyGetConfig = ioctlReadCmd (rawIoctlCommand 0x5401)

-- | Set serial port settings
--
-- TCSETS
ttySetConfig :: (MonadInIO m) => TermConfig -> Handle -> Excepts '[ErrorCode] m ()
ttySetConfig = ioctlWriteCmd (rawIoctlCommand 0x5402)

-- | Allow the output buffer to drain, and set serial port settings
--
-- TCSETSW
ttySetConfigDrain :: (MonadInIO m) => TermConfig -> Handle -> Excepts '[ErrorCode] m ()
ttySetConfigDrain = ioctlWriteCmd (rawIoctlCommand 0x5403)

-- | Allow the output buffer to drain, discard pending input, and set serial
-- port settings
--
-- TCSETSF
ttySetConfigFlush :: (MonadInIO m) => TermConfig -> Handle -> Excepts '[ErrorCode] m ()
ttySetConfigFlush = ioctlWriteCmd (rawIoctlCommand 0x5404)

-- Skipped:
--    * struct termio
--    * ioctls: TCGETA, TCSETA TCSETAW, TCSETAF

-- | Sending a break
-- 
-- If  the  terminal is using asynchronous serial data transmission, and arg is
-- zero, then send a break (a stream of zero bits) for between 0.25 and 0.5
-- seconds.  If the terminal is not  using asynchronous  serial  data
-- transmission, then either a break is sent, or the function returns without
-- doing anything.
--
-- If arg is nonzero, waits until all output written to the object referred to
-- by handle has been transmitted.
-- 
-- TCSBRK
ttyDrain :: (MonadInIO m) => Int -> Handle -> Excepts '[ErrorCode] m ()
ttyDrain = ioctlSignalCmd (rawIoctlCommand 0x5409)

-- | Drain for the given amount of deciseconds
--
-- So-called "POSIX version" of TCSBRK. It treats nonzero arg as a
-- timeinterval measured in deciseconds, and does nothing when the driver does
-- not support breaks.
--
-- TCSBRKP
ttyDrainDuration :: (MonadInIO m) => Int -> Handle -> Excepts '[ErrorCode] m ()
ttyDrainDuration = ioctlSignalCmd (rawIoctlCommand 0x5425)

-- | Start draining
--
-- Turn break on, that is, start sending zero bits.
--
-- TCSSBRK
ttyDrainStart :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m ()
ttyDrainStart = ioctlSignalCmd (rawIoctlCommand 0x5427) False

-- | Stop draining
--
-- Turn break off, that is, stop sending zero bits.
--
-- TCSCBRK
ttyDrainStop :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m ()
ttyDrainStop = ioctlSignalCmd (rawIoctlCommand 0x5428) False

-- | Suspends transmission or reception of data on the object referred to by
-- handle
--
-- TCXONC
ttyFlowControl :: (MonadInIO m) => FlowControl -> Handle -> Excepts '[ErrorCode] m ()
ttyFlowControl e = ioctlSignalCmd (rawIoctlCommand 0x540A) (fromEnum e)


-- | Discards data written to the object referred to by handle but not
-- transmitted, or data received but not read
--
-- TCFLSH
ttyFlush :: (MonadInIO m) => FlushArg -> Handle -> Excepts '[ErrorCode] m ()
ttyFlush e = ioctlSignalCmd (rawIoctlCommand 0x540B) (fromEnum e)

-- | Put the terminal into exclusive mode.  No further open(2) operations on the
-- terminal are  per‐ mitted.  (They will fail with EBUSY, except for a process
-- with the CAP_SYS_ADMIN capability.)
--
-- TIOCEXCL
ttyEnableExclusive :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m ()
ttyEnableExclusive = ioctlSignalCmd (rawIoctlCommand 0x540C) (0 :: Int)

-- | Indicate if the terminal is currently in exclusive mode (since Linux 3.8).
--
-- TIOCGEXCL
ttyIsExclusive :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m Bool
ttyIsExclusive fd = ioctlRead 0x54 0x40 fd ||> (/= (0::Int))

-- | Disable exclusive mode.
--
-- TIOCNXCL
ttyDisableExclusive :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m ()
ttyDisableExclusive = ioctlSignalCmd (rawIoctlCommand 0x540D) (0 :: Int)

-- | Set controlling terminal
--
-- Make the given terminal the controlling terminal of the calling process.  The
-- calling process must be a session leader and not have a controlling terminal
-- already. For this case, allowStolen should be specified as False.
-- 
-- If this terminal is already the controlling terminal of a different
-- session group, then the ioctl fails with EPERM, unless the caller has the
-- CAP_SYS_ADMIN capability and allowStealing is True, in which case the
-- terminal is stolen, and all processes that had it as controlling terminal
-- lose it.
-- 
--
-- TIOCSCTTY
ttySetControllingTerminal :: (MonadInIO m) => Bool -> Handle -> Excepts '[ErrorCode] m ()
ttySetControllingTerminal allowStealing = ioctlSignalCmd (rawIoctlCommand 0x540E) allowStealing


-- | Remove controlling terminal
--
-- If the given terminal was the controlling terminal of the calling process,
-- give up this controlling terminal.  If the process was session leader, then
-- send SIGHUP and SIGCONT to the foreground process group and all processes in
-- the current session lose their controlling terminal.
--
-- TIOCNOTTY
ttyRemoveControllingTerminal :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m ()
ttyRemoveControllingTerminal = ioctlSignalCmd (rawIoctlCommand 0x5422) False

-- | Get the process group ID of the foreground process group on this terminal.
--
-- TIOCGPGRP
ttyGetForegroundProcessGroupId :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m GroupID
ttyGetForegroundProcessGroupId fd = ioctlReadCmd (rawIoctlCommand 0x540F) fd ||> GroupID

-- | Set the foreground process group ID of this terminal.
--
-- TIOCSPGRP
ttySetForegroundProcessGroupId :: (MonadInIO m) => GroupID -> Handle -> Excepts '[ErrorCode] m ()
ttySetForegroundProcessGroupId (GroupID x) = ioctlWriteCmd (rawIoctlCommand 0x5410) x

-- | Get  the session ID of the given terminal.  This will fail with ENOTTY in
-- case the terminal is not a master pseudoterminal and not our controlling
-- terminal.  Strange.
--
-- TIOCGSID
ttyGetSessionId :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m SessionID
ttyGetSessionId fd = ioctlReadCmd (rawIoctlCommand 0x5429) fd ||> SessionID


-- | Get the number of bytes in the input buffer
--
-- FIONREAD/TIOCINQ
ttyGetInputSize :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m Int
ttyGetInputSize = ioctlReadCmd (rawIoctlCommand 0x541B)

-- | Get the number of bytes in the output buffer
--
-- TIOCOUTQ
ttyGetOutputSize :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m Int
ttyGetOutputSize = ioctlReadCmd (rawIoctlCommand 0x5411)

-- | Insert the given byte in the input queue
--
-- TIOCSTI
ttyFakeInput :: (MonadInIO m) => Word8 -> Handle -> Excepts '[ErrorCode] m ()
ttyFakeInput = ioctlWriteCmd (rawIoctlCommand 0x5412)


-- | Get window size
--
-- TIOCGWINSZ
ttyGetWindowSize :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m WindowSize
ttyGetWindowSize = ioctlReadCmd (rawIoctlCommand 0x5413)

-- | Set window size
--
-- TIOCSWINSZ
ttySetWindowSize :: (MonadInIO m) => WindowSize -> Handle -> Excepts '[ErrorCode] m ()
ttySetWindowSize = ioctlWriteCmd (rawIoctlCommand 0x5414)

-- | Get modem bits
--
-- TIOCMGET
ttyGetModemBits :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m Int
ttyGetModemBits = ioctlReadCmd (rawIoctlCommand 0x5415)

-- | Set modem bits
--
-- TIOCMSET
ttySetModemBits :: (MonadInIO m) => Int -> Handle -> Excepts '[ErrorCode] m ()
ttySetModemBits = ioctlWriteCmd (rawIoctlCommand 0x5418)


-- | Clear the indicated modem bits
--
-- TIOCMBIC
ttyClearSomeModemBits :: (MonadInIO m) => Int -> Handle -> Excepts '[ErrorCode] m ()
ttyClearSomeModemBits = ioctlWriteCmd (rawIoctlCommand 0x5417)

-- | Set the indicated modem bits
--
-- TIOCMBIS
ttySetSomeModemBits :: (MonadInIO m) => Int -> Handle -> Excepts '[ErrorCode] m ()
ttySetSomeModemBits = ioctlWriteCmd (rawIoctlCommand 0x5416)

-- | Wait for any of the 4 modem bits (DCD, RI, DSR, CTS) to change. The bits
-- of interest are specified as a bit mask in arg, by ORing together any of the
-- bit values, TIOCM_RNG, TIOCM_DSR, TIOCM_CD, and TIOCM_CTS. The caller should
-- use TIOCGICOUNT to see which bit has changed.
--
-- TIOCMIWAIT
--
-- FIXME: blocking call!
ttyModemWait :: (MonadInIO m) => Int -> Handle -> Excepts '[ErrorCode] m ()
ttyModemWait = ioctlWriteCmd (rawIoctlCommand 0x545C)


-- | Get software carreir flag
--
-- Get the status of the CLOCAL flag in the c_cflag  field  of  the termios
-- structure.
--
-- TIOCGSOFTCAR
ttyGetSoftwareCarrierFlag :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m Int
ttyGetSoftwareCarrierFlag = ioctlReadCmd (rawIoctlCommand 0x5419)

-- | Set software carrier flag
--
-- Set  the  CLOCAL flag in the termios structure when *argp is nonzero, and
-- clear it otherwise.
--
-- TIOCSSOFTCAR
ttySetSoftwareCarrierFlag :: (MonadInIO m) => Int -> Handle -> Excepts '[ErrorCode] m ()
ttySetSoftwareCarrierFlag = ioctlWriteCmd (rawIoctlCommand 0x541A)

-- | Redirect console output
--
-- Redirect output that would have gone to /dev/console or /dev/tty0 to the
-- given terminal. If that was a pseudoterminal master, send it to the slave. In
-- Linux before version 2.6.10, any‐ body can do this as long as the output was
-- not redirected yet; since version 2.6.10, only a process with the
-- CAP_SYS_ADMIN capability may do this. If output was redirected already EBUSY
-- is returned, but redirection can be stopped by using this ioctl with fd
-- pointing at /dev/con‐ sole or /dev/tty0.
--
-- TIOCCONS
ttyRedirectConsole :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m ()
ttyRedirectConsole = ioctlSignalCmd (rawIoctlCommand 0x541D) False


-- | Enable/disable packet mode
--
-- TIOCPKT
ttySetPacketMode :: (MonadInIO m) => Bool -> Handle -> Excepts '[ErrorCode] m ()
ttySetPacketMode b = ioctlWriteCmd (rawIoctlCommand 0x5420) (if b then 1 else 0 :: Int)

-- | Get packet mode
--
-- TIOCGPKT
ttyGetPacketMode :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m Bool
ttyGetPacketMode fd = ioctlRead 0x54 0x38 fd ||> (/= (0 :: Int32))


-- | Get the line discipline of the terminal
--
-- TIOCGETD
ttyGetLineDiscipline :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m Int
ttyGetLineDiscipline = ioctlReadCmd (rawIoctlCommand 0x5424)


-- | Set the line discipline of the terminal
--
-- TIOCSETD
ttySetLineDiscipline :: (MonadInIO m) => Int -> Handle -> Excepts '[ErrorCode] m ()
ttySetLineDiscipline = ioctlWriteCmd (rawIoctlCommand 0x5423)

-- | Get extended configuration
--
-- TCGETS2
ttyGetConfigExt :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m TermConfigExt
ttyGetConfigExt = ioctlRead 0x54 0x2A

-- | Set extended configuration
--
-- TCSETS2
ttySetConfigExt :: (MonadInIO m) => TermConfigExt -> Handle -> Excepts '[ErrorCode] m ()
ttySetConfigExt = ioctlWrite 0x54 0x2B

-- | Set extended configuration, allow the output buffer to drain
--
-- TCSETSW2
ttySetConfigDrainExt :: (MonadInIO m) => TermConfigExt -> Handle -> Excepts '[ErrorCode] m ()
ttySetConfigDrainExt = ioctlWrite 0x54 0x2C

-- | Set extended configuration, allow the output buffer to drain, discard
-- pending input
--
-- TCSETSF2
ttySetConfigFlushExt :: (MonadInIO m) => TermConfigExt -> Handle -> Excepts '[ErrorCode] m ()
ttySetConfigFlushExt = ioctlWrite 0x54 0x2D

-- | Get Pty number (of pty-mux device)
--
-- TIOCGPTN
ttyGetPtyNumber :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m Word32
ttyGetPtyNumber = ioctlRead 0x54 0x30

-- | Lock/unlock Pty
--
-- TIOCSPTLCK
ttySetPtyLock :: (MonadInIO m) => Bool -> Handle -> Excepts '[ErrorCode] m ()
ttySetPtyLock b = ioctlWrite 0x54 0x31 (if b then 1 else 0 :: Int)

-- | Get Pty lock state
--
-- TIOCGPTLCK
ttyGetPtyLock :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m Bool
ttyGetPtyLock fd = ioctlRead 0x54 0x39 fd ||> (/= (0 :: Int32))

-- | Get primary device node of /dev/console
--
-- TIOCGDEV
ttyGetConsoleDevice :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m Word32
ttyGetConsoleDevice = ioctlRead 0x54 0x32

-- | pty: generate signal
--
-- TIOCSIG
ttyGeneratePtySignal :: (MonadInIO m) => Int32 -> Handle -> Excepts '[ErrorCode] m ()
ttyGeneratePtySignal = ioctlWrite 0x54 0x36

-- | Get the locking status of the terminal attributes
--
-- The lock is itself a TermConfig structure, with nonzero bits or fields
-- indicating a locked value.
--
-- TIOCGLCKTRMIOS
ttyGetConfigLock :: (MonadInIO m) => Handle -> Excepts '[ErrorCode] m TermConfig
ttyGetConfigLock = ioctlReadCmd (rawIoctlCommand 0x5456)

-- | Lock/unlock terminal attributes
--
-- The lock is itself a TermConfig structure, with nonzero bits or fields
-- indicating a locked value.
--
-- TIOCSLCKTRMIOS
ttySetConfigLock :: (MonadInIO m) => TermConfig -> Handle -> Excepts '[ErrorCode] m ()
ttySetConfigLock = ioctlWriteCmd (rawIoctlCommand 0x5457)

-- | Enable/disable non-blocking mode (disabled by default)
--
-- FIONBIO
ttySetNonBlockingMode :: (MonadInIO m) => Bool -> Handle -> Excepts '[ErrorCode] m ()
ttySetNonBlockingMode b = ioctlWriteCmd (rawIoctlCommand 0x5421) (if b then 1 else 0 :: Word64)

-- | Enable/disable asynchronous mode (disabled by default)
--
-- Sends SIGIO signals when I/O is possible.
--
-- FIOASYNC
ttySetAsyncMode :: (MonadInIO m) => Bool -> Handle -> Excepts '[ErrorCode] m ()
ttySetAsyncMode b = ioctlWriteCmd (rawIoctlCommand 0x5452) (if b then 1 else 0 :: Word64)

-- TODO:
--
-- TIOCGICOUNT 0x545D (require serial_icounter_struct from uapi/linux/serial.h)
-- TIOCLINUX   0x541C
-- TIOCGSERIAL 0x541E
-- TIOCSSERIAL 0x541F
--
-- TIOCGRS485  0x542E
-- TIOCSRS485  0x542F
-- 
--  TCGETX              0x5432 /* SYS5 TCGETX compatibility */
--                         ^ overload with ttyGetConsoleDevice?
--  TCSETX              0x5433
--  TCSETXF             0x5434
--  TCSETXW             0x5435
--  TIOCVHANGUP 0x5437
-- 
--  FIONCLEX    0x5450
--  FIOCLEX             0x5451
--  TIOCSERCONFIG       0x5453
--  TIOCSERGWILD        0x5454
--  TIOCSERSWILD        0x5455
--  TIOCSERGSTRUCT      0x5458 /* For debugging only */
--  TIOCSERGETLSR   0x5459 /* Get line status register */
--  TIOCSERGETMULTI 0x545A /* Get multiport config  */
--  TIOCSERSETMULTI 0x545B /* Set multiport config */
-- 
-- -- Warning: this one is arch specific!!
-- FIOQSIZE       0x5460 -- get exact space used by quota



-- | Packet control flags
data PacketControlFlag
   = PacketFlushRead
   | PacketFlushWrite
   | PacketStop
   | PacketStart
   | PacketNoStop
   | PacketDoStop
   | PacketIOCTL
   deriving (Show,Eq,Enum,BitOffset)

type PacketControlFlags = BitSet Word8 PacketControlFlag
