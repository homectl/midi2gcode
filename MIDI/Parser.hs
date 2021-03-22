{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module MIDI.Parser where

import           Data.Binary          (Binary (get, put))
import           Data.Binary.Get      (Get)
import qualified Data.Binary.Get      as Get
import           Data.Bits            (Bits, shiftL, shiftR, testBit, (.&.),
                                       (.|.))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Int             (Int16, Int8)
import           Data.Word            (Word16, Word32, Word8)

newtype Int14 = Int14 Int16 deriving (Num, Integral, Real, Ord, Enum, Eq, Bits)
newtype Word24 = Word24 Word32 deriving (Num, Integral, Real, Ord, Enum, Eq, Bits)

instance Show Int14 where show (Int14 x) = show x
instance Show Word24 where show (Word24 x) = show x

instance Binary Int14 where
  put _ = error "Word14"
  get = do
    a <- fromIntegral <$> Get.getWord8
    b <- fromIntegral <$> Get.getWord8
    return $! fromIntegral $ (a .|. (b `shiftL` 7) :: Int14) - 8192

instance Binary Word24 where
  put _ = error "Word24"
  get = do
    a <- fromIntegral <$> Get.getWord16be
    b <- fromIntegral <$> Get.getWord8
    return $! fromIntegral (b .|. (a `shiftL` 8) :: Word24)

-- No Ord instance, because we can't order by relative time.
newtype DeltaTime = DeltaTime Word32
  deriving (Show, Eq)

newtype AbsTime = AbsTime Word32
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance Binary DeltaTime where
  put _ = error "DeltaTime"
  get = DeltaTime <$> getVarInt

getVarInt :: Get Word32
getVarInt = go 0
  where
    go acc = do
      b <- fromIntegral <$> Get.getWord8
      let acc' = acc `shiftL` 7 .|. b .&. 0x7f
      if testBit b 7
        then go acc'
        else return acc'

newtype Tempo = Tempo Word24 deriving (Show, Binary, Eq, Ord, Num, Enum, Real, Integral)
newtype Channel = Channel Word8 deriving (Show, Binary, Eq, Ord)
newtype Velocity = Velocity Word8 deriving (Show, Binary, Eq, Ord)
newtype Note = Note Int8 deriving (Show, Binary, Eq, Ord)
newtype MidiFormat = MidiFormat Word16 deriving (Show, Binary, Eq, Num)

data MidiHeader = MidiHeader
  { format  :: MidiFormat
  , ntracks :: Word16
  , tickdiv :: Word16
  }
  deriving (Show)

data ChannelEvent
  = NoteOff Note Velocity
  | NoteOn Note Velocity
  | PolyphonicPressure Note Word8
  | Controller Word8 Word8
  | ProgramChange Word8
  | ChannelPressure Word8
  | PitchBend Int14
  deriving (Show, Eq)

data MetaEvent
  = SequenceNumber Word16
  | Text BS.ByteString
  | Copyright BS.ByteString
  | TrackName BS.ByteString
  | InstrumentName BS.ByteString
  | DeviceName BS.ByteString
  | ChannelPrefix Word8
  | EndOfTrack
  | TempoChange Tempo
  | SMPTEOffset Word8 Word8 Word8 Word8 Word8
  | TimeSignature Word8 Word8 Word8 Word8
  | KeySignature Word8 Word8
  | SequencerSpecificEvent BS.ByteString
  deriving (Show, Eq)

data MidiEvent
  = ChannelEvent Channel ChannelEvent
  | SysExEvent BS.ByteString
  | MetaEvent MetaEvent
  deriving (Show, Eq)

data MidiTimedEvent time = MidiTimedEvent time MidiEvent
  deriving (Show, Eq)

instance Ord time => Ord (MidiTimedEvent time) where
  (MidiTimedEvent a _) <= (MidiTimedEvent b _) = a <= b

data MidiTrack time = MidiTrack
  { track  :: Word16
  , events :: [MidiTimedEvent time]
  }
  deriving (Show)

data MidiFile time = MidiFile MidiHeader [MidiTrack time]
  deriving (Show)

instance Binary MetaEvent where
  put _ = error "OMG"
  get = do
    ty <- Get.getWord8
    len <- fromIntegral <$> getVarInt
    case ty of
      0x00 -> SequenceNumber <$> get
      0x01 -> Text <$> Get.getByteString len
      0x02 -> Copyright <$> Get.getByteString len
      0x03 -> TrackName <$> Get.getByteString len
      0x04 -> InstrumentName <$> Get.getByteString len
      0x09 -> DeviceName <$> Get.getByteString len
      0x20 -> ChannelPrefix <$> get
      0x2f -> return EndOfTrack
      0x51 -> TempoChange <$> get
      0x54 -> SMPTEOffset <$> get <*> get <*> get <*> get <*> get
      0x58 -> TimeSignature <$> get <*> get <*> get <*> get
      0x59 -> KeySignature <$> get <*> get
      0x7f -> SequencerSpecificEvent <$> Get.getByteString len
      _    -> fail $ "MetaEvent: " ++ show ty

getChannelEvent :: Word8 -> Get ChannelEvent
getChannelEvent 0x8 = NoteOff <$> get <*> get
getChannelEvent 0x9 = NoteOn <$> get <*> get
getChannelEvent 0xa = PolyphonicPressure <$> get <*> get
getChannelEvent 0xb = Controller <$> get <*> get
getChannelEvent 0xc = ProgramChange <$> get
getChannelEvent 0xd = ChannelPressure <$> get
getChannelEvent 0xe = PitchBend <$> get
getChannelEvent ty  = fail $ "getChannelEvent: " ++ show ty

getMidiEvent :: Word8 -> Get MidiEvent
getMidiEvent = \case
  0xf0                          -> SysExEvent <$> (getVarInt >>= Get.getByteString . fromIntegral)
  0xff                          -> MetaEvent <$> get
  ty | ty >= 0x80 && ty <= 0xef -> ChannelEvent (Channel $ ty .&. 0x7) <$> getChannelEvent (ty `shiftR` 4)
  ty                            -> fail $ "MidiEvent: " ++ show ty

getMidiEvents :: (Show time, Binary time) => Word8 -> Get [MidiTimedEvent time]
getMidiEvents prevStatus = do
  deltaTime <- get
  status <- do
    maybeStatus <- Get.lookAhead Get.getWord8
    if maybeStatus .&. 0x80 == 0
      then return prevStatus
      else Get.getWord8
  event <- MidiTimedEvent deltaTime <$> getMidiEvent status
  (event :) <$> case event of
    MidiTimedEvent _ (MetaEvent EndOfTrack) -> return []
    _                                       -> getMidiEvents status

getChunk :: Binary a => BS.ByteString -> Get a
getChunk ty = do
  ty' <- Get.getByteString 4
  len <- fromIntegral <$> Get.getWord32be
  if ty == ty'
    then Get.isolate len get
    else fail $ show ty'

instance Binary MidiHeader where
  put _ = error "OMG"
  get = MidiHeader <$> get <*> get <*> get

instance (Show time, Binary time) => Binary (MidiTrack time) where
  put _ = error "OMG"
  get = MidiTrack 0 <$> getMidiEvents 0

instance (Show time, Binary time) => Binary (MidiFile time) where
  put _ = error "OMG"
  get = do
    hdr@MidiHeader{..} <- getChunk "MThd"
    MidiFile hdr <$> mapM (\track -> (\x -> x{track}) <$> getChunk "MTrk") [0..ntracks-1]

decodeMidiFile :: LBS.ByteString -> MidiFile DeltaTime
decodeMidiFile bytes =
  Get.runGet (Get.isolate (fromIntegral . LBS.length $ bytes) get) bytes
