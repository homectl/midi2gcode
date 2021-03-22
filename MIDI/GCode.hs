{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
module MIDI.GCode where

import qualified Data.ByteString.Lazy as LBS
import           Data.Int             (Int8)
import           Data.List            (mapAccumL)
import qualified Data.List            as List
import           Data.Maybe           (catMaybes)
import           Data.Word            (Word16)
import           Debug.Trace          (trace)
import           MIDI.Notes           (NoteEvent (..), getEvents,
                                       parseMidiEvents)
import           MIDI.Parser          (MidiFile (MidiFile),
                                       MidiHeader (MidiHeader, tickdiv),
                                       Note (Note), decodeMidiFile)
import           MIDI.Time            (toAbsolute)
import           Numeric              (showFFloat)

data Axis = X | Y | Z | A | B | C
  deriving (Show, Eq)

newtype PPU = PPU Int
  deriving (Show, Integral, Real, Enum, Num, Ord, Eq)

data AxisParameters = AxisParameters
  { ppu     :: PPU
  , safeMin :: Position
  , safeMax :: Position
  }

data Machine = Machine
  { name :: String
  , axes :: [(Axis, AxisParameters)]
  }


newtype Duration = Duration Double
  deriving (Show)

data ActiveNotes a = ActiveNotes Duration a
  deriving (Show, Functor)

data BentNote = BentNote Note Double
  deriving (Show, Eq, Ord)

newtype Freq = Freq Double
  deriving (Show)

newtype Feed = Feed Double
  deriving (Show)

newtype Distance = Distance Double
  deriving (Show)

newtype Position = Position Double
  deriving (Show)

newtype Direction = Direction Double
  deriving (Show)


tempoScalingFactor :: Double
tempoScalingFactor = 1000000.0

computeActiveNotes
  :: Word16
  -> [NoteEvent]
  -> [ActiveNotes [BentNote]]
computeActiveNotes division = catMaybes . snd . mapAccumL processNote (0, [])
  where
    processNote (lastTime, activeNotes) NoteEvent{..} =
      let
        bentNote = BentNote noteNo (fromIntegral bend / 4096)

        -- Get the duration in seconds from the MIDI values in divisions,
        -- at the given tempo.
        duration =
          fromIntegral (absolute - lastTime) / fromIntegral division * (fromIntegral tempo / tempoScalingFactor)

        -- When it's time to produce a new command, duration will be non-zero.
        change = if duration > 0
          then Just $ ActiveNotes (Duration duration) activeNotes
          else Nothing

        -- Add/remove active notes based on the on/off event.
        activeNotes' = if on
          then if bentNote `elem` activeNotes
            -- Avoid turning on the same note twice.
            then activeNotes
            else reverse . List.sort $ bentNote : activeNotes
          else List.delete bentNote activeNotes
      in
      ((absolute, activeNotes'), change)


-- MIDI note 69     = A4(440Hz)
-- 2^(69-69) / 12 * 440 = A4 440Hz
-- 2^(64-69) / 12 * 440 = E4 329.627Hz
computeFreq
  :: ActiveNotes [BentNote]
  -> ActiveNotes [Freq]
computeFreq = fmap . map $ noteToFreq
  where
    noteToFreq (BentNote (Note note) bend) =
      Freq $ (2 ** ((fromIntegral (note - 69) + bend) / 12.0)) * 440


-- Here is where we need smart per-axis feed conversions
-- to enable use of all axes. We use the machine description,
-- so we end up with potentially fewer active notes in the
-- output than in the input frequency lists.
--
-- Feed rate is expressed in mm / minutes so 60 times
-- scaling factor is required.
computeFeed
  :: Machine
  -> ActiveNotes [Freq]
  -> ActiveNotes [(Axis, Feed)]
computeFeed Machine{axes} = fmap . zipWith (curry freqToFeed) $ axes
  where
    freqToFeed ((axis, params), Freq freq) =
      (axis, Feed $ (freq * 60.0) / fromIntegral (ppu params))


data AxisMovement = AxisMovement Axis Feed Distance
  deriving (Show)

-- Get the actual relative distance travelled per axis in mm.
computeAxisMovement
  :: ActiveNotes [(Axis, Feed)]
  -> ActiveNotes [AxisMovement]
computeAxisMovement (ActiveNotes dur notes) =
  ActiveNotes dur $ map (feedToDistance dur) notes
  where
    feedToDistance (Duration duration) (a, Feed feed) =
      AxisMovement a (Feed feed) (Distance $ (feed * duration) / 60.0)


data CombinedMovement a
  = CombinedMovement [a] Feed
  | Rest Duration
  deriving (Show)

-- Compute relative movement and combined feed rate for all axes together.
computeCombinedMovement
  :: ActiveNotes [AxisMovement]
  -> CombinedMovement Distance
computeCombinedMovement = combineFeeds
  where
    combineFeeds (ActiveNotes duration relMoves) =
      let
        combinedFeed = sqrt . sum . map ((** 2) . feed) $ relMoves
        axisTravel = map (\(AxisMovement _ _ d) -> d) relMoves
      in
      if combinedFeed > 0
        then CombinedMovement axisTravel (Feed combinedFeed)
        else Rest duration

    feed (AxisMovement _ (Feed f) _) = f


-- Compute absolute positioning for the axes, considering the safe envelope we
-- can move within.
computePositioning
  :: Machine
  -> [CombinedMovement Distance]
  -> [CombinedMovement Position]
computePositioning Machine{axes} = snd . mapAccumL distToPos initPos
  where
    initPos = map (const (Position 0, Direction 1)) axes

    distToPos
      :: [(Position, Direction)]
      -> CombinedMovement Distance
      -> ([(Position, Direction)], CombinedMovement Position)
    distToPos mach (Rest duration) = (mach, Rest duration)
    distToPos mach (CombinedMovement relMoves feed) =
      let mach' = zipWith3 nextMove axes relMoves mach in
      (mach' ++ drop (length mach') mach, CombinedMovement (map fst mach') feed)

    nextMove
      :: (Axis, AxisParameters)
      -> Distance
      -> (Position, Direction)
      -> (Position, Direction)
    nextMove (_, params) dist (pos, dir) =
      let
        -- Turn around BEFORE crossing the limits of the
        -- safe working envelope.
        dir' =
          if reachedLimit params pos dist dir
            then invert dir
            else dir
        pos' = advance pos dist dir'
      in
      (pos', dir')

    invert :: Direction -> Direction
    invert (Direction dir) = Direction (-dir)

    advance :: Position -> Distance -> Direction -> Position
    advance (Position pos) (Distance dist) (Direction dir) =
      Position (pos + dist * dir)

    reachedLimit :: AxisParameters -> Position -> Distance -> Direction -> Bool
    reachedLimit AxisParameters{safeMin=Position safeMin, safeMax=Position safeMax}
        (Position pos) (Distance dist) (Direction dir)
      -- Movement in the current direction is within safe limits,
      | pos + dist * dir < safeMax && pos + dist * dir > safeMin = False
      -- Movement in the current direction violates maximum safe
      -- value, but would be safe if the direction is reversed
      | pos + dist * dir >= safeMax && pos - dist * dir > safeMin = True
      -- Movement in the current direction violates minimum safe
      -- value, but would be safe if the direction is reversed
      | pos + dist * dir <= safeMin && pos - dist * dir < safeMax = True
      -- Movement in *either* direction violates the safe working
      -- envelope, so abort.
      | otherwise = error $ unlines
          [ "\n*** ERROR ***"
          , "The current movement cannot be completed within the safe working envelope of\n"
          , "your machine. Turn on the --verbose option to see which MIDI data caused the\n"
          , "problem and adjust the MIDI file (or your safety limits if you are confident\n"
          , "you can do that safely). Aborting."
          ]


data GCode
  = G01 [(Axis, Position)] Feed
  | G04 Duration
  deriving (Show)

genGCode :: Machine -> CombinedMovement Position -> GCode
genGCode Machine{axes} = genInsn
  where
    genInsn :: CombinedMovement Position -> GCode
    genInsn (Rest dur) =
      G04 dur
    genInsn (CombinedMovement absMoves feed) =
      G01 (zip (map fst axes) absMoves) feed


gcodeToString :: GCode -> String
gcodeToString (G01 absMoves (Feed feed)) = "G01 " ++ showMoves absMoves ++ " F" ++ showNum feed
gcodeToString (G04 (Duration dur))       = "G04 P" ++ showNum (max (dur - 0.02) 0)

showNum :: RealFloat a => a -> String
showNum n = showFFloat (Just 4) n ""

showMoves :: [(Axis, Position)] -> String
showMoves = unwords . map showMove

showMove :: (Axis, Position) -> [Char]
showMove (axis, Position pos) = show axis ++ showNum pos


wrapGCode :: [String] -> [String]
wrapGCode gcode =
  [ "G21"
  , "G90"
  , "G92 X0 Y0 Z0 A0 B0"
  , "G0 X0 Y0 Z0 A0 B0 F2000.0"
  ] ++ gcode ++
  [ "G0 X10 Y0 Z0 A0 B0"
  ]


traceLines :: Show b => (a -> b) -> [a] -> [a]
traceLines f xs = trace (List.intercalate "\n" . map (show . f) $ xs) xs


midi2gcode :: LBS.ByteString -> [String]
midi2gcode bytes = wrapGCode gcode
  where
    midi@(MidiFile MidiHeader{tickdiv} _) = decodeMidiFile bytes

    gcode =
      filter (/= "G04 P0.0000")
      . map (gcodeToString . genGCode machine)
      . computePositioning machine
      . map (computeCombinedMovement . computeAxisMovement . computeFeed machine . computeFreq)
      . computeActiveNotes tickdiv
      . parseMidiEvents
      . getEvents
      . toAbsolute
      $ midi

    machine = Machine
      { name = "M&P Maker"
      , axes =
          [ (X, AxisParameters (PPU 320) (Position 0) (Position 100))
          , (Y, AxisParameters (PPU 320) (Position 0) (Position 100))
          , (Z, AxisParameters (PPU 400) (Position 0) (Position 10))
          , (A, AxisParameters (PPU 100) (Position 0) (Position 100))
          , (B, AxisParameters (PPU 100) (Position (-16)) (Position 16))
          -- , (C, AxisParameters (PPU 320) (Position 0) (Position 30))
          ]
      }

notesOnly :: ActiveNotes [Note] -> [Int8]
notesOnly (ActiveNotes _ ns) = map unNote ns
  where
    unNote :: Note -> Int8
    unNote (Note n) = n
