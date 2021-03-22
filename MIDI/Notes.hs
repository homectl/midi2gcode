{-# LANGUAGE NamedFieldPuns #-}
module MIDI.Notes where

import           Data.List   (mapAccumL)
import qualified Data.List   as List
import           Data.Maybe  (fromMaybe, listToMaybe)
import           MIDI.Parser (AbsTime, Channel (..), ChannelEvent (..), Int14,
                              MetaEvent (..), MidiEvent (..), MidiFile (..),
                              MidiTimedEvent (..), MidiTrack (..), Note (..),
                              Tempo (..), Velocity (..))

data NoteEvent = NoteEvent
  { absolute :: AbsTime
  , on       :: Bool
  , noteNo   :: Note
  , velocity :: Velocity
  , tempo    :: Tempo
  , channel  :: Channel
  , bend     :: Int14
  }
  deriving (Show, Ord, Eq)

type LastNote = [(Channel, NoteEvent)]
type LastBend = [(Channel, Int14)]
type State = (Tempo, LastNote, LastBend)

insert :: Eq k => k -> [(k, v)] -> v -> [(k, v)]
insert k m v = (k, v) : filter (\(k', _) -> k' /= k) m

parseEvent :: State -> MidiTimedEvent AbsTime -> (State, [NoteEvent])
parseEvent (_, lastNote, lastBend) (MidiTimedEvent _ (MetaEvent (TempoChange nextTempo))) =
  ((nextTempo, lastNote, lastBend), [])
parseEvent state (MidiTimedEvent _ (MetaEvent _)) =
  (state, [])
parseEvent state (MidiTimedEvent _ (SysExEvent _)) =
  (state, [])
parseEvent (curTempo, lastNote, lastBend) (MidiTimedEvent t (ChannelEvent ch evt)) =
  let (lastBend', note) = parseChannelEvent evt in
  ((curTempo, maybe lastNote (insert ch lastNote) $ listToMaybe $ reverse note, lastBend'), note)
  where
    parseChannelEvent :: ChannelEvent -> (LastBend, [NoteEvent])
    parseChannelEvent (NoteOff n v) = ((,) lastBend . (:[])) $ NoteEvent
      { absolute = t
      , on = False
      , noteNo = n
      , velocity = v
      , tempo = curTempo
      , channel = ch
      , bend = fromMaybe 0 . List.lookup ch $ lastBend
      }
    parseChannelEvent (NoteOn n (Velocity 0)) = ((,) lastBend . (:[])) $ NoteEvent
      { absolute = t
      , on = False
      , noteNo = n
      , velocity = Velocity 0
      , tempo = curTempo
      , channel = ch
      , bend = fromMaybe 0 . List.lookup ch $ lastBend
      }
    parseChannelEvent (NoteOn n v) = ((,) lastBend . (:[])) $ NoteEvent
      { absolute = t
      , on = True
      , noteNo = n
      , velocity = v
      , tempo = curTempo
      , channel = ch
      , bend = fromMaybe 0 . List.lookup ch $ lastBend
      }
    parseChannelEvent (PitchBend bend) =
      let
        lastNote' = case (\x -> x{absolute=t}) <$> lookup ch lastNote of
          Nothing                  -> []
          Just NoteEvent{on=False} -> []
          Just x                   -> [x{on=False, velocity=Velocity 0}, x{bend}]
      in
      (insert ch lastBend bend, lastNote')
    parseChannelEvent Controller{} = (lastBend, [])
    parseChannelEvent ProgramChange{} = (lastBend, [])
    parseChannelEvent ChannelPressure{} = (lastBend, [])
    parseChannelEvent chevt = error $ "not implemented: " ++ show chevt


parseMidiEvents :: [MidiTimedEvent AbsTime] -> [NoteEvent]
parseMidiEvents = List.sortOn absolute . concat . snd . mapAccumL parseEvent (480000, [], [])


getEvents :: MidiFile time -> [MidiTimedEvent time]
getEvents (MidiFile _ tracks) = concatMap (\(MidiTrack _ evts) -> evts) tracks
