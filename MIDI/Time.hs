{-# LANGUAGE NamedFieldPuns #-}
module MIDI.Time where

import           MIDI.Parser (AbsTime (..), DeltaTime (..), MidiFile (..),
                              MidiHeader (..), MidiTimedEvent (..),
                              MidiTrack (..))

toAbsolute :: MidiFile DeltaTime -> MidiFile AbsTime
toAbsolute (MidiFile hdr@MidiHeader{format=1} tracks) =
  MidiFile hdr $ map mapTrack tracks
  where
    mapTrack (MidiTrack trackNo evts) =
      MidiTrack trackNo . reverse . fst $ foldl mapEvent ([], 0) evts

    mapEvent (evts, t) (MidiTimedEvent (DeltaTime dt) evt) =
      let t' = t + dt in
      (MidiTimedEvent (AbsTime t') evt : evts, t')

toAbsolute (MidiFile MidiHeader{format} _) =
  error $ "Unsupported format: " ++ show format
