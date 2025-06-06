{-# LANGUAGE ImportQualifiedPost #-}

import Data.ByteString qualified as B
import Data.ByteString.Builder as BB
import Data.ByteString.Lazy (toStrict)

import Data.List
import System.Process (runCommand)
import Text.Printf (printf)

import Foreign.C (CSize (CSize))
import Foreign.Marshal.Array (newArray)

-- A Bb B C C# D D# E F F# G  G#
-- 1 02 3 4 05 6 07 8 9 10 11 12

type Seconds = Float
type Hz = Float
type RadS = Float
type Pulse = Float

semitone :: Hz
semitone = 26.16

pitchStandard :: Hz
pitchStandard = 440.0

sampleRate :: Hz
sampleRate = 48000

amplitude :: Float
amplitude = 0.5

bpm :: Float
bpm = 120.0

beatDuration :: Float
beatDuration = 60.0 / bpm

outputFilePath :: FilePath
outputFilePath = "out/output.bin"

-- SOURCE : https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Float -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

frequency :: Hz -> Float -> [Pulse]
frequency hz interval = (* amplitude) <$> zipWith3 (\x y z -> x * y * z) release attack raw
  where
    angular_velocity = frequency * 2 * 3.14 / sampleRate
    frequency = hz
    attack = map (min 1.0) [0.0, 0.001 ..]
    raw = sin . (* angular_velocity) <$> [0.0 .. sampleRate * interval]
    release = reverse $ take (length raw) attack

note :: Float -> Float -> [Pulse]
note semitone beat = frequency (f semitone) (beat * beatDuration)

wave :: [Pulse]
-- wave = concat [ add tunes here ]
wave = concat [note i 1 | i <- [1]]

valueToByteString :: [Float] -> B.ByteString
valueToByteString value = toStrict $ BB.toLazyByteString $ foldMap BB.floatLE value

save :: String -> IO ()
save filePath = do
    B.writeFile filePath $ valueToByteString wave
    putStrLn "Wave saved successfully!!!"

play :: IO ()
play = do
    save outputFilePath
    _ <- runCommand $ printf "ffplay -f f32le -ar 48000 %s" outputFilePath
    putStrLn "Program executed succesfully!!!"

main :: IO ()
main = play
