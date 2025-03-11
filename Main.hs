import Data.ByteString qualified as B
import Data.ByteString.Builder as BB
import Data.ByteString.Lazy (toStrict)

import Data.List
import System.Process (runCommand)
import Text.Printf (printf)

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
outputFilePath = "output.bin"

-- SOURCE : https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Float -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Float -> Float -> [Pulse]
note semitone beat = frequency (f semitone) (beat * beatDuration)

frequency :: Hz -> Float -> [Pulse]
frequency hz interval = (* amplitude) <$> zipWith3 (\x y z -> x * y * z) release attack raw
  where
    angular_velocity :: RadS = frequency * 2 * 3.14 / sampleRate
    frequency :: Hz = hz -- A4 (pitch standard) (step)
    attack :: [Pulse] = map (min 1.0) [0.0, 0.001 ..]
    raw = sin . (* angular_velocity) <$> [0.0 .. sampleRate * interval]
    release :: [Pulse] = reverse $ take (length raw) attack

wave :: [Pulse]
-- wave = concat [frequency pitchStandard 1, frequency 493.880416304 0.5, frequency 523.2465458574 1.0, frequency 587.3209588584 0.5, frequency 659.2416356024 1, frequency 698.4401432553 0.5, frequency 783.9679743508 1]
wave = concat [note i 1 | i <- [0, 2, 3, 5, 7, 8, 10, 12]]

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
