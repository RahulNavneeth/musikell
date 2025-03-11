import Data.ByteString qualified as B
import Data.ByteString.Builder as BB
import Data.ByteString.Lazy (toStrict)

import System.Process (runCommand)
import Text.Printf (printf)

-- A Bb B C C# D D# E F F# G  G#
-- 1 02 3 4 05 6 07 8 9 10 11 12

type Seconds = Float
type Hz = Float
type RadS = Float
type Pulse = Float

semitone :: Float
semitone = 1.05946

sampleRate :: Hz
sampleRate = 48000

amplitude :: Float
amplitude = 0.5

outputFilePath :: FilePath
outputFilePath = "output.bin"

frequency :: Hz -> Float -> [Pulse]
frequency hz interval = (* amplitude) . sin . (* angular_velocity) <$> [0.0 .. sampleRate * interval]
  where
    angular_velocity :: RadS = frequency * 2 * 3.14 / sampleRate
    frequency :: Hz = hz -- A4 (pitch standard) (step)

wave :: [Pulse]
wave = concat [frequency 440.0 1, frequency 493.880416304 0.5, frequency 523.2465458574 1.0, frequency 587.3209588584 0.5, frequency 659.2416356024 1, frequency 698.4401432553 0.5, frequency 783.9679743508 1]

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
    return ()
