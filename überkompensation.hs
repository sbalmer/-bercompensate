module Main where

import System.Environment

import Data.WAVE

type Frame = [Double]

main = do
	(inFile:outFile:[]) <- getArgs
	inWave <- getWAVEFile inFile
	putWAVEFile outFile (übercompensate inWave)

übercompensate inWave = 
	let
		inHeader = waveHeader inWave
		inFrames :: [Frame]
		inFrames = map (map sampleToDouble) (waveSamples inWave)
		
		flipper :: (Double, Double) -> Double
		flipper (last, current) =
			if abs (current - last) >= 1
			then (-1) * signum current * (2 - abs current)
			else current
		
		compensateFrame last [] = []
		compensateFrame last (current:rest) =
			let compensated = map flipper (zip last current)
			in compensated : compensateFrame compensated rest

		compensatedFrames = compensateFrame (head inFrames) inFrames

		maxSample = maximum $ map maximum compensatedFrames
		normalization = (/maxSample)

		outFrames = map (map (doubleToSample . normalization)) compensatedFrames
	in
		WAVE { waveHeader = inHeader, waveSamples = outFrames }

