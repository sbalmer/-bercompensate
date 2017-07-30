module Main where

import System.Environment

import Data.WAVE

type Frame = [Double]

main = do
	(inFile:outFile:[]) <- getArgs
	inWave <- getWAVEFile inFile
	putWAVEFile outFile (übercompensate inWave)

übercompensate :: WAVE -> WAVE
übercompensate inWave = 
	let
		inHeader = waveHeader inWave
		inFrames :: [Frame]
		inFrames = map (map sampleToDouble) (waveSamples inWave)
		
		flipper :: (Double, Double) -> Double
		flipper (last, current) =
			if
				 abs (current - last) >= 1
			then
				(-1) * signum current * (2 - abs current)
			else 
				current

		findAbsMax currentFrame (lastFrame, maxSample) = 
			let 
				compensated = map flipper (zip lastFrame currentFrame)
				currentMax :: Double
				currentMax = maximum $ map abs compensated
			in
				(compensated, max maxSample currentMax)

		maxSample :: Double
		maxSample = snd $ foldr findAbsMax (head inFrames, 0) inFrames

		
		compensateFrame last [] = []
		compensateFrame last (current:rest) =
			let
				compensated = map flipper (zip last current)
			in
				compensated : compensateFrame compensated rest

		compensatedFrames = compensateFrame (head inFrames) inFrames

		normalization = (/maxSample)
	in
		WAVE { waveHeader = inHeader, waveSamples = map (map (doubleToSample . normalization)) inFrames }

