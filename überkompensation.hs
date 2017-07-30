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

		findAbsMax :: (Frame, Double) -> Frame -> (Frame, Double)
		findAbsMax (lastFrame, maxSample) currentFrame = 
			let 
				compensated = map flipper (zip lastFrame currentFrame)
				currentMax :: Double
				currentMax = maximum $ map abs compensated
			in
				(compensated, max maxSample currentMax)

		maxSample :: Double
		maxSample = snd $ foldl findAbsMax (head inFrames, 0) inFrames

		compensateFrame :: Frame -> (Frame, [Frame]) -> (Frame, [Frame])
		compensateFrame currentFrame (lastFrame, normalizedFrames) =
			let
				compensated = map flipper (zip lastFrame currentFrame)
			in
				(compensated, compensated : normalizedFrames)

		compensatedFrames = snd $ foldr compensateFrame (head inFrames, []) inFrames

		normalization = (/maxSample)
	in
		WAVE { waveHeader = inHeader, waveSamples = map (map (doubleToSample . normalization)) compensatedFrames }

