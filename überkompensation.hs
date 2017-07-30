module Main where

import System.Environment

import Data.WAVE

main = do
	(inFile:outFile) <- getArgs
	übercompensate inFile outFile

übercompensate inPath outPath = do
	inWave <- getWAVEFile inPath
	let inHeader = waveHeader inWave
	let inFrames = map (map sampleToDouble) (waveSamples inWave)

	let flipper (last, current) =
		if
			 abs (current - last) >= 1
		then
			(-1) * sign current * (2 - abs current)
		else 
			current

	let findAbsMax (lastFrame, maxSample) currentFrame = 
		let 
			compensated = map flipper (zip lastFrame currentFrame)
 		in
			(compensated, max(maxSample, maximum $ map abs compensated))

	let maxSample = snd $ foldl findAbsMax (head inFrames, 0) inFrames
	let normalization = (/maxSample)

	let normalizeFrame (lastFrame, normalizedFrames) currentFrame =
		let
			compensated = map flipper (zip lastFrame currentFrame)
		in
			(compensated, map normalization compensated : normalizedFrames)

	let normalizedFrames = snd $ foldr compensateFrame (head inFrames, []) inFrames

	let outFrames = map (map doubleToSample) normalizedFrames

	putWAVEFile outFile outFrames

