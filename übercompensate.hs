module Main where

import System.Environment

import Data.WAVE

import Data.List

type Sample = Double
type Frame = [Sample]
type Stream = [Sample]
type Window = [Sample] 




energyRec :: Double -> Sample -> Window -> Double
energyRec en last [] = en
energyRec en last (x:xs) = energyRec (en + abs (last - x)) x xs 

energy :: Window -> Double
energy [] = 0
energy (x:xs) = energyRec 0 x xs


main = do
	(inFile:outFile:[]) <- getArgs
	inWave <- getWAVEFile inFile
	putWAVEFile outFile (übercompensate inWave)

übercompensate inWave = 
	let
		inHeader = waveHeader inWave
		inFrames :: [Frame]
		inFrames = map (map sampleToDouble) (waveSamples inWave)
		inStreams = transpose inFrames

		processSample :: Window -> [Window] -> Double -> (Window, Window, [Window])
		processSample base candidates sample = 
			let
				nextBase = sample:base
				baseEnergy :: Double
				baseEnergy = energy nextBase
				terminationCandidates = map (sample : ) candidates
				bestTermination = sort $ zip (map energy terminationCandidates) terminationCandidates
				invertedSample = signum sample * (abs sample - 2)
				-- Assume that overflows don't last long and discard long candidates
				sufficientlyShortCandidates :: [Window]
				sufficientlyShortCandidates = filter ((<10).length) candidates
				nextCandidates :: [Window]
				nextCandidates = (invertedSample : base) : map (invertedSample : ) sufficientlyShortCandidates 
				-- Discard candidates that are much worse than the base case. This should keep the candidates list empty most of the time.
				goodCandidates = filter ((< baseEnergy) . (+ 0.1) . energy) nextCandidates
			in
				case bestTermination of
					-- correct overflow: if we find a run of inversions that is better than the base, take that
					(en, bestTermCandidate):_ | en < baseEnergy -> (reverse $ tail bestTermCandidate, [sample], [])
					_ -> case goodCandidates of
						-- base case: there are no inversion candidates; output base and the current sample becomes the new base
						[] -> (reverse base, [sample], [])
						-- continue with candidates: inconclusive because no candidate terminated better than base
						newCandidates -> ([], nextBase, newCandidates)
					
		processStream :: Window -> [Window] -> Stream -> Stream
		processStream base _ [] = base
		processStream base candidates (current:rest) =
			output ++ processStream nextBase nextCandidates rest
				where (output, nextBase, nextCandidates) = processSample base candidates current

		reflowedStreams :: [Stream]
		reflowedStreams = map (\(first:rest) -> processStream [first] [] rest) inStreams
		reflowedFrames = transpose reflowedStreams
	
		maxSample = foldr1 max $ foldr1 (max.(map abs)) reflowedFrames 
		-- maxSample = 1.1
		normalization = (/maxSample)

		outFrames = map (map (doubleToSample . normalization)) reflowedFrames
	in
		WAVE { waveHeader = inHeader, waveSamples = outFrames }

