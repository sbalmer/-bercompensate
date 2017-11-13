#!/usr/bin/env python3
""" Compensate inverted signals from overflows in WAV files

An run of max 100 samples is inverted if their energy (cumulative difference)
is lower in the inverted state.

Usage:
	übercompensate <in> [<out>]

"""

import sys
import math
import collections
import numpy as np
from wavefile import WaveReader, WaveWriter
from docopt import docopt

arguments = docopt(__doc__)

maxsamples = 100

def energy(samples):
	last = None
	energy = 0
	for s in samples:
		if last is not None:
			energy += abs(last - s)
		last = s

	return energy

def MinimumEnergyInverter(maxsamples):
	base = []
	candidates = []

	def inverter(sample):
		nonlocal base
		nonlocal candidates
		if sample is None:
			return base

		nextBase = base + [sample]
		baseEnergy = energy(nextBase)

		# Calculate the energy if we terminate inversion
		def terminationEnergy(window):
			return (energy(window + [sample]), window)

		terminationRanking = sorted(map(terminationEnergy, candidates))
		bestTermination = next(iter(terminationRanking), None)
		if bestTermination and (bestTermination[0] < (baseEnergy - 0.5)):
			# Here we correct for overflow: if we find a run of inversions
			# that is better than the base
			base = [sample] # Sample becomes start of the new base
			candidates = []
			return bestTermination[1]

		invertedSample = math.copysign(1, sample) * (abs(sample) - 2)

		nextCandidates = []
		for candidate in candidates + [base]:
			if (len(candidate) < maxsamples):
				nextCandidates.append(candidate + [invertedSample])

		# Discard candidates that are not much better than base. Here
		# we discard trivial candidates.
		goodCandidates = list(filter(lambda c: energy(c) + 0.1 < baseEnergy, nextCandidates))
		if len(goodCandidates) > 0:
			# Inconclusive result: We have good candidates.
			# So we don't return any samples yet
			candidates = goodCandidates
			base = nextBase
			return []
		else:
			candidates = []
			samples = base
			base = [sample]
			return samples

	return inverter


def applyFilter(stream, proc):

	# The filter function is stateful, so we need one per channel
	filters = [MinimumEnergyInverter(maxsamples) for _ in range(stream.channels)]

	def addWindows(windows):
		for ch in range(stream.channels):
			for sample in windows[ch]:
				proc(filters[ch](sample), ch)

	for windows in stream.read_iter(size=512):
		addWindows(windows)

	# Tell filters to clear their buffers
	addWindows([[None]] * stream.channels)

def minuteMarker(stream):
	c = 0
	spm = stream.samplerate * 60
	dur = stream.frames / spm

	def mark(add):
		nonlocal c
		c += add
		if c % 1000 < (c - add) % 1000:
			sys.stderr.write("{:.3f}min of {:.3f}min \r".format(c / spm, dur))

	return mark


with WaveReader(arguments['<in>']) as inWav:
	filters = [MinimumEnergyInverter(maxsamples) for _ in range(inWav.channels)]
	framenr = [0] * inWav.channels
	absmax = 0
	mark = minuteMarker(inWav)
	def findGain(filtered, ch):
		global framenr
		global absmax
		for sample in filtered:
			abssample = abs(sample)
			absmax = max(abssample, absmax)
			if abssample > 1.0:
				sys.stderr.write("channel {} frame {}: overflow {:.5f}\n".format(ch, framenr[ch], abssample))
			framenr[ch] += 1

		if ch == 0:
			mark(len(filtered))

	applyFilter(inWav, findGain)

	sys.stderr.write("\nMax sample {:.5f}.\n".format(absmax))

out = arguments.get('<out>', None)
if out:
	with WaveReader(arguments['<in>']) as inWav:
		with WaveWriter(
			out,
				channels=inWav.channels,
				samplerate=inWav.samplerate,
			) as outWav:
			if inWav.metadata.title:
				outWav.metadata.title = inWav.metadata.title
			if inWav.metadata.artist:
				outWav.metadata.artist = inWav.metadata.artist

			gain = 1 / absmax

			# The filter functions return samples in bursts, so we need to
			# buffer them.
			bufs = [[] for _ in range(inWav.channels)]

			mark = minuteMarker(inWav)

			def flush():
				complete = min(map(len, bufs))
				if complete:
					windows = []
					for buf in bufs:
						windows.append(buf[:complete])
						del buf[:complete]
					outWav.write(np.array(windows))

			def writeCorrected(corrected, ch):
				global bufs
				bufs[ch].extend(map(lambda s: s*gain, corrected))
				if len(bufs[ch]) > 512:
					flush()

				if ch == 0:
					mark(len(corrected))

			applyFilter(inWav, writeCorrected)
			flush()
