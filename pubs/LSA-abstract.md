# How corpus created

## Materials
- word list containing every attested CVC combination in the initial syllable
- ? number of syllables per word: 2-3?
- first syllable always short vowel
- second syllable often short? sometimes long?
- ? +dʒ (mʊdʒʊ:t)

## Recording
- 3 male, 3 female, all Chahar dialect (standard form of M. in IMAR)
- subj. had list of words (grouped by initial C), read in sentence frame
- 3 repetitions (listwise)
- 16 kHz sampling rate (what was bit depth?)

# Analysis
- vowel in first syllable of each target word categorized as “deleted”, “devoiced”, or “not reduced”
- ??? how classified?  looked at spectrogram and...
- ? vowel duration measured?

## factors examined
- vowel identity / quality
- talker
- word
- preceding/following consonant obstruent (C voiced == C not obstruent)
- preceding/following consonant aspirated
- morpheme boundary between vowel and following consonant

## stats
- clmm: cumulative link mixed model (ordinal)
    - reduction ~ obs.bef + obs.aft + asp.bef + asp.aft + morph.aft + ...

# Notes
mosaic plots:
- reduction
- reduction x speaker
- reduction x vowel
- reduction x asp.bef
- reduction x asp.aft
- reduction x repetition
