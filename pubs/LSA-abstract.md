
The Mongolian languages (ISO 639-3: `khk`, `mvf`; macrolanguage `mon`) are vowel-harmony languages, involving some form of pharyngeal/tongue-root harmony in all dialects. Rounding harmony is also present in many/most dialects as well. Consequently, the information load on any single vowel in a polysyllabic word is somewhat diminished (its quality being predictable from other vowels in the word). Vowel devoicing and deletion are frequently observable in conversational speech, and occur even in more formal registers.

In this study, we analyze initial-syllable vowel devoicing and deletion in a wordlist corpus encompassing all attested C₁VC₂ sequences in the initial syllable (845 types, 21558 tokens). Talkers are 3 male and 3 female native speakers of Chahar Mongolian (the prestige dialect of Mongolian as spoken in the People’s Republic of China). Words are read in a sentence frame, and the entire word list was recorded three times for each talker.

Probability of vowel reduction is modeled using cumulative-link mixed effects regression, with fixed effects for flanking consonant manner, flanking consonant aspiration, presence of morpheme boundaries between V and C₂, vowel pharyngeality, and repetition number, with random effects for talker, vowel, and word. Results show aspiration of the following consonant to be the best predictor of vowel reduction (consistent with the fact that coda stops and affricates are pre-aspirated). Aspiration of the preceding consonant, as well as fricatives either preceding or following, were also good predictors of vowel reduction.

Results are discussed in light of:
- articulatory ease
- information load
- ???

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
