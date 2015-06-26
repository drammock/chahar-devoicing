# Predicting vowel devoicing and deletion in Chahar Mongolian
# Instability of initial-syllable vowels in Chahar Mongolian
# Devoicing and deletion of allegedly stressed vowels in Chahar Mongolian
<!-- Choose a title that clearly indicates the topic of the research and is not more than one 7-inch typed line. Note that your choice of title has considerable influence on how your paper or poster is grouped with others to form thematically coherent sessions. A clear relationship between the title and content of your abstract will help ensure it is assigned to an appropriate session, should it be selected for presentation. -->

<!-- State the problem or research question raised by prior work, with specific reference to relevant prior research. -->
The Mongolian languages (ISO 639-3: `khk`, `mvf`; macrolanguage `mon`) are vowel-harmony languages, involving some form of pharyngeal/tongue-root harmony and rounding harmony in all dialects [@Svantesson]. Thus the quality of any single vowel in a polysyllabic word is fully or partially predictable from other vowels in the word, and the information load on a given vowel is diminished. Possibly in consequence of this, vowel devoicing and deletion are frequently observable in Mongolian conversational speech, and also occur in more formal registers.

<!-- Debate in Mongolist linguistics on the placement of stress -- initial syllables supposedly stressed -- why would stressed vowels be deleted? @Svantesson, @Walker, Yurong may be able to provide more citations -->

<!-- State the main point or argument of the proposed presentation. -->
In this study, we analyze initial-syllable vowel devoicing and deletion in a wordlist corpus encompassing all attested C₁VC₂ sequences in the initial syllable in Chahar Mongolian (21558 tokens comprising 845 initial syllable types and 1204 unique words). Talkers are 3 male and 3 female native speakers of Chahar<!-- (the prestige dialect of Mongolian as spoken in the People’s Republic of China)-->. Words were read in a sentence frame, and the entire word list was recorded three times for each talker.

<!-- Regardless of the subfield, cite sufficient data, and explain why and how they support the main point or argument. When examples are in languages other than English, provide word-by-word glosses and underline the portions of the examples which are critical to the argument. Explain abbreviations at their first occurrence. -->

<!-- If your research presents the results of experiments, but collection of results is not yet complete, then report what results you've already obtained in sufficient detail that your abstract may be evaluated. Also indicate explicitly the nature of the experimental design and the specific hypothesis tested. -->

<!-- State the relevance of your ideas to past work or to the future development of the field. Describe analyses in as much detail as possible. Avoid saying in effect "a solution to this problem will be presented". If you are taking a stand on a controversial issue, summarize the arguments that led you to your position. -->
<!-- how is vowel reduction defined/measured -->Probability of vowel reduction is modeled using cumulative-link mixed effects regression, with fixed effects for flanking consonant manner, flanking consonant aspiration, presence of a morpheme boundary between V and C₂, vowel harmony class, and repetition number. Random effects are modeled for talker, vowel, and word.

Results show aspiration of the following consonant to be the best predictor of vowel reduction (consistent with the fact that non-initial stops and affricates are pre-aspirated in some dialects of Mongolian [@SvantessonKarlsson]). Aspiration of the preceding consonant, as well as presence of fricatives either preceding or following, were also good predictors of vowel reduction.
<!-- pre-aspiration citation? -->

<!-- State the contribution to linguistic research made by the analysis. -->
Results are discussed in light of potentially conflicting forces: articulatory ease [e.g., @Lindblom] and phonetic cue preservation [@Wright]. In particular, persistence of vowels flanked by sonorant consonants is explained by appeal to articulatory ease, whereas reduction of vowels flanked by fricative consonants is explained by appeal to the robustness of fricative-internal cues. The high degree of reduction around stop consonants is rather more difficult to account for, due to the relative fragility of stop-consonant cues (which would predict preservation of the vowel in order to perserve formant transition cues).

Change in progress?  New historical development (moving toward initial C clusters?)

<!-- While citation in the text of the relevant literature is essential, a separate list of references at the end of the abstract is generally unnecessary. -->

<!-- When you submit your abstract to the website, please identify a primary field on the basis of the abstract's general topic area, and use the secondary field to indicate methodology or a secondary topic. For example, a paper or poster about the production of relative clauses could have "syntax" as its primary field, and "psycholinguistics" as the secondary field. This will make it easier to assign your abstract to appropriate reviewers. -->
Phonetics
Laboratory phonology?

<!-- END ABSTRACT. NOTES/QUESTIONS FOLLOW. -->

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
- how classified?  looked at spectrogram and listened, no audible/visible vowel duration = deleted; visible vowel duration but no formants = devoiced

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
