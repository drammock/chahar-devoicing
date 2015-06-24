# Analysis of Chahar wordlist recordings
This repo contains data, scripts, and write-up for analysis of a corpus of wordlist recordings of Chahar Mongolian, created by Yurong.

## Analysis flow
1. The raw data are converted to long format by `preprocessing/clean-raw-data.R`; the output of that script can be explored interactively with `preprocessing/explore-cleaned-data.R`.
2. The cleaned, long-format data was hand-corrected, resulting in the file `clean_cons_env_zh_corrected.tsv` which is the basis of all subsequent analyses. At present there is some additional data cleaning that happens at the beginning of the next script in the analysis flow (`analyze-consonant-environment.R`).
3. Analysis of the factors influencing vowel deletion is handled in `analyze-consonant-environment.R`, which:
    - parses the columns showing preceding and following consonants into additional (binary) columns indicating natural classes
    - generates some mosaic plots
    - runs some cumulative-link mixed models to determine which aspects of the consontant environment are predictive of devoicing/deletion.
