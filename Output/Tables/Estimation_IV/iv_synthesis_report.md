# IV Exploration Report

Generated: 2026-05-28 13:02:59

## Reference: PPML without IV
- IPD coefficient: **0.0283** (SE: 0.0177, p: 0.1099)
- N observations: 932,992

## Results by instrument

### IV-1: Spatial lag (exp neighbors)
- IPD coefficient: **0.1011** (SE: 0.0370, p: 0.0062)
- Sign: **POS** (significant at 5%)
- First-stage F: 11237.1 (OK)
- Endogeneity test (v_hat p): 0.0186 -> ENDOGENEITY
- Corr(instrument, IPD): 0.769
- N: 752,354

### IV-2: Spatial lag symmetric
- IPD coefficient: **0.0432** (SE: 0.0336, p: 0.1989)
- Sign: **POS** (not significant)
- First-stage F: 27687.4 (OK)
- Endogeneity test (v_hat p): 0.1110 -> No endogeneity
- Corr(instrument, IPD): 0.862
- N: 606,658

### IV-3: Geo x post_2014
- IPD coefficient: **-0.4188** (SE: 1.1264, p: 0.7100)
- Sign: **NEG** (not significant)
- First-stage F: 15.5 (OK)
- Endogeneity test (v_hat p): 0.6907 -> No endogeneity
- Corr(instrument, IPD): 0.033
- N: 932,992

### IV-3: Geo x post_2018
- IPD coefficient: **0.0272** (SE: 0.5686, p: 0.9618)
- Sign: **POS** (not significant)
- First-stage F: 66.6 (OK)
- Endogeneity test (v_hat p): 0.9985 -> No endogeneity
- Corr(instrument, IPD): 0.008
- N: 932,992

### IV-3: Geo x post_2022
- IPD coefficient: **-0.0615** (SE: 0.3869, p: 0.8737)
- Sign: **NEG** (not significant)
- First-stage F: 141.5 (OK)
- Endogeneity test (v_hat p): 0.8152 -> No endogeneity
- Corr(instrument, IPD): -0.008
- N: 932,992

### IV-3: Geo x 3 events (joint)
- IPD coefficient: **0.0217** (SE: 0.4031, p: 0.9571)
- Sign: **POS** (not significant)
- First-stage F: 69.7 (OK)
- Endogeneity test (v_hat p): 0.9869 -> No endogeneity
- Corr(instrument, IPD): n/a
- N: 932,992

### IV-4: Leave-one-out mean IPD
- IPD coefficient: **0.0497** (SE: 0.0176, p: 0.0047)
- Sign: **POS** (significant at 5%)
- First-stage F: 9956408.5 (OK)
- Endogeneity test (v_hat p): 0.0000 -> ENDOGENEITY
- Corr(instrument, IPD): 0.614
- N: 932,992

### IV-5: GDP per capita distance
- IPD coefficient: **-0.3012** (SE: 0.6177, p: 0.6257)
- Sign: **NEG** (not significant)
- First-stage F: 240.2 (OK)
- Endogeneity test (v_hat p): 0.5954 -> No endogeneity
- Corr(instrument, IPD): 0.231
- N: 898,487

### IV-6: Alignment lag 2 (ideal pts)
- IPD coefficient: **0.1244** (SE: 0.0391, p: 0.0015)
- Sign: **POS** (significant at 5%)
- First-stage F: 28406.4 (OK)
- Endogeneity test (v_hat p): 0.0000 -> ENDOGENEITY
- Corr(instrument, IPD): 0.896
- N: 864,035

## Summary statistics
- Total IV strategies tested: 9
- Negative IPD coefficient: 3 / 9
- Positive IPD coefficient: 6 / 9
- Significant at 5%: 3 / 9

### Alternative-source IVs only
- Alternative-source IVs tested: 8
- Negative: 3
- Positive: 5
- Significant at 5%: 2

## Interpretation guidance
- If most alternative-source IVs give NEGATIVE coefficients:
  the PPML negative result is supported; endogeneity bias is small.
- If most alternative-source IVs give POSITIVE coefficients:
  the positive sign from ideal-points-based IVs is likely real,
  OR all instruments share an unobserved confound.
- If results are MIXED across IV types:
  IV identification is inconclusive for this question;
  rely on three-way FE PPML + event-study evidence.
