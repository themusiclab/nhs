********************************************************************************
** NHS viz processing **********************************************************
** Samuel Mehr 04sep2019 *******************************************************
********************************************************************************
** this script prepares csv output from code at github.com/themusiclab/nhs *****
** for visualizations produced with NHS_viz.R **********************************
********************************************************************************

/* if working in cloned repo, set your directory to /nhs/viz */

********************************
** Nothing required for Fig 0 ** // Illustrations for summary page
********************************

***********
** Fig 1 ** // Ethnography map
***********

** get lat/long
insheet using ../data/nhs/NHSEthnography_Metadata.csv, names clear
keep society latitude longitude

** export clean csv
export delimited using vizData/ethnogMap.csv, replace

****************
** Figs 2 & 3 ** // Ethnography BPCA for 3D scatter & ridgeline plot
****************

** get culture-level summaries
insheet using ../results/ethno_bpca_culture_summary_final.csv, names clear
rename culture id_nhs
destring dim, ignore("q.") replace
drop distr* other*
destring sdest sdcilo sdcihi, force replace
reshape wide meanest-sdcihi, i(id_nhs) j(dim)
sa NHSethno_culturelevel, replace

** get list of sig differences
mer m:m id_nhs using vizData/NHS_cultureNames
collapse (firstnm) meanest* meanpval* sdest* ncites, by(culture)
drop if meanest1==.
gsort -ncites
forval i = 1/3 {
	gen sig`i' = meanpval`i'<.05
	di "sig diff on dim `i'"
	li culture meanest`i' meanpval`i' if meanpval`i'<.05
	}
gen sumsig = sig1+sig2+sig3
ta sumsig
ta sig1
ta sig2
ta sig3

** get list of +/- 1.96 SDs
forval i = 1/3 {
	gen inside`i' = abs(meanest`i')<1.96*sdest`i'
	}
gsort -meanest1

** get obs-level data
insheet using ../results/ethno_bpca_scores_final.csv, names clear

** merge in culture names/ids
mer m:m indx using vizData/NHS_cultureNames
drop if indx==.
drop _merge

** get culture-level stuff
mer m:m id_nhs using NHSethno_culturelevel, nogen

** add ndocs to culture names
tostring ncites, gen(ncites_str)
replace culture = culture+" ("+ncites_str+")"

** double size of dataset for global dist
sa NHS_bpcatemp, replace
append using NHS_bpcatemp, gen(source)
replace culture = "All societies (493)" if source==1
replace id_nhs = "nan" if source==1
replace ncites = 493 if source==1
drop source

** remove tzeltal
drop if culture == "Tzeltal (1)"

** reverse dimensions
foreach v of varlist score* {
	replace `v' = -1*`v'
	}

** remove tempfiles
rm NHS_bpcatemp.dta
rm NHSethno_culturelevel.dta
	
** export clean csv
export delimited using vizData/ethnogBPCA.csv, replace

***********
** Fig 4 ** // Discography map
***********

** get lat/long and song types
insheet using ../data/nhs/NHSDiscography_Metadata.csv, names clear
keep type lat lon

** export clean csv
export delimited using vizData/discogMap.csv, replace

********************************
** Nothing required for Fig 5 ** // World Music Quiz heatmap & LASSO barplots
********************************

** n.b., heatmap was made manually in Google Sheets

************
** Fig 6A ** // Tonality histogram
************

** get multimodality data
insheet using ../results/modality_labels.csv, names clear

** merge in tonality data
mer m:m song using vizData/tonalityRatings
rename multimodal mm

** clean
gen indx = _n
reshape long tonal_pitch, i(indx) j(ratingnum)
drop indx
rename tonal_pitch pitch
drop if strpos(pitch,"ingle")>0
compress

** pitch classes
gen pc = .
replace pc = 0 if pitch=="C"
replace pc = 1 if pitch=="C#"
replace pc = 2 if pitch=="D"
replace pc = 3 if pitch=="D#"
replace pc = 4 if pitch=="E"
replace pc = 5 if pitch=="F"
replace pc = 6 if pitch=="F#"
replace pc = 7 if pitch=="G"
replace pc = 8 if pitch=="G#"
replace pc = 9 if pitch=="A"
replace pc = 10 if pitch=="A#"
replace pc = 11 if pitch=="B"
drop if pc==.

** transpose to modal pitch
egen modalpc = mode(pc), by(song) maxmode
gen transpc = modalpc-pc

** make distance measure
gen newpc = .
replace newpc = transpc if transpc<7&transpc>-7
replace newpc = 12-transpc if transpc>6
replace newpc = -12-transpc if transpc<-6
replace newpc = abs(newpc)

** export clean csv
export delimited using vizData/tonalityHistogram.csv, replace

************
** Fig 6B ** // Tonality scatter
************

** get krumhansl keys
insheet using ../data/nhs/NHSDiscography_TranscriptionFeatures.csv, names clear
keep song key*
sa keys, replace

** start with ratings
insheet using vizData/tonalityHistogram.csv, names clear
collapse (firstnm) modalpc, by(song)
mer m:m song using keys, nogen

** remove tempfile
rm keys.dta

** convert krumhansl keys to no major/minor
foreach v of varlist key* {
	replace `v' = `v'-12 if `v'>11
	}

** export clean csv
export delimited using vizData/tonalityScatter.csv, replace

***********
** Fig 7 ** // Discography BPCA
***********

** get bpca scores
insheet using ../results/disco_bpca_scores.csv, names clear

** cleanup
drop song
rename v1 song

** reverse dim 1 (dim 2 doesn't get reversed)
replace bpca1 = -1*bpca1

** export clean csv
export delimited using vizData/discogBPCA.csv, replace

********************************
** Nothing required for Fig 8 ** // Melody/rhythm bigram log-log plots
********************************

************
** Fig S1 ** // Ethnography BPCA (untrimmed version) ridgeline plot
************

** get culture-level summaries
insheet using ../results/ethno_bpca_culture_summary_robust_final.csv, names clear
rename culture id_nhs
destring dim, ignore("q.") replace
destring sdest sdcilo sdcihi, force replace
reshape wide meanest-sdcihi, i(id_nhs) j(dim)
sa NHSethno_culturelevel, replace

** get list of sig differences
mer m:m id_nhs using vizData/NHS_cultureNames
collapse (firstnm) meanest* meanpval* sdest* ncites, by(culture)
drop if meanest1==.
gsort -ncites
forval i = 1/3 {
	gen sig`i' = meanpval`i'<.05
	di "sig diff on dim `i'"
	li culture meanest`i' meanpval`i' if meanpval`i'<.05
	}
gen sumsig = sig1+sig2+sig3
ta sumsig
ta sig1
ta sig2
ta sig3

** get list of +/- 1.96 SDs
forval i = 1/3 {
	gen inside`i' = abs(meanest`i')<1.96*sdest`i'
	}
gsort -meanest1
ta inside1
ta inside2
ta inside3

** get obs-level data
insheet using ../results/ethno_bpca_scores_robust_final.csv, names clear

** merge in culture names/ids
mer m:m indx using vizData/NHS_cultureNames
drop if indx==.
drop _merge

** get culture-level stuff
mer m:m id_nhs using NHSethno_culturelevel, nogen

** add ndocs to culture names
tostring ncites, gen(ncites_str)
replace culture = culture+" ("+ncites_str+")"

** double size of dataset for global dist
sa NHS_bpcatemp, replace
append using NHS_bpcatemp, gen(source)
replace culture = "All societies (493)" if source==1
replace id_nhs = "nan" if source==1
replace ncites = 493 if source==1
drop source

** remove tzeltal
drop if culture == "Tzeltal (1)"

** reverse dim1
replace score1 = -1*score1

** remove tempfiles
rm NHS_bpcatemp.dta
rm NHSethno_culturelevel.dta

** export clean csv
export delimited using vizData/figS1.csv, replace

************
** Fig S2 ** // Ethnography BPCA (untrimmed version) within-vs-between scatter
************

** get culture level bpca data
insheet using ../results/ethno_bpca_culture_summary_robust_final.csv, names clear

** cleanup
rename culture id_nhs
destring dim, ignore("q.") replace
destring sdest sdcilo sdcihi, force replace
reshape wide meanest-sdcihi, i(id_nhs) j(dim)

** export clean csv
export delimited using vizData/figS2.csv, replace

************
** Fig S3 ** // Ethnography BPCA within-vs-between scatter
************

** get bpca scores
insheet using ../results/ethno_bpca_culture_summary_final.csv, names clear

** cleanup
rename culture id_nhs
drop distr* other*
destring dim, ignore("q.") replace
destring sdest sdcilo sdcihi, force replace
reshape wide meanest-sdcihi, i(id_nhs) j(dim)

** export clean csv
export delimited using vizData/figS3.csv, replace

************
** Fig S4 ** // Ethnography BPCA scores vs ethnographer characteristics
************

** get author data
insheet using ../results/ethno_bpca_author_summary_final.csv, names clear

** rename everything
gen dim = ""
replace dim = "Formality" if q==1
replace dim = "Arousal" if q==2
replace dim = "Religiosity" if q==3
replace coef = "Female" if coef=="author_female"
replace coef = "Field: Art" if coef=="authorfield_art"
replace coef = "Field: Business" if coef=="authorfield_business"
replace coef = "Field: Education" if coef=="authorfield_education"
replace coef = "Field: Government" if coef=="authorfield_govt"
replace coef = "Field: Linguistics" if coef=="authorfield_linguistic"
replace coef = "Indigenous" if coef=="authorfield_local"
replace coef = "Field: Medicine" if coef=="authorfield_medicine"
replace coef = "Field: Religion" if coef=="authorfield_religion"
replace coef = "Field: Natural Sciences" if coef=="authorfield_science"
replace coef = "Field: Social Sciences" if coef=="authorfield_socialscience"
replace coef = "Field: Traveler" if coef=="authorfield_travel"
replace coef = "Translation" if coef=="pubnonenglish"

** export clean csv
export delimited using vizData/figS4.csv, replace

************
** Fig S5 ** // Ethnography BPCA society-wise vs. other means
************

** get bpca scores
insheet using ../results/ethno_bpca_culture_summary_final.csv, names clear

** add culture names
rename culture id_nhs
mer m:m id_nhs using vizData/NHS_cultureNames
drop in 181/l

** rename & cleanup
replace dim = "Formality" if dim=="q.1"
replace dim = "Arousal" if dim=="q.2"
replace dim = "Religiosity" if dim=="q.3"
destring, replace

** remove Tzeltal
drop if culture=="Tzeltal"

** export clean csv
export delimited using vizData/figS5.csv, replace

************
** Fig S6 ** // Ethnography BCA scores vs number of documents per society
************

** get culture-level summaries
insheet using ../results/ethno_bpca_culture_summary_final.csv, names clear
rename culture id_nhs
drop distr* other*
destring dim, ignore("q.") replace
destring sdest sdcilo sdcihi, force replace
reshape wide meanest-sdcihi, i(id_nhs) j(dim)
sa NHSethno_culturelevel, replace

** get document-level data
insheet using ../results/ethno_bpca_doc_summary_final.csv, names clear

** cleanup
destring dim, replace ignore("q.")
drop docmeanpval
rename ncites ncites
order id_nhs ncites

** reshape
reshape wide doc*, i(cite) j(dim)
drop cite

** make docnums
egen docn = seq(), by(id_nhs)
tostring docn, replace
gen id_doc = id_nhs+"doc"+docn

** add culture-level stuff
mer m:m id_nhs using NHSethno_culturelevel

** cleanup again
drop _merge sd* *pval* npassages
foreach v of varlist mean* {
	rename `v' cult`v'
	}

** make plot order
gsort -ncites id_doc
gen supercite = ncites*100
destring docn, replace
gen supercite_doc = supercite+docn
gsort ncites id_doc
gen docnum = _n

** remove extra culture-level data
foreach v of varlist cult* {
	replace `v' = . if docn!=1
	}

** delete tempfile
rm NHSethno_culturelevel.dta
	
** export clean csv
export delimited using vizData/figS6.csv, replace

************
** Fig S7 ** // Climate BPCA ridgeline plot
************

** get country-level summaries
insheet using ../results/weather_bpca_country_summary_final.csv, names clear
drop distr* other*
destring dim, ignore("q.") replace
destring sdest sdcilo sdcihi, force replace
reshape wide meanest-sdcihi, i(country) j(dim)
sa weather_countrylevel, replace

** get list of sig differences
mer m:m country using vizData/fips-countrycodes
drop if meanest1==.
gsort -nstations
forval i = 1/3 {
	gen sig`i' = meanpval`i'<.05
	di "sig diff on dim `i'"
	li ctryname meanest`i' meanpval`i' if meanpval`i'<.05
	}
gen sumsig = sig1+sig2+sig3
ta sumsig
ta sig1
ta sig2
ta sig3
	
** get obs-level data
insheet using ../results/weather_bpca_scores_final.csv, names clear
rename country_code country

** merge in ctry names/ids
mer m:m country using vizData/fips-countrycodes
drop if date==.
drop _merge

** shorten long names
replace ctryname = "DPR Korea" if strpos(ctryname,"Korea")>0
replace ctryname = "Laos" if strpos(ctryname,"Lao P")>0

** get ctry-level stuff
mer m:m country using weather_countrylevel, nogen

** add nstations to ctry names
tostring nstations, gen(nstations_str)
replace ctryname = ctryname+" ("+nstations_str+")"

** double size of dataset for global dist
sa weather_bpcatemp, replace
append using weather_bpcatemp, gen(source)
replace ctryname = "All countries (542)" if source==1
replace country = "nan" if source==1
replace nstations = 542 if source==1
drop source

** remove empty & weird
drop if sdest1 == .
drop if ctryname == "Qatar (1)"
drop if ctryname == "Angola (2)"
drop if ctryname == "Norfolk Island (1)"

** drop tempfiles
rm weather_bpcatemp.dta
rm weather_countrylevel.dta

** export clean csv
export delimited using vizData/figS7.csv, replace

************
** Fig S8 ** // Climate BPCA scatter
************

** get bpca country-level data
insheet using ../results/weather_bpca_country_summary_final.csv, names clear

** cleanup
destring dim, ignore("q.") replace
drop distr* other*
destring sdest sdcilo sdcihi, force replace
drop if sdest==.
reshape wide meanest-sdcihi, i(country) j(dim)

** export clean csv
export delimited using vizData/figS8.csv, replace

************
** Fig S9 ** // NHS-PSF comparison within HRAF regions
************

** get HRAF regions
insheet using ../results/psf_nhs_compare_minorocm_hrafregion_final.csv, names clear

** add p-vals to nhs points
replace p = p[_n-1] if p=="NA"
destring p, replace

** export clean csv
export delimited using vizData/figS9.csv, replace

*************
** Fig S10 ** // Tonality ratings, song-wise
*************

/* note that this code generates Fig S14 directly, rather than exporting a csv */

** get modality data
insheet using ../results/modality_labels.csv, names clear 
rename multimodal mm
drop v1

** merge in tonality data
mer m:m song using vizData/tonalityRatings, nogen

** clean
drop tonal_pitch2
rename tonal_pitch1 pitch

** pitch classes
gen pc = .
replace pc = 0 if pitch=="C"
replace pc = 1 if pitch=="C#"
replace pc = 2 if pitch=="D"
replace pc = 3 if pitch=="D#"
replace pc = 4 if pitch=="E"
replace pc = 5 if pitch=="F"
replace pc = 6 if pitch=="F#"
replace pc = 7 if pitch=="G"
replace pc = 8 if pitch=="G#"
replace pc = 9 if pitch=="A"
replace pc = 10 if pitch=="A#"
replace pc = 11 if pitch=="B"
drop if pc==.

** make red (multimodal) dotplots
preserve
keep if mm==1
forval i = 1/118 {
	cap dotplot pc if song==`i', mc(red) ylab(0 "C" 1 "C#" 2 "D" 3 "D#" 4 "E" 5 "F" 6 ///
		"F#" 7 "G" 8 "G#" 9 "A" 10 "A#" 11 "B") yti("") xti("") title("`i'") ///
		name("dot`i'", replace)
	if _rc==2000{
		continue
		}
	}
restore

** make blue (unimodal) dotplots
preserve
keep if mm==0
forval i = 1/118 {
	cap dotplot pc if song==`i', mc(blue) ylab(0 "C" 1 "C#" 2 "D" 3 "D#" 4 "E" 5 "F" 6 ///
		"F#" 7 "G" 8 "G#" 9 "A" 10 "A#" 11 "B") yti("") xti("") title("`i'") ///
		name("dot`i'", replace)
	if _rc==2000{
		continue
	}
}
restore

** combine into single plot and save
gr combine dot1 dot2 dot3 dot4 dot5 dot6 dot7 dot8 dot9 dot10 dot10 dot11 dot12 dot13 ///
	dot14 dot15 dot16 dot17 dot18 dot19 dot20 dot21 dot22 dot23 dot24 dot25 dot26 ///
	dot27 dot28 dot29 dot30 dot31 dot32 dot33 dot34 dot35 dot36 dot37 dot38 dot39 ///
	dot40 dot41 dot42 dot43 dot44 dot45 dot46 dot47 dot48 dot49 dot50 dot51 dot52 ///
	dot53 dot54 dot55 dot56 dot57 dot58 dot59 dot60 dot61 dot62 dot63 dot64 dot65 ///
	dot66 dot67 dot68 dot69 dot70 dot71 dot72 dot73 dot74 dot75 dot76 dot77 dot78 ///
	dot79 dot80 dot81 dot82 dot83 dot84 dot85 dot86 dot87 dot88 dot89 dot90 dot91 ///
	dot92 dot93 dot94 dot95 dot96 dot97 dot98 dot99 dot100 dot101 dot102 dot103 ///
	dot104 dot105 dot106 dot107 dot108 dot109 dot110 dot111 dot111 dot112 dot113 dot114 ///
	dot115 dot116 dot117 dot118, xcommon col(12) holes(1 13 24 25 36 37 48 49 60 61 ///
	72 73 84 85 96 97 108 109 120 121 132 134) name(all, replace)

*************************************
** Nothing required for Figs S10-S15 ** // Ethnography BPCA diagnostic plots
*************************************

** n.b., these are produced in analysis scripts, not in viz script
