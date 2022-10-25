*** This dofile contains the code to generate the paper that became my Bachelor Thesis in Economics. It is divided into six parts:

* PART 1. Retrieve and clean mortality data years 1970-1988
* PART 2. Retrieve and clean mortality data years 1989-2019
* PART 3. Clean Population and Educational Data
* PART 4. Normalize Mortality Data using population data and educational data
* PART 5. Analysis
* PART 6. Graphs and Tables

**** PLEASE NOTE: Parts 1 and 2 reproduce the same code for different groups of years. The reason is that the format of the files changed approximately every decade. Moreover, I obtained the data from 1970-1988 and 1989-2019 at different moments in time. Data was originally coded in different dofiles, and I subsequently put all the code together. Therefore, parts 1 and 2 are extremely similar to each other.

global dt = 
global df = 
global dt0 


*************************************************************************************************************************************************************************************************************************************
**** PART 1: RETRIEVE CDC 1970-1989 DATA ********************************************************************************************************************************************************************************
*** STEP 1: IMPORT files
* These were already available in dta format!
***************************************************************************************************************************************
*** STEP 2: Select Variables and create dummies
// I divide the cleaning process into different groups because names of variables and labels change.
** YEARS 1970-1981 (FIPS NOT AVAILABLE)
forvalues year = 1970(1)1978{
use "$dt0", clear
*drop deaths to foreign residents
drop if restatus==4
* generate relevant variables
gen year="`year'"
gen discrepancy=1 if rectype==2
replace discrepancy=0 if rectype==1
gen female=(sex==2)
gen white=(race==1)
gen black=(race==2)
destring age, replace
gen young = (age>=15 & age<=34)
gen midlife = (age>=35 & age<=54)
gen old = (age>=55 & age<=99)
* keep variables of interest
keep year discrepancy restatus countyoc stateoc metro age female white black ucod young midlife old young midlife old ucr281 ucod
order year discrepancy restatus countyoc stateoc metro age female white black ucod young midlife old young midlife old ucr281 ucod
// I use the same definition as Pierce and Schott (2016)
destring(ucr281), replace
destring(ucod), replace
gen suicide = (ucr281>=32600 & ucr281 <= 33000)
gen overdose = (ucr281==30600 | (ucod>=9800 & ucod<=9805))
gen alcohol = (ucr281==24200)
g dod = (suicide==1 | overdose==1 | alcohol==1)
tostring(ucod), replace
***
*keep if suicide==1 | overdose==1 | alcohol==1
save "$dt/", replace
}
forvalues year = 1979(1)1988{
use "$dt0/", clear
*drop deaths to foreign residents
drop if restatus==4
* generate relevant variables
gen year="`year'"
gen discrepancy=1 if rectype==2
replace discrepancy=0 if rectype==1
gen female=(sex==2)
gen white=(race==1)
gen black=(race==2)
destring age, replace
gen young = (age>=15 & age<=34)
gen midlife = (age>=35 & age<=54)
gen old = (age>=55 & age<=99)
* keep variables of interest
keep year discrepancy restatus countyoc stateoc metro age female white black ucod young midlife old young midlife old ucr282
order year discrepancy restatus countyoc stateoc metro age female white black ucod young midlife old young midlife old ucr282
// I use the same definition as Pierce and Schott (2016)
destring(ucr282), replace
gen suicide = (ucr282>=33700 & ucr282 <= 34400)
gen overdose = (ucr282==31700 | ucr282==35300)
gen alcohol = (ucr282==24200)
g dod = (suicide==1 | overdose==1 | alcohol==1)
tostring(ucod), replace
***
*keep if suicide==1 | overdose==1 | alcohol==1
save "$dt/", replace
}
***************************************************************************************************************************************
*** STEP 3: Append all years
// 1970-1971
use "$dt/", clear
forvalues year = 1971(1)1981{
	append using "$dt/"
}
save "$dt/", replace
// 1982-1988
use "$dt/", clear
forvalues year = 1983(1)1988{
	append using "$dt/intermediate 1970-1988/mort_nber_`year'"
}
save "$dt/", replace

*** STEP 4.1: Generate dummies for demographic subgroups of interest
// 70-81
use "$dt/", clear
** TOTAL DEATHS OF DESPAIR
* (0): total deaths of despair (already defined as dod == 1)
*(1)-(4): total deaths of despair for each race
gen dod_white = (white == 1 & dod == 1)
gen dod_black = (black == 1 & dod == 1)
*(9)-(10): ...for each gender
gen dod_male = (female == 0 & dod == 1)
*(31)-(34): ...age groups
gen dod_young = (young == 1 & dod == 1)
gen dod_midlife = (midlife == 1 & dod == 1)
gen dod_old = (old == 1 & dod == 1)
*men for age level
gen dod_male_young = (young == 1 & female==0 & dod == 1)
gen dod_male_midlife = (midlife == 1 & female==0 & dod == 1)
gen dod_male_old = (old == 1 & female==0 & dod == 1)
*men for white
gen dod_midlife_white = (white == 1 & female==0 & dod == 1)
*men for white and midlife
gen dod_male_midlife_white = (midlife==1 & female==0 & white==1 & dod == 1)

** SUICIDE DEATHS OF DESPAIR
* (0): total deaths of despair (already defined as dod == 1)
*(1)-(4): total deaths of despair for each race
gen suicide_white = (white == 1 & suicide == 1)
gen suicide_black = (black == 1 & suicide == 1)

*(9)-(10): ...for each gender
gen suicide_male = (female == 0 & suicide == 1)
gen suicide_midlife = (midlife == 1 & suicide == 1)
gen suicide_male_midlife = (midlife == 1 & female==0 & suicide == 1)
*men for white
gen suicide_midlife_white = (white == 1 & female==0 & suicide == 1)
*men for white and midlife
gen suicide_male_midlife_white = (midlife==1 & female==0 & white==1 & suicide == 1)

** ALCOHOL DEATHS OF DESPAIR
* (0): total deaths of despair (already defined as dod == 1)
*(1)-(4): total deaths of despair for each race
gen alc_white = (white == 1 & alcohol == 1)
gen alc_black = (black == 1 & alcohol == 1)
*(9)-(10): ...for each gender
gen alc_male = (female == 0 & alcohol == 1)
gen alc_midlife = (midlife == 1 & alcohol == 1)
gen alc_male_midlife = (midlife == 1 & female==0 & alcohol == 1)
*men for white
gen alc_midlife_white = (white == 1 & female==0 & alcohol == 1)
*men for white and midlife
gen alc_male_midlife_white = (midlife==1 & female==0 & white==1 & alcohol == 1)

** OVERDOSE DEATHS OF DESPAIR
* (0): total deaths of despair (already defined as dod == 1)
*(1)-(4): total deaths of despair for each race
gen ovds_white = (white == 1 & overdose == 1)
gen ovds_black = (black == 1 & overdose == 1)
*(9)-(10): ...for each gender
gen ovds_male = (female == 0 & overdose == 1)
gen ovds_midlife = (midlife == 1 & overdose == 1)
gen ovds_male_midlife = (midlife == 1 & female==0 & overdose == 1)
*men for white
gen ovds_midlife_white = (white == 1 & midlife==1 & overdose == 1)
*men for white and midlife
gen ovds_male_midlife_white = (midlife==1 & female==0 & white==1 & overdose == 1)
save "$dt/all_mort_nber_70_81.dta", replace

// 82-88
use "$dt/all_mort_nber_82_88.dta", clear
** TOTAL DEATHS OF DESPAIR
* (0): total deaths of despair (already defined as dod == 1)
*(1)-(4): total deaths of despair for each race
gen dod_white = (white == 1 & dod == 1)
gen dod_black = (black == 1 & dod == 1)
*(9)-(10): ...for each gender
gen dod_male = (female == 0 & dod == 1)
*(31)-(34): ...age groups
gen dod_young = (young == 1 & dod == 1)
gen dod_midlife = (midlife == 1 & dod == 1)
gen dod_old = (old == 1 & dod == 1)
*men for age level
gen dod_male_young = (young == 1 & female==0 & dod == 1)
gen dod_male_midlife = (midlife == 1 & female==0 & dod == 1)
gen dod_male_old = (old == 1 & female==0 & dod == 1)
*men for white
gen dod_midlife_white = (white == 1 & midlife==1 & dod == 1)
*men for white and midlife
gen dod_male_midlife_white = (midlife==1 & female==0 & white==1 & dod == 1)

** SUICIDE DEATHS OF DESPAIR
* (0): total deaths of despair (already defined as dod == 1)
*(1)-(4): total deaths of despair for each race
gen suicide_white = (white == 1 & suicide == 1)
gen suicide_black = (black == 1 & suicide == 1)

*(9)-(10): ...for each gender
gen suicide_male = (female == 0 & suicide == 1)
gen suicide_midlife = (midlife == 1 & suicide == 1)
gen suicide_male_midlife = (midlife == 1 & female==0 & suicide == 1)

*men for white
gen suicide_midlife_white = (midlife == 1 & white==1 & suicide == 1)
*men for white and midlife
gen suicide_male_midlife_white = (midlife==1 & female==0 & white==1 & suicide == 1)

** ALCOHOL DEATHS OF DESPAIR
* (0): total deaths of despair (already defined as dod == 1)
*(1)-(4): total deaths of despair for each race
gen alc_white = (white == 1 & alcohol == 1)
gen alc_black = (black == 1 & alcohol == 1)
*(9)-(10): ...for each gender
gen alc_male = (female == 0 & alcohol == 1)
gen alc_midlife = (midlife == 1 & alcohol == 1)
gen alc_male_midlife = (midlife == 1 & female==0 & alcohol == 1)
*men for white
gen alc_midlife_white = (midlife == 1 & white==1 & alcohol == 1)
*men for white and midlife
gen alc_male_midlife_white = (midlife==1 & female==0 & white==1 & alcohol == 1)

** OVERDOSE DEATHS OF DESPAIR
* (0): total deaths of despair (already defined as dod == 1)
*(1)-(4): total deaths of despair for each race
gen ovds_white = (white == 1 & overdose == 1)
gen ovds_black = (black == 1 & overdose == 1)
*(9)-(10): ...for each gender
gen ovds_male = (female == 0 & overdose == 1)
gen ovds_midlife = (midlife == 1 & overdose == 1)
gen ovds_male_midlife = (midlife == 1 & female==0 & overdose == 1)
*men for white
gen ovds_midlife_white = (midlife == 1 & white==1 & overdose == 1)
*men for white and midlife
gen ovds_male_midlife_white = (midlife==1 & female==0 & white==1 & overdose == 1)

save "$dt/all_mort_nber_82_88.dta", replace

** STEP 3.2: Generate total DoD for specific demographic groups
// 82-88
use "$dt/all_mort_nber_82_88", clear 
rename fipsctyo FIPS
collapse (sum) cdod=dod cdod_white=dod_white cdod_black=dod_black cdod_male=dod_male csuicide=suicide csuicide_white=suicide_white csuicide_black=suicide_black csuicide_male=suicide_male calc=alcohol calc_white=alc_white calc_black=alc_black calc_male=alc_male covds=overdose covds_white=ovds_white covds_black=ovds_black covds_male=ovds_male cdod_young=dod_young cdod_midlife=dod_midlife cdod_old=dod_old cdod_male_young=dod_male_young cdod_male_midlife=dod_male_midlife cdod_male_old=dod_male_old csuicide_midlife=suicide_midlife csuicide_male_midlife=suicide_male_midlife calc_midlife=alc_midlife calc_male_midlife=alc_male_midlife covds_midlife=ovds_midlife covds_male_midlife=ovds_male_midlife cdod_midlife_white=dod_midlife_white cdod_male_midlife_white=dod_male_midlife_white csuicide_midlife_white=suicide_midlife_white csuicide_male_midlife_white=suicide_male_midlife_white calc_midlife_white=alc_midlife_white calc_male_midlife_white=alc_male_midlife_white covds_midlife_white=ovds_midlife_white covds_male_midlife_white=ovds_male_midlife_white, by(year FIPS)
save "$df/counts/total_counts_82_88", replace
// 70-81
use "$dt/all_mort_nber_70_81", clear
collapse (sum) cdod=dod cdod_white=dod_white cdod_black=dod_black cdod_male=dod_male csuicide=suicide csuicide_white=suicide_white csuicide_black=suicide_black csuicide_male=suicide_male calc=alcohol calc_white=alc_white calc_black=alc_black calc_male=alc_male covds=overdose covds_white=ovds_white covds_black=ovds_black covds_male=ovds_male cdod_young=dod_young cdod_midlife=dod_midlife cdod_old=dod_old cdod_male_young=dod_male_young cdod_male_midlife=dod_male_midlife cdod_male_old=dod_male_old csuicide_midlife=suicide_midlife csuicide_male_midlife=suicide_male_midlife calc_midlife=alc_midlife calc_male_midlife=alc_male_midlife covds_midlife=ovds_midlife covds_male_midlife=ovds_male_midlife cdod_midlife_white=dod_midlife_white cdod_male_midlife_white=dod_male_midlife_white csuicide_midlife_white=suicide_midlife_white csuicide_male_midlife_white=suicide_male_midlife_white calc_midlife_white=alc_midlife_white calc_male_midlife_white=alc_male_midlife_white covds_midlife_white=ovds_midlife_white covds_male_midlife_white=ovds_male_midlife_white, by(year countyoc)
save "$df/counts/total_counts_70_81_NOFIPS", replace

*** Get FIPS for early data
use "$dt0/raw CDC 70-88/mort1982.dta", clear
duplicates drop fipsctyo, force
keep fipsctyo countyoc
save "$dt/countyoc_to_FIPS", replace
merge m:m countyoc using "$df/counts/total_counts_70_81_NOFIPS.dta"
rename fipsctyo FIPS
order FIPS
keep if _merge==3
drop countyoc _merge
save "$df/counts/total_counts_70_81.dta", replace

*************************************************************************************************************************************************************************************************************************************
**** PART 2: RETRIEVE CDC 1989-1999 DATA ********************************************************************************************************************************************************************************************
*** STEP 1: IMPORT files from text to DTA
* years 1989-2004
forvalues year = 1989(1)1999{
qui infile using "$dict/dictionary_mort`year'.do", ///
	using("$draw/") clear
save "$d1/intermediate_mort_nber`year'", replace
}
* years 2005-2019
forvalues year = 2005(1)2017{
	clear
qui infile using "$dict/dictionary_mort_p2005.do", ///
	using("$draw/") 
save "$d1/intermediate_mort_nber`year'", replace
}
***************************************************************************************************************************************
*** STEP 2: Select Variables and create dummies
// I divide the cleaning process into 5 year groups because //
// names of variables and labels change.
** YEARS 1989-1995
forvalues year = 1989(1)1998{
use "$d1/intermediate_mort_nber`year'.dta", clear
*drop deaths to foreign residents
drop if restatus==4
* generate relevant variables
gen year="`year'"
rename statebth statbth
gen discrepancy=1 if rectype==2
replace discrepancy=0 if rectype==1
gen female=(sex==2)
gen white=(racer3==1)
gen othrace=(racer3==2)
gen black=(racer3==3)
gen hispan=(hspanicr>=1 & hspanicr<=5)
gen hsdrop = (educ<=11)
gen hsgrad = (educ==12)
gen somecol = (educ>=13 & educ<=15)
gen colgrad = (educ==16 | educ==17)
destring(age), replace
gen young = (age>=15 & age<=34)
gen midlife = (age>=35 & age<=54)
gen old = (age>=55)
* keep variables of interest
rename fipsctyo FIPS
keep year discrepancy restatus statbth FIPS metro hsdrop hsgrad somecol colgrad educ educr female white black hispan othrace ucr282 young midlife old fipssto fipsstr
order year discrepancy restatus statbth FIPS metro hsdrop hsgrad somecol colgrad educ educr female white black hispan othrace ucr282 young midlife old fipssto fipsstr
// I use the same definition as Pierce and Schott (2016)
destring(ucr282), replace
gen suicide = (ucr282>=33700 & ucr282 <= 34400)
gen overdose = (ucr282==31700 | ucr282==35300)
gen alcohol = (ucr282==24200)
gen dod = (suicide == 1 | overdose ==1 | alcohol == 1)
save "$dfinal/mort_nber_`year'", replace
}
** YEARS 1999-2004
forvalues year = 1999(1)2004{
use "$d1/intermediate_mort_nber`year'.dta", clear
*drop deaths to foreign residents
drop if restatus==4
* generate relevant variables
gen discrepancy=1 if rectype==2
replace discrepancy=0 if rectype==1
gen female=(sex==2)
gen white=(racer3==1)
gen othrace=(racer3==2)
gen black=(racer3==3)
gen hispan=(hspanicr>=1 & hspanicr<=5)
gen single=(marstat==1)
gen married=(marstat==2)
gen divorced=(marstat==4)
gen hsdrop = educ<=11
gen hsgrad = educ==12
gen somecol = (educ>=13 & educ<=15)
gen colgrad = (educ==16 | educ==17)
destring age, replace
gen young = (age>=15 & age<=34)
gen midlife = (age>=35 & age<=54)
gen old = (age>=55)
* keep variables of interest
rename fipsctyo FIPS
keep year discrepancy restatus statbth FIPS metro hsdrop hsgrad somecol colgrad educ educr female white black hispan othrace ucr358  young midlife old fipssto fipsstr
order year discrepancy restatus statbth FIPS metro hsdrop hsgrad somecol colgrad educ educr female white black hispan othrace ucr358  young midlife old fipssto fipsstr
// I use the same definition as Pierce and Schott (2016)
destring(ucr358), replace
gen suicide = (ucr358>=424 & ucr358<=431)
gen overdose = (ucr358==420 | ucr358==443)
gen alcohol = (ucr358==298)
gen dod = (suicide == 1 | overdose ==1 | alcohol == 1)
save "$dfinal/mort_nber_`year'", replace
}
)

** YEARS 2005-2019
forvalues year = 2005(1)2019{
use "$d1/intermediate_mort_nber`year'.dta", clear
*drop deaths to foreign residents
drop if restatus==4
* generate relevant variables
rename educ educr
rename educ89 educ
gen discrepancy=1 if rectype==2
replace discrepancy=0 if rectype==1
gen female=(sex=="F")
gen white=(racer3==1)
gen othrace=(racer3==2)
gen black=(racer3==3)
gen hispan=(hspanicr>=1 & hspanicr<=5)
gen single=(marstat=="S")
gen married=(marstat=="M")
gen divorced=(marstat=="W")
rename stateoc fipssto
rename countyoc fipsctyo
rename staters fipsstr
rename countyrs fipsctyr
gen hsdrop = (educ<=11 & educr==.)
replace hsdrop = 1 if educr<=2 & educ==.
gen hsgrad = (educ==12 & educr==.)
replace hsgrad=1 if educr==3 & educ==.
gen somecol = (educ>=13 & educ<=15 & educr==.)
replace somecol=1 if educr==4 & educ==.
gen colgrad = ((educ==16 | educ==17) & educr==.)
replace colgrad=1 if educr>=5 & educ==.
tostring age, replace
gen ager = substr(age,2,3) if substr(age,1,1) == "1"
destring ager, replace
gen young = (ager>=15 & ager<=34)
gen midlife = (ager>=35 & ager<=54)
gen old = (ager>=55 & ager<100)
* keep variables of interest
rename fipsctyo FIPS
keep year discrepancy restatus statbth FIPS metro hsdrop hsgrad somecol colgrad female white black hispan othrace ucr358 young midlife old fipssto fipsstr
order year discrepancy restatus statbth FIPS metro hsdrop hsgrad somecol colgrad female white black hispan othrace ucr358 young midlife old fipssto fipsstr
// I use the same definition as Pierce and Schott (2016)
destring(ucr358), replace
gen suicide = (ucr358>=424 & ucr358<=431)
gen overdose = (ucr358==420 | ucr358==443)
gen alcohol = (ucr358==298)
gen dod = (suicide == 1 | overdose ==1 | alcohol == 1)
save "$dfinal/mort_nber_`year'", replace
}

*************************************************************************************************************************************************************************************************************************************
*** STEP 3: construct county-year datasets for RESIDENCE and OCCURRENCE *************************************************************************************************************************************************************
// Convert variable year into string in all datasets to avoid problems when append
forvalues year = 1989(1)2019{
	use "$dfinal/mort_nber_`year'", clear
	tostring(year), replace
	tostring(statbth), replace
	save "$dfinal/mort_nber_`year'", replace
}
// Append all years
use "$dfinal/mort_nber_1989", clear
forvalues year = 1990(1)2019{
	append using "$dfinal/mort_nber_`year'"
}
// Convert the letters after 2003 in FIPS into numbers
foreach var in o{
replace fipsst`var' = "01" if fipsst`var' == "AL"
replace fipsst`var' = "02" if fipsst`var' == "AK"
replace fipsst`var' = "04" if fipsst`var' == "AZ"
replace fipsst`var' = "05" if fipsst`var' == "AR"
replace fipsst`var' = "06" if fipsst`var' == "CA"
replace fipsst`var' = "08" if fipsst`var' == "CO"
replace fipsst`var' = "09" if fipsst`var' == "CT"
replace fipsst`var' = "10" if fipsst`var' == "DE"
replace fipsst`var' = "11" if fipsst`var' == "DC"
replace fipsst`var' = "12" if fipsst`var' == "FL"
replace fipsst`var' = "13" if fipsst`var' == "GA"
replace fipsst`var' = "15" if fipsst`var' == "HI"
replace fipsst`var' = "16" if fipsst`var' == "ID"
replace fipsst`var' = "17" if fipsst`var' == "IL"
replace fipsst`var' = "18" if fipsst`var' == "IN"
replace fipsst`var' = "19" if fipsst`var' == "IA"
replace fipsst`var' = "20" if fipsst`var' == "KS"
replace fipsst`var' = "21" if fipsst`var' == "KY"
replace fipsst`var' = "22" if fipsst`var' == "LA"
replace fipsst`var' = "23" if fipsst`var' == "ME"
replace fipsst`var' = "24" if fipsst`var' == "MD"
replace fipsst`var' = "25" if fipsst`var' == "MA"
replace fipsst`var' = "26" if fipsst`var' == "MI"
replace fipsst`var' = "27" if fipsst`var' == "MN"
replace fipsst`var' = "28" if fipsst`var' == "MS"
replace fipsst`var' = "29" if fipsst`var' == "MO"
replace fipsst`var' = "30" if fipsst`var' == "MT"
replace fipsst`var' = "31" if fipsst`var' == "NE"
replace fipsst`var' = "32" if fipsst`var' == "NV"
replace fipsst`var' = "33" if fipsst`var' == "NH"
replace fipsst`var' = "34" if fipsst`var' == "NJ"
replace fipsst`var' = "35" if fipsst`var' == "NM"
replace fipsst`var' = "36" if fipsst`var' == "NY"
replace fipsst`var' = "37" if fipsst`var' == "NC"
replace fipsst`var' = "38" if fipsst`var' == "ND"
replace fipsst`var' = "39" if fipsst`var' == "OH"
replace fipsst`var' = "40" if fipsst`var' == "OK"
replace fipsst`var' = "41" if fipsst`var' == "OR"
replace fipsst`var' = "42" if fipsst`var' == "PA"
replace fipsst`var' = "44" if fipsst`var' == "RI"
replace fipsst`var' = "45" if fipsst`var' == "SC"
replace fipsst`var' = "46" if fipsst`var' == "SD"
replace fipsst`var' = "47" if fipsst`var' == "TN"
replace fipsst`var' = "48" if fipsst`var' == "TX"
replace fipsst`var' = "49" if fipsst`var' == "UT"
replace fipsst`var' = "50" if fipsst`var' == "VT"
replace fipsst`var' = "51" if fipsst`var' == "VA"
replace fipsst`var' = "53" if fipsst`var' == "WA"
replace fipsst`var' = "54" if fipsst`var' == "WV"
replace fipsst`var' = "55" if fipsst`var' == "WI"
replace fipsst`var' = "56" if fipsst`var' == "WY"
replace FIPS = fipssto + substr(FIPS,3,3)
}
save "$dfinal/all_mort_nber_", replace
// Generate dummies for demographic subgroups of interest
use "$dfinal/all_mort_nber_", replace
** TOTAL DEATHS OF DESPAIR
foreach var in dod overdose suicide alcohol{
* (0): total deaths of despair (already defined as dod == 1)
gen `var'_total = (`var' == 1)
*(1)-(4): total deaths of despair for each race
gen `var'_white = (white == 1 & `var' == 1)
gen `var'_black = (black == 1 & `var' == 1)
*(9)-(10): ...for each gender
gen `var'_male = (female == 0 & `var' == 1)
*(31)-(34): ...age groups
gen `var'_young = (young == 1 & `var' == 1)
gen `var'_midlife = (midlife == 1 & `var' == 1)
gen `var'_old = (old == 1 & dod == 1)
*men for age groups and male
gen `var'_male_young = (young == 1 & female==0 & `var' == 1)
gen `var'_male_midlife = (midlife == 1 & female==0 & `var' == 1)
gen `var'_male_old = (old == 1 & female==0 & `var' == 1)
*men for midlife white
gen `var'_midlife_white = (white == 1 & midlife==1 & `var' == 1)
*men for midlife male
gen `var'_male_white = (midlife==1 & white==1 & `var' == 1)
*men for midlife male white
gen `var'_male_midlife_white = (midlife==1 & female==0 & white==1 & `var' == 1)
*for educational attainment
gen `var'_hsdrop = (hsdrop==1 & `var'==1)
gen `var'_hsgrad = (hsgrad==1 & `var'==1)
gen `var'_somecol = (somecol==1 & `var'==1)
gen `var'_colgrad = (colgrad==1 & `var'==1)
}

save "$dfinal/all_mort_nber", replace

** STEP 3.2: Generate total DoD for specific demographic groups
* ocurrence
use "$dfinal/all_mort_nber", clear
collapse (sum) cdod_total = dod_total cdod_white = dod_white cdod_black = dod_black cdod_male = dod_male cdod_young = dod_young cdod_midlife = dod_midlife cdod_old = dod_old cdod_male_young = dod_male_young cdod_male_midlife = dod_male_midlife cdod_male_old =  dod_male_old cdod_midlife_white = dod_midlife_white cdod_male_midlife_white = dod_male_midlife_white cdod_hsdrop = dod_hsdrop cdod_hsgrad = dod_hsgrad cdod_somecol = dod_somecol cdod_colgrad = dod_colgrad coverdose_total = overdose_total coverdose_white = overdose_white coverdose_black = overdose_black coverdose_male = overdose_male coverdose_young = overdose_young coverdose_midlife = overdose_midlife coverdose_old = overdose_old coverdose_male_young = overdose_male_young coverdose_male_midlife = overdose_male_midlife coverdose_male_old =  overdose_male_old coverdose_midlife_white = overdose_midlife_white coverdose_male_midlife_white = overdose_male_midlife_white coverdose_hsdrop = overdose_hsdrop coverdose_hsgrad = overdose_hsgrad coverdose_somecol = overdose_somecol coverdose_colgrad = overdose_colgrad csuicide_total = suicide_total csuicide_white = suicide_white csuicide_black = suicide_black csuicide_male = suicide_male csuicide_young = suicide_young csuicide_midlife = suicide_midlife csuicide_old = suicide_old csuicide_male_young = suicide_male_young csuicide_male_midlife = suicide_male_midlife csuicide_male_old =  suicide_male_old csuicide_midlife_white = suicide_midlife_white csuicide_male_midlife_white = suicide_male_midlife_white csuicide_hsdrop = suicide_hsdrop csuicide_hsgrad = suicide_hsgrad csuicide_somecol = suicide_somecol csuicide_colgrad = suicide_colgrad calcohol_total = alcohol_total calcohol_white = alcohol_white calcohol_black = alcohol_black calcohol_male = alcohol_male calcohol_young = alcohol_young calcohol_midlife = alcohol_midlife calcohol_old = alcohol_old calcohol_male_young = alcohol_male_young calcohol_male_midlife = alcohol_male_midlife calcohol_male_old =  alcohol_male_old calcohol_midlife_white = alcohol_midlife_white calcohol_male_midlife_white = alcohol_male_midlife_white calcohol_hsdrop = alcohol_hsdrop calcohol_hsgrad = alcohol_hsgrad calcohol_somecol = alcohol_somecol calcohol_colgrad = alcohol_colgrad, by(year FIPS)
save "$df/counts/total_counts_89_19", replace




*************************************************************************************************************************************************************************************************************************************
**** PART 3: RETRIEVE POPULATION DATA (demographic) *********************************************************************************************************************************************************************************

* Transform raw pop data to readable dta format
qui infile using "/Users/carmenarbaizar/Desktop/Uni/TFG Eco/code/Stata/dict_pop.do", using("$dt0/us.1969_2020.19ages.txt") clear
save "$dteco/population/us.1969_2020.19ages.dta", replace
** Create variables of interest
use "$dteco/population/us.1969_2020.19ages.dta", clear
gen FIPS = stateFIPS + countyFIPS
gen female=(sex=="2")
gen white=(race=="1")
gen black=(race=="2")
destring(age), replace
gen young=(age>=4 & age<=7)
gen midlife=(age>=8 & age<=11)
gen old=(age>=12)

destring(population), replace
clonevar t_female = pop if female==1
clonevar t_male = pop if female==0
clonevar t_black = pop if black==1
clonevar t_white = pop if white==1
clonevar t_young = pop if young==1
clonevar t_midlife = pop if midlife == 1
clonevar t_old = pop if old == 1
clonevar t_male_young = pop if young == 1 & female==0
clonevar t_male_midlife = pop if midlife == 1 & female==0
clonevar t_male_old = pop if old == 1 & female==0
clonevar t_female_young = pop if young == 1 & female==0
clonevar t_female_midlife = pop if midlife == 1 & female==0
clonevar t_female_old = pop if old == 1 & female==0
clonevar t_midlife_white = pop if white==1 & midlife==1
clonevar t_male_midlife_white = pop if female==0 & midlife==1 & white==1
** Create aggregate statistics
collapse (sum) cpop=population cfemale = t_female cmale = t_male cblack = t_black cwhite = t_white cyoung = t_young cmidlife = t_midlife cold = t_old c_male_young=t_male_young c_male_midlife=t_male_midlife c_male_old=t_male_old c_female_young=t_female_young c_female_midlife=t_female_midlife c_female_old=t_female_old c_midlife_white=t_midlife_white c_male_midlife_white=t_male_midlife_white, by(Year FIPS)
rename Year year
save "$dt/population/population.dta", replace

*** POPULATION DATA (education)
import excel "$dt/raw data/Education.xls", sheet("Education 1970 to 2019") cellrange(A7:AU3288) clear
rename H hsdrop1970
rename I hsgrad1970
rename J somecol1970
rename K colgrad1970
rename P hsdrop1980
rename Q hsgrad1980
rename R somecol1980
rename S colgrad1980
rename X hsdrop1990
rename Y hsgrad1990
rename Z somecol1990
rename AA colgrad1990
rename AF hsdrop2000
rename AG hsgrad2000
rename AH somecol2000
rename AI colgrad2000
rename AN hsdrop2010
rename AO hsgrad2010
rename AP somecol2010
rename AQ colgrad2010
rename A fipscty
keep fipscty hs* some* col*
***
drop if substr(fipscty,5,1)=="0"
***
foreach var in hsdrop hsgrad somecol colgrad{
	forvalues i = 1991(1)1999{
		clonevar `var'`i' = `var'1990
	}
	forvalues i = 2001(1)2009{
		clonevar `var'`i' = `var'2000
	}
	forvalues i = 2011(1)2019{
		clonevar `var'`i' = `var'2010
	}
}
***
reshape long hsdrop hsgrad somecol colgrad, i(fipscty) j(year)
drop if year==1970 | year==1980
sort year fipscty
order year
*** Drop foreign counties
drop if substr(fipscty,1,2)=="72"
*** Eliminate commas
tostring(hsdrop), replace
destring(hsdrop), replace 
tostring(hsgrad), replace
destring(hsgrad), replace 
tostring(somecol), replace
destring(somecol), replace 
tostring(colgrad), replace
destring(colgrad), replace 
rename fipscty FIPS
*** Save Education Files
save "$dt/population/Education.dta", replace


*************************************************************************************************************************************************************************************************************************************
**** PART 4: NORMALIZE MORTALITY DATA USING POPULATION DATA *************************************************************************************************************************************************************************
*** STEP 1: merge DoD and Population
use "$df/counts/total_counts_70_81.dta", clear
append using "$df/counts/total_counts_82_88.dta"
append using "$df/counts/total_counts_89_19.dta"
save "$df/counts/total_counts", replace
// 1970-1981
use "$dt/population/population.dta", clear
destring(year), replace
keep if year>1969 & year<1982
tostring(year), replace
merge 1:1 year FIPS using "$df/counts/total_counts_70_81.dta"
keep if _merge==3
drop _merge
destring(year), replace
save "$df/norm/normalized_dod_70_81", replace
* Normalize by specific count
// 1982-1988
use "$dt/population/population.dta", clear
destring(year), replace
keep if year>1981 & year<1989
tostring(year), replace
merge 1:1 year FIPS using "$df/counts/total_counts_82_88.dta"
keep if _merge==3
drop _merge
destring(year), replace
save "$df/norm/normalized_dod_82_88", replace
// 1989-2019
use "$dt/population/population.dta", clear
destring(year), replace
keep if year>1988 & year<2020
tostring(year), replace
merge 1:1 year FIPS using "$df/counts/total_counts_89_19.dta"
keep if _merge==3
drop _merge
destring(year), replace
merge 1:1 year FIPS using "$dt/population/Education.dta"
drop if _merge==2
drop _merge
destring(year), replace
save "$df/norm/normalized_dod_89_19", replace
* Append all years
use "$df/norm/normalized_dod_70_81", clear
append using "$df/norm/normalized_dod_82_88"
append using "$df/norm/normalized_dod_89_19"


foreach var in dod overdose suicide alcohol{
gen n`var'_total = (c`var'_total/cpop)*100000
gen n`var'_white = (c`var'_white/cwhite)*100000
gen n`var'_black = (c`var'_black/cblack)*100000
gen n`var'_male = (c`var'_male/cmale)*100000
gen n`var'_young = (c`var'_young/cyoung)*100000
gen n`var'_midlife = (c`var'_midlife/cmidlife)*100000
gen n`var'_old = (c`var'_old/cold)*100000
gen n`var'_male_young = (c`var'_male_young/c_male_young)*100000 
gen n`var'_male_midlife = (c`var'_male_midlife/c_male_midlife)*100000
gen n`var'_male_old = (c`var'_male_old/c_male_old)*100000
gen n`var'_midlife_white = (c`var'_midlife_white/c_midlife_white)*100000
gen n`var'_male_midlife_white = (c`var'_male_midlife_white/c_male_midlife_white)*100000
gen n`var'_hsdrop = (c`var'_hsdrop/hsdrop)*100000
gen n`var'_hsgrad = (c`var'_hsgrad/hsgrad)*100000
gen n`var'_somecol = (c`var'_somecol/somecol)*100000
gen n`var'_colgrad = (c`var'_colgrad/colgrad)*100000
}

keep year FIPS n*
keep if year>1989
save "$df/norm/normalized_dod.dta", replace

*** Add State abbrev
use "$df/norm/normalized_dod.dta", clear
gen state_num=substr(FIPS,1,2)
order year state_num FIPS
save "$df/norm/normalized_dod.dta", replace

import excel "/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/US territory codes/uscities.xlsx", sheet("Sheet1") firstrow clear
keep state_id county_fips
tostring(county_fips), replace
replace county_fips= "0" + county_fips if substr(county_fips,5,1)==""
gen state_num = substr(county_fips,1,2)
drop county_fips
duplicates drop state_id, force
merge 1:m state_num using "$df/norm/normalized_dod.dta"
keep if _merge==3
save "$df/norm/normalized_dod.dta", replace

*** ADDITIONAL: Create yearly means
use "$df/norm/normalized_dod.dta", clear
gen states=substr(FIPS,1,2)
order year states
keep if year>1972
collapse (mean) mdod=ndod movds=novds malc=nalc msuicide=nsuicide mdod_col=ndod_col mdod_nocol=ndod_nocol mdod_male=ndod_male mdod_female=ndod_female mdod_male_young=ndod_male_young mdod_male_midlife=ndod_male_midlife mdod_male_old=ndod_male_old mdod_hsdrop=ndod_hsdrop mdod_hsgrad=ndod_hsgrad mdod_somecol=ndod_somecol mdod_colgrad=ndod_colgrad ///
, by(year states)
save "$df/means/total_means", replace


*************************************************************************************************************************************************************************************************************************************
**** PART 5: ANALYSIS ***************************************************************************************************************************************************************************************************************
* Import Dataset for Million Dollar Plants (MDPs)
import excel "$dt0/Data - MDPs.xlsx", sheet("Hoja1") firstrow clear
drop I
tostring(FIPS), replace
replace FIPS="0"+FIPS if substr(FIPS,5,1)=="" 
replace Type="Losers" if Type=="Loser"
** Drop rcounties that were losers and then become winners
drop if Case==65
drop if Case==9
drop if Case==20
drop if Case==63
drop if Case==21
drop if Case==58
drop if FIPS=="12025" & Type=="Losers"
drop if FIPS=="39095" & Type=="Losers"
drop if FIPS=="42091" & Type=="Losers"
drop if FIPS=="54003" & Type=="Losers"
drop if FIPS=="8031" & Type=="Losers"
drop if FIPS=="8069" & Type=="Losers"
drop if FIPS=="12057" & Type=="Losers"
drop if FIPS=="18097" & Type=="Losers"
drop if FIPS=="21059" & Type=="Losers"
drop if FIPS=="21093" & Type=="Losers"
drop if FIPS=="29189" & Type=="Losers"
drop if FIPS=="51087" & Type=="Losers"
drop if FIPS=="13089" & Type=="Losers"
drop if FIPS=="25017" & Type=="Losers"
drop if FIPS=="37063" & Type=="Losers"
drop if FIPS=="51059" & Type=="Losers"

replace FIPS = "0" + FIPS if substr(FIPS,5,1)=="_"
save "$df/data - MDPs", replace

* Prepare using dataset
use "$df/norm/normalized_dod.dta", clear
replace FIPS="12086" if FIPS=="12025"
destring(FIPS), replace
expand 2 if FIPS==51121 | FIPS==39061 | FIPS==25025 | FIPS==53061 | FIPS==53033 | FIPS==8069 | ///
FIPS==25017 | FIPS==35001 | FIPS==40109 | FIPS==12031 | FIPS==42091 | FIPS==21209 | FIPS==34003 | /// 
FIPS==48113 | FIPS==48355 | FIPS==27123 | FIPS==45079 | FIPS==13089 | FIPS==47157 | FIPS==36083 | ///
FIPS==42045 | FIPS==37081 | FIPS==21111 | FIPS==18129 | FIPS==9001 | FIPS==31055 | FIPS==6111 | FIPS==36119 | FIPS==37119, gen(newv)
tostring(FIPS), replace
replace FIPS = FIPS + "_" if newv==1
save "$df/norm/normalized_dod_added.dta", replace
* Merge data on LPO and DoD
use "$df/norm/normalized_dod_added.dta", clear
drop _merge
tostring FIPS, replace
replace FIPS = "0" + FIPS if substr(FIPS,5,1)=="" | substr(FIPS,5,1)=="_" 
replace FIPS="12086" if FIPS=="12025"
merge m:m FIPS using "$df/data - MDPs.dta"
keep if _merge==3
drop _merge
sort Year FIPS
* Keep FIPS of Interest
replace FIPS = subinstr(FIPS, "_", "",.)
keep if FIPS == "39151" | FIPS == "51121" | FIPS == "13185" | FIPS == "18129" | FIPS == "12011" | FIPS == "12025" | FIPS == "12101" | FIPS == "13113" | FIPS == "39061" | FIPS == "21049" | FIPS == "21173" | FIPS == "37119" | FIPS == "37183"  | FIPS == "48309"  | FIPS == "25025"  | FIPS == "53061" | FIPS == "53033" | FIPS == "08069" | FIPS == "06085" | FIPS == "29183" | FIPS == "29189" | FIPS == "47149" | FIPS == "18163"  | FIPS == "25017" | FIPS == "25005" | FIPS == "40101" | FIPS == "05107" | FIPS == "51059" | FIPS == "51107" | FIPS == "51121" | FIPS == "53011" | FIPS == "22033" | FIPS == "48453" | FIPS == "35001" | FIPS == "48355" | FIPS == "04019" | FIPS == "08041" | FIPS == "35001" | FIPS == "13103" | FIPS == "45053" | FIPS == "19103" | FIPS == "19113" | FIPS == "47119" | FIPS == "48181" | FIPS == "26077" | FIPS == "21211" | FIPS == "21209" | FIPS == "47189" | FIPS == "20209" | FIPS == "51700" | FIPS == "51087" | FIPS == "37045" | FIPS == "37063" | FIPS == "13211" | FIPS == "37021" | FIPS == "45039" | FIPS == "45079" | FIPS == "42077" | FIPS == "18157" | FIPS == "17167" | FIPS == "21093" | FIPS == "22019" | FIPS == "40109" | FIPS == "12031" | FIPS == "13077" | FIPS == "17093" | FIPS == "06029" | FIPS == "06099" | FIPS == "01017" | FIPS == "13215" | FIPS == "01113" | FIPS == "13285" | FIPS == "41043" | FIPS == "53053" | FIPS == "36003" | FIPS == "09003" | FIPS == "21091" | FIPS == "21059" | FIPS == "42029" | FIPS == "42101" | FIPS == "41051" | FIPS == "41067" | FIPS == "53033" | FIPS == "55055" | FIPS == "55105" | FIPS == "17037" | FIPS == "13015" | FIPS == "13139" | FIPS == "47093" | FIPS == "13089" | FIPS == "40143" | FIPS == "40131" | FIPS == "13135" | FIPS == "06081" | FIPS == "21209" | FIPS == "06001" | FIPS == "51087" | FIPS == "34003" | FIPS == "25017" | FIPS == "25025" | FIPS == "34007" | FIPS == "34021" | FIPS == "48113" | FIPS == "12057" | FIPS == "18057" | FIPS == "06111" | FIPS == "48057" | FIPS == "48167" | FIPS == "48355" | FIPS == "26161" | FIPS == "36099" | FIPS == "39173" | FIPS == "39095" | FIPS == "08069" | FIPS == "56021" | FIPS == "08123" | FIPS == "08013" | FIPS == "47179" | FIPS == "27123" | FIPS == "39095" | FIPS == "18003" | FIPS == "20203" | FIPS == "28151" | FIPS == "47163" | FIPS == "45079" | FIPS == "13089" | FIPS == "12095" | FIPS == "47157" | FIPS == "45055" | FIPS == "36083" | FIPS == "34009" | FIPS == "23005" | FIPS == "37063" | FIPS == "36119" | FIPS == "42091" | FIPS == "42045" | FIPS == "48439" | FIPS == "05071" | FIPS == "27123" | FIPS == "13089" | FIPS == "47157" | FIPS == "45083" | FIPS == "34035" | FIPS == "12117" | FIPS == "51059" | FIPS == "08031" | FIPS == "17019" | FIPS == "40109" | FIPS == "18097" | FIPS == "37081" | FIPS == "51059" | FIPS == "54003" | FIPS == "39061" | FIPS == "21111" | FIPS == "01073" | FIPS == "47097" | FIPS == "12053" | FIPS == "12105" | FIPS == "51550" | FIPS == "34003" | FIPS == "51085" | FIPS == "25017" | FIPS == "53061" | FIPS == "53035" | FIPS == "18097" | FIPS == "08031" | FIPS == "21059" | FIPS == "18129" | FIPS == "06077" | FIPS == "06067" | FIPS == "37119" | FIPS == "54003" | FIPS == "06061" | FIPS == "13121" | FIPS == "09001" | FIPS == "45045" | FIPS == "31055" | FIPS == "18141" | FIPS == "42003" | FIPS == "12086" | FIPS == "12031" | FIPS == "29189" | FIPS == "06111" | FIPS == "54003" | FIPS == "10003" | FIPS == "13057" | FIPS == "39035" | FIPS == "42091" | FIPS == "36083" | FIPS == "12057" | FIPS == "09001" | FIPS == "13021" | FIPS == "21111" | FIPS == "48113" | FIPS == "19153" | FIPS == "51161" | FIPS == "36119" | FIPS == "01125" | FIPS == "45015" | FIPS == "13059" | FIPS == "37001" | FIPS == "45023" | FIPS == "37063" | FIPS == "31055" | FIPS == "47001" | FIPS == "37157" | FIPS == "37081"
*** Generate Dummies for FE
** Pre and Post Treatment
sort year
keep if year>1972
sort FIPS year
destring(year), replace
gen year_set = 0 if year==Year
forv i=1(1)40{
replace year_set = -`i' if year==Year-`i'
}
forv i=1(1)40{
replace year_set = `i' if year==Year+`i'
}
// Diff in Diff
gen post = (year_set>0)
gen treatment = (Type=="Winner")
gen post_treatment = post*treatment
// Create dummies for each year after policy
*drop y_*
destring(year_set), replace
forvalues i = 0(1)37{
	gen y_`i'=(year_set*treatment) if year_set==`i'
	replace y_`i'=1 if y_`i'!=.
	replace y_`i'=0 if y_`i'==.
}
tostring(year_set), replace
forvalues i = 1(1)23{
	gen y_pre`i'=(year_set*treatment) if year_set=="-`i'"
	replace y_pre`i'="1" if y_pre`i'!=""
	replace y_pre`i'="0" if y_pre`i'==""
}
destring(y_pre*), replace

order year Case FIPS Case Type year_set
sort Case FIPS year
** INDUSTRY
gen industry=(Sector=="Mnfg")
replace industry=2 if Sector=="Tran & Util"
replace industry=3 if Sector=="Services"
replace industry=4 if Sector=="Trade"
replace industry=5 if Sector=="Finnancials"
drop Sector
forvalues i = 1(1)5{
	gen industry_`i'=(industry==`i')
}
** STATE
gen St=substr(FIPS,1,2)
destring(St), replace
forvalues i = 1(1)56{
	gen St_`i'=(St==`i')
}
** CASE
destring(year_set), replace
gen Case_FE = (year_set>-8 & year_set<=25)
*** Create Different Post_Treatment Groups
destring(year_set), replace
gen post1 = (year_set>0 & year_set<10)
gen post2 = (year_set>=10 & year_set<=20)
gen post3 = (year_set>20)
gen post_treatment1 = post1*treatment
gen post_treatment2 = post2*treatment
gen post_treatment3 = post3*treatment

*** REGRESIONS
*** BASELINE MEANS
sum ndod if year_set>0 & treatment==0
sum ndod if year_set>0 & year_set<10 & treatment==0
sum ndod_midlife if year_set>0 & treatment==0
sum ndod_midlife if year_set>0 & year_set<10 & treatment==0
sum ndod_male_midlife if year_set>0 & treatment==0
sum ndod_male_midlife if year_set>0 & year_set<10 & treatment==0
sum ndod if year_set>=10 & year_set<20 & treatment==0
sum ndod_midlife if year_set>=10 & year_set<20 & treatment==0
sum ndod_male_midlife if year_set>=10 & year_set<20 & treatment==0
sum ndod if year_set>=20 & treatment==0
sum ndod_midlife if year_set>=20 & treatment==0
sum ndod_male_midlife if year_set>=20 & treatment==0

// 1 BASELINE Regressions

destring FIPS, replace
xtset year
drop Case_FE
gen Case_FE = (year_set>-9 & year_set<=26)
eststo m1: reghdfe ndod treatment post post_treatment if year>1973, absorb(year Case#Case_FE St) vce(robust)
eststo m2: reghdfe ndod treatment post post_treatment1 post_treatment2 post_treatment3 if year>1973, absorb(industry#year Case#Case_FE St) vce(robust)
eststo m3: reghdfe ndod_midlife treatment post post_treatment if year>1973, absorb(year Case#Case_FE St) vce(robust)
eststo m4: reghdfe ndod_midlife post treatment post_treatment1 post_treatment2 post_treatment3 if year>1973,  absorb(year Case#Case_FE St) vce(robust)
eststo m5: reghdfe ndod_male_midlife treatment post post_treatment if year>1973,  absorb(year Case#Case_FE St) vce(robust)
eststo m6: reghdfe ndod_male_midlife treatment post post_treatment1 post_treatment2 post_treatment3 if year>1973,  absorb(year Case#Case_FE St) vce(robust)


// 1.1 BASELINE Regressions with clustered standard errors

drop Case_FE
gen Case_FE = (year_set>-9 & year_set<=26)
eststo m7: reghdfe ndod post treatment post_treatment if year>1973, absorb(year Case#Case_FE St) vce(cluster FIPS St)
eststo m8: reghdfe ndod post treatment post_treatment1 post_treatment2 post_treatment3 if year>1973, absorb(year Case#Case_FE St) vce(cluster FIPS St)
eststo m9: reghdfe ndod_midlife_white post treatment post_treatment if year>1973, absorb(year Case#Case_FE St) vce(cluster FIPS)
eststo m10: reghdfe ndod_midlife post treatment post_treatment1 post_treatment2 post_treatment3 if year>1973,  absorb(year Case#Case_FE St) vce(cluster FIPS)
eststo m11: reghdfe ndod_male_midlife_white post treatment post_treatment if year>1973,  absorb(year Case#Case_FE St) vce(cluster FIPS)
eststo m12: reghdfe ndod_male_midlife post treatment post_treatment1 post_treatment2 post_treatment3 if year>1973,  absorb(year Case#Case_FE St) vce(cluster FIPS)


// 2 ADD TREND

sum year_set
gen trend=year_set-r(min)
xtset year
drop Case_FE
gen Case_FE = (year_set>-9 & year_set<=26)
eststo m1: reghdfe ndod treatment trend post post_treatment if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(robust)
reghdfe ndod treatment trend post post_treatment if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(cluster FIPS)
eststo m2: reghdfe ndod treatment trend post post_treatment1 post_treatment2 post_treatment3 if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(robust)
reghdfe ndod treatment trend post post_treatment1 post_treatment2 post_treatment3 if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(cluster FIPS)
eststo m3: reghdfe ndod_midlife treatment post trend post_treatment if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(robust)
reghdfe ndod_midlife treatment post trend post_treatment if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(cluster FIPS)
eststo m4: reghdfe ndod_midlife treatment trend post post_treatment1 post_treatment2 post_treatment3 if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(robust)
reghdfe ndod_midlife treatment trend post post_treatment1 post_treatment2 post_treatment3 if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(cluster FIPS)
eststo m5: reghdfe ndod_male_midlife treatment post post_treatment if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(robust)
reghdfe ndod_male_midlife treatment post post_treatment if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(cluster FIPS)
eststo m6: reghdfe ndod_male_midlife treatment post post_treatment1 post_treatment2 post_treatment3 if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(robust)
reghdfe ndod_male_midlife treatment post post_treatment1 post_treatment2 post_treatment3 if year>1973, absorb(year industry St St#trend Case#Case_FE) vce(cluster FIPS)

estfe m1 m2 m3 m4 m5 m6,labels(year "Year Fixed Effects" St "State Fixed Effects" Case "Case Fixed Effects" industry "Industry Fixed Effects")
esttab m1 m2 m3 m4 m5 m6 using "/Users/Carmenarbaizar/Desktop/Example.txt", title("Effect of attracting a large plant (Robust Standard Errors)") cells(b(star fmt(3)) se(par fmt(2)))p(post_treatment*) indicate(`r(indicate_fe)') starlevels(* 0.10 ** 0.05 *** 0.01) legend label varlabels(post_treatment PT post_treatment1 PTshort post_treatment2 PTmedium post_treatment3 PTlong _cons Constant) stats(N r2, fmt(0 2) label(N R-sqr)) replace nomtitle nolabel collabels(none) mgroups("Total Deaths of Despair" "Midlife Adults" " Midlife Male", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) scalars("RFE Region FE") note("p-values in parenthesis") drop(treatment _cons trend) booktabs

******** Check each disease

// SUICIDE

*** BASELINE MEANS
sum nsuicide if year_set>0 & treatment==0
sum nsuicide if year_set>0 & year_set<10 & treatment==0
sum nsuicide_midlife if year_set>0 & treatment==0
sum nsuicide_midlife if year_set>0 & year_set<10 & treatment==0
sum nsuicide_male_midlife if year_set>0 & treatment==0
sum nsuicide_male_midlife if year_set>0 & year_set<10 & treatment==0
sum nsuicide if year_set>=10 & year_set<20 & treatment==0
sum nsuicide_midlife if year_set>=10 & year_set<20 & treatment==0
sum nsuicide_male_midlife if year_set>=10 & year_set<20 & treatment==0
sum nsuicide if year_set>=20 & treatment==0
sum nsuicide_midlife if year_set>=20 & treatment==0
sum nsuicide_male_midlife if year_set>=20 & treatment==0

eststo m1: reghdfe nsuicide treatment post post_treatment if year>1973, absorb(year Case#Case_FE St) vce(robust)
eststo m2: reghdfe nsuicide treatment post post_treatment1 post_treatment2 post_treatment3 if year>1973, absorb(industry#year Case#Case_FE St) vce(robust)
eststo m3: reghdfe nsuicide_midlife post treatment post_treatment if year>1973, absorb(year Case#Case_FE St) vce(robust)
eststo m4: reghdfe nsuicide_midlife post treatment post_treatment1 post_treatment2 post_treatment3 if year>1973,  absorb(year Case#Case_FE St) vce(robust)
eststo m5: reghdfe nsuicide_male_midlife post treatment post_treatment if year>1973,  absorb(year Case#Case_FE St) vce(robust)
eststo m6: reghdfe nsuicide_male_midlife post treatment post_treatment1 post_treatment2 post_treatment3 if year>1973,  absorb(year Case#Case_FE St) vce(robust)

// ALCOHOL

sum nalc if year_set>0 & treatment==0
sum nalc if year_set>0 & year_set<10 & treatment==0
sum nalc_midlife if year_set>0 & treatment==0
sum nalc_midlife if year_set>0 & year_set<10 & treatment==0
sum nalc_male_midlife if year_set>0 & treatment==0
sum nalc_male_midlife if year_set>0 & year_set<10 & treatment==0
sum nalc if year_set>=10 & year_set<20 & treatment==0
sum nalc_midlife if year_set>=10 & year_set<20 & treatment==0
sum nalc_male_midlife if year_set>=10 & year_set<20 & treatment==0
sum nalc if year_set>=20 & treatment==0
sum nalc_midlife if year_set>=20 & treatment==0
sum nalc_male_midlife if year_set>=20 & treatment==0

eststo m1: reghdfe nalc treatment post post_treatment if year>1973, absorb(year Case#Case_FE St) vce(robust)
eststo m2: reghdfe nalc treatment post post_treatment1 post_treatment2 post_treatment3 if year>1973, absorb(industry#year Case#Case_FE St) vce(robust)
eststo m3: reghdfe nalc_midlife post post_treatment if year>1973, absorb(year Case#Case_FE St) vce(robust)
eststo m4: reghdfe nalc_midlife post post_treatment1 post_treatment2 post_treatment3 if year>1973,  absorb(year Case#Case_FE St) vce(robust)
eststo m5: reghdfe nalc_male_midlife post post_treatment if year>1973,  absorb(year Case#Case_FE St) vce(robust)
eststo m6: reghdfe nalc_male_midlife post treatment post_treatment1 post_treatment2 post_treatment3 if year>1973,  absorb(year Case#Case_FE St) vce(robust)

// OVERDOSE

sum novds if year_set>0 & treatment==0
sum novds if year_set>0 & year_set<10 & treatment==0
sum novds_midlife if year_set>0 & treatment==0
sum novds_midlife if year_set>0 & year_set<10 & treatment==0
sum novds_male_midlife if year_set>0 & treatment==0
sum novds_male_midlife if year_set>0 & year_set<10 & treatment==0
sum novds if year_set>=10 & year_set<20 & treatment==0
sum novds_midlife if year_set>=10 & year_set<20 & treatment==0
sum novds_male_midlife if year_set>=10 & year_set<20 & treatment==0
sum novds if year_set>=20 & treatment==0
sum novds_midlife if year_set>=20 & treatment==0
sum novds_male_midlife if year_set>=20 & treatment==0

eststo m1: reghdfe novds treatment post post_treatment if year>1973, absorb(year Case#Case_FE St) vce(robust)
eststo m2: reghdfe novds treatment post post_treatment1 post_treatment2 post_treatment3 if year>1973, absorb(industry#year Case#Case_FE St) vce(robust)
eststo m3: reghdfe novds_midlife post post_treatment if year>1973, absorb(year Case#Case_FE St) vce(robust)
eststo m4: reghdfe novds_midlife post post_treatment1 post_treatment2 post_treatment3 if year>1973,  absorb(year Case#Case_FE St) vce(robust)
eststo m5: reghdfe novds_male_midlife post post_treatment if year>1973,  absorb(year Case#Case_FE St) vce(robust)
eststo m6: reghdfe novds_male_midlife post treatment post_treatment1 post_treatment2 post_treatment3 if year>1973,  absorb(year Case#Case_FE St) vce(robust)


// Callaway and Sant'Anna (2020) 
use "/Users/CarmenArbaizar/Desktop/provisional_MDPs.dta", clear
gen year_set2 = 0 if year<1983
forvalues i = 1983(1)2019{
replace year_set2 = `i' - 1982 if year==`i'
}
drop post2
gen post2=(year>=1982)
destring(FIPS), replace
gen treatment_cohort = 1 if treatment==1 & Case<=5
replace treatment_cohort = 2 if treatment==1 & (Case>5 & Case<=8)
replace treatment_cohort = 3 if treatment==1 & (Case>8 & Case<=11)
replace treatment_cohort = 4 if treatment==1 & (Case>11 & Case<=17)
replace treatment_cohort = 5 if treatment==1 & (Case>17 & Case<=23)
replace treatment_cohort = 6 if treatment==1 & (Case>23 & Case<=31)
replace treatment_cohort = 7 if treatment==1 & (Case>31 & Case<=40)
replace treatment_cohort = 8 if treatment==1 & (Case>40 & Case<=47)
replace treatment_cohort = 9 if treatment==1 & (Case>47 & Case<=53)
replace treatment_cohort = 10 if treatment==1 & (Case>53 & Case<=65)
replace treatment_cohort = 11 if treatment==1 & (Case>65 & Case<=75)
replace treatment_cohort = 12 if treatment==1 & (Case>75 & Case<=82)
replace treatment_cohort = 0 if treatment==0
eststo m1: csdid ndod, time(post2) gvar(treatment_cohort)
eststo m2: csdid ndod, time(post2) gvar(treatment)
eststo m3: csdid ndod_midlife, time(post2) gvar(treatment_cohort)
eststo m4: csdid ndod_midlife, time(post2) gvar(treatment)
eststo m5: csdid ndod_male_midlife, time(post2) gvar(treatment_cohort)
eststo m6: csdid ndod_male_midlife, time(post2) gvar(treatment)
*** Check pretrend
preserve 
csdid ndod, ivar(FIPS) time(post2) gvar(treatment_cohort)
csdid ndod, ivar(FIPS) time(post2) gvar(treatment)
estat pretrend
csdid ndod_midlife, ivar(FIPS) time(post2) gvar(treatment_cohort)
csdid ndod_midlife, ivar(FIPS) time(post2) gvar(treatment)
estat pretrend
csdid ndod_male_midlife, ivar(FIPS) time(post2) gvar(treatment_cohort)
csdid ndod_male_midlife, ivar(FIPS) time(post2) gvar(treatment)
estat pretrend
restore

*************************************************************************************************************************************************************************************************************************************
**** PART 6: CONSTRUCT MAIN TABLES AND GRAPHS ***************************************************************************************************************************************************************************************

*** Create Tables
esttab m1 m2 m5 m6 m9 m10 using "/Users/Carmenarbaizar/Desktop/Example.txt", title("Effect of attracting a large plant (Robust Standard Errors)") cells(b(star fmt(3)) se(par fmt(2)))p(post_treatment*) indicate("Year Fixed Effects = *year" "Industry Fixed Effects = *industry" "State Fixed Effects = *St" "Case Fixed Effects = *Case") starlevels(* 0.10 ** 0.05 *** 0.01) legend label varlabels(post_treatment PT post_treatment1 PTshort post_treatment2 PTmedium post_treatment3 PTlong _cons Constant) stats(N r2, fmt(0 2) label(N R-sqr)) replace nomtitle nolabel collabels(none) mgroups("Total Deaths" "Deaths among Midlife Adults" "Deaths Among Midlife Male", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) booktabs scalars("RFE Region FE") note("p-values in parenthesis") drop(treatment)

estfe m3 m4 m7 m8 m11 m12,labels(year "Year Fixed Effects" St "State Fixed Effects" Case "Case Fixed Effects" industry "Industry Fixed Effects")
esttab m3 m4 m7 m8 m11 m12 using "/Users/Carmenarbaizar/Desktop/Example.txt", title("Effect of attracting a large plant (Robust Standard Errors)") cells(b(star fmt(3)) se(par fmt(2)))p(post_treatment*) indicate(`r(indicate_fe)') starlevels(* 0.10 ** 0.05 *** 0.01) legend label varlabels(post_treatment PT post_treatment1 PTshort post_treatment2 PTmedium post_treatment3 PTlong _cons Constant) stats(N r2, fmt(0 2) label(N R-sqr)) replace nomtitle nolabel collabels(none) mgroups("Total Deaths" "Deaths among Midlife Adults" "Deaths Among Midlife Male", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) scalars("RFE Region FE") note("p-values in parenthesis") drop(treatment _cons) booktabs

estfe m13 m14 m15 m16 m17 m18,labels(year "Year Fixed Effects" St "State Fixed Effects" Case "Case Fixed Effects" industry "Industry Fixed Effects")
esttab m13 m14 m15 m16 m17 m18, title("Effect of attracting a large plant (Robust Standard Errors)") cells(b(star fmt(3)) se(par fmt(2)))p(post_treatment*) indicate(`r(indicate_fe)') starlevels(* 0.15 ** 0.05 *** 0.01) legend label varlabels(post_treatment PT post_treatment1 PTshort post_treatment2 PTmedium post_treatment3 PTlong _cons Constant) stats(N r2, fmt(0 2) label(N R-sqr)) replace nomtitle nolabel collabels(none) mgroups("Total Deaths" "Deaths among Midlife Adults" "Deaths Among Midlife Male", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) scalars("RFE Region FE") note("p-values in parenthesis") drop(treatment _cons) 

*** Create Coefplots
qui eststo TFG_reg: reghdfe ndod_midlife y_* if year>1973,  absorb(year St) vce(robust)
coefplot TFG_reg, keep(y_*) drop(county_trend y_pre20 y_pre19 y_pre18 y_pre17 y_pre16 y_pre15 y_pre14 y_pre13 y_pre12 y_22 y_23 y_24 y_25 y_26 y_27 y_28 y_29 y_30 y_31 y_32 y_33 y_34 y_35 y_36 y_37) xtitle(Year normalized to 0 for plant opening) title("Effect of Treatment on Total Deaths by Year") yline(0) rename(y_pre11=-11 y_pre10=-10 y_pre9=-9 y_pre8=-8 y_pre7=-7 y_pre6=-6 y_pre5=-5 y_pre4=-4 y_pre3=-3 y_pre2=-2 y_pre1=-1 y_0=0 y_1=1 y_2=2 y_3=3 y_4=4 y_5=5 y_6=6 y_7=7 y_8=8 y_9=9 y_10=10 y_11=11 y_12=12 y_13=13 y_14=14 y_15=15 y_16=16 y_17=17 y_18=18 y_19=19 y_20=20 y_21=21) order(-15 -14 -13 -12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1) vertical xsize(10) xline(11, lwidth(thin) lcolor(black) lpattern(dash)) ciopts(recast(rcap) lpattern(solid) lcolor(black) lwidth(medthin))

// Table 2
import excel "/Users/carmenarbaizar/Desktop/Data - MDPs_.xlsx", sheet("Hoja1") firstrow clear
drop I FIPS 
order Case Year County State Firm Sector Type
replace Type="Loser" if Type=="Losers"
tab Year Type
tab Sector Type
codebook County

// Graph 1
use "/Users/carmenarbaizar/Desktop/Uni/TFG Eco/data/mortality/final data/means/total_means.dta", clear

line mdod movds malc msuicide year, title("Deaths of Despair by Cause") subtitle("1973-2019") legend(label(1 "Total") label(2 "Overdose") label(3 "Alcohol") label(4 "Suicide")) ytitle("Total Deaths (100, 000 population)") 

line mdod_male mdod_female year, title("Deaths of Despair by Gender") subtitle("1973-2019") legend(label(1 "Male") label(2 "Female")) ytitle("Total Deaths (100, 000 population)") 

line mdod_male_young mdod_male_midlife mdod_male_old year, title("Deaths of Despair by Age Cohort") subtitle("1973-2019") legend(label(1 "Ages 15-29") label(2 "Ages 30-54") label(3 "Ages 55+")) ytitle("Total Deaths (100, 000 population)") 

preserve
keep if year>1989
line mdod_hsdrop mdod_hsgrad mdod_somecol year, title("Deaths of Despair by Educational Attainment") subtitle("1973-2019") legend(label(1 "High School Dropouts") label(2 "High School Graduates") label(3 "College Graduates")) ytitle("Total Deaths (100, 000 population)") 
restore
**************************************************
*************************************************************************************************************************************************************************************************************************************
