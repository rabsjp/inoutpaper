clear
cd "~/Desktop/jotarepos/inoutrab/data/"

insheet using "datainout.csv", clear

drop v1
encode player, generate(players)
encode session_id, generate(senumber)

g id = session*100+players
g idsu = senumber*10000
replace idsu = idsu+players*100
replace idsu = idsu + round_number
g points = x_t
replace points = 92 if choice ==0
bysort ids: egen everq = max(1-choice)

sort id round_number tick
by id round_number: gen q = choice[_n] - choice[_n+1]
by id round_number: gen dx_t = x_t[_n] - x_t[_n+1]
replace q =0 if q<0 | q==.

g dhigh = 0
replace dhigh = 1 if x_t>92

by id round_number: gen enter = choice[_n] -choice[_n-1]
replace enter = 1 if enter<0 | enter==0

***********************************************
****Generate indicators of low realizations and persistent
****

bysort idsu: egen lowq = sum(x_t<80)
preserve
keep if lowq>30
bootstrap, cluster(idsu) idcluster(subja) reps(200) seed(1234): reg points tre
restore



******************************
**Run logit regression Pr(Out)
**and points regression (OLS)
******************************
***TableBasic***
***Data Prediction DataCounter PredicitionCounter
***1row Pr(oUT), 2row mean points, 3row sd points
bootstrap, cluster(id) idcluster(subja) reps(100) seed(1234): logit choice tre 
eststo clear
*eststo: quietly logit choice tre bayes tbayes bayex tbayex, vce(cluster id)
eststo: quietly logit choice tre, vce(cluster id)
local intercept = (exp(_b[_cons])/(1+exp(_b[_cons])))
quietly logit choice tre, vce(cluster id) 
margins, dydx(*) post
eststo modlog

local prcounter = `intercept'+_b[tre]

putexcel A1= (`intercept') using tablebasic, replace
putexcel B1 = (`prcounter') using tablebasic, modify

eststo: quietly reg points tre, vce(cluster id)

putexcel A2= (_b[_cons]) using tablebasic, modify
putexcel B2 = (_b[_cons]+_b[tre]) using tablebasic, modify

sum points if tre==0 
putexcel A3= (r(sd)) using tablebasic, modify

sum points if tre==1 
putexcel B3= (r(sd)) using tablebasic, modify

esttab using reg.csv, replace se brackets star(* 0.10 ** 0.05 *** 0.01)

******************************
***Transition or Switching probabilities***
***This is computed given the state so we only consider the current state!
eststo: quietly logit q tre if choice==1, vce(cluster id) 
local interceptq = (exp(_b[_cons])/(1+exp(_b[_cons])))
quietly logit q tre bayes tbayes if choice==0, vce(cluster id) 
margins, dydx(*) post
local prbayesq = `interceptq'+_b[bayes]
local prcounterq = `interceptq'+_b[tre]
local prbayescounterq = `interceptq'+_b[tbayes]+_b[tre]+_b[bayes]

putexcel A4= (`interceptq') using tablebasic, modify
putexcel B4 =(`prbayesq') using tablebasic, modify
putexcel C4 = (`prcounterq') using tablebasic, modify
putexcel D4 = (`prbayescounterq') using tablebasic, modify

eststo: quietly logit enter tre  if choice==0, vce(cluster id) 
local intercepte = (exp(_b[_cons])/(1+exp(_b[_cons])))
quietly logit enter tre bayes tbayes if choice==1, vce(cluster id) 
margins, dydx(*) post
local prbayese = `intercepte'+_b[bayes]
local prcountere = `intercepte'+_b[tre]
local prbayescountere = `intercepte'+_b[tbayes]+_b[tre]+_b[bayes]

putexcel A5= (`intercepte') using tablebasic, modify
putexcel B5 =(`prbayese') using tablebasic, modify
putexcel C5 = (`prcountere') using tablebasic, modify
putexcel D5 = (`prbayescountere') using tablebasic, modify


******************************
**Run survival regression
**OUT->IN
******************************
eststo clear
tsset idsu tick
tsspell choice
sort idsu tick
keep if choice==0
bysort tre: su _seq, detail


gen censored = 1
replace censored =0 if tick==160
gen t = _seq
replace t = 160 if censored==0
stset t , failure(censored)
preserve
*keep if bayes>0
*sts graph, by(tre)
keep if round>9

eststo: streg tre, distrib(weibull) vce(cluster id) nohr 
restore
eststo: streg tre bayes tbayes, distrib(weibull) vce(cluster id) nohr
esttab using regsurvout.csv, replace se brackets star(* 0.10 ** 0.05 *** 0.01)
putexcel A1=matrix(e(b)) using regsurvcoeff, replace

stcurve, survival at1(tre=0) at2(tre=1)

******************************
**Run survival regression
**IN->OUT at what x do they exit? 
******************************
eststo clear
egen maxvalue= max(x_t)
*bysort idr: egen everq = max(choice)
gen xflip = maxvalue - x_t
preserve
keep if q==1
bysort idsu: egen lowpq = min(q*tick)
keep if tick == lowpq

save quitdata.dta, replace
restore
keep if tick==160 & everq==0
append using quitdata

gen censored = 1-q
egen xlow_flip = max(maxvalue)
gen t = xflip if q==1
replace t = xlow_flip if everq==0
stset t , failure(censored==0)
g _tr = maxvalue- _t
preserve
keep if round>9
eststo: streg tre, distrib(weibull) vce(cluster id) nohr
esttab using regsurvin.csv, replace se brackets star(* 0.10 ** 0.05 *** 0.01)
stcurve, survival at1(tre=1) at2(tre=0)
putexcel set reginoutwe.xlsx, replace
putexcel A1= matrix(e(b))
restore

eststo: streg tre, distrib(weibull) vce(cluster id) nohr

stcurve, survival at1(tre=1 bayes=0 tbayes=0) at2(tre=0 bayes=0 tbayes=0)
putexcel set reginoutwe.xlsx, replace
putexcel A1= matrix(e(b))

*******************************
*** Tobit regression 
*** IN -> OUT
preserve
keep if q==1
bysort idsu: egen lowpq = min(q*tick)
keep if tick == lowpq
save quitdatatobit.dta, replace
restore
keep if tick==160 & everq==0
append using quitdatatobit
replace x_t = 0 if everq==0

tobit x_t tre, ll(0)
bootstrap, cluster(id) idcluster(subja) reps(200) seed(1234): tobit x_t tre , ll(0) 

keep if round_number>9
bootstrap, cluster(id) idcluster(subja) reps(200) seed(1234): tobit x tre, ll(0) 

*g tbayex = tre*bayex


******************************
**Run survival regression
**IN->OUT but at what x do they enter? 
******************************
eststo clear
bysort idsu: egen evere = max(enter)

egen maxvalue= max(x)
preserve
keep if enter==1
save enterdata.dta, replace
restore
keep if tick==160 & evere==0
append using enterdata

keep if tre>0
gen censored = 1-enter
gen t = x_t if enter==1
replace t = maxvalue if evere==0
stset t, failure(censored==0)
g _tr = _t
eststo: streg bayes, distrib(weibull) vce(cluster id) nohr
stcurve, survival at1(bayes=0) at2(bayes=1)
putexcel set reginoutenterwe.xlsx, replace
putexcel A1= matrix(e(b))


******************************
**Run Tobit regression
**OUT->IN at what x do they enter? 
******************************
eststo clear
bysort idsu: egen evere = max(enter)

egen maxvalue= max(x_t)
preserve
keep if enter==1
save enterdata.dta, replace
restore
keep if tick==160 & evere==0
append using enterdata

replace x_t = 500 if evere==0
bootstrap, cluster(id) idcluster(subja) reps(200) seed(1234): tobit x_t tre, ul(500) 

keep if round_number>9
bootstrap, cluster(id) idcluster(subja) reps(200) seed(1234): tobit x_t tre, ul(500) 














