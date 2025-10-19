*******************************************
*	 	ASSIGNEMENT 1
* 		Mattia Zen
*******************************************
clear all
capture log close

log using zen_assignment1.log, text replace


* Importing the dataset

cd "C:\Users\matti\OneDrive\Desktop\uni\MSc\Panel Data\Assignements"
use soep-2.dta

*********************************************
* 1. The effects of age, cohort and time
* need to improve b and c

*********************************************
xtset persnr year
xtsum
gen cohort = year - age


** a)
* This is more likely due to multicollinearity of the variables since age is the difference between calendar time and birth year
reg s_life year age cohort
reg s_life year cohort
reg s_life age cohort

** b)
* hist s_life

**** separate line for cohort
preserve 

collapse (mean) s_life, by (age cohort)

levelsof (cohort), local(gc)

local call ""
local leg ""
local i=1
foreach g of local gc {
	local call `call' || line s_life age if cohort == `g', sort
	local leg `leg' label(`i++' "`g'")
}

graph twoway `call' legend(`leg') xtitle(Age) ytitle(Life Satisfaction)

gr export 1b_1.png, replace

restore

**** change
preserve

gen d_s_life = d.s_life
collapse (mean) d_s_life, by (age cohort)

levelsof (cohort), local (gc)

local call ""
local leg ""
local i=1
foreach g of local gc {
	local call `call' || line d_s_life age if cohort == `g', sort
	local leg `leg' label(`i++' "`g'")
}

graph twoway `call' legend(`leg') xtitle(Age) ytitle(Life Satisfaction)

gr export 1b_2.png, replace

restore

** c)
cap drop mn_s_life
bysort age: egen mn_s_life=mean(s_life)
graph twoway connected mn_s_life age, xtitle(Age) ytitle(Life Satisfaction)

gr export 1c.png, replace

*********************************************
* 2. One draw of simulated data
*********************************************
clear all

*** DGP
set seed 345398
* setting the seed so that I have the same result every time I run the code
drawnorm alpha_i, n(200)
* random draw 200 invididual terms
expand 5
* expand this term across 5 time period (keeping it the same)
drawnorm nu_it e_it, n(1000)
* random draw the error term and nu_it
g x_it=nu_it+alpha_i
* generate the independent variable as the sum of nu and the individual term
drop nu_it
* drop nu since we do not need it anymore (we only needed it to generate the independent variable of interest)
g y_it=3+alpha_i +2*x_it+e_it
* generate the dependent variable of out model, with the intercept, alpha, the slop and the error term

* b)
asdoc pwcorr, sig save(2b_2.tex),replace


reg y_it x_it
est store b


* c)
reg y_it x_it alpha_i
est store c
esttab b c using 2.tex, se replace



*********************************************
* 3. Many draws of simulated data
*********************************************
clear all

*** DGP
set seed 345398
capture program drop mcprog
program mcprog
	clear
	drawnorm alpha_i, n(200)
	expand 5
	drawnorm nu_it e_it, n(1000)
	g x_it=nu_it+alpha_i
	drop nu_it
	g y_it=3+alpha_i+2*x_it+e_it
	regress y_it x_it
end

simulate _b _se, reps(100): mcprog
sum

estout using 3.tex, replace


* c) CLUSTERING SE
clear all

set seed 345398
capture program drop mcprog
program mcprog, rclass
	drawnorm alpha_i, n(200)
	gen persnr=_n
	expand 5
	bysort persnr : gen year=_n
	drawnorm nu_it e_it, n(1000)
	g x_it=nu_it+alpha_i
	drop nu_it
	g y_it=3+alpha_i+2*x_it+e_it
	
	xtset persnr year
	
	** POLS
	reg y_it x_it
	scalar b_pols =_b[x_it]
	scalar se_pols=_se[x_it]
	
	** clustered 
	reg y_it x_it, cluster(persnr)
	scalar b_cl=_b[x_it]
	scalar se_cl=_se[x_it]
	
	** RE
	xtreg y_it x_it, re cluster(persnr)
	scalar b_re=_b[x_it]
	scalar se_re=_se[x_it]
	
	drop e_it x_it y_it alpha_i persnr year
	
end

simulate b_pols se_pols b_cl se_cl b_re se_re, reps(100):mcprog
renvarlab _sim_1-_sim_6, label
summarize b_pols se_pols b_cl se_cl b_re se_re


estpost summarize
esttab using 3_cl.tex, cells("mean sd min max") replace





*********************************************
* 4. Fixed Effects and First Differences Estimation
*********************************************
clear all

set seed 345398
drawnorm alpha_i, n(200)
g persnr = _n
expand 5
bysort persnr : g year=_n
drawnorm nu_it e_it, n(1000)
g x_it=nu_it+alpha_i
drop nu_it
g y_it=3+alpha_i+2*x_it+e_it

* a)
xtset persnr year
xtreg y_it x_it, fe
est store fe1
* conditions for the FE estimator


reg D.(y_it x_it), nocons
est store fd1
* conditions for the FD estimator


* b)
xtset persnr year
xtreg y_it x_it, re
est store re1

esttab fe1 fd1 re1 using 4.tex, se replace
* the RE is the most efficient estimator 


*********************************************
* 5. Dynamic model
*********************************************
	
*** T=5
set seed 345398

capture program drop mcprog
program mcprog
	clear
	drawnorm alpha_i, n(200)
	gen persnr = _n
	expand 5
	drawnorm nu_it e_it, n(1000)
	gen x_it=nu_it+alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it 
	bysort persnr: gen year = _n
	xtset persnr year
	replace y_it = 3 + alpha_i + 0.5*l.y_it + 2*x_it + e_it if year > 1
	xtreg y_it l.y_it x_it, fe
	est store t5
end

simulate _b _se, reps(100): mcprog
sum



*** T=10
set seed 345398

capture program drop mcprog
program mcprog
	clear
	drawnorm alpha_i, n(200)
	gen persnr = _n
	expand 10
	drawnorm nu_it e_it, n(2000)
	gen x_it=nu_it+alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it 
	bysort persnr: gen year = _n
	xtset persnr year
	replace y_it = 3 + alpha_i + 0.5*l.y_it + 2*x_it + e_it if year > 1
	xtreg y_it l.y_it x_it, fe
	est store t10
end

simulate _b _se, reps(100): mcprog
sum


*** T=20
set seed 345398

capture program drop mcprog
program mcprog
	clear
	drawnorm alpha_i, n(200)
	gen persnr = _n
	expand 20
	drawnorm nu_it e_it, n(4000)
	gen x_it=nu_it+alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it 
	bysort persnr: gen year = _n
	xtset persnr year
	replace y_it = 3 + alpha_i + 0.5*l.y_it + 2*x_it + e_it if year > 1
	xtreg y_it l.y_it x_it, fe
	est store t20
end

simulate _b _se, reps(100): mcprog
sum

set seed 345398

capture program drop mcprog
program mcprog
	clear
	drawnorm alpha_i, n(200)
	gen persnr = _n
	expand 50
	drawnorm nu_it e_it, n(10000)
	gen x_it=nu_it+alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it 
	bysort persnr: gen year = _n
	xtset persnr year
	replace y_it = 3 + alpha_i + 0.5*l.y_it + 2*x_it + e_it if year > 1
	xtreg y_it l.y_it x_it, fe
	est store t50
end

simulate _b _se, reps(100): mcprog
sum


esttab t5 t10 t20 t50 using 5.tex, replace

*********************************************
* 6. Instrumental variables estimation
*********************************************
clear all

set seed 345398

capture program drop mcprog
program mcprog
	clear
	drawnorm alpha_i, n(200)
	gen persnr = _n
	expand 5
	drawnorm nu_it e_it, n(1000)
	gen x_it=nu_it+alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it 
	bysort persnr: gen year = _n
	xtset persnr year
	replace y_it = 3 + alpha_i + 0.5*l.y_it + 2*x_it + e_it if year > 1
	**ABond estimator
	xtabond2 y_it l.y_it x_it, gmm(l.y_it) nolevel
end

simulate _b _se, reps(100): mcprog
sum

esttab  using 6.tex, cells("mean sd min max") replace


log close
