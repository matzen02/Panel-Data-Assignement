*******************************************
*	 	ASSIGNEMENT 2
* 		Mattia Zen
*******************************************
clear all
capture log close

log using zen_assignment2.log, text replace

************
* Question 1
************
	
* Importing the dataset

cd "C:\Users\matti\OneDrive\Desktop\uni\MSc\Panel Data\Assignements\Assignement2"
use stocks_balanced-1
	
* 1.a

egen hh = group(ident)
drop if hh<881
drop if hh>2098
drop hh

xtset ident wave

* 1.b
gen dstock = (share>0)
by wave, sort : count if dstock==1



* 1.c
by wave, sort: summarize share, detail	
* most of the people not own any shares

************
* Question 2
************

* 2.a
gen ltotfw =ln(totfw)

xtprobit dstock female age edyrs nonwhite hispanic marrlt ltotfw, re
est store re1
esttab using 2_a.tex, replace


* 2.b


* 2.c
xtprobit dstock female age edyrs nonwhite hispanic marrlt ltotfw i.wave, re
est store re2
esttab using 2_c.tex, replace

lrtest re2 re1

* 2.d
xtprobit dstock female age edyrs nonwhite hispanic marrlt ltotfw wave, re
est store re3

* LR test
lrtest re1 re2
lrtest re3 re2
lrtest re1 re3


* 2.e 
xtlogit dstock female age edyrs nonwhite hispanic marrlt ltotfw, re
est store re4


estout re1 re2 re3 re4
esttab re1 re2 re3 re4 using 2.tex, r2 replace


************
* Question 3
************
*3.a
gen share_0 =share
by ident, sort: replace share_0 =share[1]

gen dstock_0 = (share_0>0)

xtset ident wave

xtprobit dstock L.dstock female age edyrs nonwhite hispanic marrlt ltotfw if ident>1, re
esttab using 3a.tex, replace

*3.e
xtprobit dstock L.dstock female age edyrs nonwhite hispanic marrlt ltotfw if ident>1, re



************
* Question 4
************

* 4.a
xttobit share female age edyrs nonwhite hispanic marrlt ltotfw, ll(0)
est store tobit1


* 4.c
gen ltotfw_sq=ltotfw^2
xttobit share female age edyrs nonwhite hispanic marrlt ltotfw ltotfw_sq, ll(0)
est store tobit2

lrtest tobit1 tobit2

* 4.d
xttobit share female age edyrs nonwhite hispanic marrlt ltotfw ltotfw_sq, ll(0)
margins, dydx(ltotfw) at(ltotfw=(min(max)))
marginsplot

* 4.e
xttobit share female age edyrs nonwhite hispanic marrlt ltotfw ltotfw_sq, ll(0) ul(1)
est store tobit3

estout tobit1 tobit2 tobit3, cells(b(star) se(par))

************
* Question 5
************
* 5.a
egen ltotfw_mean = mean(ltotfw), by(ident)
xttobit share female age edyrs nonwhite hispanic marrlt ltotfw ltotfw_sq ltotfw_mean, ll(0) ul(1) 
est store tobit4

lrtest tobit4 tobit1







