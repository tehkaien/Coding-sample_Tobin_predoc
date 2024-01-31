*Codes for MPSA paper*
*By: Joshua Kai-En Teh, Dec. 2022*
//Pro-US score in 2008 and 2020 TEDS data//
tab m7
tab n1b
tab k9a
tab q1b
recode m7 (1/2=0)(3=1)(4=2)(95/98=.), gen(ma2008_score)
recode k9a (-5/-1=0)(0=1)(1/5=2)(95/98=.), gen(tsai2020_score)
recode m7 (1/2=-1)(3=0)(4=1)(95/98=.), gen(ma2008_score_new)
recode k9a (-5/-1=-1)(0=0)(1/5=1)(95/98=.), gen(tsai2020_score_new)
recode n1b (3/7=.)(95/99=.), gen(ma2008_prty)
recode q1b (3/41=.)(95/99=.), gen(tsai2020_prty)
label variable ma2008_score "Pro-US score in Ma 2008"
label variable tsai2020_score "Pro-US score in Tsai 2020"
label variable ma2008_prty "Party ID in Ma 2008"
label variable tsai2020_prty "Party ID in Tsai 2020"
label define ma2008_prty 1 "KMTers" 2 "DPPers", modify
label define tsai2020_prty 1 "KMTers" 2 "DPPers", modify
label values ma2008_prty ma2008_prty
label values tsai2020_prty tsai2020_prty
sum ma2008_score
sum tsai2020_score
tab ma2008_prty
tab tsai2020_prty
mean ma2008_score, over(ma2008_prty)
mean tsai2020_score, over(tsai2020_prty)
mean ma2008_score_new, over(ma2008_prty)
mean tsai2020_score_new, over(tsai2020_prty)

//Preparation//
import excel "C:\Users\User\Documents\Operation Holy Grail\Phase 4\Data\Pork n beef import\MPSA\Ma_admin.xlsx", ///
sheet("Variables") firstrow
import excel "C:\Users\User\Documents\Operation Holy Grail\Phase 4\Data\Pork n beef import\MPSA\Ma_admin_firstbf.xlsx", ///
sheet("Variables") firstrow
import excel "C:\Users\User\Documents\Operation Holy Grail\Phase 4\Data\Pork n beef import\MPSA\Ma_admin_secondbf.xlsx", ///
sheet("Variables") firstrow
import excel "C:\Users\User\Documents\Operation Holy Grail\Phase 4\Data\Pork n beef import\MPSA\Tsai_admin.xlsx", ///
sheet("Variables") firstrow
import excel "C:\Users\User\Documents\Operation Holy Grail\Phase 4\Data\Pork n beef import\MPSA\Tsai_admin_firstbf.xlsx", ///
sheet("Variables") firstrow
import excel "C:\Users\User\Documents\Operation Holy Grail\Phase 4\Data\Pork n beef import\MPSA\Tsai_admin_secondbf.xlsx", ///
sheet("Variables") firstrow
import excel "C:\Users\User\Documents\Operation Holy Grail\Phase 4\Data\Pork n beef import\MPSA\aggregate.xlsx", ///
sheet("Variables") firstrow
label variable imports "Imports score"
label variable prous "Pro-US score"
label variable era "Presidency"
label variable us_appraise "Appraisement of Washington"
label variable pub_oriented "Legislators mentioned 'public' terms"
label variable government "Ruling party legislators"
label variable opposition "Opposition party legislators"
label variable partylist "Partylist legislators"
label variable male "Male legislators"
label variable ls_type "Legislator's type"
label variable session "Session's type"
label variable disboth "Senior legislators"
label variable proimports "Pro-imports legislators"
label variable antiimports "Anti-imports legislators"
label variable KMT "KMT legislators"
label variable DPP "DPP legislators"
label variable sittingno "Sitting number"
label variable ambition "Diplomatic intention"
label variable imports_cat "imports variable in categorical form"
label define era 1 "Ma" 2 "Tsai", modify
label values era era
label define government 0 "No" 1 "Yes", modify
label values government government
label define ambition 0 "No" 1 "Yes", modify
label values ambition ambition
label define ls_type 1 "Party-list" 2 "District-elected" 3 "Aboriginal district", modify
label values ls_type ls_type
label define pub_oriented 0 "No" 1 "Yes", modify
label values pub_oriented pub_oriented
label define disboth 0 "No" 1 "Yes", modify
label values disboth disboth
label define session 1 "Plenary" 2 "Committee" 3 "Caucus", modify
label values session session
label define session 1 "Plenary" 2 "Committee" 3 "Public hearing", modify
label values session session
sum imports
tab us_appraise

//Models//
//Interval regression//
intreg imports imports i.government i.pub_oriented i.disboth ib(1).ls_type ib(3).session
intreg imports imports i.government##i.disboth i.pub_oriented ib(1).ls_type ib(3).session
intreg imports imports i.KMT##i.disboth
estimates store m1
estimates store m2
intreg imports imports i.DPP##i.disboth
estimates store m3
estimates store m4
estimates store m5
estimates store m6
intreg imports imports ib(1).ls_type ib(3).session i.male i.disboth##i.government i.pub_oriented##i.government
regress imports ib(1).ls_type ib(3).session i.male i.disboth##i.government i.pub_oriented##i.government i.partylist##i.government
regress imports i.government##i.era i.pub_oriented i.disboth ib(1).ls_type i.male
fitstat
estat ic
vif
//Coefficient plot//
coefplot m1, drop(_cons) mcolor("blue") xtitle("Coefficients") ytitle("Variables") title("Model 1", size(medium)) /// 
xline(0) coeflabels(1.government = "Ruling party legislators") ciopts(lcolor("blue")) ///
name(m1f, replace)
graph save m1f, replace
coefplot m2, drop(_cons) mcolor("blue") xtitle("Coefficients") ytitle("Variables") title("Model 2 (Full model)", size(medium)) /// 
xline(0) coeflabels(1.government = "Ruling party legislators" 1.pub_oriented = "Legislators mentioned public-related terms" 1.disboth = "Senior legislators" 2.ls_type = "District v.s. Party-list" 3.ls_type = "Aborigine district v.s. Party-list" 1.session = "Plenary vs Public hearing" 2.session = "Commitee v.s. Public hearing", interaction(" x ")) ciopts(lcolor("blue")) ///
name(m2f, replace)
graph save m2f, replace

coefplot m3, drop(_cons) mcolor("green") xtitle("Coefficients") ytitle("Variables") title("Model 3", size(medium)) /// 
xline(0) coeflabels(1.government = "Ruling party legislators") ciopts(lcolor("green")) ///
name(m3f, replace)
graph save m3f, replace
coefplot m4, drop(_cons) mcolor("green") xtitle("Coefficients") ytitle("Variables") title("Model 4 (Full model)", size(medium)) /// 
xline(0) coeflabels(1.government = "Ruling party legislators" 1.pub_oriented = "Legislators mentioned public-related terms" 1.disboth = "Senior legislators" 2.ls_type = "District v.s. Party-list" 3.ls_type = "Aborigine district v.s. Party-list" 1.session = "Plenary vs Caucus" 2.session = "Commitee v.s. Caucus", interaction(" x ")) ciopts(lcolor("green")) ///
name(m4f, replace)
graph save m4f, replace
graph combine m1f.gph m2f.gph m3f.gph m4f.gph, rows(2) cols(2) xcommon ycommon name(m1tom4, replace)
graph combine m1f.gph m2f.gph, cols(1) xcommon ycommon name(m1m2, replace)
graph combine m3f.gph m4f.gph, cols(1) xcommon ycommon name(m3m4, replace)
graph use "C:\Users\User\Documents\Operation Holy Grail\Phase 4\Data\Pork n beef import\MPSA\Stata\m1tom4.gph"

coefplot m5, drop(_cons) mcolor("blue") xtitle("Coefficients") ytitle("Variables") title("Model 5 (Ma's era)", size(medium)) /// 
xline(0) coeflabels(1.government = "Ruling party legislators" 1.pub_oriented = "Legislators mentioned public-related terms" 1.disboth = "Senior legislators" government#disboth = "Ruling party legislators x Senior legislators" 2.ls_type = "District v.s. Party-list" 3.ls_type = "Aborigine district v.s. Party-list" 1.session = "Plenary vs Public hearing" 2.session = "Commitee v.s. Public hearing", interaction(" x ")) ciopts(lcolor("blue")) ///
name(m5f, replace)
graph save m5f, replace
coefplot m6, drop(_cons) mcolor("green") xtitle("Coefficients") ytitle("Variables") title("Model 6 (Tsai's era)", size(medium)) /// 
xline(0) coeflabels(1.government = "Ruling party legislators" 1.pub_oriented = "Legislators mentioned public-related terms" 1.disboth = "Senior legislators" 2.ls_type = "District v.s. Party-list" 3.ls_type = "Aborigine district v.s. Party-list" 1.session = "Plenary vs Caucus" 2.session = "Commitee v.s. Caucus", interaction(" x ")) ciopts(lcolor("green")) ///
name(m6f, replace)
graph save m6f, replace
graph combine m5f.gph m6f.gph, cols(1) xcommon ycommon name(m5m6, replace)
graph use "C:\Users\User\Documents\Operation Holy Grail\Phase 4\Data\Pork n beef import\MPSA\Stata\m5m6.gph"

graph export "C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/m5m7.png", replace width(1600) height(1000)
graph export "C:/Users/User/Documents/Operation Holy Grail/Phase 4/Data/Pork n beef import/MPSA/m6m8.png", replace width(1600) height(1000)

//Panel mlogit regression//
recode imports (-1=2)(1=1)(0=3), gen(imports_cat)
label define imports_cat 1 "Pro-imports" 2 "Anti-imports" 3 "Neutral", modify
label values imports_cat imports_cat
xtset ambition
xtmlogit imports_cat i.government i.pub_oriented i.disboth ib(1).ls_type ib(3).session
fitstat
mlogtest, iia

//Mixed-effects interval regression//
meintreg imports imports i.government || ambition:
meintreg imports imports i.government i.pub_oriented i.disboth ib(1).ls_type ib(3).session || ambition:
meintreg imports imports i.KMT##i.disboth i.pub_oriented ib(1).ls_type ib(3).session || ambition:
estat icc
estat ic

//Mixed-effects linear regression//
xtmixed imports i.government || ambition: government
xtmixed imports i.government i.pub_oriented i.disboth ib(1).ls_type ib(3).session || ambition: government
xtmixed imports i.government##i.disboth i.pub_oriented ib(1).ls_type ib(3).session || ambition:
estat ic
mltrsq

//Margins plots//
margins government, atmeans predict(outcome(1))
margins ls_type, atmeans predict(outcome(1))
margins pub_oriented, atmeans predict(outcome(1))
margins session, atmeans predict(outcome(1))
margins male, atmeans predict(outcome(1))
margins disboth, atmeans predict(outcome(1))
margins government, atmeans predict(outcome(2))
margins ls_type, atmeans predict(outcome(2))
margins pub_oriented, atmeans predict(outcome(2))
marginsplot

//Aggregate dataset//
label define us_appraise_m 1 "Pro-U.S." 2 "U.S.-skeptic" 3 "Unknown", modify
label define us_appraise_t 1 "Pro-U.S." 2 "U.S.-skeptic" 3 "Unknown", modify
label define pub_oriented_m 0 "No" 1 "Yes", modify
label define pub_oriented_t 0 "No" 1 "Yes", modify
label define government_m 0 "No" 1 "Yes", modify
label define government_t 0 "No" 1 "Yes", modify
label define male_m 0 "Female" 1 "Male", modify
label define male_t 0 "Female" 1 "Male", modify
label define ls_type_m 1 "Party-list" 2 "District-elected" 3 "Aborigine", modify
label define ls_type_t 1 "Party-list" 2 "District-elected" 3 "Aborigine", modify
label define session_m 1 "Plenary" 2 "Committee" 3 "PH", modify
label define session_t 1 "Plenary" 2 "Committee" 3 "Caucus", modify
label define disboth_m 0 "No" 1 "Yes", modify
label define disboth_t 0 "No" 1 "Yes", modify
label values us_appraise_m us_appraise_m
label values us_appraise_t us_appraise_t
label values pub_oriented_m pub_oriented_m
label values pub_oriented_t pub_oriented_t
label values government_m government_m
label values government_t government_t
label values male_m male_m
label values male_t male_t
label values ls_type_m ls_type_m
label values ls_type_t ls_type_t
label values session_m session_m
label values session_t session_t
label values disboth_m disboth_m
label values disboth_t disboth_t
label variable us_appraise_m "Legislators' appraisals of Washington in Ma era"
label variable us_appraise_t "Legislators' appraisals of Washington in Tsai era"
label variable pub_oriented_m "Public-related terms (Ma era)"
label variable pub_oriented_t "Public-related terms (Tsai era)"
label variable government_m "Ruling party legislators in Ma era"
label variable government_t "Ruling party legislators in Tsai era"
label variable male_m "Male legislators in Ma era"
label variable male_t "Male legislators in Tsai era"
label variable ls_type_m "Legislator's type (Ma era)"
label variable ls_type_t "Legislator's type (Tsai era)"
label variable session_m "Session's type (Ma era)"
label variable session_t "Session's type (Tsai era)"
label variable disboth_m "Legislators gave speeches in both periods (Ma era)"
label variable disboth_t "Legislators gave speeches in both periods (Tsai era)"
//Coefficient plot for Model 3 & 4//
mlogit us_appraise_m i.government_m i.pub_oriented_m i.disboth_m ib(1).ls_type_m ib(3).session_m i.male_m, baseoutcome(2)
estimates store m1
mlogit us_appraise_t i.government_t i.pub_oriented_t i.disboth_t ib(1).ls_type_t ib(3).session_t i.male_t, baseoutcome(2)
estimates store m2
coefplot m1, bylabel(Model 3 (Ma era)) mcolor(dimgrey) ciopts(color(dimgrey)) || m2, bylabel (Model 4 (Tsai era)) xline(0) nolabel ///
keep(1.government_m 1.government_t 0.pub_oriented_t 0.disboth_t 2.ls_type_t 1.session_t 1.male_t 2:)

