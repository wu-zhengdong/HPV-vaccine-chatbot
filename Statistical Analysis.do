
///import database
use 'the storage path of the database'

///////// Table 1 | Baseline characteristics of parents and their daughters by classes and individuals

///classes level
preserve
bysort treatment grade class: keep if _n == 1
bysort treatment: tabulate grade, missing
restore

preserve
bysort treatment citytype class: keep if _n == 1
bysort treatment: tabulate citytype, missing
restore

///Individual level-Participants’ daughter
tab treatment grade if time==0, chi2 row
ttest Girls_age if time==0,by(treatment) 
tab treatment only_child if time==0, chi2 row
tab treatment Left_behind if time==0, chi2 row
tab treatment sexual_education if time==0, chi2 row
tab treatment influenza_vaccination if time==0, chi2 row

///Individual level-Participants
tab treatment citytype if time==0, chi2 row
tab treatment relationship if time==0, chi2 row
ttest newage if time==0,by(treatment) 
tab treatment education if time==0, chi2 row
tab treatment occupation if time==0, chi2 row
tab treatment income if time==0, chi2 row
tab treatment 母亲接种 if time==0, chi2 row



////////  Table 2 | Primary and secondary outcomes analyses: HPV vaccination uptake among middle school girls, health professional
////////  consultations, parental HPV-related literacy, vaccine confidence, and willingness to vaccinate
xtset class
///HPV vaccination uptake
tab treatment vac_bahaviour, chi2 row
by treatment, sort: ci proportions vac_bahaviour, wald
xtgee vac_bahaviour treatment ib3.citytype ib2.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship ///
newage ib2.m_vaccinated ib3.education i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

///Health professional consultation
tab treatment bi_consultation, chi2 row
by treatment, sort: ci proportions bi_consultation, wald
xtgee bi_consultation treatment ib3.citytype ib2.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship ///
newage i.m_vaccinated ib3.education i.occupation ib4.income, family(binomial) link(log) corr(independent) vce(robust) eform

///Willingness to vaccinate
by treatment,sort:tab time bi_willingness,chi2 row
cs bi_willingness time if treatment==1
cs bi_willingness time if treatment==0
xtgee bi_willingness treatment time treatment_time ib3.citytype newage i.relationship i.m_vaccinated i.education, family(binomial) link(log) corr(exchangeable) vce(robust) eform 

///High confidence
by treatment,sort:tab time high_confidence,chi2 row
cs high_confidence time if treatment==1
cs high_confidence time if treatment==0
xtgee high_confidence treatment time treatment_time ib3.citytype i.influenza_vaccination i.relationship newage i.m_vaccinated ///
ib3.education i.occupation, family(binomial) link(log) corr(exchangeable) vce(robust) eform

/// HPV related lireracy
by treatment,sort:ttest literacy,by(time)
by treatment,sort:ttest knowledge_literacy,by(time)
by treatment,sort:ttest rumor_literacy,by(time)
xtmixed literacy treatment time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)
xtmixed knowledge_literacy treatment time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)
xtmixed rumor_literacy treatment time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)


///////// Fig. 2 | Stratified multivariable Generalized estimating equation (GEE) to compare HPV vaccination uptake of two arms.
tab treatment vac_bahaviour, chi2 row

foreach var of varlist citytype Left_behind relationship m_vaccinated education occupation income {
  by `var',sort:tab treatment vac_bahaviour, chi2 row
}

xtgee vac_bahaviour treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship ///
newage ib2.m_vaccinated ib3.education i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

by education,sort: xtgee vac_bahaviour treatment i.only_child influenza_vaccination i.sexual_education i.Left_behind ib3.citytype newage ///
i.relationship i.m_vaccinated i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

by income,sort: xtgee vac_bahaviour treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind ib3.citytype ///
newage i.relationship i.m_vaccinated i.education i.occupation, family(binomial) link(log) corr(exchangeable) vce(robust) eform

by occupayion,sort: xtgee vac_bahaviour treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind ib3.citytype ///
newage i.relationship i.m_vaccinated i.education i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

by citytype,sort: xtgee vac_bahaviour treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind newage i.relationship ///
ib2.m_vaccinated i.education i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

by Left_behind, xtgee vac_bahaviour treatment i.only_child i.influenza_vaccination i.sexual_education ib3.citytype newage i.relationship ///
i.m_vaccinated i.education i.occupation i.income if Left_behind==0, family(binomial) link(log) corr(exchangeable) vce(robust) eform

by relationship,sort: xtgee vac_bahaviour treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind ib3.citytype ///
newage i.m_vaccinated i.education i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

by relationship,sort: xtgee vac_bahaviour treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind ib3.citytype ///
newage i.relationship i.education i.occupation i.income if 母亲接种 ==2, family(binomial) link(log) corr(exchangeable) vce(robust) eform


////////// Extended Data Fig. 1 | Stratified multivariable Generalized estimating equation (GEE) to compare health professional consultation of two arms.
tab treatment bi_consultation, chi2 row
foreach var of varlist citytype Left_behind relationship m_vaccinated education occupation income {
  by `var',sort:tab treatment bi_consultation, chi2 row
}

xtgee bi_consultation treatment ib3.citytype ib2.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship ///
newage i.m_vaccinated ib3.education i.occupation ib4.income, family(binomial) link(log) corr(independent) vce(robust) eform

xtgee bi_consultation treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship i.m_vaccinated ib3.education i.occupation ib4.income if citytype==1, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship i.m_vaccinated ib3.education i.occupation ib4.income if citytype==2, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship newage i.m_vaccinated ib3.education i.occupation if citytype==3, family(binomial) link(log) corr(independent) vce(robust) eform

xtgee bi_consultation treatment ib3.citytype i.only_child i.relationship i.m_vaccinated ib3.education i.occupation if Left_behind==1, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.relationship i.m_vaccinated ib3.education i.occupation ib4.income if Left_behind==0, family(binomial) link(log) corr(independent) vce(robust) eform

xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind newage i.m_vaccinated ib3.education i.occupation ib4.income if relationship==1, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind newage i.m_vaccinated ib3.education i.occupation ib4.income if relationship==2, family(binomial) link(log) corr(independent) vce(robust) eform

xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind newage i.relationship ib3.education i.occupation ib4.income if m_vaccinated==1, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind newage i.relationship ib3.education i.occupation ib4.income if m_vaccinated==0, family(binomial) link(log) corr(independent) vce(robust) eform

xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind newage i.relationship ib3.education i.m_vaccinated ib4.income if occupation==1, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind newage i.relationship ib3.education i.m_vaccinated ib4.income if occupation==0, family(binomial) link(log) corr(independent) vce(robust) eform

xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education newage i.relationship i.occupation i.m_vaccinated ib4.income if education==1, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination newage i.relationship i.occupation i.m_vaccinated ib4.income if education==2, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education newage i.relationship i.occupation i.m_vaccinated ib4.income if education==3, family(binomial) link(log) corr(independent) vce(robust) eform

xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education newage i.relationship i.occupation i.m_vaccinated ib3.education if income==1, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination newage i.relationship i.occupation i.m_vaccinated ib3.education if income==2, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment ib3.citytype i.only_child i.relationship i.occupation i.m_vaccinated ib3.education if income==3, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment ib3.citytype i.relationship i.m_vaccinated i.education i.income if income==4, family(binomial) link(log) corr(independent) vce(robust) eform



///////// Extended Data Fig. 2 | Stratified multivariable mixed effect model to compare HPV literacy of two arms. 
by treatment, sort: ttest literacy ,by(time)
foreach var of varlist citytype Left_behind relationship m_vaccinated education occupation income {
  by `var' treatment,sort: ttest literacy ,by(time)
}

xtmixed literacy treatment time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)

by citytype,sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship newage i.m_vaccinated i.education i.occupation i.income, vce(cluster class)

by Left_behind,sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.relationship newage i.m_vaccinated i.education i.occupation i.income, vce(cluster class)

by relationship,sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.Left_behind newage i.m_vaccinated i.education i.occupation i.income, vce(cluster class)

by m_vaccinated,sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.Left_behind newage i.relationship i.education i.occupation i.income, vce(cluster class)

by education,sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.Left_behind newage i.relationship i.m_vaccinated i.occupation i.income, vce(cluster class)

by occupation,sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.Left_behind newage i.relationship i.m_vaccinated i.education i.income, vce(cluster class)

by income,sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.Left_behind newage i.relationship i.m_vaccinated i.education i.occupation, vce(cluster class)



//////// Extended Data Table 2 | Proportions of parents correctly answered HPV or its vaccine related statements.
foreach var of varlist statements1 statements2 statements3 statements4 statements5 statements6 statements7 statements8 statements9 statements10 {
  by time,sort:tab treatment `var', chi2 row
}


//////// Extended Data Table 3 | Sensitivity analysis
/// HPV vaccination uptake
tab chatbot vac_bahaviour, chi2 row
by chatbot, sort: ci proportions vac_bahaviour, wald
xtgee vac_bahaviour chatbot ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship ///
newage ib2.m_vaccinated ib3.education i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

/// Health professional consultation
tab chatbot bi_consultation, chi2 row
by chatbot, sort: ci proportions bi_consultation, wald
xtgee bi_consultation chatbot ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
newage i.m_vaccinated ib3.education i.occupation ib4.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

/// Willingness to vaccinate
by chatbot,sort:tab time bi_willingness,chi2 row
cs bi_willingness time if chatbot==1
cs bi_willingness time if chatbot==0
xtgee bi_willingness chatbot time treatment_time ib3.citytype i.only_child newage i.relationship i.m_vaccinated i.occupation i.Left_behind, family(binomial) link(log) corr(exchangeable) vce(robust) eform 

/// High confidence
by chatbot,sort:tab time high_confidence,chi2 row
cs high_confidence time if chatbot==1
cs high_confidence time if chatbot==0
xtgee high_confidence chatbot time treatment_time ib3.citytype i.only_child newage i.relationship i.m_vaccinated ///
i.occupation i.Left_behind, family(binomial) link(log) corr(exchangeable) vce(robust) eform


/// HPV related lireracy
by chatbot,sort:ttest literacy,by(time)
by chatbot,sort:ttest knowledge_literacy,by(time)
by chatbot,sort:ttest rumor_literacy,by(time)
xtmixed literacy chatbot time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)
xtmixed knowledge_literacy chatbot time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)
xtmixed rumor_literacy chatbot time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)











