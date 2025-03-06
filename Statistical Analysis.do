///import database
use 'the storage path of the database of ...'

************************************************
** Table 1 | Baseline characteristics of parents and their daughters by classes and individuals
************************************************

* Class-level analysis
preserve
bysort treatment grade class: keep if _n == 1
bysort treatment: tabulate grade, missing
restore

preserve
bysort treatment citytype class: keep if _n == 1
bysort treatment: tabulate citytype, missing
restore

* Individual-level analysis: Daughters' characteristics
tab treatment grade if time==0, chi2 row
ttest Girls_age if time==0, by(treatment) 
tab treatment only_child if time==0, chi2 row
tab treatment Left_behind if time==0, chi2 row
tab treatment sexual_education if time==0, chi2 row
tab treatment influenza_vaccination if time==0, chi2 row

* Individual-level analysis: Parents' characteristics
tab treatment citytype if time==0, chi2 row
tab treatment relationship if time==0, chi2 row
ttest newage if time==0, by(treatment) 
tab treatment education if time==0, chi2 row
tab treatment occupation if time==0, chi2 row
tab treatment income if time==0, chi2 row
tab treatment m_vaccinated if time==0, chi2 row


************************************************
** Table 2 | Effect of HPV vaccine chatbot intervention on primary and secondary outcomes
************************************************
xtset class

* Primary outcome: HPV vaccine receipt or scheduled appointment among female students
tab treatment vac_bahaviour, chi2 row
by treatment, sort: ci proportions vac_bahaviour, wald
xtgee vac_bahaviour treatment ib3.citytype ib2.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship ///
      newage ib2.m_vaccinated ib3.education i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

* Secondary outcome: HPV vaccination-specific consultation
tab treatment bi_consultation, chi2 row
by treatment, sort: ci proportions bi_consultation, wald
xtgee bi_consultation treatment ib3.citytype ib2.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship ///
      newage i.m_vaccinated ib3.education i.occupation ib4.income, family(binomial) link(log) corr(independent) vce(robust) eform

* Secondary outcome: Parental willingness to vaccinate their daughter
by treatment, sort: tab time bi_willingness, chi2 row
cs bi_willingness time if treatment==1
cs bi_willingness time if treatment==0
xtgee bi_willingness treatment time treatment_time ib3.citytype newage i.relationship i.m_vaccinated i.education, ///
      family(binomial) link(log) corr(exchangeable) vce(robust) eform 

* Secondary outcome: HPV vaccine confidence
by treatment, sort: tab time high_confidence, chi2 row
cs high_confidence time if treatment==1
cs high_confidence time if treatment==0
xtgee high_confidence treatment time treatment_time ib3.citytype i.influenza_vaccination i.relationship newage i.m_vaccinated ///
      ib3.education i.occupation, family(binomial) link(log) corr(exchangeable) vce(robust) eform

* Secondary outcome: HPV-related literacy
by treatment, sort: ttest literacy, by(time)
by treatment, sort: ttest knowledge_literacy, by(time)
by treatment, sort: ttest rumor_literacy, by(time)
xtmixed literacy treatment time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
        newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)
xtmixed knowledge_literacy treatment time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
        newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)
xtmixed rumor_literacy treatment time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
        newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)


************************************************
** Fig. 2 | Stratified generalized estimating equation (GEE) to compare HPV vaccine receipt or scheduled appointment between two arms
************************************************

* Overall comparison
tab treatment vac_bahaviour, chi2 row

* Stratified analyses by demographic variables
foreach var of varlist citytype Left_behind relationship m_vaccinated education occupation income {
  by `var', sort: tab treatment vac_bahaviour, chi2 row
}

* Overall adjusted model
xtgee vac_bahaviour treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship ///
      newage ib2.m_vaccinated ib3.education i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

* Stratified models by education
by education, sort: xtgee vac_bahaviour treatment i.only_child influenza_vaccination i.sexual_education i.Left_behind ib3.citytype newage ///
      i.relationship i.m_vaccinated i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

* Stratified models by income
by income, sort: xtgee vac_bahaviour treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind ib3.citytype ///
      newage i.relationship i.m_vaccinated i.education i.occupation, family(binomial) link(log) corr(exchangeable) vce(robust) eform

* Stratified models by occupation
by occupation, sort: xtgee vac_bahaviour treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind ib3.citytype ///
      newage i.relationship i.m_vaccinated i.education i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

* Stratified models by city type
by citytype, sort: xtgee vac_bahaviour treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind newage i.relationship ///
      ib2.m_vaccinated i.education i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

* Stratified models by left-behind status
by Left_behind, sort: xtgee vac_bahaviour treatment i.only_child i.influenza_vaccination i.sexual_education ib3.citytype newage i.relationship ///
      i.m_vaccinated i.education i.occupation i.income if Left_behind==0, family(binomial) link(log) corr(exchangeable) vce(robust) eform

* Stratified models by relationship
by relationship, sort: xtgee vac_bahaviour treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind ib3.citytype ///
      newage i.m_vaccinated i.education i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform


************************************************
** Extended Data Fig. 1 | Stratified generalized estimating equation (GEE) to compare HPV vaccination-specific consultation with health professionals between two arms
************************************************

* Overall comparison
tab treatment bi_consultation, chi2 row

* Stratified analyses by demographic variables
foreach var of varlist citytype Left_behind relationship m_vaccinated education occupation income {
  by `var', sort: tab treatment bi_consultation, chi2 row
}

* Overall adjusted model
xtgee bi_consultation treatment ib3.citytype ib2.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship ///
      newage i.m_vaccinated ib3.education i.occupation ib4.income, family(binomial) link(log) corr(independent) vce(robust) eform

* Stratified models by city type
foreach level in 1 2 3 {
  xtgee bi_consultation treatment i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship newage i.m_vaccinated ib3.education i.occupation ib4.income if citytype==`level', family(binomial) link(log) corr(independent) vce(robust) eform
}

* Stratified models by left-behind status
xtgee bi_consultation treatment ib3.citytype i.only_child i.relationship i.m_vaccinated ib3.education i.occupation if Left_behind==1, family(binomial) link(log) corr(independent) vce(robust) eform
xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.relationship i.m_vaccinated ib3.education i.occupation ib4.income if Left_behind==0, family(binomial) link(log) corr(independent) vce(robust) eform

* Stratified models by relationship
foreach level in 1 2 {
  xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind newage i.m_vaccinated ib3.education i.occupation ib4.income if relationship==`level', family(binomial) link(log) corr(independent) vce(robust) eform
}

* Stratified models by mother's vaccination status
foreach level in 0 1 {
  xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind newage i.relationship ib3.education i.occupation ib4.income if m_vaccinated==`level', family(binomial) link(log) corr(independent) vce(robust) eform
}

* Stratified models by occupation
foreach level in 0 1 {
  xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind newage i.relationship ib3.education i.m_vaccinated ib4.income if occupation==`level', family(binomial) link(log) corr(independent) vce(robust) eform
}

* Stratified models by education
foreach level in 1 2 3 {
  xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education newage i.relationship i.occupation i.m_vaccinated ib4.income if education==`level', family(binomial) link(log) corr(independent) vce(robust) eform
}

* Stratified models by income
foreach level in 1 2 3 4 {
  xtgee bi_consultation treatment ib3.citytype i.only_child i.influenza_vaccination i.sexual_education newage i.relationship i.occupation i.m_vaccinated ib3.education if income==`level', family(binomial) link(log) corr(independent) vce(robust) eform
}


************************************************
** Extended Data Fig. 2 | Stratified mixed-effects model to compare HPV literacy between two arms
************************************************

* Overall comparison
by treatment, sort: ttest literacy, by(time)

* Stratified analyses by demographic variables
foreach var of varlist citytype Left_behind relationship m_vaccinated education occupation income {
  by `var' treatment, sort: ttest literacy, by(time)
}

* Overall adjusted model
xtmixed literacy treatment time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
        newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)

* Stratified models by city type
by citytype, sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship newage i.m_vaccinated i.education i.occupation i.income, vce(cluster class)

* Stratified models by left-behind status
by Left_behind, sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.relationship newage i.m_vaccinated i.education i.occupation i.income, vce(cluster class)

* Stratified models by relationship
by relationship, sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.Left_behind newage i.m_vaccinated i.education i.occupation i.income, vce(cluster class)

* Stratified models by mother's vaccination status
by m_vaccinated, sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.Left_behind newage i.relationship i.education i.occupation i.income, vce(cluster class)

* Stratified models by education
by education, sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.Left_behind newage i.relationship i.m_vaccinated i.occupation i.income, vce(cluster class)

* Stratified models by occupation
by occupation, sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.Left_behind newage i.relationship i.m_vaccinated i.education i.income, vce(cluster class)

* Stratified models by income
by income, sort: xtmixed literacy treatment time treatment_time i.only_child i.influenza_vaccination i.sexual_education i.citytype i.Left_behind newage i.relationship i.m_vaccinated i.education i.occupation, vce(cluster class)


************************************************
** Extended Data Table 1 | Comparative characteristics of enrolled and non-enrolled participants
************************************************

use "the storage path of the database of ...", clear

* Categorical variables comparison
foreach var of varlist grade only_child sexual_education influenza_vaccination citytype relationship education occupation income m_vaccinated {
  by time, sort: tab enrolled `var', chi2 row
}

* Continuous variables comparison
ttest girls_age, by(enrolled)
ttest newage, by(enrolled)


************************************************
** Extended Data Table 2 | Proportions of parents correctly answered HPV or its vaccine related statements
************************************************

use "the storage path of the database of ...", clear

foreach var of varlist statements1-statements10 {
  by time, sort: tab treatment `var', chi2 row
}


************************************************
** Extended Data Table 3 | Comparative characteristics of chatbot users and non-users in the intervention group
************************************************

* Categorical variables comparison at baseline
foreach var of varlist grade only_child sexual_education influenza_vaccination citytype relationship education occupation income m_vaccinated {
  tab chatbot_use `var' if time==0, chi2 row
}

* Continuous variables comparison at baseline
ttest girls_age if time==0, by(chatbot_use)
ttest newage if time==0, by(chatbot_use)


************************************************
** Extended Data Table 5 | Per-protocol analysis of HPV vaccine chatbot intervention effects on primary and secondary outcomes
************************************************

* Primary outcome: HPV vaccine receipt or scheduled appointment among female students
tab chatbot vac_bahaviour, chi2 row
by chatbot, sort: ci proportions vac_bahaviour, wald
xtgee vac_bahaviour chatbot ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship ///
      newage ib2.m_vaccinated ib3.education i.occupation i.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

* Secondary outcome: HPV vaccination-specific consultation
tab chatbot bi_consultation, chi2 row
by chatbot, sort: ci proportions bi_consultation, wald
xtgee bi_consultation chatbot ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
      newage i.m_vaccinated ib3.education i.occupation ib4.income, family(binomial) link(log) corr(exchangeable) vce(robust) eform

* Secondary outcome: Parental willingness to vaccinate their daughter
by chatbot, sort: tab time bi_willingness, chi2 row
cs bi_willingness time if chatbot==1
cs bi_willingness time if chatbot==0
xtgee bi_willingness chatbot time treatment_time ib3.citytype i.only_child newage i.relationship i.m_vaccinated i.occupation i.Left_behind, ///
      family(binomial) link(log) corr(exchangeable) vce(robust) eform 

* Secondary outcome: HPV vaccine confidence
by chatbot, sort: tab time high_confidence, chi2 row
cs high_confidence time if chatbot==1
cs high_confidence time if chatbot==0
xtgee high_confidence chatbot time treatment_time ib3.citytype i.only_child newage i.relationship i.m_vaccinated ///
      i.occupation i.Left_behind, family(binomial) link(log) corr(exchangeable) vce(robust) eform

* Secondary outcome: HPV-related literacy
by chatbot, sort: ttest literacy, by(time)
by chatbot, sort: ttest knowledge_literacy, by(time)
by chatbot, sort: ttest rumor_literacy, by(time)
xtmixed literacy chatbot time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
      newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)
xtmixed knowledge_literacy chatbot time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
      newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)
xtmixed rumor_literacy chatbot time treatment_time ib3.citytype i.only_child i.influenza_vaccination i.sexual_education i.Left_behind i.relationship /// 
      newage i.m_vaccinated ib3.education i.occupation i.income, vce(cluster class)











