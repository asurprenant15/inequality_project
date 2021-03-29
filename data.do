clear all
set more off


global datasets "cl90 cl92 cl94 cl96 cl98 cl00 cl03 cl06 cl09 cl11 cl13 cl15"

global vars "hid pid dname sex age relation hours1 pile pils unemp deflator emp educ wexptl pwgt ppopwgt"
global varsh "hid hile hils nhhmem hitp hic hits dhi hpopwgt"

program define figure6_data
	foreach ccyy in $datasets {
		use $vars using $`ccyy'p, clear
		
		//income is said to be reported in monthly amounts and hours are weekly
		// since no data is provided on the number of weeks worked per year, I approximate it with 4 per month and 52 per year
		keep if inrange(age, 25, 60) & hours >= 5
		
		
		//variance of log hourly wages for men and women
		gen hourwageM_`ccyy' = (pile + 0.67*pils) / (52*hours)
		replace hourwageM_`ccyy'=. if hourwageM_`ccyy'<=0 | sex != 1
		
		gen hourwageF_`ccyy' = (pile + 0.67*pils) / (52*hours)
		replace hourwageF_`ccyy'=. if hourwageF_`ccyy'<=0 | sex == 1

		
		gen logWageM_`ccyy' = log(hourwageM_`ccyy')
		quietly sum logWageM_`ccyy' [w= ppopwgt]
		quietly return list
		gen varLogWageM_`ccyy' = r(Var)
		
		gen logWageF_`ccyy' = log(hourwageF_`ccyy')
		quietly sum logWageF_`ccyy' [w= ppopwgt]
		quietly return list
		gen varLogWageF_`ccyy' = r(Var)

		
		// variance of log anual hours for men and women
		gen logAnualHoursM_`ccyy' = log(52*hours)
		replace logAnualHoursM_`ccyy'=. if hourwageM_`ccyy'<=0 | sex != 1
		quietly sum logAnualHoursM_`ccyy' [w = ppopwgt]
		quietly return list
		gen varLogAnualHoursM_`ccyy' = r(Var)
		
		gen logAnualHoursF_`ccyy' = log(52*hours)
		replace logAnualHoursF_`ccyy'=. if hourwageF_`ccyy'<=0 | sex == 1
		quietly sum logAnualHoursF_`ccyy' [w= ppopwgt]
		quietly return list
		gen varLogAnualHoursF_`ccyy' = r(Var)
		
		
		//correlation between log hours and log wages
		quietly correlate logAnualHoursF_`ccyy' logWageF_`ccyy' [w= ppopwgt]
		quietly return list
		gen cohwF_`ccyy' = r(rho)
		
		quietly correlate logAnualHoursM_`ccyy' logWageM_`ccyy' [w= ppopwgt]
		quietly return list
		gen cohwM_`ccyy' = r(rho)
		
		
		//Variance of log anual earnings for men and women
		gen logAnualEarningsM_`ccyy' = log(pile + 0.67*pils)
		replace logAnualEarningsM_`ccyy'=. if hourwageM_`ccyy'<=0 | sex != 1
		quietly sum logAnualEarningsM_`ccyy' [w= ppopwgt]
		quietly return list
		gen varLogAnualEarningsM_`ccyy' = r(Var)
		
		gen logAnualEarningsF_`ccyy' = log(pile + 0.67*pils)
		replace logAnualEarningsF_`ccyy'=. if hourwageF_`ccyy'<=0 | sex == 1
		quietly sum logAnualEarningsF_`ccyy' [w= ppopwgt]
		quietly return list
		gen varLogAnualEarningsF_`ccyy' = r(Var)
		
		//put everything in a table
		tabstat varLogWageF_`ccyy' varLogWageM_`ccyy' varLogAnualHoursF_`ccyy' varLogAnualHoursM_`ccyy' cohwF_`ccyy' cohwM_`ccyy' varLogAnualEarningsF_`ccyy' varLogAnualEarningsM_`ccyy'

	
	}

end
figure6_data


program define figure7_data
	//calculate base 
	use $vars using $cl90p, clear
		keep if inrange(age, 25, 60) & hours >= 5 & sex == 1 & pile > 0
		
		//Create Base earnings deciles p0-p10, p45-p55, and p90-p100
		gen earnings_base = (pile + 0.67*pils)
		pctile pct = earnings_base [w= ppopwgt], nq(20) 
		quietly return list
		gen p10 = r(r2)
		gen p45 = r(r9)
		gen p55 = r(r11)
		gen p90 = r(r18)
		local baseP50 = r(r10)
		
		quietly sum earnings_base [w= ppopwgt]
		quietly return list
		gen p0 = r(min)
		gen p100 = r(max)
		
		//earnings
		
		gen p0_p10_base = earnings_base
		replace p0_p10_base=. if p0_p10_base > p10
		quietly sum p0_p10_base [w= ppopwgt]
		quietly return list
		local low_base = r(mean)
		
		gen p45_p55_base = earnings_base
		replace p45_p55_base=. if p45_p55_base > p55 | p45_p55_base < p45
		quietly sum p45_p55_base [w= ppopwgt]
		quietly return list
		local mid_base = r(mean)
		
		gen p90_p100_base = earnings_base
		replace p90_p100_base=. if p90_p100_base < p90
		quietly sum p90_p100_base [w= ppopwgt]
		quietly return list
		local high_base = r(mean)
		
		//hours
		
		gen p0_p10_hours_base = hours if earnings_base < p10
		quietly sum p0_p10_hours_base [w= ppopwgt]
		quietly return list
		local low_hours_base = r(mean)
		
		gen p45_p55_hours_base = hours if earnings_base > p45 & earnings_base < p55
		quietly sum p45_p55_hours_base [w= ppopwgt]
		quietly return list
		local mid_hours_base = r(mean)
		
		gen p90_p100_hours_base = hours if earnings_base > p90
		quietly sum p90_p100_hours_base [w= ppopwgt]
		quietly return list
		local high_hours_base = r(mean)
		
		//wages
		
		local low_wages_base = `low_base' / (52*`low_hours_base')
		local high_wages_base = `high_base' / (52*`high_hours_base')
		local mid_wages_base = `mid_base' / (52*`mid_hours_base')

		
		

	foreach ccyy in $datasets {
		use $vars using $`ccyy'p, clear
		keep if inrange(age, 25, 60) & 52*hours >= 260 & sex == 1 & pile > 0
		
		//Create earnings deciles p0-p10, p45-p55, and p90-p100
		gen earnings_`ccyy' = (pile + 0.67*pils)
		pctile pct = earnings_`ccyy' [w= ppopwgt], nq(20)
		quietly return list
		gen p10_`ccyy' = r(r2)
		gen p45_`ccyy' = r(r9)
		gen p50_`ccyy' = r(r10)
		gen p55_`ccyy' = r(r11)
		gen p90_`ccyy' = r(r18)
		
		quietly sum earnings_`ccyy' [w= ppopwgt]
		quietly return list
		gen p0_`ccyy' = r(min)
		gen p100_`ccyy' = r(max)
		
		//earnings
		
		gen p0_p10_`ccyy' = earnings_`ccyy'
		replace p0_p10_`ccyy' =. if p0_p10_`ccyy' > p10_`ccyy'
		quietly sum p0_p10_`ccyy' [w= ppopwgt]
		quietly return list
		gen low_`ccyy' = r(mean)
		
		gen p45_p55_`ccyy' = earnings_`ccyy'
		replace p45_p55_`ccyy' =. if p45_p55_`ccyy' > p55_`ccyy' | p45_p55_`ccyy' < p45_`ccyy'
		quietly sum p45_p55_`ccyy' [w= ppopwgt]
		quietly return list
		gen mid_`ccyy' = r(mean)
		
		gen p90_p100_`ccyy' = earnings_`ccyy'
		replace p90_p100_`ccyy' =. if p90_p100_`ccyy' < p90_`ccyy'
		quietly sum p90_p100_`ccyy' [w= ppopwgt]
		quietly return list
		gen high_`ccyy' = r(mean)
		
		//hours
		
		gen p0_p10_hours_`ccyy' = hours if earnings_`ccyy' < p10_`ccyy'
		quietly sum p0_p10_hours_`ccyy' [w= ppopwgt]
		quietly return list
		gen low_hours_`ccyy' = r(mean)
		
		gen p45_p55_hours_`ccyy' = hours if earnings_`ccyy' > p45_`ccyy' & earnings_`ccyy' < p55_`ccyy'
		quietly sum p45_p55_hours_`ccyy' [w= ppopwgt]
		quietly return list
		gen mid_hours_`ccyy' = r(mean)
		
		gen p90_p100_hours_`ccyy' = hours if earnings_`ccyy' > p90_`ccyy'
		quietly sum p90_p100_hours_`ccyy' [w= ppopwgt]
		quietly return list
		gen high_hours_`ccyy' = r(mean)
		
		//wages
		
		
		gen low_wages_`ccyy' = low_`ccyy' / (52*low_hours_`ccyy')
		gen high_wages_`ccyy' = high_`ccyy' / (52*high_hours_`ccyy')
		gen mid_wages_`ccyy' = mid_`ccyy'/ (52*mid_hours_`ccyy')

		
		//calculate relative change in earnings since base year
		//difference of logs
		
		gen lowPercentChange`ccyy' = log(low_`ccyy') -  log(`low_base')
		replace lowPercentChange`ccyy' =round(lowPercentChange`ccyy', 0.001)
		gen midPercentChange`ccyy' = log(mid_`ccyy') - log(`mid_base')
		replace midPercentChange`ccyy' = round(midPercentChange`ccyy', 0.001)
		gen HighPercentChange`ccyy' = log(high_`ccyy') - log(`high_base')
		replace HighPercentChange`ccyy' = round(HighPercentChange`ccyy', 0.001)
					
		
		tabstat lowPercentChange`ccyy' midPercentChange`ccyy' HighPercentChange`ccyy'

		
		//calculate relative change in hours wrt base year
		
		gen lowHoursChange`ccyy' = log(low_hours_`ccyy') -  log(`low_hours_base')
		replace lowHoursChange`ccyy' =round(lowHoursChange`ccyy', 0.001)
		gen midHoursChange`ccyy' = log(mid_hours_`ccyy') - log(`mid_hours_base')
		replace midHoursChange`ccyy' = round(midHoursChange`ccyy', 0.001)
		gen HighHoursChange`ccyy' = log(high_hours_`ccyy') - log(`high_hours_base')
		replace HighHoursChange`ccyy' = round(HighHoursChange`ccyy', 0.001)
		
		
		tabstat lowHoursChange`ccyy' midHoursChange`ccyy' HighHoursChange`ccyy'
		
		//calculate relative change in wages wrt base year
		
		gen lowWagesChange`ccyy' = log(low_wages_`ccyy') -  log(`low_wages_base')
		replace lowWagesChange`ccyy' =round(lowWagesChange`ccyy', 0.001)
		gen midWagesChange`ccyy' = log(mid_wages_`ccyy') - log(`mid_wages_base')
		replace midWagesChange`ccyy' = round(midWagesChange`ccyy', 0.001)
		gen HighWagesChange`ccyy' = log(high_wages_`ccyy') - log(`high_wages_base')
		replace HighWagesChange`ccyy' = round(HighWagesChange`ccyy', 0.001)
		
		
		tabstat lowWagesChange`ccyy' midWagesChange`ccyy' HighWagesChange`ccyy'
		}
end		
figure7_data

program define unemployment_over_time 
	foreach ccyy in $datasets {
		use $vars using $`ccyy'p, clear
		keep if inrange(age, 25, 60)
		gen unemp`ccyy' = unemp
		gen emp`ccyy' = emp
		quietly sum unemp`ccyy' [w= ppopwgt]
		quietly return list
		gen unempNum`ccyy' = r(sum)
		qui sum emp`ccyy' [w= ppopwgt]
		qui return list
		gen empNum`ccyy' = r(sum)
		
		gen unempRate`ccyy' = (unempNum`ccyy' / (unempNum`ccyy' + empNum`ccyy'))*100
		tabstat unempRate`ccyy'
	}
	
end
unemployment_over_time

program define figure8_data
	foreach ccyy in $datasets {
		//using sample B/household data
		use $vars using $`ccyy'p, clear
		qui merge m:1 hid using $`ccyy'h, keepusing($varsh)
		keep if inrange(age,25,54) & (hile + hils*.67) > 0 & relation<=2200
		
		
		
		
		//using the LIS equivalence scale
		gen eq_hh_earnings_`ccyy' = (hile + hils*.67)/(nhhmem^0.5)
		
		//variance of equivalized log HH earnings
		
		gen log_eq_hh_earnings_`ccyy' = log(eq_hh_earnings_`ccyy')
		qui sum log_eq_hh_earnings_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen velhhe_`ccyy' = r(Var)
		
		// p50-p10 & p90-p50 ratios
		qui sum eq_hh_earnings_`ccyy' [w= hpopwgt*nhhmem], de
		qui return list
		gen p50_p10_ratio_`ccyy' = r(p50) / r(p10)
		gen p90_p50_ratio_`ccyy' = r(p90) / r(p50)
		
		// gini coefficient
		qui ineqdec0 eq_hh_earnings_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen GINI_`ccyy' = r(gini)
		
		//make a table
		
		tabstat GINI_`ccyy' p50_p10_ratio_`ccyy' p90_p50_ratio_`ccyy' velhhe_`ccyy'
		
		
	}	
end
figure8_data

program define figures10_11_12_data
	foreach ccyy in $datasets {
		//using sampleB/household data
		use $vars using $`ccyy'p, clear
		qui merge m:1 hid using $`ccyy'h, keepusing($varsh)
		keep if inrange(age,25,60) & (hile + hils*.67) > 0 & relation<=2200
		
		// variance of log main earner earnings 
		gen main_earnings_`ccyy' =(hile + hils*.67) if pid == 1
		gen log_main_earnings_`ccyy' = log(main_earnings_`ccyy')
		qui sum log_main_earnings_`ccyy' [w= ppopwgt]
		qui return list
		gen vlme_`ccyy' = r(Var)
		
		//variance of log HH earninings
		gen hh_earnings_`ccyy' = (hile + hils*.67)
		gen log_hh_earnings_`ccyy' = log(hh_earnings_`ccyy')
		qui sum log_hh_earnings_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen vlhhe_`ccyy' = r(Var)
		
		//variance of log hh earnings + private transfers
		gen hh_earnings_pt_`ccyy' = (hile + hils*.67) + hitp
		gen log_hh_earnings_pt_`ccyy' = log(hh_earnings_pt_`ccyy')
		qui sum log_hh_earnings_pt_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen vlhhept_`ccyy' = r(Var)
		
		//variance of log HH earnings + private transfers + asset income i.e. pre-gov't income
		gen hh_earnings_asset_`ccyy' = (hile + hils*.67) + hitp + hic
		gen log_hh_earnings_asset_`ccyy' = log(hh_earnings_asset_`ccyy')
		qui sum log_hh_earnings_asset_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen vlhhea_`ccyy' = r(Var)
		
		// variance of log income + public transfers
		gen public_transfer_income_`ccyy' = (hile + hils*.67) + hitp + hic + hits
		gen log_public_transfer_income_`ccyy' = log(public_transfer_income_`ccyy')
		qui sum log_public_transfer_income_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen vlpti_`ccyy' = r(Var)
		
		//variance of log disposable income 
		gen disposable_income_`ccyy' = dhi
		gen log_disposable_income_`ccyy' = log(disposable_income_`ccyy')
		qui sum log_disposable_income_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen vldi_`ccyy' = r(Var)
		
		//gini of main earner earnings 
		qui ineqdec0 main_earnings_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen main_gini_`ccyy' = r(gini)
		
		//gini of HH earnings
		qui ineqdec0 hh_earnings_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen hh_gini_`ccyy' = r(gini)
		
		//gini of HH earnings + private transfers
		qui ineqdec0 hh_earnings_pt_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen pt_gini_`ccyy' = r(gini)
		
		//gini of HH earnings + private transfers + asset income
		qui ineqdec0 hh_earnings_asset_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen asset_gini_`ccyy' = r(gini)
		
		//gini of gross HH income
		qui ineqdec0 public_transfer_income_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen gross_gini_`ccyy' = r(gini)
		
		// gini of disposable income
		qui ineqdec0 disposable_income_`ccyy' [w= hpopwgt*nhhmem]
		qui return list
		gen disp_gini_`ccyy' = r(gini)
		
		
		//make a table
		tabstat vlme_`ccyy' vlhhe_`ccyy' vlhhept_`ccyy' vlhhea_`ccyy' vlpti_`ccyy' vldi_`ccyy' main_gini_`ccyy' hh_gini_`ccyy' pt_gini_`ccyy' asset_gini_`ccyy' gross_gini_`ccyy' disp_gini_`ccyy' 
		
	}
end
figures10_11_12_data

program define figure1_data
	//Since I unfortunatly have no data for annual weeks worked or wages, I'll approximate weekly earnings by dividing annual earnings by 52
	use $vars using $cl90p, clear
	keep if inrange(age, 16, 64) & hours >= 35 & pile > 0
	
	gen me = pile if sex == 1
	gen fe = pile if sex != 1
	
	
	forvalues ii = 3/97 {
	
		// men
		pctile me_pct_1_`ii' = me [w= pwgt], nq(100)
		qui return list
		local me_weekly_earnings_1_`ii' = log(r(r`ii')/52)
		
		//women 
		pctile fe_pct_1_`ii' = fe [w= pwgt], nq(100)
		qui return list
		local fe_weekly_earnings_1_`ii' = log(r(r`ii')/52)
	}
	
		
	
	use $vars using $cl15p, clear
	keep if inrange(age, 16, 64) & hours >= 35 & pile > 0
		
	gen me = pile if sex == 1
	gen fe = pile if sex != 1
	
	
	//using 3rd through 97th percentiles for some implicit top and bottom coding in line with AKK
	forvalues ii = 3/97 {
		
		//men
		pctile me_pct_2_`ii' = me [w= pwgt], nq(100)
		qui return list
	
		gen me_weekly_earnings_2_`ii' = log(r(r`ii')/52)
		
		gen me_diff_`ii' = me_weekly_earnings_2_`ii' - `me_weekly_earnings_1_`ii''
		replace me_diff_`ii' = round(me_diff_`ii', 0.001)
		
		//women
		pctile fe_pct_2_`ii' = fe [w= pwgt], nq(100)
		qui return list
	
		gen fe_weekly_earnings_2_`ii' = log(r(r`ii')/52)
		
		gen fe_diff_`ii' = fe_weekly_earnings_2_`ii' - `fe_weekly_earnings_1_`ii''
		replace fe_diff_`ii' = round(fe_diff_`ii', 0.001)
		
		
		tabstat me_diff_`ii' fe_diff_`ii'
	} 
end
figure1_data

program figure2_data
	foreach ccyy in $datasets {
		use $vars using $`ccyy'p, clear
		keep if inrange(age, 16, 64) & hours >= 35 & pile > 0 
		
		// Since I have no data for experience or the age that people started working I'll construct a proxy for experience using age and education level
		gen start_working`ccyy' = 16
		replace start_working`ccyy' = 18 if educ == 2
		replace start_working`ccyy' = 21 if educ == 3
		
		gen proxy_exp`ccyy' = age - start_working`ccyy'
		
		gen proxy_exp_2`ccyy' = proxy_exp`ccyy'^2
		
		//since I have no data for weeks per year, I just divide annual earnings by 52, as reccomoended by the LIS
		gen weekly_wage`ccyy' = pile/52
		gen logWage`ccyy' = log(weekly_wage`ccyy')
		

		//estimate P90-P10 ratio
		pctile pctl_`ccyy' = logWage`ccyy' [w=pwgt], nq(10)
		qui return list
		gen p90_p10_`ccyy' = r(r9) - r(r1)
		
		
		
		//estimate residual 90/10 
		//usings standardized weights to control for compositional effects over time
		qui reg logWage`ccyy' proxy_exp`ccyy' proxy_exp_2`ccyy' i.educ##i.sex [aw= pwgt]
		predict double resid, residuals
        qui sum resid, de
		gen within_`ccyy' = r(p90)- r(p10)
		
		//constant composition education premium
		qui reg logWage`ccyy' proxy_exp`ccyy' proxy_exp_2`ccyy' i.educ##i.sex [aw= pwgt]
		mat b = e(b)
		gen edu_prem_`ccyy' = (b[1,5] - b[1,4])

		tabstat log_edu_premium_`ccyy' p90_p10_`ccyy' within_`ccyy' edu_prem_`ccyy'
	}
end
figure2_data

program define Mincer_wage_regression
	foreach ccyy in $datasets {
		use $vars using $`ccyy'p, clear
		keep if inrange(age, 16, 64) & hours >= 35 & pile > 0
		
		gen start = 16
		replace start = 18 if educ == 2
		replace start = 21 if educ == 3 
		
		gen exp = age - start
		replace exp = age - start
		gen exp2 = exp^2
		
		//since I have no data for weeks per year, I just divide annual earnings by 52 as reccomended by the LIS self learning resource
		gen hourly_wage`ccyy' = pile/52*hours
		gen logWage`ccyy' = log(hourly_wage`ccyy')
		
		//regress usings standardized weights to control for sample composition
		reg logWage`ccyy' exp exp2 i.sex##i.educ [aw=pwgt]
		estimates store equation_`ccyy'

	}
	est tab equation_cl90 equation_cl92 equation_cl94 equation_cl96 equation_cl98 equation_cl00 equation_cl03 equation_cl06 equation_cl09 equation_cl11 equation_cl13 equation_cl15
end
Mincer_wage_regression

		