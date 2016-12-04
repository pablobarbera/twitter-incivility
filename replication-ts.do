cd "~/Dropbox/research/twitter-trolls/replication"
log using "replication-ts.log"
clear all

** opening and appending data
use "germany-time-series.dta"
append using "uk-time-series.dta", force
append using "spain-time-series.dta", force
append using "greece-time-series.dta", force

** strings to factors
encode(country), gen(code)
encode(electability), gen(elec)
encode(twitter), gen(id)
gen logfoll = log(followers_count+1)
gen voteshare = votenl/100
table country

** computing weights
by twitter, sort: egen sumtweets = sum(ntweets)
sum ntweets
gen weight = 1/(sumtweets / r(mean))

** dropping missing values
drop if engaging_tweets == .
drop if ntweets < 2

** keeping only first 3 weeks
drop if week == 4

** setting TSCS structure of data
xtset id week


** Table 3 ***
	
xtreg d.impolite_mentions l.engaging_tweets [pweight=weight], fe cluster(id)
estimates store m1, title("All")

margins, dyex(*)
sum engaging_tweets
di  .1892352  * .2784413  
sum impolite_mentions
di  (.1892352  * .2784413)/.0720724 

xtreg d.impolite_mentions l.engaging_tweets [pweight=weight] if country=="UK", fe cluster(id)
estimates store m2, title("UK")

xtreg d.impolite_mentions l.engaging_tweets [pweight=weight] if country=="Spain", fe cluster(id)
estimates store m3, title("Spain")

xtreg d.impolite_mentions l.engaging_tweets [pweight=weight] if country=="Germany", fe cluster(id)
estimates store m4, title("Germany")

xtreg d.impolite_mentions l.engaging_tweets [pweight=weight] if country=="Greece", fe cluster(id)
estimates store m5, title("Greece")
	

** Generating table
estout m1 m2 m3 m4 m5 /*displays models 1-5*/ ///
	using "tabl3.tex", replace style(tex) ///
	title(OLS regressions of impolite tweets received on engaging tweets sent, with candidate fixed effects.) ///
	prehead(	\def\one{\footnotesize{$\ast$}} ///
				\def\two{\footnotesize{$\ast\ast$}} ///
				\def\three{\footnotesize{$\ast\ast$$\ast$}} ///
				\begin{table}[h!]\caption{@title} ///
				\label{tab:table4} ///
				\centering\begin{threeparttable}\begin{tabular}{l*{@M}{r@{}l}}\hline\hline ///
				) ///
	collabels(none) /*hides labels of columns*/ ///
	posthead(\hline\\) ///
	cells(b(star fmt(%9.2f)) se(par)) /*displays coefs with 1 decimal*/ ///
	starlevels(\one 0.10 \two 0.05 \three 0.01) ///
		/*reported levels of significance can changed*/ ///
	varlabels(_cons Constant L.engaging_tweets "\% Engaging tweets sent (lagged)") /*modify label of constant*/ ///
	prefoot(\\\hline) ///
	stats(N_clust N r2, fmt(%9.0g %9.0g %9.2f ) labels("N (candidates)" "N (observations)" "$R^2$")) ///
	nolegend /*hides significance symbols legend (I'll do it manually)*/ ///
	label  /*make use of variable labels*/ ///
	stardetach /*display stars in separated column*/ ///
	wrap varwidth(30) ///
	postfoot( ///
				\hline\hline\end{tabular} \begin{tablenotes} ///
				\item \footnotesize{Dependent variable: Change in proportion of engaging tweets sent, by week. ///
					Robust standard errors in parentheses. ///
				Signif.: \one 10\% \two 5\% \three 1\%.} ///
				\end{tablenotes}\end{threeparttable}\end{table} ///
				)	
	
	
	
log close	
	
