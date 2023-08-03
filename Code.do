************************************************************************
*	A) Cleaning Variables 
* 	B) Generating Table 1a and 1b
*	C) Post-session survey
*	D) Follow-up survey 
* 	E) DS Application Status 
************************************************************************

use "S1-S4_Deidentified_Dataset.dta",clear


/* A. Cleaning Variables */
/* A.1. Pre-session Survey */
	* Combining race and ethnicity variables into one race and ethnicity variable 
		/* Such that : 1) African American/Black 2) 2  Asian (Other than Filipino,Hmong or Vietnamese)
		3) Filipino, Hmong, or Vietnamese 4) Hispanic/Latinx 5) Other 6) White 
		*/
codebook raceethnicity race_* ethnicity

replace raceethnicity=1 if race_2==1 & ethnicity==1   
replace raceethnicity=2 if race_4==1 & ethnicity==1   
replace raceethnicity=3 if race_5==1 & ethnicity==1   
replace raceethnicity=4 if ethnicity==2
replace raceethnicity=5 if (race_3==1 & ethnicity==1) | (race_6==1 & ethnicity==1) | (race_7==1 & ethnicity==1)
replace raceethnicity=6 if race_1==1 & ethnicity==1   

egen raceethnicity_other_text= concat(Ifyouidentifyasmultiraciala race_TEXT)

	* Generating  an SFSU specific variable for insitution of trainee 
encode institution, gen(institute)
recode institute (11=1) (1/10 12/29=0), gen(SFSU)

recode disability (1=0 "No disability") (2/max=1 "Reports disability"), gen(disabilityreported)

/* B. Generating Table 1a */
foreach var of varlist gender raceethnicity disabilityreported firstgen primaryresearcharea propelinterest pre_NIHsteps SFSU {
	tab  `var' indicator if presession==1, col chi2
}

tab indicator if presession==1 
bysort indicator: tab gender if presession==1 
bysort indicator: tab raceethnicity if presession==1 
bysort indicator: tab institution if presession==1 
bysort indicator: tab disability if presession==1 
bysort indicator: tab firstgen if presession==1 
bysort indicator: tab primaryresearcharea if presession==1
bysort indicator: tab propelinterest if presession==1
bysort indicator: tab pre_NIHsteps if presession==1
tab level if presession==1
list level level_text  if presession==1 & level==2 & level_text!=""
tab UCSFrole if presession==1
tab mainreasonfaculty if presession==1
tab mainreasonstudent if presession==1


* Generating Table 1 - w/ values for missingness 
foreach var of varlist gender raceethnicity disabilityreported firstgen primaryresearcharea propelinterest pre_NIHsteps SFSU {
	tab  `var' indicator if presession==1, m col chi2
}
tab indicator if presession==1 , m
bysort indicator: tab gender if presession==1 , m
bysort indicator: tab raceethnicity if presession==1 , m 
bysort indicator: tab institution if presession==1 , m
bysort indicator: tab disability if presession==1 , m
bysort indicator: tab firstgen if presession==1 , m
bysort indicator: tab primaryresearcharea if presession==1 , m
bysort indicator: tab propelinterest if presession==1 , m
bysort indicator: tab pre_NIHsteps if presession==1 , m
tab level if presession==1 , m
list level level_text  if presession==1 & level==2 & level_text!="" 
tab UCSFrole if presession==1 , m
tab mainreasonfaculty if presession==1 , m
tab mainreasonstudent if presession==1 , m

/* B. Generating Table 1b */
* Generating baseline table on students
foreach i of numlist 1/5 {
	tab studentdiversity__`i' 
	sum studentdiversity__`i'  
}
* Recoding perceived benefit so that 5 is the best 
recode studentbenefits_1 studentbenefits_2 studentbenefits_3 studentbenefits_4 studentbenefits_5 (1=5) (2=4) (3=3) (4=2) (5=1), gen(re_studentbenefits_1 re_studentbenefits_2 re_studentbenefits_3 re_studentbenefits_4 re_studentbenefits_5)
foreach i of numlist 1/5 {
	tab re_studentbenefits_`i' 
	sum re_studentbenefits_`i' 
}
* Science identity 
foreach i of numlist 1/5 {
	tab sciencecommunity_`i'
	sum sciencecommunity_`i'
}
* Research self-efficacy - BOTH
foreach i of numlist 1/9 {
	tab scienceconfidence_`i'
	sum scienceconfidence_`i' 
}

* generating Table 1b - Faculty -specific 
foreach i of numlist 1/4 { 
	tab facultydiversity__`i'
	sum facultydiversity__`i'
}
* Recoding perceived so that 4 is the best 
recode facultybenefits_1 facultybenefits_2 facultybenefits_3 facultybenefits_4 (1=4) (2=3) (3=2) (4=1), gen(re_facultybenefits_1 re_facultybenefits_2 re_facultybenefits_3 re_facultybenefits_4)
foreach i of numlist 1/4 { 
	tab re_facultybenefits_`i'
	sum re_facultybenefits_`i'
}
foreach i of numlist 1/4 {
	tab facultydiversity__`i' 
	sum facultydiversity__`i'  
}

* Bivariate Analysis - faculty // exploratory
foreach var of varlist gender raceethnicity disabilityreported firstgen primaryresearcharea  mainreasonfaculty propelinterest pre_NIHsteps UCSFrole  {
	oneway facultydiversity__1 `var', tabulate
	oneway facultydiversity__2 `var', tabulate
	oneway facultydiversity__3 `var', tabulate
	oneway facultydiversity__4 `var', tabulate
**
	oneway re_facultybenefits_1 `var', tabulate
	oneway re_facultybenefits_2 `var', tabulate
	oneway re_facultybenefits_3 `var', tabulate
	oneway re_facultybenefits_4 `var', tabulate
}

* Bivariate Analysis - trainee // exploratory
foreach var of varlist gender raceethnicity disabilityreported firstgen primaryresearcharea mainreasonstudent propelinterest pre_NIHsteps SFSU  {
	oneway studentdiversity__1 `var', tabulate
	oneway studentdiversity__2 `var', tabulate
	oneway studentdiversity__3 `var', tabulate
	oneway studentdiversity__4 `var', tabulate
	oneway studentdiversity__5 `var', tabulate	
**
	oneway re_studentbenefits_1 `var', tabulate
	oneway re_studentbenefits_2 `var', tabulate
	oneway re_studentbenefits_3 `var', tabulate
	oneway re_studentbenefits_4 `var', tabulate
	oneway re_studentbenefits_5 `var', tabulate
**
	oneway sciencecommunity_1 `var', tabulate 
	oneway sciencecommunity_2 `var', tabulate 
	oneway sciencecommunity_3 `var', tabulate 
	oneway sciencecommunity_4 `var', tabulate 
	oneway sciencecommunity_5 `var', tabulate 

	oneway scienceconfidence_1 `var', tabulate
	oneway scienceconfidence_2 `var', tabulate
	oneway scienceconfidence_3 `var', tabulate
	oneway scienceconfidence_4 `var', tabulate
	oneway scienceconfidence_5 `var', tabulate
	oneway scienceconfidence_6 `var', tabulate
	oneway scienceconfidence_7 `var', tabulate
	oneway scienceconfidence_8 `var', tabulate
	oneway scienceconfidence_9 `var', tabulate	
}
 
* Bivariate by mentor/trainee status 
egen diveristy1= rowtotal(facultydiversity__1 studentdiversity__1), m
egen diversity2= rowtotal(facultydiversity__2  studentdiversity__2), m
ttest diveristy1, by(indicator)
ttest diversity2, by(indicator)

foreach i of numlist 1/9 {
	ttest scienceconfidence_`i', by(indicator)
}

/* C. Post-session Survey */
* Baseline characteristics of those who answered post-session survey 
foreach var of varlist gender raceethnicity disability firstgen primaryresearcharea propelinterest post_NIHsteps {
	tab  `var' indicator if postsession==1, col chi2
}
tab indicator if postsession==1 
bysort indicator: tab gender if postsession==1 
bysort indicator: tab raceethnicity if postsession==1 
bysort indicator: tab institution if postsession==1 
bysort indicator: tab disabilityreported if postsession==1 
bysort indicator: tab firstgen if postsession==1 
bysort indicator: tab primaryresearcharea if postsession==1
bysort indicator: tab propelinterest if postsession==1
bysort indicator: tab post_NIHsteps if postsession==1
tab UCSFrole if postsession==1

	** OTHER 

	* Assessing if there are baseline characteristic differences between people 
	* who answered different surveys
			* No significant differences // good 
gen surveyscomplete=1 if presession==1 
replace surveyscomplete=2 if presession==1 & postsession==1 
replace surveyscomplete=3 if presession==1 & postsession==1  & followup==1
foreach var of varlist gender raceethnicity disability firstgen primaryresearcharea propelinterest pre_NIHsteps {
	tab  `var' surveyscomplete, col chi2
}

** Assessing MME effectiveness by mentor/trainee 
bysort indicator: tab satisfiedwithevent if postsession==1 
bysort indicator: tab recommend if postsession==1 
bysort indicator: tab howeffectiveinterviews if postsession==1
bysort indicator: tab howmanymet if postsession==1
bysort indicator: tab post_NIHsteps if postsession==1 /* R1 - NIHsteps */
bysort indicator: tab preeventmaterial if postsession==1 /* students only */
bysort indicator: tab willyoufollowup if postsession==1 

tab satisfiedwithevent indicator if postsession==1, col chi2 m
tab recommend indicator if postsession==1, col chi2 m
tab howeffectiveinterviews indicator if postsession==1, col chi2 m
tab howmanymet indicator if postsession==1, col chi2 m
tab post_NIHsteps indicator if postsession==1, col chi2 m
tab preeventmaterial indicator if postsession==1, col chi2 m
tab willyoufollowup indicator if postsession==1, col chi2 m

** Assessing differences in MME effectiveness by those who had more interviews 
		** by mentor/trainee// exploratory 
recode howmanymet (0/4=0 "< 5") (5/max=1 "5 or more"), gen(met5ormore)

bysort indicator: tab satisfiedwithevent met5ormore if postsession==1, col chi2
bysort indicator: tab recommend met5ormore if postsession==1 , col chi2
bysort indicator: tab howeffectiveinterviews met5ormore  if postsession==1, col chi2
bysort indicator: tab howmanymet met5ormore if postsession==1, col chi2
bysort indicator: tab post_NIHsteps met5ormore if postsession==1 , col chi2/* R1 - NIHsteps */
bysort indicator: tab preeventmaterial met5ormore if postsession==1 , col chi2 /* students only */
bysort indicator: tab willyoufollowup met5ormore if postsession==1 , col chi2

foreach var of varlist gender raceethnicity disabilityreported firstgen primaryresearcharea  mainreasonfaculty propelinterest pre_NIHsteps UCSFrole  {
	bysort indicator: tab `var' satisfiedwithevent, col chi2 
	bysort indicator: tab `var' recommend, col chi2 
	bysort indicator: tab `var' howeffectiveinterviews, col chi2 
	bysort indicator: tab `var' post_NIHsteps, col chi2 
}

/* D. Follow Up Survey */ 
recode traineeposition (2=1 "Paid") (3=0 "Not paid"), gen(paidposition)
replace paidposition=0 if traineeposition==. & currentlytalking==1 


** Assessing if there are differences in trainees in paid position
	** by baseline characteristics
foreach var of varlist gender raceethnicity disabilityreported firstgen primaryresearcharea propelinterest pre_NIHsteps {
	tab  `var' paidposition if status==1, col chi2
}

* Estimate on increase in DS application knowledge
ttest pre_NIHsteps==post_NIHsteps
bysort indicator: ttest pre_NIHsteps==post_NIHsteps

	recode pre_NIHsteps (1=5) (2=4) (3=3) (4=2) (5=1)
	recode post_NIHsteps (1=5) (2=4) (3=3) (4=2) (5=1)

ttest pre_NIHsteps==post_NIHsteps
bysort indicator: ttest pre_NIHsteps==post_NIHsteps
	* Differences in DS application by mentor/trainee 

	
* Bivariate differences across baseline characteristics by whether they matched
	* Match definition - currently talking with the goal of them joining lab
gen match=0 if presession==1 & indicator=="S"
replace match=1 if currentlytalking==1 & indicator=="S" 
replace match=0 if currentlytalking==0 & indicator=="S" 
label val match yn

foreach var of varlist gender raceethnicity disabilityreported firstgen primaryresearcharea propelinterest pre_NIHsteps SFSU {
	tab  `var' match, col chi2
}

foreach var of varlist re_studentbenefits_1 re_studentbenefits_2 re_studentbenefits_3 re_studentbenefits_4 re_studentbenefits_5 studentdiversity__1 studentdiversity__2 studentdiversity__3 studentdiversity__4 studentdiversity__5 studentpriorexperience_1 studentpriorexperience_2 studentpriorexperience_3 studentpriorexperience_4 studentpriorexperience_5 {
	tab  `var' match, col chi2
}

foreach var of varlist satisfiedwithevent recommend howeffectiveinterviews howmanymet preeventmaterial willyoufollowup {
	tab  `var' match, col chi2
}

* Bivariate differences across baseline characteristics by whether they made a connection 
	* Mentor-trainee connection definition - after MME (binary measure reflecting whether a trainee or mentor reported having at least one followup conversation after the MME collected via a questionnaire (Q5 from follow-up survey) administered by email to event attendees [time frame])

recode number_followups (1=0 "No conversation") (1/max=1 "Yes, made connection"), gen(connection)
	tab connection indicator, col chi2

foreach var of varlist indicator gender raceethnicity disabilityreported firstgen primaryresearcharea propelinterest pre_NIHstep {
	tab `var' connection if followup==1, col chi2
}	

foreach var of varlist gender raceethnicity disabilityreported firstgen primaryresearcharea propelinterest pre_NIHstep {
	bysort indicator: tab `var' connection if followup==1, col chi2
}	 

/* Part of Table 3 */

* Post connections made
tab connection

* Average number of follow up connections
recode number_followups (1=0) (2=1) (3=2) (4=3) (5=4) (6=5) , gen(number_followups_cont)
sum number_followups_cont

* 72 participants answered Survey 4; however, we removed 5 participants for not 
* any of the outcome items (Table 3 footnote)
tab followup //n=72
gen FLAG=1 if connection==. & followup==1 /* 5 observations */
list attendedevent status connection currentlytalking planningtosubmitDS DSstage DSissues traineeposition if FLAG==1
gen followup_r=1 if followup==1 & FLAG!=1


* For the following responses; we don't include the second set of responses of dyad pairs in which mentors and trainees both answered (n=11) (Table 3 footnote)

* Made a connection 
tab connection if followup_r==1 & dyadFlag_trainee!=1 , m /* excludes n=5 missing outcome date and n=11 repeated information from dyad pair */ 

* Planning to submit an NIH DS
	* Skip pattern of survey : if currentlytalking==1
tab planningtosubmitDS if followup_r==1 & currentlytalking==1 & dyadFlag_trainee!=1 , m

* DS application status 3-months post event 
	* Skip pattern of survey: if planningtosubmitDS==1
tab DSstage if followup_r==1 & (currentlytalking==1 & planningtosubmitDS==1) & dyadFlag_trainee!=1 , m


	** OTHER 

* Note. In text estimates or supporting tables   
* Trainee position in lab  
	* Skip pattern of survey: if planningtosubmitDS==1
tab traineeposition if followup_r==1 & (currentlytalking==1 & planningtosubmitDS==1) & dyadFlag_trainee!=1 , m


* Other exploratory results related to differences based on trainee status etc. 

	* Summarizing outcomes among those who were part of a connection 
foreach var of varlist number_followups currentlytalking planningtosubmitDS DSstage traineeposition {
	tab `var' if dyadFlag_faculty!=1 & followup==1, m
}

	* Bivariate differences by trainee position in lab 
foreach var of varlist gender raceethnicity disabilityreported firstgen primaryresearcharea propelinterest pre_NIHsteps {
	tab  `var' traineeposition if dyadFlag_faculty!=1 & followup==1, col chi2
}

	* Bivariate demographic differences in among those currently talking to faculty
	** by trainee mentee status
foreach var of varlist gender raceethnicity disabilityreported  {
	tab  `var' indicator if currentlytalking==1, col chi2
}

	* Bivariate demographic differences in among those who matched 
	** by trainee mentee status
foreach var of varlist gender raceethnicity disabilityreported  {
	tab  `var' indicator if dyad==1, col chi2
}

* Creating variable for reference group to be include all participants from the MME event who didn't make a match, not just those who answered survey 4
gen currentlytalking_overall=currentlytalking
replace currentlytalking_overall=0 if currentlytalking==. & presession==1

	* Bivariate demographic differences in among trainee
	** by whether they made a match 
foreach var of varlist gender raceethnicity disabilityreported  {
	tab  `var' currentlytalking_overall if indicator=="S", col chi2
}
	
clear 

/*	E. DS Application Status */
** Diversity Supplement Outcome - only asked among faculty 
use "S5_Deidentified_Dataset.dta", clear

tab DSoutcome

clear 
