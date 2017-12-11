

libname  Titanic 'c:/users/dk2539/ML Nanodegree/titanic_survival_exploration/sas_data' ;


data gg0 ;
  set Titanic.titanic_data ;

  rannum = uniform(554) ;

  *proc univariate;
   * var fare ;

  proc sort ;
    by descending fare ;

	proc print ;

	run ;

data training ;
   set GG0 ;
     if rannum < .75 ;

  cabin_1st  = upcase(substr(cabin,1,1)) ;

  if cabin_1st = 'A' then
     A_Cabin   = 1 ;
  else
     A_Cabin   = 0 ;


  if cabin_1st = 'B' then
     B_Cabin   = 1 ;
  else
     B_Cabin   = 0 ;


	 
  if cabin_1st = 'C' then
     C_Cabin   = 1 ;
  else
     C_Cabin   = 0 ;


  if cabin_1st = 'D' then
     D_Cabin   = 1 ;
  else
     D_Cabin   = 0 ;

	 
  if cabin_1st = 'E' then
     E_Cabin   = 1 ;
  else
     E_Cabin   = 0 ;


  if cabin_1st = 'F' then
     F_Cabin   = 1 ;
  else
     F_Cabin   = 0 ;


   if  Embarked    = 'C' then
       I_Embarked_C  = 1 ;
   else
       I_Embarked_C  = 0 ;

   If   Embarked = 'S'  then
      I_Embarked_S  = 1 ;
   else
       I_Embarked_S  = 0 ;




  if sex = 'male' then
  I_male = 1 ;
  else
  I_male = 0 ;



  if pclass = 1 then
     p_class_1st = 1 ;
  else
      p_class_1st = 0 ;

  if pclass = 2 then
     p_class_2nd  = 1 ;
  else
     p_class_2nd  = 0 ;


	 ticket_1st = substr(ticket,1,1) ;

	 if ticket_1st = '1' then
	    ticket_1st_1 = 1 ;
	 else
		ticket_1st_1 = 0 ;

	 if ticket_1st = '2' then
	    ticket_1st_2 = 1 ;
	 else
		ticket_1st_2 = 0 ;


	 if ticket_1st = '3' then
	    ticket_1st_3 = 1 ;
	 else
		ticket_1st_3 = 0 ;

	 if ticket_1st = '4' then
	    ticket_1st_4 = 1 ;
	 else
		ticket_1st_4 = 0 ;

		
	 if ticket_1st = '5' then
	    ticket_1st_5 = 1 ;
	 else
		ticket_1st_5 = 0 ;

	 if ticket_1st = '6' then
	    ticket_1st_6 = 1 ;
	 else
		ticket_1st_6 = 0 ;

	  if ticket_1st = '7' then
	    ticket_1st_7 = 1 ;
	 else
		ticket_1st_7 = 0 ;

	 if ticket_1st = '8' then
	    ticket_1st_8 = 1 ;
	 else
		ticket_1st_8 = 0 ;


	 if ticket_1st = '9' then
	    ticket_1st_9 = 1 ;
	 else
		ticket_1st_9 = 0 ;

	 if ticket_1st = 'A' then
	    ticket_1st_A = 1 ;
	 else
		ticket_1st_A = 0 ;

		
	 if ticket_1st = 'C' then
	    ticket_1st_C = 1 ;
	 else
		ticket_1st_C = 0 ;

	 if ticket_1st = 'D' then
	    ticket_1st_D = 1 ;
	 else
		ticket_1st_D = 0 ;

		 if ticket_1st = 'F' then
	    ticket_1st_F = 1 ;
	 else
		ticket_1st_F = 0 ;

		 if ticket_1st = 'L' then
	    ticket_1st_L = 1 ;
	 else
		ticket_1st_L = 0 ;

		 if ticket_1st = 'P' then
	    ticket_1st_P = 1 ;
	 else
		ticket_1st_P = 0 ;

		 if ticket_1st = 'S' then
	    ticket_1st_S = 1 ;
	 else
		ticket_1st_S = 0 ;


		if SibSp = 0 then
           I_SibSP = 0 ;
        else 
            I_SibSP = 1 ;               

		if Parch = 0 then
           I_Parch = 0 ;
        else 
            I_Parch = 1 ; 
** fare ;

	   if fare < 14 then
	      fare_Grp = 'a_lt_14         ';
	   else
	    if fare < 31 then
	      fare_Grp = 'b_ge_14_n_lt_31';
	   else
       if fare < 79 then
	      fare_Grp = 'c_ge_31_lt_79';
	   
	   if fare >= 79 then
	      fare_Grp = 'd_ge_79';

**  age ;

	   if age < 10 then
	      age_grp  = 'a_lt_10        ' ;
	   else
	    if age < 20 then
	      age_grp  = 'b_ge_10_n_lt_20' ;
	   else
	    if age < 30 then
	      age_grp  = 'c_ge_20_n_lt_30' ;
	   else
	    if age < 40 then
	      age_grp  = 'd_ge_30_n_lt_40' ;
		else
		if age < 50 then
	      age_grp  = 'e_ge_40_n_lt_50' ;
	   else
	    if age < 60 then
	      age_grp  = 'f_ge_50_n_lt_60' ;
	   else
	    if age =60 then
	      age_grp  = 'g_ge_60' ;
	   
	   
	   




  proc contents position ;


  run ;

  proc print ;
    where uniform(554) > .99 ;

  proc freq data = training ;
    tables ticket_1st * survived / missing  nocol nocum nopercent;;;

	run ;

  proc sort ;
    by sex
	   fare ;

  proc gplot ;
   plot survived * fare / grid overlay ;
     by sex ;
   symbol1 i = none  c = red  v = star  w = 2  l = 1 ;

   proc sort ;
     by name ;

   proc print ;
     var survived sex age fare name pclass ;

  proc corr ;
    var sex 
        age        
		pclass 
		fare
        SibSp    
        Parch
		Cabin_1st          
        Embarked ;

# 	ticket_1st_1 
		ticket_1st_2 
		ticket_1st_3 
		ticket_1st_4
		ticket_1st_5 
		ticket_1st_6 
		ticket_1st_7 
		ticket_1st_8 
		ticket_1st_9

		ticket_1st_A 
		ticket_1st_C 
		ticket_1st_D 
		ticket_1st_F 
		ticket_1st_L 
		ticket_1st_P 
		ticket_1st_S 

		I_SibSP 
        I_Parch ; 
;

  
	 proc genmod ;
	  * model survived   = I_male
	                      age
	                      p_class_1st
						  p_class_2nd

						  I_Embarked_C
						  I_Embarked_S

						 
                            
						  fare
                          SibSp    
                          Parch
						  / dist = bin
                            link = logit
                            lrci
                            ;
	 * model survived   = I_male *
	                      age
	                    
                            
						  fare
						  
	ticket_1st_1 
		ticket_1st_2 
		ticket_1st_3 
		ticket_1st_4
		ticket_1st_5 
		ticket_1st_6 
		ticket_1st_7 
		ticket_1st_8 
		ticket_1st_9

		ticket_1st_A 
		ticket_1st_C 
		ticket_1st_D 
		ticket_1st_F 
		ticket_1st_L 
		ticket_1st_P 
		ticket_1st_S 

        I_SibSP 
        I_Parch 

  
                          
						  / dist = bin
                            link = logit
                            lrci
                            ;
model survived = fare ;
output out       = results_training
          pred      = Pred
          resraw    = Resraw
          reschi    = Reschi
          resdev    = Resdev
          stdreschi = Stdreschi
          stdresdev = Stdresdev
          reslik    = Reslik;
ods output PARAMETERestimates = est1   ;

proc print data = est1 ;

run ;

proc print data = results_training ;


run ;


data results_training1 ;
   set results_training ;

   r_pred = 1 - pred ;

proc sort   ;
   by r_pred ;

proc gplot  ;
   plot  (survived survived )* r_pred / grid overlay ;
   symbol1 i = none  c = red    v = star  w = 1  l = 1 ;
   symbol2 i = sm50  c = green  v = none  w = 2  l = 4 ;
   title1  "Titanic Survival Analysis: Logistic Regression" ;
   label  survived   = 'Prob Survived' ;
   label  r_pred     = 'Prediction Prob' ;
   format survived    percent12.0 ;
   format r_pred      percent12.0 ;

   run ;

proc freq data = results_training ;
  tables I_male * survived / missing nocol nocum nopercent;;
	                    
   run ;
     proc freq data = gg ;
	   tables sex * survived
               
		pclass      * survived
		I_male      * survived

		
        SibSp       * survived 
        Parch       * survived
		Cabin_1st   * survived      
        Embarked    * survived 

        p_class_1st  * survived 
		p_class_2nd * survived 

		I_Embarked_C * survived 
		I_Embarked_S  * survived / missing nocol nocum nopercent;


  proc freq data = training  ;
    tables sex * age_grp * ticket_1st * fare_grp * survived / missing nocol nocum nopercent;

	run ;

	proc univariate data = training ;
	  var fare ;

   run ;

************************************** ;		
data test ;
  set GG0 ;
    if rannum >= .75 ;

    cabin_1st  = upcase(substr(cabin,1,1)) ;

  if cabin_1st = 'A' then
     A_Cabin   = 1 ;
  else
     A_Cabin   = 0 ;


  if cabin_1st = 'B' then
     B_Cabin   = 1 ;
  else
     B_Cabin   = 0 ;


	 
  if cabin_1st = 'C' then
     C_Cabin   = 1 ;
  else
     C_Cabin   = 0 ;


  if cabin_1st = 'D' then
     D_Cabin   = 1 ;
  else
     D_Cabin   = 0 ;

	 
  if cabin_1st = 'E' then
     E_Cabin   = 1 ;
  else
     E_Cabin   = 0 ;


  if cabin_1st = 'F' then
     F_Cabin   = 1 ;
  else
     F_Cabin   = 0 ;


   if  Embarked    = 'C' then
       I_Embarked_C  = 1 ;
   else
       I_Embarked_C  = 0 ;

   If   Embarked = 'S'  then
      I_Embarked_S  = 1 ;
   else
       I_Embarked_S  = 0 ;




  if sex = 'male' then
  I_male = 1 ;
  else
  I_male = 0 ;



  if pclass = 1 then
     p_class_1st = 1 ;
  else
      p_class_1st = 0 ;

  if pclass = 2 then
     p_class_2nd  = 1 ;
  else
     p_class_2nd  = 0 ;


	 ticket_1st = substr(ticket,1,1) ;

	 if ticket_1st = '1' then
	    ticket_1st_1 = 1 ;
	 else
		ticket_1st_1 = 0 ;

	 if ticket_1st = '2' then
	    ticket_1st_2 = 1 ;
	 else
		ticket_1st_2 = 0 ;


	 if ticket_1st = '3' then
	    ticket_1st_3 = 1 ;
	 else
		ticket_1st_3 = 0 ;

	 if ticket_1st = '4' then
	    ticket_1st_4 = 1 ;
	 else
		ticket_1st_4 = 0 ;

		
	 if ticket_1st = '5' then
	    ticket_1st_5 = 1 ;
	 else
		ticket_1st_5 = 0 ;

	 if ticket_1st = '6' then
	    ticket_1st_6 = 1 ;
	 else
		ticket_1st_6 = 0 ;

	  if ticket_1st = '7' then
	    ticket_1st_7 = 1 ;
	 else
		ticket_1st_7 = 0 ;

	 if ticket_1st = '8' then
	    ticket_1st_8 = 1 ;
	 else
		ticket_1st_8 = 0 ;


	 if ticket_1st = '9' then
	    ticket_1st_9 = 1 ;
	 else
		ticket_1st_9 = 0 ;

	 if ticket_1st = 'A' then
	    ticket_1st_A = 1 ;
	 else
		ticket_1st_A = 0 ;

		
	 if ticket_1st = 'C' then
	    ticket_1st_C = 1 ;
	 else
		ticket_1st_C = 0 ;

	 if ticket_1st = 'D' then
	    ticket_1st_D = 1 ;
	 else
		ticket_1st_D = 0 ;

		 if ticket_1st = 'F' then
	    ticket_1st_F = 1 ;
	 else
		ticket_1st_F = 0 ;

		 if ticket_1st = 'L' then
	    ticket_1st_L = 1 ;
	 else
		ticket_1st_L = 0 ;

		 if ticket_1st = 'P' then
	    ticket_1st_P = 1 ;
	 else
		ticket_1st_P = 0 ;

		 if ticket_1st = 'S' then
	    ticket_1st_S = 1 ;
	 else
		ticket_1st_S = 0 ;


		if SibSp = 0 then
           I_SibSP = 0 ;
        else 
            I_SibSP = 1 ;               

		if Parch = 0 then
           I_Parch = 0 ;
        else 
            I_Parch = 1 ; 

     iii = 1 ;

	 


**** ;
data  est2 ;
   set est1 ;


   iii = 1 ;

   keep parameter estimate ;

   proc print ;

   run ;



data testing_est ;
  set 
        test ;

		if age < 0 then
		   age = 50 ;



		   Intercept_est     =    0.3636 ;
           I_male_Age_est    =    0.0863  ;
           Fare_est          =   -0.0023 ;
           ticket_1st_1_est  =   -3.4452 ;
           ticket_1st_2_est  =   -1.9324 ;
           ticket_1st_3_est  =   -0.4973;
           ticket_1st_4_est  =   -0.4979;
           ticket_1st_5_est   =  21.4642;
           ticket_1st_6_est  =   18.5371;
           ticket_1st_7_est  =   22.7459;
           ticket_1st_8_est  =   21.8057;
           ticket_1st_9_est  =    0.0000;
           ticket_1st_A_est  =   -0.1188;
           ticket_1st_C_est  =   -1.5042;
           ticket_1st_D_est  =    0.0000;
           ticket_1st_F_est  =   -2.4786;
           ticket_1st_L_est  =   -1.8116;
           ticket_1st_P_est  =   -3.0359;
           ticket_1st_S_est  =   -1.7905;
           I_SibSP_est       =    0.6338;
           I_Parch_est       =   -0.0764;
           Scale__est         =    1.0000;


		   Log_Score = 
           I_male_Age_est   * I_male * Age
           + Fare_est        * fare
           + ticket_1st_1_est  * ticket_1st_1
           + ticket_1st_2_est  * ticket_1st_1
           + ticket_1st_3_est  * ticket_1st_1
           + ticket_1st_4_est  * ticket_1st_1
           + ticket_1st_5_est  * ticket_1st_1
           + ticket_1st_6_est  * ticket_1st_1
           + ticket_1st_7_est  * ticket_1st_1
           + ticket_1st_8_est  * ticket_1st_1
           + ticket_1st_9_est  * ticket_1st_1
           + ticket_1st_A_est  * ticket_1st_1
           + ticket_1st_C_est  * ticket_1st_1
           + ticket_1st_D_est  * ticket_1st_1
           + ticket_1st_F_est  * ticket_1st_1
           + ticket_1st_L_est  * ticket_1st_1
           + ticket_1st_P_est  * ticket_1st_1
           + ticket_1st_S_est  * ticket_1st_1
           + I_SibSP_est      *    I_SibSP
           + I_Parch_est       * I_Parch ;


          
		   score = 1 / ( 1 + exp(log_score) );


proc print ;
  var survived
      ticket_1st
	  I_male
	  age
	  fare
	  I_SibSP
	  I_Parch 
      log_score
      score ;
  where uniform(76) > .9 ;

  run ;


    proc sort ;
	  by score ;

		
     proc gplot ;
	   plot (survived 
             survived )* score / grid overlay ;
	    symbol1 i = none  c = red    v = star  w = 1  l = 1 ;
   symbol2 i = sm80  c = green  v = none  w = 2  l = 4 ;
   title1  "Titanic Survival Analysis: Test Data Using Logistic Regression Results" ;
   label  survived   = 'Prob Survived' ;
   label  score    = 'Prediction Prob' ;
   format survived    percent12.0 ;
   format score      percent12.0 ;




  run ;
