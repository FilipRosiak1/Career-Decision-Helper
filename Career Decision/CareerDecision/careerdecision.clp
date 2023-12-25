;;; ***************************
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

(deftemplate UI-state
   (slot id (default-dynamic (gensym*)))
   (slot display)
   (slot relation-asserted (default none))
   (slot response (default none))
   (multislot valid-answers)
   (slot state (default middle)))
   
(deftemplate state-list
   (slot current)
   (multislot sequence))
  
(deffacts startup
   (state-list))
   
;;;****************
;;;* STARTUP RULE *
;;;****************

(defrule system-banner ""

  =>
  
  (assert (UI-state (display WelcomeMessage)
                    (relation-asserted start)
                    (state initial)
                    (valid-answers))))

;;;***************
;;;* QUESTION RULES *
;;;***************

(defrule determine-responsible-job ""
  (logical (start))
  =>
  (assert (UI-state (display StartQuestion)
                    (relation-asserted responsible-job)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-higher-60 ""
  (logical (responsible-job Yes))
  =>
  (assert (UI-state (display MarksQuestion)
                    (relation-asserted marks-higher)
                    (response No)
                    (valid-answers No Yes))))

(defrule determine-study-hard ""
  (or ?f <- (marks-higher Yes)
  	  ?f <- (done-40-times No))
  =>
  (retract ?f)
  (assert (UI-state (display StudyHardQuestion)
                    (relation-asserted willing-to-study)
                    (response No)
                    (valid-answers No Maybe Yes))))

(defrule determine-work-40 ""
  ?f <- (worked)
  =>
  (retract ?f)
  (assert (UI-state (display RetireQuestion)
                    (relation-asserted done-40-times)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-help ""
  (logical (on-university))
  =>
  (assert (UI-state (display HelpQuestion)
                    (relation-asserted want-to-help)
                    (response No)
                    (valid-answers No Yes))))
   
(defrule determine-language ""
  (or ?f <- (want-to-help Yes)
      ?f <- (took-classes))
  =>
  (retract ?f)
  (assert (UI-state (display LanguageQuestion)
                    (relation-asserted good-at-en-fr)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-want-money ""
  (logical (good-at-en-fr Yes))
  =>
  (assert (UI-state (display MoneyQuestion)
                    (relation-asserted want-to-be-rich)
                    (response No)
                    (valid-answers No Yes))))

(defrule determine-practical-problems ""
  (logical (want-to-be-rich No))
  =>
  (assert (UI-state (display PracticalProblemsQuestion)
                    (relation-asserted practical-problems)
                    (response No)
                    (valid-answers No Yes))))               

(defrule determine-endless-questions ""
  (logical (practical-problems Yes))
  =>
  (assert (UI-state (display EndlessQuestionsQuestion)
                    (relation-asserted endless-questions)
                    (response No)
                    (valid-answers No Yes)))) 

(defrule determine-further-schooling ""
  (or ?f <- (marks-higher No) 
  	  ?f <- (can-get-job No)
  	  ?f <- (will-last No)
  	  ?f <- (willing-to-study No))
  =>
  (retract ?f)
  (assert (UI-state (display FurtherSchoolingQuestion)
                    (relation-asserted further-schooling)
                    (response No)
                    (valid-answers No Yes)))) 
                    
(defrule determine-sciences-interest ""
  (logical (technical-school))
  =>
  (assert (UI-state (display SciencesInterestQuestion)
                    (relation-asserted sciences-interest)
                    (response No)
                    (valid-answers No Yes)))) 
                    
(defrule determine-outdoor-work ""
  (logical (sciences-interest Yes))
  =>
  (assert (UI-state (display OutdoorWorkQuestion)
                    (relation-asserted outdoor-work)
                    (response No)
                    (valid-answers No Yes)))) 
                    
(defrule determine-like-biology ""
  (logical (outdoor-work Yes))
  =>
  (assert (UI-state (display LikeBiologyQuestion)
                    (relation-asserted like-biology)
                    (response No)
                    (valid-answers No Yes)))) 
                    

(defrule determine-can-get-job ""
  ?f <- (further-schooling No)
  =>
  (retract ?f)
  (assert (UI-state (display CanGetJobQuestion)
                    (relation-asserted can-get-job)
                    (response No)
                    (valid-answers No Yes))))
                    

(defrule determine-will-last ""
  ?f <- (can-get-job Yes)
  =>
  (retract ?f)
  (assert (UI-state (display WillLastQuestion)
                    (relation-asserted will-last)
                    (response No)
                    (valid-answers No Yes))))
                    
                    
(defrule determine-electronics ""
  (logical (outdoor-work No))
  =>
  (assert (UI-state (display ElectronicsQuestion)
                    (relation-asserted electronics)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-shop-working ""
  (logical (electronics No))
  =>
  (assert (UI-state (display ShopWorkingQuestion)
                    (relation-asserted shop-working)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-detail-bug ""
  (logical (shop-working No))
  =>
  (assert (UI-state (display DetailBugQuestion)
                    (relation-asserted detail-bug)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-drafting ""
  (logical (detail-bug No))
  =>
  (assert (UI-state (display DraftingQuestion)
                    (relation-asserted drafting)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-questions-sense ""
  (logical (drafting No))
  =>
  (assert (UI-state (display QuestionsSenseQuestion)
                    (relation-asserted questions-sense)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-chemistry ""
  (logical (questions-sense Yes))
  =>
  (assert (UI-state (display ChemistryQuestion)
                    (relation-asserted chemistry)
                    (response No)
                    (valid-answers No Yes))))
                    
                    
(defrule determine-science ""
  (logical (endless-questions No))
  =>
  (assert (UI-state (display ScienceQuestion)
                    (relation-asserted science)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-mathematics ""
  (logical (science Yes))
  =>
  (assert (UI-state (display MathematicsQuestion)
                    (relation-asserted mathematics)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-good-math ""
  (logical (mathematics Yes))
  =>
  (assert (UI-state (display GoodMathQuestion)
                    (relation-asserted good-math)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-logical-problems ""
  (logical (good-math Yes))
  =>
  (assert (UI-state (display LogicalProblemsQuestion)
                    (relation-asserted logical-problems)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-application-prefer ""
  (logical (logical-problems Yes))
  =>
  (assert (UI-state (display ApplicationPreferQuestion)
                    (relation-asserted application-prefer)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-math-alot ""
  (logical (tuition-fee))
  =>
  (assert (UI-state (display MathAlotQuestion)
                    (relation-asserted math-alot)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-electronics2 ""
  (logical (math-alot Yes))
  =>
  (assert (UI-state (display ElectronicsQuestion) ;;; to samo pytanie ale oddzielny fakt
                    (relation-asserted electronics2)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-wish-hadnt-taken "" 
  (logical (or (electrical-engineering)
  		       (chemical-engineering)
  		       (mining-engineering)
  		       (petroleum-engineering)
  		       (mechanical-engineering)
  		       (agricultural-engineering)
  		       (civil-engineering)
  		       (consult-calendar) ))
  =>
  (assert (UI-state (display WishHadntTakenQuestion)
                    (relation-asserted wish-hadnt-taken)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-chemistry2 ""
  (logical (electronics2 No))
  =>
  (assert (UI-state (display ChemistryQuestion) ;;; to samo pytanie ale oddzielny fakt
                    (relation-asserted chemistry2)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-geology ""
  (logical (or (chemistry2 No)
  			   (math-alot No) ))
  =>
  (assert (UI-state (display GeologyQuestion) 
                    (relation-asserted geology)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-claustrophobia ""
  (logical (geology Yes))
  =>
  (assert (UI-state (display ClaustrophobiaQuestion) 
                    (relation-asserted claustrophobia)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-machinery-working ""
  (logical (geology No))
  =>
  (assert (UI-state (display MachineryWorkingQuestion) 
                    (relation-asserted machinery-working)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-agriculture ""
  (logical (machinery-working Yes))
  =>
  (assert (UI-state (display AgricultureQuestion) 
                    (relation-asserted agriculture)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-with-us ""
  (logical (machinery-working No))
  =>
  (assert (UI-state (display WithUsQuestion) 
                    (relation-asserted with-us)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-construction ""
  (logical (with-us Yes))
  =>
  (assert (UI-state (display ConstructionQuestion) 
                    (relation-asserted construction)
                    (response No)
                    (valid-answers No Yes))))
                    
                    
(defrule determine-graduate ""
  (logical (wish-hadnt-taken No))
  =>
  (assert (UI-state (display GraduateQuestion) 
                    (relation-asserted graduate)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-administration ""
  (logical (graduate Yes))
  =>
  (assert (UI-state (display AdministrationQuestion) 
                    (relation-asserted administration)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-math-really ""
  (logical (administration No))
  =>
  (assert (UI-state (display MathReallyQuestion) 
                    (relation-asserted math-really)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-teaching ""
  (logical (or (engineering-degree)
  			   (administration-degree) ))
  =>
  (assert (UI-state (display TeachingQuestion) 
                    (relation-asserted teaching)
                    (response No)
                    (valid-answers No Yes))))
                    
(defrule determine-research ""
  (logical (teaching No))
  =>
  (assert (UI-state (display ResearchQuestion) 
                    (relation-asserted research)
                    (response No)
                    (valid-answers No Yes))))



;;;****************
;;;* STATEMENT RULES *
;;;****************

(defrule state-become-hippie ""
  (logical (responsible-job No))
  =>
  (assert (UI-state (display HippieStatement)
                    (relation-asserted stop))))

(defrule state-work-year ""
  ?f <- (willing-to-study Maybe)
  =>
  (retract ?f)
  (assert (UI-state (display WorkStatement)
                    (relation-asserted worked))))
                    
(defrule state-retired ""
  (logical (done-40-times Yes))
  =>
  (assert (UI-state (display RetireStatement)
                    (relation-asserted stop))))
                    
(defrule state-go-to-uni ""
  (logical (willing-to-study Yes))
  =>
  (assert (UI-state (display UniversityStatement)
                    (relation-asserted on-university))))
                    
(defrule state-lawyer ""
  (logical (want-to-help No))
  =>
  (assert (UI-state (display LawyerStatement)
                    (relation-asserted lawyer))))
                    
(defrule state-parliament ""
  (logical (lawyer))
  =>
  (assert (UI-state (display ParliamentStatement)
                    (relation-asserted parliament))))
                    
(defrule state-senate ""
  (logical (parliament))
  =>
  (assert (UI-state (display SenateStatement)
                    (relation-asserted stop))))

(defrule state-taking-class ""
  ?f <- (good-at-en-fr No)
  =>
  (retract ?f)
  (assert (UI-state (display LanguageClassStatement)
                    (relation-asserted took-classes))))
                    
(defrule state-cant-help ""
  (logical (or (want-to-be-rich Yes) 
  			   (too-unrealistic)
  			   (sciences-interest No)
  			   (like-biology Yes)
  			   (detail-bug Yes) 
  			   (science No)
  			   (logical-problems No)
  			   (with-us No) ))
  =>
  (assert (UI-state (display CantHelpStatement)
                    (relation-asserted noHelp))))
                    
(defrule state-dont-bother ""
  (logical (noHelp))
  =>
  (assert (UI-state (display DontBotherStatement)
                    (relation-asserted goaway))))
                    
(defrule state-too-unrealistic ""
  (logical (endless-questions Yes))
  =>
  (assert (UI-state (display TooUnrealisticStatement)
                    (relation-asserted too-unrealistic))))

(defrule state-technical-school ""
  (logical (further-schooling Yes))
  =>
  (assert (UI-state (display TechnicalSchoolStatement)
                    (relation-asserted technical-school))))   
                    
(defrule state-luckier ""
  (logical (will-last Yes))
  =>
  (assert (UI-state (display LuckierStatement)
                    (relation-asserted luckier))))       
                    
(defrule state-engineering-aptitude ""
  (logical (application-prefer Yes))
  =>
  (assert (UI-state (display EngineeringAptitudeStatement)
                    (relation-asserted engineering-aptitude))))  
                    
(defrule state-enroll-college ""
  (logical (engineering-aptitude))
  =>
  (assert (UI-state (display EnrollCollegeStatement)
                    (relation-asserted enroll-college))))     
                    
(defrule state-tuition-fee ""
  (logical (enroll-college))
  =>
  (assert (UI-state (display TuitionFeeStatement)
                    (relation-asserted tuition-fee))))   
                    
(defrule state-electrical-engineering ""
  (logical (electronics2 Yes))
  =>
  (assert (UI-state (display ElectricalEngineeringStatement)
                    (relation-asserted electrical-engineering))))  
                    
(defrule state-chemical-engineering ""
  (logical (chemistry2 Yes))
  =>
  (assert (UI-state (display ChemicalEngineeringStatement)
                    (relation-asserted chemical-engineering))))
                    
(defrule state-mining-engineering ""
  (logical (claustrophobia No))
  =>
  (assert (UI-state (display MiningEngineeringStatement)
                    (relation-asserted mining-engineering))))
                    
(defrule state-petroleum-engineering ""
  (logical (claustrophobia Yes))
  =>
  (assert (UI-state (display PetroleumEngineeringStatement)
                    (relation-asserted petroleum-engineering))))
                    
(defrule state-mechanical-engineering ""
  (logical (agriculture No))
  =>
  (assert (UI-state (display MechanicalEngineeringStatement)
                    (relation-asserted mechanical-engineering))))
                    
(defrule state-agricultural-engineering ""
  (logical (agriculture Yes))
  =>
  (assert (UI-state (display AgriculturalEngineeringStatement)
                    (relation-asserted agricultural-engineering))))
                    
(defrule state-civil-engineering ""
  (logical (construction Yes))
  =>
  (assert (UI-state (display CivilEngineeringStatement)
                    (relation-asserted civil-engineering))))
                    
(defrule state-consult-calendar ""
  (logical (construction No))
  =>
  (assert (UI-state (display ConsultCalendarStatement)
                    (relation-asserted consult-calendar))))
                    
(defrule state-engineering-degree ""
  (logical (math-really Yes))
  =>
  (assert (UI-state (display EngineeringDegreeStatement)
                    (relation-asserted engineering-degree))))
                    
(defrule state-administration-degree ""
  (logical (administration Yes))
  =>
  (assert (UI-state (display AdministrationDegreeStatement)
                    (relation-asserted administration-degree))))
                    


;;;****************
;;;* CAREER RULES *
;;;****************

(defrule Stop ""
  (logical (stop))
  =>
  (assert (UI-state (display StopAnswer)
                    (state final))))
                    
(defrule Go-Away ""
  (logical (goaway))
  =>
  (assert (UI-state (display GoAwayAnswer)
                    (state final))))
                     
(defrule Become-Statistician ""
  (logical (practical-problems No))
  =>
  (assert (UI-state (display StatisticianAnswer)
                    (state final))))

(defrule Continue-Luck ""
  (logical (luckier))
  =>
  (assert (UI-state (display ContinueLuckAnswer)
                    (state final))))
                    
(defrule Surveying-Course ""
  (logical (like-biology No))
  =>
  (assert (UI-state (display SurveyingCourseAnswer)
                    (state final))))
                    
(defrule Electronics-Course ""
  (logical (electronics Yes))
  =>
  (assert (UI-state (display ElectronicsCourseAnswer)
                    (state final))))
                    
(defrule Shop-Course ""
  (logical (shop-working Yes))
  =>
  (assert (UI-state (display ShopCourseAnswer)
                    (state final))))
                    
(defrule Drafting-Course ""
  (logical (drafting Yes))
  =>
  (assert (UI-state (display DraftingCourseAnswer)
                    (state final))))
                    
(defrule Lake-Ending ""
  (logical (questions-sense No))
  =>
  (assert (UI-state (display LakeEndingAnswer)
                    (state final))))
                    
(defrule Chemistry-Course ""
  (logical (chemistry Yes))
  =>
  (assert (UI-state (display ChemistryCourseAnswer)
                    (state final))))
                    
(defrule Other-Options ""
  (logical (chemistry No))
  =>
  (assert (UI-state (display OtherOptionsAnswer)
                    (state final))))
                    
(defrule Field-Biologist ""
  (logical (mathematics No))
  =>
  (assert (UI-state (display FieldBiologistAnswer)
                    (state final))))
                    
(defrule Tech-Journalist ""
  (logical (good-math No))
  =>
  (assert (UI-state (display TechJournalistAnswer)
                    (state final))))
                    
(defrule Research-Scientist ""
  (logical (application-prefer No))
  =>
  (assert (UI-state (display ResearchScientistAnswer)
                    (state final))))
                    
(defrule Still-Research-Scientist ""
  (logical (wish-hadnt-taken Yes))
  =>
  (assert (UI-state (display StillResearchScientistAnswer)
                    (state final))))
                    
(defrule Other-Fiald ""
  (logical (math-really No))
  =>
  (assert (UI-state (display OtherFieldAnswer)
                    (state final))))
                    
(defrule Become-Professor ""
  (logical (teaching Yes))
  =>
  (assert (UI-state (display BecomeProfessorAnswer)
                    (state final))))
                    
(defrule Join-Research ""
  (logical (research Yes))
  =>
  (assert (UI-state (display JoinResearchAnswer)
                    (state final))))
                    
(defrule Production-Stuff ""
  (logical (or (research No)
  			   (graduate No)))
  =>
  (assert (UI-state (display ProductionStuffAnswer)
                    (state final))))
                    
                    
;;;*************************
;;;* GUI INTERACTION RULES *
;;;*************************

(defrule ask-question

   (declare (salience 5))
   
   (UI-state (id ?id))
   
   ?f <- (state-list (sequence $?s&:(not (member$ ?id ?s))))
             
   =>
   
   (modify ?f (current ?id)
              (sequence ?id ?s))
   
   (halt))

(defrule handle-next-no-change-none-middle-of-chain

   (declare (salience 10))
   
   ?f1 <- (next ?id)

   ?f2 <- (state-list (current ?id) (sequence $? ?nid ?id $?))
                      
   =>
      
   (retract ?f1)
   
   (modify ?f2 (current ?nid))
   
   (halt))

(defrule handle-next-response-none-end-of-chain

   (declare (salience 10))
   
   ?f <- (next ?id)

   (state-list (sequence ?id $?))
   
   (UI-state (id ?id)
             (relation-asserted ?relation))
                   
   =>
      
   (retract ?f)

   (assert (add-response ?id)))   

(defrule handle-next-no-change-middle-of-chain

   (declare (salience 10))
   
   ?f1 <- (next ?id ?response)

   ?f2 <- (state-list (current ?id) (sequence $? ?nid ?id $?))
     
   (UI-state (id ?id) (response ?response))
   
   =>
      
   (retract ?f1)
   
   (modify ?f2 (current ?nid))
   
   (halt))

(defrule handle-next-change-middle-of-chain

   (declare (salience 10))
   
   (next ?id ?response)

   ?f1 <- (state-list (current ?id) (sequence ?nid $?b ?id $?e))
     
   (UI-state (id ?id) (response ~?response))
   
   ?f2 <- (UI-state (id ?nid))
   
   =>
         
   (modify ?f1 (sequence ?b ?id ?e))
   
   (retract ?f2))
   
(defrule handle-next-response-end-of-chain

   (declare (salience 10))
   
   ?f1 <- (next ?id ?response)
   
   (state-list (sequence ?id $?))
   
   ?f2 <- (UI-state (id ?id)
                    (response ?expected)
                    (relation-asserted ?relation))
                
   =>
      
   (retract ?f1)

   (if (neq ?response ?expected)
      then
      (modify ?f2 (response ?response)))
      
   (assert (add-response ?id ?response)))   

(defrule handle-add-response

   (declare (salience 10))
   
   (logical (UI-state (id ?id)
                      (relation-asserted ?relation)))
   
   ?f1 <- (add-response ?id ?response)
                
   =>
      
   (str-assert (str-cat "(" ?relation " " ?response ")"))
   
   (retract ?f1))   

(defrule handle-add-response-none

   (declare (salience 10))
   
   (logical (UI-state (id ?id)
                      (relation-asserted ?relation)))
   
   ?f1 <- (add-response ?id)
                
   =>
      
   (str-assert (str-cat "(" ?relation ")"))
   
   (retract ?f1))   

(defrule handle-prev

   (declare (salience 10))
      
   ?f1 <- (prev ?id)
   
   ?f2 <- (state-list (sequence $?b ?id ?p $?e))
                
   =>
   
   (retract ?f1)
   
   (modify ?f2 (current ?p))
   
   (halt))
   
