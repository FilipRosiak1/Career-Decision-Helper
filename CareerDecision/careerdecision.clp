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
;;; petla 1
(defrule determine-study-hard ""
  (logical (or (marks-higher Yes)
  			   (done-40-times No)))
  =>
  (assert (UI-state (display StudyHardQuestion)
                    (relation-asserted willing-to-study)
                    (response No)
                    (valid-answers No Maybe Yes))))
;;;petla 1
(defrule determine-work-40 ""
  (logical (worked))
  =>
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
;;; petla 2       
(defrule determine-language ""
  (logical (or (want-to-help Yes)
               (took-classes)))
  =>
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

;;;****************
;;;* STATEMENT RULES *
;;;****************

(defrule state-become-hippie ""
  (logical (responsible-job No))
  =>
  (assert (UI-state (display HippieStatement)
                    (relation-asserted stop))))
;;; petla 1
(defrule state-work-year ""
  (logical (willing-to-study Maybe))
  =>
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
;;; petla 2
(defrule state-taking-class ""
  (logical (good-at-en-fr No))
  =>
  (assert (UI-state (display LanguageClassStatement)
                    (relation-asserted took-classes))))
                    
(defrule state-cant-help ""
  (logical (want-to-be-rich Yes))
  =>
  (assert (UI-state (display CantHelpStatement)
                    (relation-asserted noHelp))))
                    
(defrule state-dont-bother ""
  (logical (noHelp))
  =>
  (assert (UI-state (display DontBotherStatement)
                    (relation-asserted goaway))))

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
   
