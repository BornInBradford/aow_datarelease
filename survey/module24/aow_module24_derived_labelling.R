# Module 24 survey labelling and tidying

source("tools/aow_survey_functions.R")

module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module24_derived.rds")


# 80 chars:                           
################################################################################

## Labelling derived variables

module <- module |> set_variable_labels(own_fin_total = "Own Financial Resources (higher = more sources)",
                                        food_avail_total = "Food Availability (higher = more food insecurity)",
                                        edeqs_total = "Eating Disorder Examination (higher = more disordered)",
                                        edeqs_cat = "Eating Disorder Examination category",
                                        paqa_total = "Physical Activity Questionnaire (higher = more active)",
                                        paqa_mean = "Physical Activity Questionnaire (higher = more active)",
                                        yapsed_total = "Youth Activity Profile (higher = more sedentary)",
                                        yapsed_mean = "Youth Activity Profile mean (higher = more sedentary)",
                                        rcad_ga = "RCADS-25 General anxiety. Raw score",
                                        rcad_md = "RCADS-25 Major depression. Raw score",
                                        rcad_total = "RCADS-25 Anxiety and depression. Raw score",
                                        rcad_md_t = "RCADS-25 Major depression (higher = more depressed). Scaled score",
                                        rcad_ga_t = "RCADS-25 General anxiety (higher = more anxiety). Scaled score",
                                        rcad_total_t = "RCADS-25 Anxiety and depression. Scaled score",
                                        rcad_md_cat = "RCADS-25 Major depression category",
                                        rcad_ga_cat = "RCADS-25 General anxiety category",
                                        rcad_total_cat = "RCADS-25 Anxiety and depression category",
                                        sdq_emotion = "Strengths and Difficulties Questionnaire (SDQ): Emotional Problems Score",
                                        sdq_emotion_cat = "SDQ: Emotional Problems Score. 4-band classification",
                                        sdq_conduct = "Stengths and Difficulties Questionnaire (SDQ): Conduct Problems Score",
                                        sdq_conduct_cat = "SDQ: Conduct Problems Score. 4-band classification",
                                        sdq_hyperact = "Stengths and Difficulties Questionnaire (SDQ): Hyperactivity Score",
                                        sdq_hyperact_cat = "SDQ: Hyperactivity Problems Score. 4-band classification",
                                        sdq_peer = "Strengths and Difficulties Questionnaire (SDQ): Peer Problems Score",
                                        sdq_peer_cat = "SDQ: Peer Problems Score. 4-band classification",
                                        sdq_prosoc = "Strengths and Difficulties Questionnaire (SDQ): Prosocial Score",
                                        sdq_prosoc_cat = "SDQ: Prosocial Score. 4-band classification",
                                        sdq_external = "Strengths and Difficulties Questionnaire (SDQ): Externalising Score",
                                        sdq_internal = "Strengths and Difficulties Questionnaire (SDQ): Internalising Score",
                                        sdq_total = "Strengths and Difficulties Questionnaire (SDQ): Total Score",
                                        sdq_total_cat = "SDQ: Total Score. 4-band classification",
                                        wellbeing = "Warwick-Edinburgh Mental Wellbeing Scale (higher = greater wellbeing). Raw score",
                                        swemwbs_total = "Short Warwick-Edinburgh Mental Wellbeing Scale (higher = greater wellbeing). Scaled score",
                                        swemwbs_cat = "Short Warwick-Edinburgh Mental Wellbeing Scale category",
                                        loneliness = "UCLA Loneliness Scale (higher = more lonely)",
                                        emo_inf = "General Help Seeking Qstnnaire Informal Sources (higher = more help seeking)",
                                        emo_form = "General Help Seeking Qstnnaire Formal Sources (higher = more help seeking)",
                                        emo_sch = "General Help Seeking Qstnnaire School (higher = more help seeking from school)",
                                        emo_total = "General Help Seeking Questionnaire (higher = more help seeking)",
                                        brs_total = "Brief Resilience Scale (higher = more resilient)",
                                        brs_mean = "Brief Resilience Scale mean",
                                        brs_cat = "Brief Resilience Scale category",
                                        addi_inst_exp = "Adolescent Discrimination Distress Index Institutional (higher = more discrim)",
                                        addi_peer_exp = "Adolescent Discrimination Distress Index Peer (higher = more discrim)",
                                        addi_total = "Adolescent Discrimination Distress Index total (higher = more discrimination)"
                                        
)


# export
saveRDS(module, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module24_derived_labelled.rds")



  