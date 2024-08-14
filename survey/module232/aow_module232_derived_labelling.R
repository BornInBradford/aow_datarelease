# Module 232 survey labelling and tidying

source("tools/aow_survey_functions.R")

module <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module232_derived.rds")


# 80 chars:                           
################################################################################

## Labelling derived variables

module <- module |> set_variable_labels(rcad_ga = "Revised childrens anxiety and depression scale. General anxiety raw score",
                                        rcad_md = "Revised childrens anxiety and depression scale. Major depression raw score",
                                        rcad_total = "Revised childrens anxiety and depression scale. Total raw score",
                                        rcad_nas = "Revised childrens anxiety and depression scale. NAs",
                                        rcad_ga_miss = "Revised childrens anxiety and depression scale. General anxiety NAs",
                                        rcad_md_miss = "Revised childrens anxiety and depression scale. Major depression NAs",
                                        rcad_missing = "Revised childrens anxiety and depression scale. Total NAs",
                                        depression_int = "Depression int",
                                        depression_factor = "Depression factor",
                                        anxiety_int = "Anxiety int",
                                        anxiety_factor = "Anxiety factor",
                                        total_int = "Total int",
                                        total_factor = "Total factor",
                                        rcad_md_t = "RCADS Depression (higher = more depressed). Scaled score",
                                        rcad_ga_t = "RCADS Anxiety (higher = more anxiety). Scaled score",
                                        rcad_total_t = "RCADS Combined Total scaled score",
                                        rcad_md_cat = "Revised childrens anxiety and depression scale. Major depression category",
                                        rcad_ga_cat = "Revised childrens anxiety and depression scale. General anxiety category",
                                        rcad_total_cat = "Revised childrens anxiety and depression scale. Total category",
                                        sdq_emotion = "Strengths and Difficulties Questionnaire (SDQ): Emotional Problems Score",
                                        sdq_emotion_miss = "SDQ: Emotional Problems Score. Number of missing responses",
                                        sdq_emotion_cat = "SDQ: Emotional Problems Score. 4-band classification",
                                        sdq_conduct = "Stengths and Difficulties Questionnaire (SDQ): Conduct Problems Score",
                                        sdq_conduct_miss = "SDQ: Conduct Problems Score. Number of missing responses",
                                        sdq_conduct_cat = "SDQ: Conduct Problems Score. 4-band classification",
                                        sdq_hyperact = "Stengths and Difficulties Questionnaire (SDQ): Hyperactivity Score",
                                        sdq_hyperact_miss = "SDQ: Hyperactivity Score. Number of missing responses",
                                        sdq_hyperact_cat = "SDQ: Hyperactivity Problems Score. 4-band classification",
                                        sdq_peer = "Strengths and Difficulties Questionnaire (SDQ): Peer Problems Score",
                                        sdq_peer_miss = "SDQ: Peer Problems Score. Number of missing responses",
                                        sdq_peer_cat = "SDQ: Peer Problems Score. 4-band classification",
                                        sdq_prosoc = "Strengths and Difficulties Questionnaire (SDQ): Prosocial Score",
                                        sdq_prosoc_miss = "SDQ: Prosocial Score. Number of missing responses",
                                        sdq_prosoc_cat = "SDQ: Prosocial Score. 4-band classification",
                                        sdq_external = "Strengths and Difficulties Questionnaire (SDQ): Externalising Score",
                                        sdq_internal = "Strengths and Difficulties Questionnaire (SDQ): Internalising Score",
                                        sdq_total = "Strengths and Difficulties Questionnaire (SDQ): Total Score",
                                        sdq_total_cat = "SDQ: Total Score. 4-band classification",
                                        wellbeing = "Warwick-Edinburgh Mental Wellbeing Scale (higher = greater wellbeing). Raw score",
                                        wellbeing_nas = "Warwick-Edinburgh Mental Wellbeing Scale NAs",
                                        wellbeing_missing = "Warwick-Edinburgh Mental Wellbeing Scale missing",
                                        swemwbs_total = "Short Warwick-Edinburgh Mental Wellbeing Scale (higher = greater wellbeing). Scaled score",
                                        swemwbs_cat = "Short Warwick-Edinburgh Mental Wellbeing Scale category",
                                        loneliness = "UCLA Loneliness Scale (higher = more lonely)",
                                        loneliness_nas = "UCLA Loneliness Scale NAs",
                                        loneliness_missing = "UCLA Loneliness Scale missing",
                                        emo_inf = "General Help Seeking Qstnnaire Informal Sources (higher = more help seeking)",
                                        emo_form = "General Help Seeking Qstnnaire Formal Sources (higher = more help seeking)",
                                        emo_sch = "General Help Seeking Qstnnaire School (higher = more help seeking from school)",
                                        emo_total = "General Help Seeking Questionnaire (higher = more help seeking)",
                                        emo_nas = "General Help Seeking Questionnaire NAs",
                                        emo_missing = "General Help Seeking Questionnaire missing",
                                        brs_total = "Brief Resilience Scale (higher = more resilient)",
                                        brs_nas = "Brief Resilience Scale NAs",
                                        brs_missing = "Brief Resilience Scale missing",
                                        brs_mean = "Brief Resilience Scale mean",
                                        brs_cat = "Brief Resilience Scale category",
                                        addi_inst_exp = "Adolescent Discrimination Distress Index Institutional (higher = more discrim)",
                                        addi_peer_exp = "Adolescent Discrimination Distress Index Peer (higher = more discrim)",
                                        addi_total = "Adolescent Discrimination Distress Index total (higher = more discrimination)",
                                        addi_nas = "Adolescent Discrimination Distress Index NAs",
                                        addi_missing = "£Adolescent Discrimination Distress Index missing"
)



# export
saveRDS(module, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module232_derived_labelled.rds")



  