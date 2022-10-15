## code to prepare `DATASET` dataset goes here

blank_data <- list(
    CmaxUnit = "ng/mL",
    AUCUnit = "ng*h/mL",
    "Clinical Information" = list(
        HumanWeight = 60,
        MgKg = F,
        "Start Dose" = list(
            StartDose = NULL,
            StartDoseMgKg = NULL,
            StartDoseCmax = NULL,
            StartDoseAUC = NULL
        ),
        "MRHD" = list(
            MRHD = NULL,
            MRHDMgKg = NULL,
            MRHDCmax = NULL,
            MRHDAUC = NULL
        ),
        "Custom Dose" = list(
            CustomDose = NULL,
            CustomDoseMgKg = NULL,
            CustomDoseCmax = NULL,
            CustomDoseAUC = NULL
        )
    ),
    "Nonclinical Information" = list(
        "New Study" = list(
			IND_number = "",
			studyid_name = "",
            Species = NULL,
            Sex_include = NULL,
            Duration = "",
            Notes = NULL,
            check_note = F,
            nDoses = 1,
            Doses = list(Dose1 = list(
                Dose = "",
                NOAEL = F,
                Cmax = "",
                AUC = ""
            )),
            nFindings = 1,
            Findings = list(Finding1 = list(
                Finding = "",
                Reversibility = "[Rev]",
                Severity = list(
                    Dose1 = "Absent"
                )
            ))
        )
    )
)



##########

applications_demo <- list(CmaxUnit = "ng/mL", AUCUnit = "ng*h/mL",
 `Clinical Information` = list(
    HumanWeight = 60L, MgKg = FALSE, `Start Dose` = list(
        StartDose = 1.5,
        StartDoseMgKg = NULL, StartDoseCmax = 2L, StartDoseAUC = 13L
    ),
    MRHD = list(MRHD = 2L, MRHDMgKg = NULL, MRHDCmax = 3L, MRHDAUC = 18L),
    `Custom Dose` = list(
        CustomDose = NULL, CustomDoseMgKg = NULL,
        CustomDoseCmax = NULL, CustomDoseAUC = NULL
    )
), `Nonclinical Information` = list(
    `New Study` = list(
        Species = NULL, Duration = "", Notes = NULL,
        check_note = FALSE, nDoses = 1, Doses = list(Dose1 = list(
            Dose = "", NOAEL = FALSE, Cmax = "", AUC = ""
        )),
        nFindings = 1, Findings = list(Finding1 = list(
            Finding = "",
            Reversibility = "[Rev]", Severity = list(Dose1 = "Absent")
        ))
    ),
    `Dog: 4 Weeks (IV)` = list(
        Species = "Dog", Duration = "4 Weeks (IV)",
        Notes = "Note for Dog 4 Weeks (IV)", check_note = TRUE,
        nDoses = 3L, Doses = list(Dose1 = list(
            Dose = 0.25, NOAEL = FALSE,
            Cmax = 92L, AUC = 74L
        ), Dose2 = list(
            Dose = 0.8,
            NOAEL = FALSE, Cmax = 512L, AUC = 333L
        ), Dose3 = list(
            Dose = 2.5, NOAEL = FALSE, Cmax = 875L, AUC = 1045L
        )),
        nFindings = 5L, Findings = list(Finding1 = list(
            Finding = "Duodenum: Necrosis",
            Reversibility = "[Rev]", Severity = list(
                Dose1 = "Absent",
                Dose2 = "Absent", Dose3 = "Absent"
            )
        ), Finding2 = list(
            Finding = "ALT Increased", Reversibility = "[Rev]",
            Severity = list(
                Dose1 = "Absent", Dose2 = "Absent",
                Dose3 = "Marked"
            )
        ), Finding3 = list(
            Finding = "AST Increased",
            Reversibility = "[Rev]", Severity = list(
                Dose1 = "Minimal",
                Dose2 = "Mild", Dose3 = "Moderate"
            )
        ), Finding4 = list(
            Finding = "Stomach: Erosion", Reversibility = "",
            Severity = list(
                Dose1 = "Mild", Dose2 = "Moderate",
                Dose3 = "Marked"
            )
        ), Finding5 = list(
            Finding = "Spleen: Necrosis",
            Reversibility = "[Rev]", Severity = list(
                Dose1 = "Mild",
                Dose2 = "Moderate", Dose3 = "Marked"
            )
        ))
    ), `Rat: 13 Weeks (SC)` = list(
        Species = "Rat", Duration = "13 Weeks (SC)", 
		Notes = "Note for Rat 13 Weeks (SC)",
        check_note = TRUE, nDoses = 3L, Doses = list(
            Dose1 = list(
                Dose = 0.8, NOAEL = FALSE, Cmax = 32L, AUC = 70L
            ),
            Dose2 = list(
                Dose = 2.5, NOAEL = TRUE, Cmax = 108L,
                AUC = 240L
            ), Dose3 = list(
                Dose = 8L, NOAEL = FALSE,
                Cmax = 306L, AUC = 688L
            )
        ), nFindings = 4L, Findings = list(
            Finding1 = list(
                Finding = "Spleen: Necrosis", Reversibility = "",
                Severity = list(
                    Dose1 = "Absent", Dose2 = "Absent",
                    Dose3 = "Mild"
                )
            ), Finding2 = list(
                Finding = "Liver: Necrosis",
                Reversibility = "", Severity = list(
                    Dose1 = "Absent",
                    Dose2 = "Absent", Dose3 = "Mild"
                )
            ), Finding3 = list(
                Finding = "AST Increased", Reversibility = "",
                Severity = list(
                    Dose1 = "Minimal", Dose2 = "Mild",
                    Dose3 = "Mild"
                )
            ), Finding4 = list(
                Finding = "Liver: Hypertrophy",
                Reversibility = "[Rev]", Severity = list(
                    Dose1 = "Absent",
                    Dose2 = "Absent", Dose3 = "Marked"
                )
            )
        )
    ), `Rat: 4 Weeks (SC)` = list(
        Species = "Rat", Duration = "4 Weeks (SC)",
		 Notes = "Note for Rat 4 Weeks (SC)",
        check_note = TRUE, nDoses = 4L, Doses = list(
            Dose1 = list(
                Dose = 0.25, NOAEL = FALSE, Cmax = 5L, AUC = 14L
            ),
            Dose2 = list(
                Dose = 0.8, NOAEL = FALSE, Cmax = 11L,
                AUC = 38L
            ), Dose3 = list(
                Dose = 2.5, NOAEL = TRUE,
                Cmax = 26L, AUC = 99L
            ), Dose4 = list(
                Dose = 8L,
                NOAEL = FALSE, Cmax = 90L, AUC = 355L
            )
        ), nFindings = 4L,
        Findings = list(
            Finding1 = list(
                Finding = "Duodenum: Necrosis",
                Reversibility = "[Rev]", Severity = list(
                    Dose1 = "Absent",
                    Dose2 = "Absent", Dose3 = "Absent", Dose4 = "Present"
                )
            ),
            Finding2 = list(
                Finding = "Mortality", Reversibility = "[NR]",
                Severity = list(
                    Dose1 = "Absent", Dose2 = "Absent",
                    Dose3 = "Absent", Dose4 = "Present"
                )
            ), Finding3 = list(
                Finding = "Stomach: Degeneration/regeneration",
                Reversibility = "", Severity = list(
                    Dose1 = "Absent",
                    Dose2 = "Absent", Dose3 = "Absent", Dose4 = "Marked"
                )
            ),
            Finding4 = list(
                Finding = "Liver: Necrosis", Reversibility = "",
                Severity = list(
                    Dose1 = "Absent", Dose2 = "Absent",
                    Dose3 = "Absent", Dose4 = "Mild"
                )
            )
        )
    ), `Rat: 13 Weeks with 8 Weeks Recovery (IV)` = list(
        Species = "Rat", Duration = "13 Weeks with 8 Weeks Recovery (IV)",
        Notes = "Note for Rat 13 Weeks with 8   \nWeeks Recovery (IV)",
        check_note = TRUE, nDoses = 3L, Doses = list(
            Dose1 = list(
                Dose = 0.5, NOAEL = TRUE, Cmax = 354L, AUC = 142L
            ),
            Dose2 = list(
                Dose = 1.5, NOAEL = FALSE, Cmax = 4520L,
                AUC = 1672L
            ), Dose3 = list(
                Dose = 5L, NOAEL = FALSE,
                Cmax = 7412L, AUC = 4325L
            )
        ), nFindings = 1L,
        Findings = list(Finding1 = list(
            Finding = "Liver: Necrosis",
            Reversibility = "[Rev]", Severity = list(
                Dose1 = "Absent",
                Dose2 = "Minimal", Dose3 = "Mild"
            )
        ))
    ), `Dog: 13 Weeks with 8 Weeks Recovery (IV)` = list(
        Species = "Dog", Duration = "13 Weeks with 8 Weeks Recovery (IV)",
        Notes = "Nothing to mention", check_note = TRUE, nDoses = 3L,
        Doses = list(Dose1 = list(
            Dose = 0.08, NOAEL = FALSE,
            Cmax = 35L, AUC = 18L
        ), Dose2 = list(
            Dose = 0.4,
            NOAEL = FALSE, Cmax = 145L, AUC = 98L
        ), Dose3 = list(
            Dose = 1L, NOAEL = FALSE, Cmax = 402L, AUC = 358L
        )),
        nFindings = 5L, Findings = list(Finding1 = list(
            Finding = "Duodenum: Necrosis",
            Reversibility = "[Rev]", Severity = list(
                Dose1 = "Absent",
                Dose2 = "Absent", Dose3 = "Present"
            )
        ), Finding2 = list(
            Finding = "AST Increased", Reversibility = "[Rev]",
            Severity = list(
                Dose1 = "Minimal", Dose2 = "Mild",
                Dose3 = "Moderate"
            )
        ), Finding3 = list(
            Finding = "Duodenum: Congestion",
            Reversibility = "[PR]", Severity = list(
                Dose1 = "Absent",
                Dose2 = "Minimal", Dose3 = "Moderate"
            )
        ), Finding4 = list(
            Finding = "Spleen: Necrosis", Reversibility = "[Rev]",
            Severity = list(
                Dose1 = "Mild", Dose2 = "Moderate",
                Dose3 = "Severe"
            )
        ), Finding5 = list(
            Finding = "Liver: Necrosis",
            Reversibility = "[PR]", Severity = list(
                Dose1 = "Absent",
                Dose2 = "Minimal", Dose3 = "Mild"
            )
        ))
    ), `Dog: 39 Weeks with 8 Weeks Recovery (SC)` = list(
        Species = "Dog", Duration = "39 Weeks with 8 Weeks Recovery (SC)",
        Notes = "", check_note = FALSE, nDoses = 3L, Doses = list(
            Dose1 = list(
                Dose = 0.08, NOAEL = TRUE, Cmax = 3L,
                AUC = 8L
            ), Dose2 = list(
                Dose = 0.25, NOAEL = FALSE,
                Cmax = 13L, AUC = 51L
            ), Dose3 = list(
                Dose = 0.8,
                NOAEL = FALSE, Cmax = 18L, AUC = 194L
            )
        ), nFindings = 6L,
        Findings = list(Finding1 = list(
            Finding = "Mortality",
            Reversibility = "[NR]", Severity = list(
                Dose1 = "Absent",
                Dose2 = "Absent", Dose3 = "Present"
            )
        ), Finding2 = list(
            Finding = "AST Increased", Reversibility = "[Rev]",
            Severity = list(
                Dose1 = "Absent", Dose2 = "Mild",
                Dose3 = "Moderate"
            )
        ), Finding3 = list(
            Finding = "ALT Increased",
            Reversibility = "[Rev]", Severity = list(
                Dose1 = "Absent",
                Dose2 = "Minimal", Dose3 = "Marked"
            )
        ), Finding4 = list(
            Finding = "Stomach: Degeneration/regeneration",
			 Reversibility = "[PR]",
            Severity = list(
                Dose1 = "Absent", Dose2 = "Minimal",
                Dose3 = "Moderate"
            )
        ), Finding5 = list(
            Finding = "Liver: Necrosis",
            Reversibility = "[Rev]", Severity = list(
                Dose1 = "Absent",
                Dose2 = "Minimal", Dose3 = "Mild"
            )
        ), Finding6 = list(
            Finding = "Spleen: Vacuolation", Reversibility = "[Rev]",
            Severity = list(
                Dose1 = "Absent", Dose2 = "Minimal",
                Dose3 = "Marked"
            )
        ))
    )
))
usethis::use_data(blank_data,
applications_demo,
 overwrite = TRUE, internal = TRUE)
