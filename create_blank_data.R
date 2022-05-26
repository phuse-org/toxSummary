

Data <- list(
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

saveRDS(Data, "blankData.rds")