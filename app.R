pkgload::load_all(".")

db_path <- "test_data/test_db.db"
study_list_path <- "test_data/IND_with_studies_2.csv"

toxSummary::toxSummary_app(
    database_path = db_path,
    studyid_file = study_list_path,
    save_file_path = NULL
)
