# Script to create package data files
# Run this script to regenerate the data files in data/

# =============================================================================
# UEF Unit Translations and Hierarchy
# =============================================================================
# Maps Finnish and English unit names to standardized English names,
# abbreviations, departments, and faculties.

# Helper function to add rows
add_unit <- function(df, unit, unit_en, unit_abbrev, department, faculty) {
  rbind(df, data.frame(
    unit = unit,
    unit_en = unit_en,
    unit_abbrev = unit_abbrev,
    department = department,
    faculty = faculty,
    stringsAsFactors = FALSE
  ))
}

# Initialize data frame
unit_translations <- data.frame(
  unit = character(),
  unit_en = character(),
  unit_abbrev = character(),
  department = character(),
  faculty = character(),
  stringsAsFactors = FALSE
)

# =============================================================================
# HEALTH SCIENCES FACULTY
# =============================================================================

# A.I. Virtanen Institute
unit_translations <- add_unit(unit_translations, "A.I. Virtanen -instituutti", "A.I. Virtanen Institute", "AI Virtanen", "A.I. Virtanen Institute", "Health Sciences")
unit_translations <- add_unit(unit_translations, "A.I. Virtanen -instituutti / Bioteknologia ja molekulaarinen lääketiede", "A.I. Virtanen Institute", "AI Virtanen", "A.I. Virtanen Institute", "Health Sciences")
unit_translations <- add_unit(unit_translations, "A.I. Virtanen -instituutti / Koe-eläinkeskus", "A.I. Virtanen Institute", "AI Virtanen", "A.I. Virtanen Institute", "Health Sciences")
unit_translations <- add_unit(unit_translations, "A.I. Virtanen -instituutti / Neurobiologia", "A.I. Virtanen Institute", "AI Virtanen", "A.I. Virtanen Institute", "Health Sciences")
unit_translations <- add_unit(unit_translations, "A.I. Virtanen Institute", "A.I. Virtanen Institute", "AI Virtanen", "A.I. Virtanen Institute", "Health Sciences")

# Biomedicine
unit_translations <- add_unit(unit_translations, "Biolääketiede", "Biomedicine", "Biomedicine", "Biomedicine", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Biolääketieteen yksikkö", "Biomedicine", "Biomedicine", "Biomedicine", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Biomedicine", "Biomedicine", "Biomedicine", "Biomedicine", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Lääketieteen laitos / Biolääketiede", "Biomedicine", "Biomedicine", "Biomedicine", "Health Sciences")

# Dentistry
unit_translations <- add_unit(unit_translations, "Hammaslääketiede", "Dentistry", "Dentistry", "Dentistry", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Hammaslääketieteen yksikkö", "Dentistry", "Dentistry", "Dentistry", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Dentistry", "Dentistry", "Dentistry", "Dentistry", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Lääketieteen laitos / Hammaslääketiede", "Dentistry", "Dentistry", "Dentistry", "Health Sciences")

# Nursing Science
unit_translations <- add_unit(unit_translations, "Hoitotieteen laitoksen toiminta", "Nursing Science", "Nursing Sci.", "Nursing Science", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Hoitotieteen laitos", "Nursing Science", "Nursing Sci.", "Nursing Science", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Hoitotieteen laitos / Toiminta", "Nursing Science", "Nursing Sci.", "Nursing Science", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Department of Nursing Science", "Nursing Science", "Nursing Sci.", "Nursing Science", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Department of Nursing Sciences", "Nursing Science", "Nursing Sci.", "Nursing Science", "Health Sciences")

# Public Health and Clinical Nutrition
unit_translations <- add_unit(unit_translations, "Kansanterveystiede ja kliininen ravitsemustiede", "Public Health", "Public Health", "Public Health", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Lääketieteen laitos / Kansanterveystiede", "Public Health", "Public Health", "Public Health", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Lääketieteen laitos / Kansanterveystiede ja kliininen ravitsemustiede", "Public Health", "Public Health", "Public Health", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Lääketieteen laitos / Kliininen ravitsemustiede", "Public Health", "Public Health", "Public Health", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Public Health and Clinical Nutrition", "Public Health", "Public Health", "Public Health", "Health Sciences")

# Clinical Medicine
unit_translations <- add_unit(unit_translations, "Kliininen lääketiede", "Clinical Medicine", "Clinical Med.", "Clinical Medicine", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Kliinisen lääketieteen yksikkö", "Clinical Medicine", "Clinical Med.", "Clinical Medicine", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Lääketieteen laitos / Kliininen lääketiede", "Clinical Medicine", "Clinical Med.", "Clinical Medicine", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Clinical Medicine", "Clinical Medicine", "Clinical Med.", "Clinical Medicine", "Health Sciences")

# Lab Animal Centre
unit_translations <- add_unit(unit_translations, "Koe-eläinkeskus", "Lab Animal Centre", "Lab Animal", "Lab Animal Centre", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Koe-eläinkeskus / Toiminta", "Lab Animal Centre", "Lab Animal", "Lab Animal Centre", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Lab Animal Centre", "Lab Animal Centre", "Lab Animal", "Lab Animal Centre", "Health Sciences")

# Medicine (shared)
unit_translations <- add_unit(unit_translations, "Lääketieteen laitoksen yhteiset", "Medicine", "Medicine", "Medicine", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Lääketieteen laitos / Yhteiset", "Medicine", "Medicine", "Medicine", "Health Sciences")
unit_translations <- add_unit(unit_translations, "School of Medicine", "Medicine", "Medicine", "Medicine", "Health Sciences")

# Pharmacy
unit_translations <- add_unit(unit_translations, "Farmasian laitoksen toiminta", "Pharmacy", "Pharmacy", "Pharmacy", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Farmasian laitos", "Pharmacy", "Pharmacy", "Pharmacy", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Farmasian laitos / Toiminta", "Pharmacy", "Pharmacy", "Pharmacy", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Itä-Suomen yliopiston apteekki", "Pharmacy", "Pharmacy", "Pharmacy", "Health Sciences")
unit_translations <- add_unit(unit_translations, "School of Pharmacy", "Pharmacy", "Pharmacy", "Pharmacy", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Pharmacy of University of Eastern Finland", "Pharmacy", "Pharmacy", "Pharmacy", "Health Sciences")

# Health Sciences (shared/admin)
unit_translations <- add_unit(unit_translations, "Terveystieteiden tiedekunnan hallinto / Hallintopalvelukeskus", "Health Sciences (admin)", "Health (admin)", "Health Sciences (admin)", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Terveystieteiden tiedekunnan hallinto / Yhteiset", "Health Sciences (admin)", "Health (admin)", "Health Sciences (admin)", "Health Sciences")
unit_translations <- add_unit(unit_translations, "Faculty of Health Sciences, shared activities", "Health Sciences (shared)", "Health (shared)", "Health Sciences (shared)", "Health Sciences")

# =============================================================================
# SCIENCE AND FORESTRY FACULTY
# =============================================================================

# Physics and Mathematics
unit_translations <- add_unit(unit_translations, "Fysiikan ja matematiikan laitoksen toiminta", "Physics and Mathematics", "Physics & Math", "Physics and Mathematics", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Fysiikan ja matematiikan laitos", "Physics and Mathematics", "Physics & Math", "Physics and Mathematics", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Fysiikan ja matematiikan laitos / Toiminta", "Physics and Mathematics", "Physics & Math", "Physics and Mathematics", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Department of Physics and Mathematics", "Physics and Mathematics", "Physics & Math", "Physics and Mathematics", "Science and Forestry")

# Chemistry
unit_translations <- add_unit(unit_translations, "Kemian ja kestävän teknologian laitos", "Chemistry", "Chemistry", "Chemistry", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Kemian laitoksen toiminta", "Chemistry", "Chemistry", "Chemistry", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Kemian laitos", "Chemistry", "Chemistry", "Chemistry", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Kemian laitos / Toiminta", "Chemistry", "Chemistry", "Chemistry", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Department of Chemistry", "Chemistry", "Chemistry", "Chemistry", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Department of Chemistry and Sustainable Technology", "Chemistry", "Chemistry", "Chemistry", "Science and Forestry")

# Technical Physics
unit_translations <- add_unit(unit_translations, "Sovelletun fysiikan laitoksen toiminta", "Technical Physics", "Tech. Physics", "Technical Physics", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Sovelletun fysiikan laitos / Sovelletun fysiikan laitoksen toiminta", "Technical Physics", "Tech. Physics", "Technical Physics", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Teknillisen fysiikan laitos", "Technical Physics", "Tech. Physics", "Technical Physics", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Department of Technical Physics", "Technical Physics", "Tech. Physics", "Technical Physics", "Science and Forestry")

# Computing
unit_translations <- add_unit(unit_translations, "Tietojenkäsittelytieteen laitoksen toiminta", "Computing", "Computing", "Computing", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Tietojenkäsittelytieteen laitos", "Computing", "Computing", "Computing", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Tietojenkäsittelytieteen laitos / HIS", "Computing", "Computing", "Computing", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Tietojenkäsittelytieteen laitos / Tietojenkäsittelytieteen laitoksen toiminta", "Computing", "Computing", "Computing", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Tietojenkäsittelytieteen laitos / Yhteiset", "Computing", "Computing", "Computing", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "School of Computing", "Computing", "Computing", "Computing", "Science and Forestry")

# Environmental and Biological Sciences
unit_translations <- add_unit(unit_translations, "Ympäristö- ja biotieteiden laitoksen toiminta", "Env. & Bio. Sciences", "Env. & Bio. Sci.", "Env. & Bio. Sciences", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Ympäristö- ja biotieteiden laitos", "Env. & Bio. Sciences", "Env. & Bio. Sci.", "Env. & Bio. Sciences", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Ympäristö- ja biotieteiden laitos / Toiminta", "Env. & Bio. Sciences", "Env. & Bio. Sci.", "Env. & Bio. Sciences", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Biologian laitos / Biologian laitoksen toiminta", "Env. & Bio. Sciences", "Env. & Bio. Sci.", "Env. & Bio. Sciences", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Biologian laitos / Yhteiset", "Env. & Bio. Sciences", "Env. & Bio. Sci.", "Env. & Bio. Sciences", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Department of Environmental and Biological Sciences", "Env. & Bio. Sciences", "Env. & Bio. Sci.", "Env. & Bio. Sciences", "Science and Forestry")

# Forest Sciences
unit_translations <- add_unit(unit_translations, "Metsätieteiden osasto", "Forest Sciences", "Forest Sci.", "Forest Sciences", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Metsätieteiden osasto / Toiminta", "Forest Sciences", "Forest Sci.", "Forest Sciences", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Metsätieteiden osaston toiminta", "Forest Sciences", "Forest Sci.", "Forest Sciences", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "School of Forest Sciences", "Forest Sciences", "Forest Sci.", "Forest Sciences", "Science and Forestry")

# Science and Forestry (admin)
unit_translations <- add_unit(unit_translations, "Luonnontieteiden ja metsätieteiden tiedekunnan hallinto / Hallintopalvelukeskus", "Sci. & Forestry (admin)", "Sci.For. (admin)", "Sci. & Forestry (admin)", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Luonnontieteiden ja metsätieteiden tiedekunnan hallinto / Yhteiset", "Sci. & Forestry (admin)", "Sci.For. (admin)", "Sci. & Forestry (admin)", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Luonnontieteiden, metsätieteiden ja tekniikan tiedekunnan yhteiset", "Sci. & Forestry (admin)", "Sci.For. (admin)", "Sci. & Forestry (admin)", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "Mekrijärven tutkimusasema / Toiminta", "Research Station", "Research Stn.", "Research Station", "Science and Forestry")
unit_translations <- add_unit(unit_translations, "SIB-labs -infrastruktuuriyksikön toiminta", "SIB-labs", "SIB-labs", "SIB-labs", "Science and Forestry")

# =============================================================================
# SOCIAL SCIENCES AND BUSINESS FACULTY
# =============================================================================

# Business School
unit_translations <- add_unit(unit_translations, "Kauppatieteiden laitoksen toiminta", "Business School", "Business", "Business School", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Kauppatieteiden laitos", "Business School", "Business", "Business School", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Kauppatieteiden laitos / Toiminta", "Business School", "Business", "Business School", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Business School", "Business School", "Business", "Business School", "Social Sciences and Business")

# Geographical and Historical Studies
unit_translations <- add_unit(unit_translations, "Historia", "Geog. & Hist. Studies", "Geog. & Hist.", "Geog. & Hist. Studies", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Historia- ja maantieteiden laitoksen yhteiset", "Geog. & Hist. Studies", "Geog. & Hist.", "Geog. & Hist. Studies", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Historia- ja maantieteiden laitos", "Geog. & Hist. Studies", "Geog. & Hist.", "Geog. & Hist. Studies", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Historia- ja maantieteiden laitos / Historia", "Geog. & Hist. Studies", "Geog. & Hist.", "Geog. & Hist. Studies", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Historia- ja maantieteiden laitos / Maantiede", "Geog. & Hist. Studies", "Geog. & Hist.", "Geog. & Hist. Studies", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Historia- ja maantieteiden laitos / Yhteiset", "Geog. & Hist. Studies", "Geog. & Hist.", "Geog. & Hist. Studies", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Historia- ja maantieteiden laitos / Ympäristöpolitiikka", "Geog. & Hist. Studies", "Geog. & Hist.", "Geog. & Hist. Studies", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Maantiede", "Geog. & Hist. Studies", "Geog. & Hist.", "Geog. & Hist. Studies", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Ympäristöpolitiikka", "Geog. & Hist. Studies", "Geog. & Hist.", "Geog. & Hist. Studies", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Department of Geographical and Historical Studies", "Geog. & Hist. Studies", "Geog. & Hist.", "Geog. & Hist. Studies", "Social Sciences and Business")

# Health and Social Management
unit_translations <- add_unit(unit_translations, "Sosiaali- ja terveysjohtamisen laitoksen toiminta", "Health & Social Management", "Health & Soc. Mgmt.", "Health & Social Management", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Sosiaali- ja terveysjohtamisen laitos", "Health & Social Management", "Health & Soc. Mgmt.", "Health & Social Management", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Sosiaali- ja terveysjohtamisen laitos / Toiminta", "Health & Social Management", "Health & Soc. Mgmt.", "Health & Social Management", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Department of Health and Social Management", "Health & Social Management", "Health & Soc. Mgmt.", "Health & Social Management", "Social Sciences and Business")

# Social Sciences
unit_translations <- add_unit(unit_translations, "Yhteiskuntatieteiden laitoksen toiminta", "Social Sciences", "Social Sci.", "Social Sciences", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Yhteiskuntatieteiden laitos", "Social Sciences", "Social Sci.", "Social Sciences", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Yhteiskuntatieteiden laitos / Toiminta", "Social Sciences", "Social Sci.", "Social Sciences", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Department of Social Sciences", "Social Sciences", "Social Sci.", "Social Sciences", "Social Sciences and Business")

# Law School
unit_translations <- add_unit(unit_translations, "Oikeustieteiden laitoksen toiminta", "Law School", "Law", "Law School", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Oikeustieteiden laitos", "Law School", "Law", "Law School", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Oikeustieteiden laitos / Toiminta", "Law School", "Law", "Law School", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Law School", "Law School", "Law", "Law School", "Social Sciences and Business")

# Karelian Institute
unit_translations <- add_unit(unit_translations, "Karjalan tutkimuslaitoksen toiminta", "Karelian Institute", "Karelian Inst.", "Karelian Institute", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Karjalan tutkimuslaitos", "Karelian Institute", "Karelian Inst.", "Karelian Institute", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Karjalan tutkimuslaitos / Toiminta", "Karelian Institute", "Karelian Inst.", "Karelian Institute", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Karelian Institute", "Karelian Institute", "Karelian Inst.", "Karelian Institute", "Social Sciences and Business")

# Social Sciences and Business (shared/admin)
unit_translations <- add_unit(unit_translations, "Matkailualan opetus- ja tutkimuslaitos / Toiminta", "Tourism Studies", "Tourism", "Tourism Studies", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Yhteiskuntatieteiden ja kauppatieteiden tiedekunnan hallinto / Yhteiset", "Soc. Sci. & Bus. (admin)", "Soc.Bus. (admin)", "Soc. Sci. & Bus. (admin)", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Yhteiskuntatieteiden ja kauppatieteiden tiedekunnan yhteiset", "Soc. Sci. & Bus. (admin)", "Soc.Bus. (admin)", "Soc. Sci. & Bus. (admin)", "Social Sciences and Business")
unit_translations <- add_unit(unit_translations, "Faculty of Social Sciences and Business Studies, shared activities", "Soc. Sci. & Bus. (shared)", "Soc.Bus. (shared)", "Soc. Sci. & Bus. (shared)", "Social Sciences and Business")

# =============================================================================
# PHILOSOPHICAL FACULTY
# =============================================================================

# Applied Educational Science and Teacher Education
unit_translations <- add_unit(unit_translations, "Soveltavan kasvatustieteen ja opettajakoulutuksen osasto", "Applied Educ. Science", "Applied Educ.", "Applied Educ. Science", "Philosophical")
unit_translations <- add_unit(unit_translations, "Soveltavan kasvatustieteen ja opettajankoulutuksen osasto", "Applied Educ. Science", "Applied Educ.", "Applied Educ. Science", "Philosophical")
unit_translations <- add_unit(unit_translations, "Soveltavan kasvatustieteen ja opettajankoulutuksen osasto / Joensuu", "Applied Educ. Science", "Applied Educ.", "Applied Educ. Science", "Philosophical")
unit_translations <- add_unit(unit_translations, "Soveltavan kasvatustieteen ja opettajankoulutuksen osasto / Savonlinna", "Applied Educ. Science", "Applied Educ.", "Applied Educ. Science", "Philosophical")
unit_translations <- add_unit(unit_translations, "Soveltavan kasvatustieteen ja opettajankoulutuksen osasto / Yhteiset", "Applied Educ. Science", "Applied Educ.", "Applied Educ. Science", "Philosophical")
unit_translations <- add_unit(unit_translations, "School of Applied Educational Science and Teacher Education", "Applied Educ. Science", "Applied Educ.", "Applied Educ. Science", "Philosophical")

# Educational Sciences and Psychology
unit_translations <- add_unit(unit_translations, "Kasvatustieteiden ja psykologian osasto", "Educ. Sciences & Psychology", "Educ. & Psych.", "Educ. Sciences & Psychology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Kasvatustieteiden ja psykologian osasto / Erityispedagogiikka", "Educ. Sciences & Psychology", "Educ. & Psych.", "Educ. Sciences & Psychology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Kasvatustieteiden ja psykologian osasto / Kasvatustiede, aikuiskasvatus ja ohjaus", "Educ. Sciences & Psychology", "Educ. & Psych.", "Educ. Sciences & Psychology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Kasvatustieteiden ja psykologian osasto / Psykologia", "Educ. Sciences & Psychology", "Educ. & Psych.", "Educ. Sciences & Psychology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Kasvatustieteiden ja psykologian osasto / Yhteiset", "Educ. Sciences & Psychology", "Educ. & Psych.", "Educ. Sciences & Psychology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Kasvatustieteiden ja psykologian osaston yhteiset", "Educ. Sciences & Psychology", "Educ. & Psych.", "Educ. Sciences & Psychology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Erityispedagogiikka", "Educ. Sciences & Psychology", "Educ. & Psych.", "Educ. Sciences & Psychology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Kasvatustiede, aikuiskasvatus ja ohjaus", "Educ. Sciences & Psychology", "Educ. & Psych.", "Educ. Sciences & Psychology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Psykologia", "Educ. Sciences & Psychology", "Educ. & Psych.", "Educ. Sciences & Psychology", "Philosophical")
unit_translations <- add_unit(unit_translations, "School of Educational Sciences and Psychology", "Educ. Sciences & Psychology", "Educ. & Psych.", "Educ. Sciences & Psychology", "Philosophical")

# Humanities
unit_translations <- add_unit(unit_translations, "Humanistinen osasto", "Humanities", "Humanities", "Humanities", "Philosophical")
unit_translations <- add_unit(unit_translations, "Humanistinen osasto / Suomen kieli ja kulttuuritieteet", "Humanities", "Humanities", "Humanities", "Philosophical")
unit_translations <- add_unit(unit_translations, "Humanistinen osasto / Vieraat kielet ja käännöstiede", "Humanities", "Humanities", "Humanities", "Philosophical")
unit_translations <- add_unit(unit_translations, "Humanistinen osasto / Yhteiset", "Humanities", "Humanities", "Humanities", "Philosophical")
unit_translations <- add_unit(unit_translations, "Humanistisen osaston yhteiset", "Humanities", "Humanities", "Humanities", "Philosophical")
unit_translations <- add_unit(unit_translations, "Suomen kieli ja kulttuuritieteet", "Humanities", "Humanities", "Humanities", "Philosophical")
unit_translations <- add_unit(unit_translations, "Vieraat kielet ja käännöstiede", "Humanities", "Humanities", "Humanities", "Philosophical")
unit_translations <- add_unit(unit_translations, "School of Humanities", "Humanities", "Humanities", "Humanities", "Philosophical")

# Theology
unit_translations <- add_unit(unit_translations, "Teologian osasto", "Theology", "Theology", "Theology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Teologian osasto / Läntinen teologia", "Theology", "Theology", "Theology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Teologian osasto / Ortodoksinen teologia", "Theology", "Theology", "Theology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Teologian osasto / Yhteiset", "Theology", "Theology", "Theology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Teologian osaston yhteiset", "Theology", "Theology", "Theology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Läntinen teologia", "Theology", "Theology", "Theology", "Philosophical")
unit_translations <- add_unit(unit_translations, "Ortodoksinen teologia", "Theology", "Theology", "Theology", "Philosophical")
unit_translations <- add_unit(unit_translations, "School of Theology", "Theology", "Theology", "Theology", "Philosophical")

# Teacher Training Schools
unit_translations <- add_unit(unit_translations, "Harjoittelukoulun toiminta", "Teacher Training School", "Teacher Training", "Teacher Training Schools", "Philosophical")
unit_translations <- add_unit(unit_translations, "Harjoittelukoulun yhteiset", "Teacher Training School", "Teacher Training", "Teacher Training Schools", "Philosophical")
unit_translations <- add_unit(unit_translations, "Rantakylän koulun perusaste", "Teacher Training School", "Teacher Training", "Teacher Training Schools", "Philosophical")
unit_translations <- add_unit(unit_translations, "Tulliportti School, basic education", "Teacher Training School", "Teacher Training", "Teacher Training Schools", "Philosophical")
unit_translations <- add_unit(unit_translations, "University Teacher Training School, shared activities", "Teacher Training School", "Teacher Training", "Teacher Training Schools", "Philosophical")

# Philosophical Faculty (admin)
unit_translations <- add_unit(unit_translations, "Filosofisen tiedekunnan hallinto / Hallintopalvelukeskus", "Philosophical (admin)", "Philos. (admin)", "Philosophical (admin)", "Philosophical")
unit_translations <- add_unit(unit_translations, "Filosofisen tiedekunnan hallinto / Yhteiset", "Philosophical (admin)", "Philos. (admin)", "Philosophical (admin)", "Philosophical")
unit_translations <- add_unit(unit_translations, "Filosofisen tiedekunnan yhteiset", "Philosophical (admin)", "Philos. (admin)", "Philosophical (admin)", "Philosophical")
unit_translations <- add_unit(unit_translations, "Philosophical Faculty, shared activities", "Philosophical (shared)", "Philos. (shared)", "Philosophical (shared)", "Philosophical")

# =============================================================================
# ADMINISTRATION
# =============================================================================

# University administration
unit_translations <- add_unit(unit_translations, "Itä-Suomen yliopisto", "University of Eastern Finland", "UEF", "University Admin", "Administration")
unit_translations <- add_unit(unit_translations, "Itä-Suomen yliopiston johto / Rehtori", "University Admin", "UEF Admin", "University Admin", "Administration")
unit_translations <- add_unit(unit_translations, "Itä-Suomen yliopiston johto / Yhteiset", "University Admin", "UEF Admin", "University Admin", "Administration")
unit_translations <- add_unit(unit_translations, "Yliopiston johto", "University Admin", "UEF Admin", "University Admin", "Administration")
unit_translations <- add_unit(unit_translations, "Yliopistopalvelujen toiminta", "University Admin", "UEF Admin", "University Admin", "Administration")
unit_translations <- add_unit(unit_translations, "Yliopistopalvelut  / Kehittämispalvelut", "University Admin", "UEF Admin", "University Admin", "Administration")
unit_translations <- add_unit(unit_translations, "University of Eastern Finland", "University of Eastern Finland", "UEF", "University Admin", "Administration")

# Human Resources
unit_translations <- add_unit(unit_translations, "Henkilöstöpalvelut", "Human Resources", "HR Services", "Human Resources", "Administration")
unit_translations <- add_unit(unit_translations, "Hallintokeskus / Henkilöstöyksikkö, Henkilöstön kehittäminen", "Human Resources", "HR Services", "Human Resources", "Administration")
unit_translations <- add_unit(unit_translations, "Yliopistopalvelut / Henkilöstöpalvelut", "Human Resources", "HR Services", "Human Resources", "Administration")
unit_translations <- add_unit(unit_translations, "Human Resources Services", "Human Resources", "HR Services", "Human Resources", "Administration")

# Language Centre
unit_translations <- add_unit(unit_translations, "Kielikeskus", "Language Centre", "Language Ctr.", "Language Centre", "Administration")
unit_translations <- add_unit(unit_translations, "Language Centre", "Language Centre", "Language Ctr.", "Language Centre", "Administration")

# Library
unit_translations <- add_unit(unit_translations, "Kirjasto", "Library", "Library", "Library", "Administration")
unit_translations <- add_unit(unit_translations, "Library", "Library", "Library", "Library", "Administration")

# Student Services
unit_translations <- add_unit(unit_translations, "Koulutuspalvelut", "Student Services", "Student Svc.", "Student Services", "Administration")
unit_translations <- add_unit(unit_translations, "Opintopalvelut", "Student Services", "Student Svc.", "Student Services", "Administration")
unit_translations <- add_unit(unit_translations, "Yliopistopalvelut / Opinto- ja opetuspalvelut", "Student Services", "Student Svc.", "Student Services", "Administration")
unit_translations <- add_unit(unit_translations, "Student and Learning Services", "Student Services", "Student Svc.", "Student Services", "Administration")

# IT Services
unit_translations <- add_unit(unit_translations, "Tietotekniikkakeskus", "IT Services", "IT Services", "IT Services", "Administration")

# Continuous Learning
unit_translations <- add_unit(unit_translations, "Jatkuvan oppimisen keskuksen hallinto", "Continuous Learning", "Cont. Learning", "Continuous Learning", "Administration")
unit_translations <- add_unit(unit_translations, "Jatkuvan oppimisen keskuksen liiketoiminta", "Continuous Learning", "Cont. Learning", "Continuous Learning", "Administration")
unit_translations <- add_unit(unit_translations, "Hyvinvoinnin edistämispalvelut, täydennyskoulutus ja kehittämistoiminta", "Continuous Learning", "Cont. Learning", "Continuous Learning", "Administration")

# Other admin
unit_translations <- add_unit(unit_translations, "Talouspalvelut", "Financial Services", "Finance", "Financial Services", "Administration")
unit_translations <- add_unit(unit_translations, "Yleishallinto- ja lakipalvelut", "Legal Services", "Legal", "Legal Services", "Administration")
unit_translations <- add_unit(unit_translations, "Yliopistopalvelut / Yleishallinto- ja lakipalvelut", "Legal Services", "Legal", "Legal Services", "Administration")
unit_translations <- add_unit(unit_translations, "Ei laitostietoa", "Unknown", "Unknown", "Unknown", "Administration")

# Save the data
usethis::use_data(unit_translations, overwrite = TRUE)

# =============================================================================
# UEF Units (Legacy - for backward compatibility)
# =============================================================================
uef_units <- data.frame(
  unit_name = c(
    "A.I. Virtanen Institute", "Biomedicine", "Business School", "Clinical Medicine",
    "Dentistry", "Department of Chemistry and Sustainable Technology",
    "Department of Environmental and Biological Sciences",
    "Department of Geographical and Historical Studies",
    "Department of Health and Social Management", "Department of Nursing Science",
    "Department of Physics and Mathematics", "Department of Social Sciences",
    "Department of Technical Physics", "Faculty of Health Sciences, shared activities",
    "Faculty of Social Sciences and Business Studies, shared activities",
    "Human Resources Services", "Karelian Institute", "Lab Animal Centre",
    "Language Centre", "Law School", "Library", "Public Health and Clinical Nutrition",
    "School of Applied Educational Science and Teacher Education",
    "School of Computing", "School of Educational Sciences and Psychology",
    "School of Forest Sciences", "School of Humanities", "School of Medicine",
    "School of Pharmacy", "School of Theology", "Student and Learning Services",
    "Tulliportti School, basic education", "University of Eastern Finland",
    "University Teacher Training School, shared activities"
  ),
  unit_abbrev = c(
    "AI Virtanen", "Biomedicine", "Business", "Clinical Med.", "Dentistry",
    "Chemistry", "Env. & Bio. Sci.", "Geog. & Hist.", "Health & Soc. Mgmt.",
    "Nursing Sci.", "Physics & Math", "Social Sci.", "Tech. Physics",
    "Health (shared)", "Soc.Bus. (shared)", "HR Services", "Karelian Inst.",
    "Lab Animal", "Language Ctr.", "Law", "Library", "Public Health",
    "Applied Educ.", "Computing", "Educ. & Psych.", "Forest Sci.",
    "Humanities", "Medicine", "Pharmacy", "Theology", "Student Svc.",
    "Teacher Training", "UEF", "Teacher Training"
  ),
  faculty = c(
    "Health Sciences", "Health Sciences", "Social Sciences and Business",
    "Health Sciences", "Health Sciences", "Science and Forestry",
    "Science and Forestry", "Social Sciences and Business",
    "Social Sciences and Business", "Health Sciences", "Science and Forestry",
    "Social Sciences and Business", "Science and Forestry", "Health Sciences",
    "Social Sciences and Business", "Administration", "Social Sciences and Business",
    "Health Sciences", "Administration", "Social Sciences and Business",
    "Administration", "Health Sciences", "Philosophical", "Science and Forestry",
    "Philosophical", "Science and Forestry", "Philosophical", "Health Sciences",
    "Health Sciences", "Philosophical", "Administration", "Philosophical",
    "Administration", "Philosophical"
  ),
  stringsAsFactors = FALSE
)

usethis::use_data(uef_units, overwrite = TRUE)

# =============================================================================
# Sample Publications Dataset
# =============================================================================
set.seed(42)
n_pubs <- 100

sample_publications <- data.frame(
  publication_id = paste0("PUB", sprintf("%04d", 1:n_pubs)),
  authors = sapply(1:n_pubs, function(i) {
    n_authors <- sample(1:6, 1, prob = c(0.1, 0.3, 0.25, 0.2, 0.1, 0.05))
    authors <- paste0(
      sample(c("Smith", "Johnson", "Williams", "Brown", "Jones",
               "Garcia", "Miller", "Davis", "Rodriguez", "Martinez"),
             n_authors, replace = TRUE),
      ", ", sample(LETTERS, n_authors, replace = TRUE), "."
    )
    paste(authors, collapse = "; ")
  }),
  first_author = paste0(
    sample(c("Smith", "Johnson", "Williams", "Brown", "Jones",
             "Garcia", "Miller", "Davis", "Rodriguez", "Martinez"),
           n_pubs, replace = TRUE),
    ", ", sample(LETTERS, n_pubs, replace = TRUE), "."
  ),
  publication_year = sample(2019:2024, n_pubs, replace = TRUE,
                            prob = c(0.1, 0.15, 0.15, 0.2, 0.2, 0.2)),
  jufo_level_of_publication = sample(c(0, 1, 2, 3), n_pubs, replace = TRUE,
                                      prob = c(0.3, 0.4, 0.2, 0.1)),
  title_of_publication = paste0("Publication Title ", 1:n_pubs),
  first_authors_unit = sample(uef_units$unit_name, n_pubs, replace = TRUE),
  stringsAsFactors = FALSE
)

usethis::use_data(sample_publications, overwrite = TRUE)

cat("Data files created successfully!\n")
cat("- data/unit_translations.rda (", nrow(unit_translations), " unit mappings)\n")
cat("- data/uef_units.rda\n")
cat("- data/sample_publications.rda\n")
