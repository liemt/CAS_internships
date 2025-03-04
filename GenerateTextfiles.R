library(readxl)
setwd("D:/CAS_Internship_Git/CAS_Internships")

# Load the Excel file
file_path <- "Connection- Mission Statements & Webpage LTT.xlsx"
data <- read_excel(file_path, col_types = "text")  # Ensure all columns are read as text
print(colnames(data))

# Debugging: Print first few rows to check data loading
print(head(data))

# Standardize column names by removing leading/trailing spaces
colnames(data) <- trimws(colnames(data))

# Rename columns to match exact names from the latest file
colnames(data) <- c("BusinessType", "Name", "Missionstatement", "Additionaldescription", "LongtermInternship", 
                    "ShorttermInternshipShadowing", "Paid/Unpaid", "ResearchCourseProjectOpportunities", "DivisionalInterest")

# Function to create a formatted text file for each organization
create_text_file <- function(org_name, mission_statement, business_type, additional_desc, long_term_intern, 
                             short_term_intern, paid_unpaid, research_opps, divisional_interest) {
  
  # Handle potential NA values by replacing them with "Not provided"
  fields <- list(mission_statement, business_type, additional_desc, long_term_intern, short_term_intern, 
                 paid_unpaid, research_opps, divisional_interest)
  fields <- lapply(fields, function(x) ifelse(is.na(x) | x == "", "Not provided", x))
  
  # Clean organization name for the filename
  clean_org_name <- gsub("[^A-Za-z0-9]", "_", trimws(org_name))
  
  content <- sprintf("%s\n\nAbout the Organization\n%s\n\nInternship Details\nBusiness Type: %s\nAdditional Description: %s\nLong-term Internship: %s\nShort-term Internship/Shadowing: %s\nPaid/Unpaid: %s\nResearch & Course Project Opportunities: %s\nDivisional Interest: %s\n\nHow to Apply\nPlease reach out to the contact listed below.\n\nWebsite: https://artsci.utk.edu/\n\nContact Information\nName: Cat Cox\nEmail: ltran1@utk.edu\nTel: 333-333-3434\n",
                     org_name, fields[[1]], fields[[2]], fields[[3]], fields[[4]], fields[[5]], fields[[6]], fields[[7]], fields[[8]])
  
  file_name <- paste0(clean_org_name, ".txt")  # Ensure valid filename
  writeLines(content, file_name)
}

# Verify column names match exactly before accessing them
expected_columns <- c("Name", "Missionstatement", "BusinessType", "Additionaldescription", "LongtermInternship", 
                      "ShorttermInternshipShadowing", "Paid/Unpaid", 
                      "ResearchCourseProjectOpportunities", "DivisionalInterest")
missing_columns <- setdiff(expected_columns, colnames(data))

if (length(missing_columns) > 0) {
  stop(paste("Missing columns in data:", paste(missing_columns, collapse = ", ")))  # Stop execution if columns are missing
}

# Loop through each row and create text files
for (i in seq_len(nrow(data))) {
  create_text_file(
    org_name = data$Name[i],
    mission_statement = data$Missionstatement[i],
    business_type = data$BusinessType[i],
    additional_desc = data$Additionaldescription[i],
    long_term_intern = data$LongtermInternship[i],
    short_term_intern = data$ShorttermInternshipShadowing[i],
    paid_unpaid = data$`Paid/Unpaid`[i],
    research_opps = data$ResearchCourseProjectOpportunities[i],
    divisional_interest = data$DivisionalInterest[i]
  )
}
