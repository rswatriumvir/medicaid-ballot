# load libraries --------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(tabulizer)
library(tabulizerjars)
library(sjmisc)

options(scipen = 999) ##eliminates scientific notation, releavent for Idaho's data which was in scientific notation 

# deal with excel -------------------------------------------------------------------



## oklahoma
okl = read_csv("raw_data/oklahoma_june_2020_elec.csv")

okl_dt = okl %>% 
  filter(race_description == "STATE QUESTION NO. 802 INITIATIVE PETITION NO. 419") %>%
  select(elec_date, county, cand_name, cand_tot_votes) %>%
  pivot_wider(names_from=cand_name, values_from=cand_tot_votes) %>%
  rename(votes_for =`FOR THE PROPOSAL - YES`) %>%
  rename(votes_against = `AGAINST THE PROPOSAL - NO`) %>%
  mutate(state = "Oklahoma") %>%
  mutate(total_votes = votes_for + votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100) 



## idaho

# original file has useless first five rows; skip them
id = read_excel("raw_data/idaho_2018_results.xls", 
                sheet="Props - Voting Stats", 
                skip = 5)

id_dt = id %>%
  select(Counties, YES...4, NO...5) %>%
  rename(votes_for = YES...4) %>%
  rename(votes_against = NO...5) %>%
  mutate(state = "Idaho") %>%
  mutate(total_votes = votes_for + votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100) %>%
  mutate(elec_date = '11/06/2018') %>%
  rename(county = Counties) %>%
  slice(-(45:47))

  
  
  
  


## montana

# original file has useless first 6 rows; skip them
mt = read_excel("raw_data/montana_2018_tobacco.xlsx", 
                skip = 6)




mt_dt = mt %>%
  select(County,`YES on INITIATIVE NO. 185`,`NO on INITIATIVE NO. 185`) %>%
  mutate(state = "Montana") %>%
  mutate(elec_date = '11/06/2018') %>%
  rename(votes_for=`YES on INITIATIVE NO. 185`) %>%
  rename(votes_against=`NO on INITIATIVE NO. 185`) %>%
  mutate(total_votes = votes_for + votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100) %>%
  rename(county = County) %>%
  slice(-(57))

#maine
me = read_excel("raw_data/maine_prop_results.xlsx")
me_dt_tn = me %>% #Maine Municipality Level Data
  select(...1, ...2, `Question 2:  Citizen Initiative`, ...7, ...8) %>%
  rename(county = ...1) %>%
  rename(municipality = ...2) %>%
  rename(votes_for = `Question 2:  Citizen Initiative`) %>%
  rename(votes_against = ...7) %>%
  rename(votes_blank = ...8)

me_dt_tn$votes_for = as.numeric(me_dt_tn$votes_for)
me_dt_tn$votes_against = as.numeric(me_dt_tn$votes_against)
me_dt_tn$votes_blank = as.numeric(me_dt_tn$votes_blank)


me_dt = me_dt_tn %>% #Maine County Level Data
  slice(-(1)) %>%
  select(-(county)) %>%
  select(-(votes_blank)) %>%
  slice(-(513:514)) %>%
  rename(county = municipality)%>% #renaming so we can eliminate the towns in this column
  filter((grepl("County Totals", county) == TRUE)) %>%
  mutate(total_votes = votes_for+votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100) %>%
  mutate(state = "Maine") %>%
  mutate(elec_date = "11/07/2017")
  

me_dt$county = gsub("County Totals", "", me_dt$county)
me_dt$county = gsub(":", "", me_dt$county)
  
#utah
ut = read_excel("raw_data/2018 General Election Canvass Utah.xlsx", sheet = "Statewide Ballot Questions", skip = 2)

ut_dt = ut %>%
  select(County, FOR...6, AGAINST...7) %>% 
  rename(votes_for = FOR...6) %>%
  rename(votes_against = AGAINST...7) %>%
  rename(county = County) %>%
  mutate(total_votes = votes_for+votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100) %>%
  mutate(state = "Utah") %>%
  mutate(elec_date = "11/06/2018") %>%
  slice(-(30:32))

  


# deal with Missouri ----------------------------------------------------------------


# this is taken from NYT website
missouri = tibble::tribble(~`County`,	`Yes`	,`No`,	`Rpt.`,
                "St. Louis County", "181,501", "67,906",  "100%",
              "Jackson", "90,583", "32,466", "100",
          "St. Charles", "40,363", "38,171",  "100",
       "St. Louis City",
               "65,214",
                "8,556",
                  "100",
               "Greene",
               "27,772",
               "25,343",
                  "100",
                 "Clay",
               "26,467",
               "18,008",
                  "100",
            "Jefferson",
               "18,990",
               "22,063",
                  "100",
                "Boone",
               "23,697",
               "11,891",
                  "100",
                 "Cass",
                "9,853",
               "11,850",
                  "100",
               "Platte",
               "12,567",
                "7,810",
                  "100",
             "Franklin",
                "7,409",
               "11,892",
                  "100",
                 "Cole",
                "7,407",
               "11,313",
                  "100",
       "Cape Girardeau",
                "5,517",
               "11,756",
                  "100",
               "Jasper",
                "5,779",
               "11,401",
                  "100",
            "Christian",
                "5,656",
               "10,940",
                  "100",
             "Buchanan",
                "6,988",
                "7,408",
                  "100",
         "St. Francois",
                "5,274",
                "7,058",
                  "100",
              "Lincoln",
                "3,860",
                "7,903",
                  "100",
                "Taney",
                "3,711",
                "7,141",
                  "100",
               "Camden",
                "3,588",
                "7,227",
                  "100",
               "Newton",
                "2,837",
                "7,645",
                  "100",
                "Scott",
                "2,834",
                "6,074",
                  "100",
               "Pettis",
                "3,318",
                "5,437",
                  "100",
               "Phelps",
                "3,397",
                "5,357",
                  "100",
              "Johnson",
                "3,559",
                "4,983",
                  "100",
               "Howell",
                "2,624",
                "5,870",
                  "100",
                "Stone",
                "2,663",
                "5,710",
                  "100",
             "Callaway",
                "3,227",
                "5,056",
                  "100",
               "Butler",
                "2,418",
                "5,823",
                  "100",
             "Lawrence",
                "2,329",
                "5,583",
                  "100",
               "Warren",
                "2,621",
                "5,117",
                  "100",
                "Barry",
                "2,234",
                "5,388",
                  "100",
              "Webster",
                "2,094",
                "4,982",
                  "100",
             "Stoddard",
                "1,823",
                "4,961",
                  "100",
            "Lafayette",
                "2,836",
                "3,889",
                  "100",
              "LaClede",
                "1,816",
                "4,890",
                  "100",
                 "Polk",
                "1,988",
                "4,178",
                  "100",
              "Pulaski",
                "1,836",
                "4,274",
                  "100",
               "Miller",
                "1,573",
                "4,159",
                  "100",
                "Texas",
                "1,632",
                "4,045",
                  "100",
               "Marion",
                "1,672",
                "3,454",
                  "100",
                "Adair",
                "1,941",
                "3,034",
                  "100",
               "Vernon",
                "1,786",
                "3,124",
                  "100",
             "Randolph",
                "1,521",
                "3,286",
                  "100",
               "Andrew",
                "1,650",
                "3,117",
                  "100",
               "Wright",
                "1,064",
                "3,598",
                  "100",
              "Nodaway",
                "1,935",
                "2,666",
                  "100",
                "Henry",
                "1,887",
                "2,698",
                  "100",
             "Crawford",
                "1,484",
                "3,015",
                  "100",
              "Clinton",
                "1,819",
                "2,644",
                  "100",
           "Washington",
                "1,607",
                "2,799",
                  "100",
              "Dunklin",
                "1,322",
                "3,042",
                  "100",
                 "Pike",
                "1,517",
                "2,832",
                  "100",
                  "Ray",
                "1,846",
                "2,502",
                  "100",
               "Benton",
                "1,514",
                "2,827",
                  "100",
                "Perry",
                "1,214",
                "3,113",
                  "100",
            "Gasconade",
                "1,334",
                "2,949",
                  "100",
           "Livingston",
                "1,244",
                "2,985",
                  "100",
               "Cooper",
                "1,423",
                "2,752",
                  "100",
              "Douglas",
                "1,135",
                "2,984",
                  "100",
                 "Dent",
                "1,229",
                "2,836",
                  "100",
             "McDonald",
                "1,150",
                "2,911",
                  "100",
               "Morgan",
                "1,285",
                "2,677",
                  "100",
       "Ste. Genevieve",
                "1,735",
                "2,204",
                  "100",
              "Audrain",
                "1,426",
                "2,434",
                  "100",
               "Saline",
                "1,809",
                "2,033",
                  "100",
                "Bates",
                "1,285",
                "2,246",
                  "100",
               "Barton",
                  "686",
                "2,768",
                  "100",
             "Moniteau",
                  "782",
                "2,614",
                  "100",
                "Cedar",
                  "931",
                "2,416",
                  "100",
                "Osage",
                  "762",
                "2,580",
                  "100",
               "Dallas",
                  "935",
                "2,402",
                  "100",
            "Bollinger",
                  "719",
                "2,369",
                  "100",
           "Montgomery",
                  "994",
                "2,084",
                  "100",
                "Wayne",
                  "801",
                "2,268",
                  "100",
                "Ozark",
                  "897",
                "2,149",
                  "100",
                "Macon",
                  "735",
                "2,304",
                  "100",
                "Ralls",
                  "846",
                "2,112",
                  "100",
               "Ripley",
                  "921",
                "1,899",
                  "100",
            "St. Clair",
                  "931",
                "1,811",
                  "100",
                 "Linn",
                  "858",
                "1,805",
                  "100",
           "New Madrid",
                  "898",
                "1,740",
                  "100",
             "Caldwell",
                  "856",
                "1,734",
                  "100",
          "Mississippi",
                  "892",
                "1,660",
                  "100",
              "Shannon",
                  "909",
                "1,578",
                  "100",
               "Oregon",
                  "860",
                "1,609",
                  "100",
               "Howard",
                  "895",
                "1,487",
                  "100",
              "Hickory",
                  "725",
                "1,570",
                  "100",
               "DeKalb",
                  "630",
                "1,628",
                  "100",
              "Madison",
                  "755",
                "1,490",
                  "100",
               "Monroe",
                  "638",
                "1,599",
                  "100",
               "Maries",
                  "581",
                "1,655",
                  "100",
             "Pemiscot",
                  "762",
                "1,462",
                  "100",
                 "Dade",
                  "503",
                "1,590",
                  "100",
               "Grundy",
                  "537",
                "1,544",
                  "100",
              "Carroll",
                  "630",
                "1,413",
                  "100",
             "Reynolds",
                  "700",
                "1,257",
                  "100",
              "Daviess",
                  "599",
                "1,337",
                  "100",
             "Chariton",
                  "550",
                "1,312",
                  "100",
             "Harrison",
                  "495",
                "1,292",
                  "100",
               "Gentry",
                  "726",
                "1,033",
                  "100",
             "Atchison",
                  "546",
                "1,080",
                  "100",
               "Shelby",
                  "412",
                "1,189",
                  "100",
               "Carter",
                  "493",
                "1,090",
                  "100",
                "Lewis",
                  "377",
                "1,184",
                  "100",
                "Clark",
                  "402",
                "1,000",
                  "100",
             "Scotland",
                  "438",
                  "765",
                  "100",
                 "Holt",
                  "355",
                  "810",
                  "100",
             "Sullivan",
                  "218",
                  "786",
                  "100",
                 "Knox",
                  "237",
                  "666",
                  "100",
               "Mercer",
                  "186",
                  "674",
                  "100",
               "Putnam",
                  "171",
                  "628",
                  "100",
             "Schuyler",
                  "220",
                  "555",
                  "100",
                "Worth",
                  "220",
                  "455",
                  "100",
                 "Iron",
                   "64",
                   "70",
                  "100"
  )




## deal with PDFs --------------------------------------------------------------------

nb = extract_tables("raw_data/nebraska_2018_elec.pdf", pages = 65)
nb_tb = do.call(rbind, nb)

#nebraska

nb_tb = as.data.frame(nb_tb) #converting nb_tb1 from matrix to dataframe
nb_sl = nb_tb[4:6] #creating new dataframe with other set of counties from columns 4 to 6
nb_tb = nb_tb[1:3] #reducing original table to columns 1 to 3
nb_tb = slice(nb_tb,-(1:2)) #removing redundant rows


#renaming variables to merge using rbind
nb_tb1 = nb_tb %>%
  rename(county = V1) %>%
  rename(votes_for = V2) %>%
  rename(votes_against = V3)

nb_sl1 = nb_sl %>%
  rename(county = V4) %>%
  rename(votes_for = V5) %>%
  rename(votes_against = V6) %>%
  slice(-(48))

nb_tb_fn = rbind(nb_tb1, nb_sl1) 
nb_tb_fn$votes_for = as.numeric(gsub(",", "", nb_tb_fn$votes_for)) # convert variable from character string to numeric with commas removed
nb_tb_fn$votes_against = as.numeric(gsub(",", "", nb_tb_fn$votes_against)) #same process

nb_dt = nb_tb_fn %>%
  mutate(state = "Nebraska") %>%
  mutate(elec_date = "11/06/2018") %>%
  mutate(total_votes = votes_for + votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100)
  





# bind all the data
aca_referendums = rbind(mt_dt, okl_dt, id_dt, nb_dt, me_dt, ut_dt)




# quality control -------------------------------------------------------------------


# all vote shares should be less than 100%
aca_referendums$share_var <= 100


# othe checks...



# output table
write_rds(aca_referendums, "output_data/aca_refs.rds")

