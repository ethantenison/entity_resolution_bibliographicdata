# 1. Generate a codebook of this dataset
-----------------------------------------

In this exercise we created two separate dataframes using our
disambiguation method.

### 1. Top Authors 

Number of variables: 3  
Number of records: 3,505  

Varibles:  
author_fullname  
(factor) The author’s full name  

unique_id  
(num) Each author’s unique identification number  

person_sum  
(num) Total number of publications cited by author  

### 2.Top Organizations 

Number of variables: 3  
Number of records: 1,580  

Varibles:  
affiliate  
(factor) Name of the organization affiliate  

unique_id  
(num) Each organizations’s unique identification number  

org_sum  
(num) Total number of publications attributed to the organization  

# 2. Create criteria for disambiguating authors and affiliations
-----------------------------------------------------------------

##### (Done in Python)

Capturing all of the authors first and last names and appending them
together.

    import pandas as pd 
    import os
    import math
    import pickle

    os.chdir('../../CUSS/Disambig/') # CD here

    df = pd.read_pickle('../..//CUSS/Disambig/df_core_article_full_info_sample.pkl.gz')


    author_firstname = []
    author_lastname = []

    for i in df['authors']:
        if type(i) is not dict and type(i) is not list:
            if i is None:
                name = ' '
                author_firstname.append(name)
                continue
        else:
            name = i['author'][0]['preferred-name']['ce:given-name']
            author_firstname.append(name)
        
    df['author_firstname'] = author_firstname

    for i in df['authors']:
        if type(i) is not dict and type(i) is not list:
            if i is None:
                name = ' '
                author_lastname.append(name)
                continue
        else:
            name = i['author'][0]['preferred-name']['ce:surname']
            author_lastname.append(name)
        
    df['author_lastname'] = author_lastname

    df['author_fullname'] = df['author_firstname'] + " " + df['author_lastname']

    def cleantext(x):
        x = x.str.strip()  # Remove extra white space
        x = x.str.lower() # Convert to lower
        x = x.str.replace('.','') # Delete period
        x = x.str.replace('\'',  '') # delete colon
        x = x.str.replace('-',  ' ') # delete - 
        return(x)

    df['author_fullname'] = cleantext(df['author_fullname'])

Compiling the cities, countries, and affiliations that each author is connected to
==================================================================================

    city = []
    country = []
    affiliate = []

    for i in df['affiliation']:
        if type(i) is not dict and type(i) is not list:
            if math.isnan(i):
                city_i = ' '
                country_i = ' '
                affil = ' '
                city.append(city_i)
                country.append(country_i)
                affiliate.append(affil)
                continue
        if type(i) is list:
            city_i = i[0]['affiliation-city']
            country_i = i[0]['affiliation-country']
            affil = i[0]['affilname']
            city.append(city_i)
            country.append(country_i)
            affiliate.append(affil)
            continue
        else:
            city_i = i['affiliation-city']
            country_i = i['affiliation-country']
            affil = i['affilname']
            city.append(city_i)
            country.append(country_i)
            affiliate.append(affil)

    df['city'] = city
    df['country'] = country
    df['affiliate']=affiliate

    df['city'] = cleantext(df['city'])
    df['country'] = cleantext(df['country'])
    df['affiliate'] = cleantext(df['affiliate'])

    subject = []
    for i in df['subject-areas']:
        if i is None:
            sub = ' '
            subject.append(sub)
            continue
        else:
            sub = i['subject-area'][0]['$']
            subject.append(sub)

    df['subject'] = subject
    df['subject'] = cleantext(df['subject'])


    pickle.dump(df, open( "extracted_df.p", "wb" ) )

# 3. Explain why the criteria can generate valid results
---------------------------------------------------------

There is relatively low distance between the actual data and the fuzzy
match that we used, with just a 10% threshold of different.

# 4. Compile a function according to the criteria, run the function on records, and give unique IDs to these entities
----------------------------------------------------------------------------------------------------------------------

##### (Done in R)

Creating the unique ids with the name match

    library("dplyr")
    library("writexl")


    #Name Matching
    rm(list=ls())

    df <- read.csv("./extracted_df.csv")

    class(df$author_fullname)

    df$author_fullname <- as.character(df$author_fullname)

    df_namematch <- df %>% select(X, author_fullname)
    df_namematch <- df_namematch %>% subset(author_fullname != "")
    df_namematch$bins <- sapply(df_namematch$author_fullname, function(n)
      paste(as.integer(agrepl(n, df_namematch$author_fullname, max.distance = .1)), collapse=""))
    df_namematch$unique_id <- as.integer(as.factor(df_namematch$bins))
    df_namematch$bins <- NULL
    df_namematch$author_fullname <-NULL

    df <- left_join(df, df_namematch, by = "X")

### Next we match the organizations

    df_orgmatch <- df %>% select(X, affiliate)
    df_orgmatch <- df_orgmatch %>% subset(affiliate != "")
    df_orgmatch$bins <- sapply(df_orgmatch$affiliate, function(n)
      paste(as.integer(agrepl(n, df_orgmatch$affiliate, max.distance = .1)), collapse=""))
    df_orgmatch$unique_id_org <- as.integer(as.factor(df_orgmatch$bins))
    df_orgmatch$bins <- NULL
    df_orgmatch$affiliate <-NULL

    df <- left_join(df, df_orgmatch, by = "X")

### Finally we aggregated the top authors and the top cited affiliations

    df_2 <- df 

    df_2 <- df_2 %>% group_by(unique_id) %>% mutate(person_sum = sum(n())) %>% ungroup()
    df_2 <- df_2 %>% group_by(unique_id_org) %>% mutate(org_sum = sum(n())) %>% ungroup()

    top_people <- df_2  %>% select(author_fullname, unique_id, person_sum)
    top_people <- top_people[!duplicated(top_people$unique_id),]
    top_people <- top_people %>%  arrange(desc(person_sum))
    top_orgs <- df_2  %>% select(affiliate, unique_id_org, org_sum)
    top_orgs <- top_orgs[!duplicated(top_orgs$unique_id_org),]
    top_orgs <- top_orgs %>%  arrange(desc(org_sum))

    head(top_people)
    head(top_orgs)

### Below are the top organizations and author’s in non-profit studies.

    library("xlsx")

    top_orgs <- read.xlsx("top_orgs.xlsx", sheetIndex = "Sheet1")
    head(top_orgs)

    ##                           affiliate unique_id_org org_sum
    ## 1                              <NA>            NA     938
    ## 2 university of southern california          1508      27
    ## 3                indiana university          1192      25
    ## 4          university of queensland          1444      24
    ## 5                harvard law school          1200      24
    ## 6        university of pennsylvania          1424      22

    top_people <- read.xlsx("top_people.xlsx", sheetIndex = "Sheet1")
    head(top_people)

    ##   author_fullname unique_id person_sum
    ## 1            <NA>        NA        177
    ## 2  edgard milhaud      3302         20
    ## 3  dennis r young      3331         18
    ## 4 anghel n rugina      3449         15
    ## 5  ernest raiklin      3042         14
    ## 6     jon van til      2765         13

# 5. Verify accuracy: choose a random sample and manually check the false positive and false negative rates
------------------------------------------------------------------------------------------------------------

# 6.
-----

1.  who are the most productive authors in nonprofit studies? The top 3
    authors are Edgard Milhaud, Dennis Young, and Anghel Rugina. All
    three had a strong background in economics.

2.  Which are the most productive institutions in nonprofit studies? The
    top 3 organizations were USC, Indiana University, and the University
    of Queensland.

3.  How you define “productive.” Our algorithm equates productive to be
    the total number of publications.
