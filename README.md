# Brazilian Protected Areas - Analysis --- pa-analysis

This project aims to scrape and analyze data from Brazilian Protected Areas available on the Internet 

- Guedes-Santos, J
- Federal University of Alagoas - PhD Student

Order:
1. getWikiLangviewsData.R
2. get_page_creation.R
When there are two pages for the same PA, it is necessary to define which page will be prioritized
Suggestion: add the values of the two pages
3. calculateMeanPageViewsData.R



Scripts

differences_in_pages.R
  
  List differences in the dataset:
    * Pages with different Wikidata Id in each language (Pt and Eng)
    * Pages with Wikidata Id only in Portuguese
    * Pages with Wikidata Id only in English

verifyDifferenceBetweenLangLists.R (Only for tests)

  Verifica no conjunto de dados se a ID Wikidata de cada Área Protegida se repete para cada idioma. Caso não, lista o conjunto de APs que não se repetem
  
getWikiLangviewsData.R
  
  Get CSV Files with PageViews from Wikipedia 
  
listTotalLanguages.R (Only for tests)

  Read all CSV Files from English Wikipages and get all languages
  
getWikiPageCreation.R
  
  Get date of page creation for each wikipage (English and Portuguese)