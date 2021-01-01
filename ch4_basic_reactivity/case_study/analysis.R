library(shiny)
library(tidyverse)

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries
#> # A tibble: 255,064 x 10
#>   trmt_date    age sex   race  body_part diag  location prod_code weight
#>   <date>     <dbl> <chr> <chr> <chr>     <chr> <chr>        <dbl>  <dbl>
#> 1 2017-01-01    71 male  white Upper Tr… Cont… Other P…      1807   77.7
#> 2 2017-01-01    16 male  white Lower Arm Burn… Home           676   77.7
#> 3 2017-01-01    58 male  white Upper Tr… Cont… Home           649   77.7
#> 4 2017-01-01    21 male  white Lower Tr… Stra… Home          4076   77.7
#> 5 2017-01-01    54 male  white Head      Inte… Other P…      1807   77.7
#> 6 2017-01-01    21 male  white Hand      Frac… Home          1884   77.7
#> # … with 255,058 more rows, and 1 more variable: narrative <chr>

products <- vroom::vroom("neiss/products.tsv")
products
#> # A tibble: 38 x 2
#>   prod_code title                            
#>       <dbl> <chr>                            
#> 1       464 knives, not elsewhere classified 
#> 2       474 tableware and accessories        
#> 3       604 desks, chests, bureaus or buffets
#> 4       611 bathtubs or showers              
#> 5       649 toilets                          
#> 6       676 rugs or carpets, not specified   
#> # … with 32 more rows

population <- vroom::vroom("neiss/population.tsv")
population
#> # A tibble: 170 x 3
#>     age sex    population
#>   <dbl> <chr>       <dbl>
#> 1     0 female    1924145
#> 2     0 male      2015150
#> 3     1 female    1943534
#> 4     1 male      2031718
#> 5     2 female    1965150
#> 6     2 male      2056625
#> # … with 164 more rows

# toilets
selected <- injuries %>% filter(prod_code == 649)
nrow(selected)
#> [1] 2993

selected %>% count(location, wt = weight, sort = TRUE)
#> # A tibble: 6 x 2
#>   location                         n
#>   <chr>                        <dbl>
#> 1 Home                       99603. 
#> 2 Other Public Property      18663. 
#> 3 Unknown                    16267. 
#> 4 School                       659. 
#> 5 Street Or Highway             16.2
#> 6 Sports Or Recreation Place    14.8

selected %>% count(body_part, wt = weight, sort = TRUE)
#> # A tibble: 24 x 2
#>   body_part        n
#>   <chr>        <dbl>
#> 1 Head        31370.
#> 2 Lower Trunk 26855.
#> 3 Face        13016.
#> 4 Upper Trunk 12508.
#> 5 Knee         6968.
#> 6 N.S./Unk     6741.
#> # … with 18 more rows

selected %>% count(diag, wt = weight, sort = TRUE)
#> # A tibble: 20 x 2
#>   diag                       n
#>   <chr>                  <dbl>
#> 1 Other Or Not Stated   32897.
#> 2 Contusion Or Abrasion 22493.
#> 3 Inter Organ Injury    21525.
#> 4 Fracture              21497.
#> 5 Laceration            18734.
#> 6 Strain, Sprain         7609.
#> # … with 14 more rows

summary <- selected %>% 
  count(age, sex, wt = weight)
summary
#> # A tibble: 208 x 3
#>     age sex         n
#>   <dbl> <chr>   <dbl>
#> 1     0 female   4.76
#> 2     0 male    14.3 
#> 3     1 female 253.  
#> 4     1 male   231.  
#> 5     2 female 438.  
#> 6     2 male   632.  
#> # … with 202 more rows

# plot line graph of age by number of injuries
summary %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")

# problem: there are probably more young people than old, so we have to control by comparing
# the number of people injured with the total population and calculating an injury rate (10,000 people)
summary <- selected  %>% 
  count(age, sex, wt = weight) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4)

# this is more accurate and males drop off quicker because women tend to live longer
summary <- selected %>% 
  count(age, sex, wt = weight) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4)

summary
#> # A tibble: 208 x 5
#>     age sex         n population   rate
#>   <dbl> <chr>   <dbl>      <dbl>  <dbl>
#> 1     0 female   4.76    1924145 0.0247
#> 2     0 male    14.3     2015150 0.0708
#> 3     1 female 253.      1943534 1.30  
#> 4     1 male   231.      2031718 1.14  
#> 5     2 female 438.      1965150 2.23  
#> 6     2 male   632.      2056625 3.07  
#> # … with 202 more rows
  
selected %>% 
  sample_n(10) %>% 
  pull(narrative)
#>  [1] "94YOF-STAFF AT NH STATES PT WAS ON TOILET AND WAS LOWERED TO THE FLOORDX: SPIRAL FX LEFT FEMUR ADMITT"                          
#>  [2] "49 YOF,PT WAS SITTING ON TOILET WHEN SHE STARTED TO FALL, TRIED TO BREAK FALL SUSTAINED INJURY TO SHOULDER. DX. SPRAIN SHOULDER"
#>  [3] "87 YO M FELL OFF TOILET STRIKING FACE ON BATHTUBHEMATOMA  FOREHEAD"                                                             
#>  [4] "88YOF FELL OFF OF TOILET TO THE FLOOR. DX FALL AND RIGHT SHOULDER PAIN"                                                         
#>  [5] "73YOF TRANSFERRING FROM THE SHOWER TO THE COMMODE AND FELL ONTO BUTTOCKS - CONTUSION BUTTOCKS"                                  
#>  [6] "79 YOM WAS SITTING ON TOILET & FELL FORWARD HITTING FACEDX FACIAL CONTUSION"                                                    
#>  [7] "69YOM AT HOME GETTING UP FROM TOILET LEGS GAVE OUT FELL FORWARD ONTO KNEES DX MECHANICAL FALL BILATERAL KNEE CONTUSION"         
#>  [8] "3YOM FELL OFF TOILET NO LOC DX RT SHOULDER INJURY/CONTUSION,RT ELBOW PAIN"                                                      
#>  [9] "20 MONTH F FELL OFF TOILET AND HIT HEAD DX:CHI"                                                                                 
#> [10] "80YF WAS BEING TX'D FROM W/C TO THE TOILET&ACC SCRAPED LEG ON THE WHEELCHAIR>>LAC"

# This was a lot of typing so now its time to write a Shiny app...
