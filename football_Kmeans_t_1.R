#God is good all the times

#libraries
library(ggplot2)
install.packages("factoextra")
library(factoextra)
library(RColorBrewer)
library(dplyr)
library(tidyr)

#Loading the data
fb = read.csv("C:/Users/Dr Vinod/Desktop/football_ kmean/data.csv",
                stringsAsFactors = TRUE)
fb$Name


dim(fb)
'18207    89'

#Checking the missing values
sapply(fb, function(x) sum(is.null(x)))
sapply(fb, function(x) sum(is.na(x)))
'48 missing values in most of the variables & 48 is less in proportion compare
complete data so removing the missing values'

#Removed the missing values
fb = fb[complete.cases(fb),]

sapply(fb, function(x) sum(is.na(x)))
'No missing values'

dim(fb)
'18147    89'
"18207-18147 = 60 removed"

str(fb)

data.frame(colnames(fb))
"
               colnames.fb.
1                       ï..
2                        ID
3                      Name
4                       Age
5                     Photo
6               Nationality
7                      Flag
8                   Overall
9                 Potential
10                     Club
11                Club.Logo
12                    Value
13                     Wage
14                  Special
15           Preferred.Foot
16 International.Reputation
17                Weak.Foot
18              Skill.Moves
19                Work.Rate
20                Body.Type
21                Real.Face
22                 Position
23            Jersey.Number
24                   Joined
25              Loaned.From
26     Contract.Valid.Until
27                   Height
28                   Weight
29                       LS
30                       ST
31                       RS
32                       LW
33                       LF
34                       CF
35                       RF
36                       RW
37                      LAM
38                      CAM
39                      RAM
40                       LM
41                      LCM
42                       CM
43                      RCM
44                       RM
45                      LWB
46                      LDM
47                      CDM
48                      RDM
49                      RWB
50                       LB
51                      LCB
52                       CB
53                      RCB
54                       RB
55                 Crossing
56                Finishing
57          HeadingAccuracy
58             ShortPassing
59                  Volleys
60                Dribbling
61                    Curve
62               FKAccuracy
63              LongPassing
64              BallControl
65             Acceleration
66              SprintSpeed
67                  Agility
68                Reactions
69                  Balance
70                ShotPower
71                  Jumping
72                  Stamina
73                 Strength
74                LongShots
75               Aggression
76            Interceptions
77              Positioning
78                   Vision
79                Penalties
80                Composure
81                  Marking
82           StandingTackle
83            SlidingTackle
84                 GKDiving
85               GKHandling
86                GKKicking
87            GKPositioning
88               GKReflexes
89           Release.Clause"

#________________1. ï..
str(fb$ï..)
'These are only index values/serial number of rows'

#________________2.ID
str(fb$ID)
'int [1:18147] 158023 20801 190871 193080 192985 183277 177003 176580 
155862 200389  ...'

length(unique(fb$ID))
"18147 - All are unique ID's"

#________________3.Name
str(fb$Name)
' Factor w/ 17194 levels "A. Ã-hman","A. Ã-mÃ¼r",..: 9689 3206 12560 4187 8671 
4478 9697 9899 15466 7835 ...'
length(unique(fb$Name))
'17140'

"Observe that some names are repeated, being the ID's are unique, assuming the
data of the persons might be differ"

#________________ 4.Age
str(fb$Age)
'int [1:18147] 31 33 26 27 27 27 32 31 32 25 ...'

summary(fb$Age)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  16.00   21.00   25.00   25.12   28.00   45.00'

#Histogram
hist(fb$Age,
     col = brewer.pal(8,'Spectral'),
     main = 'Age of the Player',
     xlab = 'Age')

#Boxplot
boxplot(fb$Age,
        col = 'darkseagreen3',
        horizontal = TRUE,
        main = 'Age of the Player')

#Checking the outliers from complete data
age_ub = quantile(fb$Age, 0.75)+1.5*IQR(fb$Age)
length(fb$Age[fb$Age>age_ub])
'47 - Small proportion of outliers Ignoring'

#________________5.Photo
str(fb$Photo)
"These column contains the url links of the photos"
head(fb$Photo)
"
[1] https://cdn.sofifa.org/players/4/19/158023.png
[2] https://cdn.sofifa.org/players/4/19/20801.png 
[3] https://cdn.sofifa.org/players/4/19/190871.png
[4] https://cdn.sofifa.org/players/4/19/193080.png
[5] https://cdn.sofifa.org/players/4/19/192985.png
[6] https://cdn.sofifa.org/players/4/19/183277.png
18207 Levels: https://cdn.sofifa.org/players/4/19/100803.png ..."

#________________6.Nationality
str(fb$Nationality)
'Factor w/ 164 levels "Afghanistan",..: 7 124 21 141 14 14 36 159 141 138 ...'

table(fb$Nationality)
#Barplot
barplot(table(fb$Nationality),
        col = brewer.pal(8,'Accent'),
        main = 'Nationality of the Players',
        las=2)

#Converting the table output to data frame
fb_nationality = as.data.frame(table(fb$Nationality))
colnames(fb_nationality) = c('Name_of_the_Country', "No_of_Players")

fb_nationality = fb_nationality[order(fb_nationality$No_of_Players,
                                      decreasing = TRUE),]                            
rownames(fb_nationality) = NULL#Resetting row index, starts from 1

#Top 10 countries having highest number of players
head(fb_nationality,10)
"
   Name_of_the_Country No_of_Players
1              England          1657
2              Germany          1195
3                Spain          1071
4            Argentina           936
5               France           911
6               Brazil           825
7                Italy           699
8             Colombia           616
9                Japan           478
10         Netherlands           452"

dim(fb_nationality[fb_nationality$No_of_Players==1,])[1]
'25 countries having only 1 player'

#________________7.Flag
str(fb$Flag)
'Factor w/ 164 levels "https://cdn.sofifa.org/flags/1.png",..: 123 108 125
115 138 138 2 132 115 114 ...'

'All 164 countries having flags, similar to above variable'

head(fb$Flag)
'[1] https://cdn.sofifa.org/flags/52.png
[2] https://cdn.sofifa.org/flags/38.png
[3] https://cdn.sofifa.org/flags/54.png
[4] https://cdn.sofifa.org/flags/45.png
[5] https://cdn.sofifa.org/flags/7.png 
[6] https://cdn.sofifa.org/flags/7.png 
164 Levels: https://cdn.sofifa.org/flags/1.png ...'

'This variable having the urls of flag images'

#________________8.Overall
str(fb$Overall)
'int [1:18147] 94 94 92 91 91 91 91 91 91 90 ...'

summary(fb$Overall)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  46.00   62.00   66.00   66.25   71.00   94.00 '

#Histogram
hist(fb$Overall,
     col = brewer.pal(8,'Set1'),
     main = 'Overall of the Player',
     xlab = 'Overall')

#Boxplot
boxplot(fb$Overall,
        col = 'gold3',
        horizontal = TRUE,
        main = 'Overall of the Player')

#Upper Boundary
oa_ub = quantile(fb$Overall, 0.75)+1.5*IQR(fb$Overall)
length(fb$Overall[fb$Overall>oa_ub])
'110 - less in proportion so ignoring'

#Lower Boundary
oa_lb = quantile(fb$Overall, 0.25)-1.5*IQR(fb$Overall)
length(fb$Overall[fb$Overall<oa_lb])
'53 - less in proportion so ignoring'

#________________9 Potential
str(fb$Potential)
'int [1:18147] 94 94 93 93 92 91 91 91 91 93 ...'

summary(fb$Potential)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  48.00   67.00   71.00   71.32   75.00   95.00 '

#Histogram
hist(fb$Potential,
     col = brewer.pal(8,'Accent'),
     main = 'Potential of the Player',
     xlab = 'Potential')

#Boxplot
boxplot(fb$Potential,
        col = 'deeppink3',
        horizontal = TRUE,
        main = 'Potential of the Player')

#Upper Boundary
pot_ub = quantile(fb$Potential, 0.75)+1.5*IQR(fb$Potential)
length(fb$Potential[fb$Potential>pot_ub])
'131 - less in proportion so ignoring'

#Lower Boundary
pot_lb = quantile(fb$Potential, 0.25)-1.5*IQR(fb$Potential)
length(fb$Potential[fb$Potential<pot_lb])
'29 - less in proportion so ignoring'

#________________10 Club
str(fb$Club)
' Factor w/ 652 levels ""," SSV Jahn Regensburg",..: 218 333 438 380 379 142 
475 218 475 65 ...'

#Barplot
barplot(table(fb$Club),
        col = brewer.pal(8,'Accent'),
        main = 'Club of the Players',
        las=2)

table(fb$Club)

#Noted some blank cells [229], replacing them with No_club
levels(fb$Club)[levels(fb$Club)==''] = 'No_club'

#Converting the table output to data frame
fb_club = as.data.frame(table(fb$Club))
colnames(fb_club) = c('Name_of_the_Club', "No_of_Players")
fb_club = fb_club[order(fb_club$No_of_Players,decreasing = TRUE),]                            
rownames(fb_club) = NULL#Resetting row index

#Top 10 countries having highest number of players
head(fb_club,10)
'
      Name_of_the_Club No_of_Players
1              No_club           229
2              Arsenal            33
3            AS Monaco            33
4     AtlÃ©tico Madrid            33
5    Borussia Dortmund            33
6              Burnley            33
7         Cardiff City            33
8          CD LeganÃ©s            33
9              Chelsea            33
10 Eintracht Frankfurt            33'

barplot(table(fb_club$No_of_Players),
        col = brewer.pal(9,'Spectral'),
        main = 'No of Clubs vs Players',
        xlab = 'Players',
        ylab = 'No of clubs', las=2)
'
 18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33 229 
  1   2  16   3   3  15  28  52  60 103 177  43 102   5  16  25   1 '
'Each club contains a minimum of 18 players maximum of 33 players. 229 players
club name is not specified'

#________________11 Club Logo
str(fb$Club.Logo)
' Factor w/ 679 levels "https://cdn.sofifa.org/flags/103.png",..: 491 
553 638 90 30 577 493 491 493 490 ...'

head(fb$Club.Logo)
'
[1] https://cdn.sofifa.org/teams/2/light/241.png
[2] https://cdn.sofifa.org/teams/2/light/45.png 
[3] https://cdn.sofifa.org/teams/2/light/73.png 
[4] https://cdn.sofifa.org/teams/2/light/11.png 
[5] https://cdn.sofifa.org/teams/2/light/10.png 
[6] https://cdn.sofifa.org/teams/2/light/5.png  
679 Levels: https://cdn.sofifa.org/flags/103.png ...'

'This variable is having url of Club logo'

#________________12 Value is in Dollars
str(fb$Value)
'Factor w/ 217 levels "â,¬0","â,¬1.1M",..: 17 196 19 191 13 214 183 ...'

table(fb$Value)

#Replace or substitute â,¬ with nothing
fb$Value = sub("â,¬","", fb$Value)

table(fb$Value)

#integer. A penalty to be applied when deciding to print numeric values in
#fixed or exponential notation. Positive values bias towards fixed and negative 
#towards scientific notation: fixed notation will be preferred unless it is 
#more than scipen digits wider.
options(scipen = 15)

#Removing M & K and keeping all the values to 1000's
'The grep R function searches for matches of certain character pattern in a 
vector of character strings and returns the indices that yielded a match.'

fb$Value[grep('K$',fb$Value)] = as.numeric(sub('K',"",fb$Value[grep('K$',fb$Value)]))

fb$Value[grep('M$',fb$Value)] = (as.numeric(sub('M',"",fb$Value[grep('M$',fb$Value)]))*1000000)/1000
fb$Value = as.numeric(fb$Value)

str(fb$Value)
'num [1:18147] 110500 77000 118500 72000 102000 ...'
#table(fb$Value)
summary(fb$Value)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0     300     675    2418    2000  118500 '

#Table output into a data frame
fb_value = as.data.frame(table(fb$Value))
colnames(fb_value) = c("Value_in_1000s", "No_of_Players")
fb_value = fb_value[order(fb_value$Value_in_1000s,decreasing = TRUE),]                            
rownames(fb_value) = NULL
fb_value

#Histogram
hist(fb$Value,
     col = brewer.pal(8,'Set2'),
     main = 'Value of the Player',
     xlab = 'Value')

#Boxplot
boxplot(fb$Value,
        col = 'lightcoral',
        horizontal = TRUE,
        main = 'Value of the Player')

#Extreme outliers belongs to the top players whose value is very highest
fb[fb$Value>=80000,]['Name']
'          Name
1      L. Messi
3     Neymar Jr
5  K. De Bruyne
6     E. Hazard
8    L. SuÃ¡rez
16    P. Dybala
17      H. Kane
26   K. MbappÃ©'

#Upper Boundary
val_ub = quantile(fb$Value, 0.75)+1.5*IQR(fb$Value)
length(fb$Value[fb$Value>val_ub])
'2487 ignoring'

#________________13 Wage
str(fb$Wage)
'Factor w/ 144 levels "â,¬0","â,¬100K",..: 95 75 56 50 67 65 78 82 71 138 ...'

table(fb$Wage)

#Replace or substitute â,¬ with nothing
fb$Wage = sub("â,¬","", fb$Wage)

#Removing K - All the values are in 1000's
'The grep R function searches for matches of certain character pattern in a 
vector of character strings and returns the indices that yielded a match.'
fb$Wage = as.numeric(sub('K',"",fb$Wage))

str(fb$Wage)
' num [1:18147] 565 405 290 260 355 340 420 455 380 94 ...'

summary(fb$Wage)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   1.000   3.000   9.759   9.000 565.000 '

#Table output into a dataframe
fb_Wage = as.data.frame(table(fb$Wage))
colnames(fb_Wage) = c("Wage_in_1000s", "No_of_Players")
fb_Wage = fb_Wage[order(fb_Wage$Wage_in_1000s,decreasing = TRUE),]                            
rownames(fb_Wage) = NULL
fb_Wage

#Histogram
hist(fb$Wage,
     col = brewer.pal(8,'Set2'),
     main = 'Wage of the Player',
     xlab = 'Wage')

#Boxplot
boxplot(fb$Wage,
        col = 'lightcoral',
        horizontal = TRUE,
        main = 'Wage of the Player')

#Extreme outliers belongs to the top players whose value is very highest
fb[fb$Wage>=300,]['Name']

"
#cs2m[cs2m$Age >= 20, ]['BP']

 BP
1  100
4  100
5   95
6  110
7  120
8  150
9  160
10 125
"

# Observed the value & wage is 0 for No_club therefore removing those 
# observations
table(fb$Wage)[1]
'  0 
229'

fb[fb$Club=='No_club',]$Value
fb[fb$Club=='No_club',]$Wage

fb = fb[fb$Club!='No_club',]
dim(fb)
'17918    89
18147-17918 = 229 observations removed'

#Upper Boundary
wag_ub = quantile(fb$Wage, 0.75)+1.5*IQR(fb$Wage)
length(fb$Wage[fb$Wage>wag_ub])
'2031 ignoring'

#________________14 Special
str(fb$Special)
'int [1:17918] 2202 2228 2143 1471 2281 2142 2280 2346 2201 1331 ...'

summary(fb$Special)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    731    1457    1636    1598    1787    2346 '

#Histogram
hist(fb$Special,
     col = brewer.pal(8,'Paired'),
     main = 'Special of the Player',
     xlab = 'Special')

#Boxplot
boxplot(fb$Special,
        col = 'turquoise4',
        horizontal = TRUE,
        main = 'Special of the Player')

#Upper Boundary
spe_ub = quantile(fb$Special, 0.75)+1.5*IQR(fb$Special)
length(fb$Special[fb$Special>spe_ub])
'1 - less in proportion so ignoring'

#Lower Boundary
spe_lb = quantile(fb$Special, 0.25)-1.5*IQR(fb$Special)
length(fb$Special[fb$Special<spe_lb])
'541 - less in proportion so ignoring'

#________________15 Preferred.Foot
str(fb$Preferred.Foot)
'Factor w/ 3 levels "","Left","Right": 2 3 3 3 3 3 3 3 3 3 ...'

table(fb$Preferred.Foot)
'       Left Right 
    0  4162 13756 '
#Resetting the levels
fb$Preferred.Foot = factor(fb$Preferred.Foot) #this we have seen before also

table(fb$Preferred.Foot)
' Left Right 
 4162 13756 '

#Barplot
barplot(table(fb$Preferred.Foot),
        col = c('darkslateblue', 'firebrick3'),
        main = 'Preferred Foot - Players')

#________________16 International.Reputation
str(fb$International.Reputation)
'int [1:17918] 5 5 5 4 4 4 4 5 4 3 ...'

table(fb$International.Reputation)
'    1     2     3     4     5 
 16305  1248   308    51     6 '

#Barplot
barplot(table(fb$International.Reputation),
        col = brewer.pal(5,'Dark2'),
        main = 'International Reputation - Players')

#________________17 Weak Foot
str(fb$Weak.Foot)
'int [1:17918] 4 4 5 3 5 4 4 4 3 3 ...'

table(fb$Weak.Foot)
'    1     2     3     4     5 
  153  3715 11201  2622   227 '

#Barplot
barplot(table(fb$Weak.Foot),
        col = brewer.pal(5,'Set1'),
        main = 'Weak Foot - Players')

#________________18 Skill.Moves
str(fb$Skill.Moves)
'int [1:17918] 4 5 5 1 4 4 4 3 3 1 ...'

table(fb$Skill.Moves)
'   1    2    3    4    5 
 1992 8443 6522  911   50 '

#Barplot
barplot(table(fb$Skill.Moves),
        col = brewer.pal(5,'Spectral'),
        main = 'Skill Moves - Players')

#________________19 Work.Rate
str(fb$Work.Rate)
'Factor w/ 10 levels "","High/ High",..: 10 3 4 10 2 4 2 4 4 10 ...'

table(fb$Work.Rate)
'                   High/ High      High/ Low   High/ Medium      Low/ High 
             0           1007            686           3131            435 
      Low/ Low    Low/ Medium   Medium/ High    Medium/ Low Medium/ Medium 
            34            440           1660            840           9685 '

#Resetting factors
fb$Work.Rate = factor(fb$Work.Rate) # for removing 0

table(fb$Work.Rate)
'    High/ High      High/ Low   High/ Medium      Low/ High       Low/ Low 
          1007            686           3131            435             34 
   Low/ Medium   Medium/ High    Medium/ Low Medium/ Medium 
           440           1660            840           9685'

#Barplot
par(mar=c(8,4,2,2))
barplot(table(fb$Work.Rate),
        col = brewer.pal(9,'Paired'),
        main = 'Work Rate - Players', las=2)

#________________20.Body.Type
str(fb$Body.Type)
'Factor w/ 11 levels "","Akinfenwa",..: 6 3 7 5 8 8 5 8 8 8 ...'

table(fb$Body.Type)
'                              Akinfenwa          C. Ronaldo            Courtois 
                  0                   1                   1                   1 
               Lean               Messi              Neymar              Normal 
               6351                   1                   1               10436 
PLAYER_BODY_TYPE_25             Shaqiri              Stocky 
                  1                   1                1124 '

#Resetting factors
fb$Body.Type = factor(fb$Body.Type) # we have done this before also

table(fb$Body.Type)
'          Akinfenwa          C. Ronaldo            Courtois                Lean 
                  1                   1                   1                6351 
              Messi              Neymar              Normal PLAYER_BODY_TYPE_25 
                  1                   1               10436                   1 
            Shaqiri              Stocky 
                  1                1124 '

#Barplot
par(mar=c(8,4,2,2))
barplot(table(fb$Body.Type),
        col = brewer.pal(9,'Paired'),
        main = 'Body Type - Players', las=2)

'Data is not proper'

#________________21 Real.Face
str(fb$Real.Face)
'Factor w/ 3 levels "","No","Yes": 3 3 3 3 3 3 3 3 3 3 ...'

table(fb$Real.Face)
'         No   Yes 
     0 16264  1654'

#Resetting factors
fb$Real.Face = factor(fb$Real.Face)

table(fb$Real.Face)
'   No   Yes 
 16264  1654 '

#Barplot
par(mar=c(5,4,3,2))
barplot(table(fb$Real.Face),
        col = c('turquoise4','tan4'),
        main = 'Real Face - Players')

#________________22 Position
str(fb$Position)
'Factor w/ 28 levels "","CAM","CB",..: 23 28 16 7 21 13 21 25 20 7 ...'

table(fb$Position)
'      CAM   CB  CDM   CF   CM   GK  LAM   LB  LCB  LCM  LDM   LF   LM   LS   LW 
   0  948 1754  936   74 1377 1992   21 1305  637  389  239   15 1086  206  374 
 LWB  RAM   RB  RCB  RCM  RDM   RF   RM   RS   RW  RWB   ST 
  78   21 1268  652  387  246   16 1114  201  365   87 2130'

#Resetting factors
fb$Position = factor(fb$Position)

table(fb$Position)
'CAM   CB  CDM   CF   CM   GK  LAM   LB  LCB  LCM  LDM   LF   LM   LS   LW  LWB 
 948 1754  936   74 1377 1992   21 1305  637  389  239   15 1086  206  374   78 
 RAM   RB  RCB  RCM  RDM   RF   RM   RS   RW  RWB   ST 
  21 1268  652  387  246   16 1114  201  365   87 2130 '

#Barplot
barplot(table(fb$Position),
        col = brewer.pal(8,'Dark2'),
        main = 'Position - Players',las=2)

#________________23 Jersey.Number
str(fb$Jersey.Number)
'int [1:17918] 10 7 10 1 7 10 10 9 15 1 ...'

summary(fb$Jersey.Number)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.00    8.00   17.00   19.64   26.00   99.00 '

#Histogram
hist(fb$Jersey.Number,
     col = brewer.pal(8,'Spectral'),
     main = 'Jersey Number - Players',las=2)

#Boxplot
boxplot(fb$Jersey.Number,
        horizontal = TRUE,
        col = 'coral3',
        main = 'Jersey Number - Players')

#Upper Boundary; as its not numeric, statistical definition of outlier
# does not apply
#jn_ub = quantile(fb$Jersey.Number, 0.75)+1.5*IQR(fb$Jersey.Number)
#length(fb$Jersey.Number[fb$Jersey.Number>jn_ub])
'632 - less in proportion so ignoring'

#________________24.Joined # no point to include in knn procedure
str(fb$Joined)
'Factor w/ 1737 levels "","Apr 1, 2008",..: 776 796 249 783 255  ..'

summary(fb$Joined)[1:10]
'Jul 1, 2018             Jul 1, 2017 Jan 1, 2018 Jul 1, 2016 Jul 1, 2015 
       1538        1264        1133         635         614         368 
Jan 1, 2017 Jul 1, 2014 Jan 1, 2016 Jul 1, 2013 
        231         226         180         156 '

head(fb$Joined)
'[1] Jul 1, 2004  Jul 10, 2018 Aug 3, 2017  Jul 1, 2011  Aug 30, 2015 Jul 1, 2012 
1737 Levels:  Apr 1, 2008 Apr 1, 2011 Apr 1, 2013 Apr 1, 2015 ... Sep 9, 2018'

#Converting the data type to date
'https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime'
fb$Joined = as.Date(fb$Joined, format = "%b %d, %Y")

str(fb$Joined)
'Date[1:17918], format: "2004-07-01" "2018-07-10" "2017-08-03"  ...'

summary(fb$Joined)
'
Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
"1991-06-01" "2016-05-23" "2017-07-03" "2016-11-15" "2018-07-01" "2018-12-20" 
NA's' 
"1264"'

head(as.data.frame(table(fb$Joined)))
'        Var1 Freq
1 1991-06-01    1
2 1998-01-01    2
3 1998-07-01    1
4 1999-01-01    1
5 2000-01-01    1
6 2000-07-01    1'

tail(as.data.frame(table(fb$Joined)))
'           Var1 Freq
1731 2018-12-05    6
1732 2018-12-06    1
1733 2018-12-07    3
1734 2018-12-08    2
1735 2018-12-10    1
1736 2018-12-20    1'

#________________25. Loaned.From # not to be included
str(fb$Loaned.From)
'Factor w/ 342 levels "","1. FC KÃ¶ln",..: 1 1 1 1 1 1 1 1 1 1 ...'

table(fb$Loaned.From)
'16654 observations are blank, no use with this column'

#________________26 Contract.Valid.Until # not to be included
str(fb$Contract.Valid.Until)
' Factor w/ 37 levels "","2018","2019",..: 5 6 6 4 7 4 4 5 4 5 ...'

table(fb$Contract.Valid.Until)

#Resetting the levels of factors
fb$Contract.Valid.Until = factor(fb$Contract.Valid.Until)

table(fb$Contract.Valid.Until)

'Noted very few having complete date rest having only year'

#________________27 Height
str(fb$Height)
"Factor w/ 22 levels "",5'1,5'10,..: 10 15 12 17 4 11 11 13 13 15 ..."


head(fb$Height)
"5'7  6'2  5'9  6'4  5'11 5'8"

table(fb$Height)
"      5'1 5'10 5'11  5'2  5'3  5'4  5'5  5'6  5'7  5'8  5'9  6'0  6'1  6'2  6'3 
   0    3 2452 2132    5   18   30  144  311  892  934 2203 2837 1886 1990  975 
 6'4  6'5  6'6  6'7  6'8  6'9 
 741  241   91   21   10    2 "

#Resetting the factors
fb$Height = factor(fb$Height)

barplot(table(fb$Height),
        col = brewer.pal(11, 'Paired'),
        main = 'Height of the Players',
        las=2)
#Column data separate and formed into 2 columns separately, using those vlaues
#coverted the feet & inches value to cm's
fb = fb %>% 
     separate(Height,c('feet','inch'), 
              sep = "'", convert = TRUE, remove = FALSE) %>%  
     mutate(Height = (12*feet+inch)*2.54)

#Dropping the newly created colunms
fb = subset(fb, select = -c(feet,inch))

barplot(table(fb$Height),
        col = brewer.pal(11, 'Paired'),
        main = 'Height of the Players',
        las=2)
                                                               
#==========================================================================
#________________28 Weight
str(fb$Weight)
'Factor w/ 58 levels "","110lbs","115lbs",..: 23 34 19 27 21 25 17 37 33 38 ...'

#Replace or substitute lbs with nothing
fb$Weight = as.numeric(sub("lbs","", fb$Weight))

#Histogram
hist(fb$Weight,
     col = brewer.pal(8,'Set1'),
     main = 'Weight of the Player')

#Boxplot
boxplot(fb$Weight,
        horizontal = TRUE,
        col = 'yellow2',
        main = 'Weight - Players')

#Upper Boundary
wt_ub = quantile(fb$Weight, 0.75)+1.5*IQR(fb$Weight)
length(fb$Weight[fb$Weight>wt_ub])
'62 - less in proportion so ignoring'

#Lower Boundary
wt_lb = quantile(fb$Weight, 0.25)-1.5*IQR(fb$Weight)
length(fb$Weight[fb$Weight < wt_lb])
'13 - less in proportion so ignoring'

#________________29 LS , we will drop this
str(fb$LS)
'Factor w/ 94 levels "","31+2","32+2",..: 93 94 88 1 85 87 75 92 67 1 ...'

table(fb$LS)
'1992 not labelled'

#Barplot
barplot(table(fb$LS),
     col = brewer.pal(8,'Set1'),
     main = 'LS- Player', las=2)

#________________30 ST , we will drop this
str(fb$ST)
'Factor w/ 94 levels "","31+2","32+2",..: 93 94 88 1 85 87 75 92 67 1 ...'

table(fb$ST)
'1992 not labelled'

#Barplot
barplot(table(fb$ST),
        col = brewer.pal(8,'Dark2'),
        main = 'ST - Player', las=2)

#________________31 RS , drop this
str(fb$RS)
'Factor w/ 94 levels "","31+2","32+2",..: 93 94 88 1 85 87 75 92 67 1 ...'

table(fb$RS)
'1992 not labelled'

#Barplot
barplot(table(fb$RS),
        col = brewer.pal(8,'Spectral'),
        main = 'RS - Player', las=2)

#________________32 LW, drop this
str(fb$LW)
'Factor w/ 106 levels "","25+2","27+2",..: 106 105 105 1 104 105 101 103 71 1 ...'

table(fb$LW)
'1992 not labelled'

#Barplot
barplot(table(fb$LW),
        col = brewer.pal(8,'Paired'),
        main = 'LW - Player', las=2)

#________________33 LF, drop this
str(fb$LF)
'Factor w/ 103 levels "","27+2","29+2",..: 103 102 101 1 98 100 95 99 68 1 ...'

table(fb$LF)
'1992 not labelled'

#Barplot
barplot(table(fb$LF),
        col = brewer.pal(8,'Set1'),
        main = 'LF - Player', las=2)

#________________34 CF, drop this
str(fb$CF)
'Factor w/ 103 levels "","27+2","29+2",..: 103 102 101 1 98 100 95 99 68 1 ...'

table(fb$CF)
'1992 not labelled'

#Barplot
barplot(table(fb$CF),
        col = brewer.pal(8,'RdYlGn'),
        main = 'CF - Player', las=2)

#________________35 RF, drop this
str(fb$RF)
'Factor w/ 103 levels "","27+2","29+2",..: 103 102 101 1 98 100 95 99 68 1 ...'

table(fb$RF)
'1992 not labelled'

#Barplot
barplot(table(fb$RF),
        col = brewer.pal(8,'BrBG'),
        main = 'RF - Player', las=2)

#________________36 RW, drop this
str(fb$RW)
'Factor w/ 106 levels "","25+2","27+2",..: 106 105 105 1 104 105 101 103 71 1 ...'

table(fb$RW)
'1992 not labelled'

#Barplot
barplot(table(fb$RW),
        col = brewer.pal(8,'PiYG'),
        main = 'RW - Player', las=2)

#________________37 LAM, drop this
str(fb$LAM)
'Factor w/ 102 levels "","27+2","28+2",..: 102 100 101 1 100 101 99 97 69 1 ...'

table(fb$LAM)
'1992 not labelled'

#Barplot
barplot(table(fb$LAM),
        col = brewer.pal(8,'RdBu'),
        main = 'LAM - Player', las=2)

#________________38 CAM, drop this
str(fb$CAM)
'Factor w/ 102 levels "","27+2","28+2",..: 102 100 101 1 100 101 99 97 69 1 ...'

table(fb$CAM)
'1992 not labelled'

#Barplot
barplot(table(fb$CAM),
        col = brewer.pal(8,'Set3'),
        main = 'CAM - Player', las=2)

#________________39 RAM, drop this
str(fb$RAM)
'Factor w/ 102 levels "","27+2","28+2",..: 102 100 101 1 100 101 99 97 69 1 ...'

table(fb$RAM)
'1992 not labelled'

#Barplot
barplot(table(fb$RAM),
        col = brewer.pal(8,'Set1'),
        main = 'RAM - Player', las=2)

#________________40 LM, drop this
str(fb$LM)
'Factor w/ 101 levels "","27+2","28+2",..: 101 99 99 1 99 100 98 96 70 1 ...'

table(fb$LM)
'1992 not labelled'

#Barplot
barplot(table(fb$LM),
        col = brewer.pal(8,'Accent'),
        main = 'LM - Player', las=2)

#________________41 LCM, drop this
str(fb$LCM)
'Factor w/ 93 levels "","30+2","31+2",..: 88 83 83 1 92 85 93 79 70 1 ...'

table(fb$LCM)
'1992 not labelled'

#Barplot
barplot(table(fb$LCM),
        col = brewer.pal(8,'YlOrRd'),
        main = 'LCM - Player', las=2)

#________________42 CM, drop this
str(fb$CM)
'Factor w/ 93 levels "","30+2","31+2",..: 88 83 83 1 92 85 93 79 70 1 ...'

table(fb$CM)
'1992 not labelled'

#Barplot
barplot(table(fb$CM),
        col = brewer.pal(8,'YlGn'),
        main = 'CM - Player', las=2)

#________________43 RCM, drop this
str(fb$RCM)
'Factor w/ 93 levels "","30+2","31+2",..: 88 83 83 1 92 85 93 79 70 1 ...'

table(fb$RCM)
'1992 not labelled'

#Barplot
barplot(table(fb$RCM),
        col = brewer.pal(8,'Paired'),
        main = 'RCM - Player', las=2)

#________________44 RM, drop this
str(fb$RM)
'Factor w/ 101 levels "","27+2","28+2",..: 101 99 99 1 99 100 98 96 70 1 ...'

table(fb$RM)
'1992 not labelled'

#Barplot
barplot(table(fb$RM),
        col = brewer.pal(8,'PRGn'),
        main = 'RM - Player', las=2)

#________________45 LWB, drop this
str(fb$LWB)
'Factor w/ 96 levels "","30+2","31+2",..: 55 58 58 1 83 60 93 67 91 1 ...'

table(fb$LWB)
'1992 not labelled'

#Barplot
barplot(table(fb$LWB),
        col = brewer.pal(8,'RdYlBu'),
        main = 'LWB - Player', las=2)

#________________46 LDM, drop this
str(fb$LDM)
'Factor w/ 100 levels "","28+2","29+2",..: 50 51 49 1 84 55 92 66 97 1 ...'

table(fb$LDM)
'1992 not labelled'
fb[is.na(fb$LDM),]


#Barplot
barplot(table(fb$LDM),
        col = brewer.pal(8,'Set1'),
        main = 'LDM - Player', las=2)

#________________47 CDM, drop this
str(fb$CDM)
'Factor w/ 100 levels "","28+2","29+2",..: 50 51 49 1 84 55 92 66 97 1 ...'

table(fb$CDM)
'1992 not labelled'

#Barplot
barplot(table(fb$CDM),
        col = brewer.pal(8,'RdYlGn'),
        main = 'CDM - Player', las=2)

#________________48 RDM, drop this
str(fb$RDM)
'Factor w/ 100 levels "","28+2","29+2",..: 50 51 49 1 84 55 92 66 97 1 ...'

table(fb$RDM)
'1992 not labelled'

#Barplot
barplot(table(fb$RDM),
        col = brewer.pal(8,'Accent'),
        main = 'RDM - Player', las=2)

#________________49 RWB, drop this
str(fb$RWB)
'Factor w/ 96 levels "","30+2","31+2",..: 55 58 58 1 83 60 93 67 91 1 ...'

table(fb$RWB)
'1992 not labelled'

#Barplot
barplot(table(fb$RWB),
        col = brewer.pal(8,'Dark2'),
        main = 'RWB - Player', las=2)

#________________50 LB, drop this
str(fb$LB)
'Factor w/ 99 levels "","29+2","30+2",..: 49 54 52 1 79 52 91 65 99 1 ...'

table(fb$LB)
'1992 not labelled'

#Barplot
barplot(table(fb$LB),
        col = brewer.pal(8,'PuOr'),
        main = 'LB - Player', las=2)

#________________51 LCB, drop this
str(fb$LCB)
'Factor w/ 109 levels "","25+2","27+2",..: 30 44 31 1 71 35 81 65 109 1 ...'

table(fb$LCB)
'1992 not labelled'

#Barplot
barplot(table(fb$LCB),
        col = brewer.pal(8,'Spectral'),
        main = 'LCB - Player', las=2)

#________________52 CB, drop this
str(fb$CB)
'Factor w/ 109 levels "","25+2","27+2",..: 30 44 31 1 71 35 81 65 109 1 ...'

table(fb$CB)
'1992 not labelled'

#Barplot
barplot(table(fb$CB),
        col = brewer.pal(8,'Paired'),
        main = 'CB - Player', las=2)

#________________53 RCB, drop this
str(fb$RCB)
'Factor w/ 109 levels "","25+2","27+2",..: 30 44 31 1 71 35 81 65 109 1 ...'

table(fb$RCB)
'1992 not labelled'

#Barplot
barplot(table(fb$RCB),
        col = brewer.pal(8,'Set2'),
        main = 'RCB - Player', las=2)

#________________54 RB, drop this
str(fb$RB)
'Factor w/ 99 levels "","29+2","30+2",..: 49 54 52 1 79 52 91 65 99 1 ...'

table(fb$RB)
'1992 not labelled'

#Barplot
barplot(table(fb$RB),
        col = brewer.pal(8,'Paired'),
        main = 'RB - Player', las=2)

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& ____below we will use
#________________55 Crossing
str(fb$Crossing)
'int [1:17918] 84 84 79 17 93 81 86 77 66 13 ...'

summary(fb$Crossing)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   38.00   54.00   49.75   64.00   93.00 '

#Histogram
hist(fb$Crossing,
     col = brewer.pal(8,'Spectral'),
     main = 'Crossing of the Player',
     xlab = 'Crossing')

#Boxplot
boxplot(fb$Crossing,
        col = 'plum3',
        horizontal = TRUE,
        main = 'Crossing of the Player')

#________________56 Finishing
str(fb$Finishing)
'int [1:17918] 95 94 87 13 82 84 72 93 60 11 ...'

summary(fb$Finishing)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00   30.00   49.00   45.58   62.00   95.00 '

#Histogram
hist(fb$Finishing,
     col = brewer.pal(8,'Paired'),
     main = 'Finishing of the Player',
     xlab = 'Finishing')

#Boxplot
boxplot(fb$Finishing,
        col = 'purple2',
        horizontal = TRUE,
        main = 'Finishing of the Player')

#________________57 Heading Accuracy
str(fb$HeadingAccuracy)
'int [1:17918] 70 89 62 21 55 61 55 77 91 15 ...'

summary(fb$HeadingAccuracy)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00   30.00   49.00   45.58   62.00   95.00 '

#Histogram
hist(fb$HeadingAccuracy,
     col = brewer.pal(8,'Dark2'),
     main = 'Heading Accuracy of the Player',
     xlab = 'Heading Accuracy')

#Boxplot
boxplot(fb$HeadingAccuracy,
        col = 'slateblue3',
        horizontal = TRUE,
        main = 'Heading Accuracy of the Player')

#Checking the outliers 
ha_lb = quantile(fb$HeadingAccuracy, 0.25)-1.5*IQR(fb$HeadingAccuracy)
length(fb$HeadingAccuracy[fb$HeadingAccuracy<ha_lb])
'985'
'Ignoring the outliers being less in proportion'

#________________58 Short Passing
str(fb$ShortPassing)
'int [1:17918] 90 81 84 50 92 89 93 82 78 29 ...'

summary(fb$ShortPassing)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00   54.00   62.00   58.71   68.00   93.00 '

#Histogram
hist(fb$ShortPassing,
     col = brewer.pal(8,'RdYlGn'),
     main = 'Short Passing of the Player',
     xlab = 'Short Passing')

#Boxplot
boxplot(fb$ShortPassing,
        col = 'brown2',
        horizontal = TRUE,
        main = 'Short Passing of the Player')

#Checking the outliers 
sp_lb = quantile(fb$ShortPassing, 0.25)-1.5*IQR(fb$ShortPassing)
length(fb$ShortPassing[fb$ShortPassing<sp_lb])
'1685 outliers, not removing'

sp_ub = quantile(fb$ShortPassing, 0.75)+1.5*IQR(fb$ShortPassing)
length(fb$ShortPassing[fb$ShortPassing>sp_ub])
'12 outliers'

#________________59 Volleys
str(fb$Volleys)
'int [1:17918] 86 87 84 13 82 80 76 88 66 13 ...'

summary(fb$Volleys)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00   30.00   44.00   42.93   57.00   90.00 '

#Histogram
hist(fb$Volleys,
     col = brewer.pal(8,'Set3'),
     main = 'Volleys of the Player',
     xlab = 'Volleys')

#Boxplot
boxplot(fb$Volleys,
        col = 'seagreen3',
        horizontal = TRUE,
        main = 'Volleys of the Player')

#________________60 Dribbling
str(fb$Dribbling)
'int [1:17918] 97 88 96 18 86 95 90 87 63 12 ...'

summary(fb$Dribbling)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00   49.00   61.00   55.41   68.00   97.00 '

#Histogram
hist(fb$Dribbling,
     col = brewer.pal(8,'Set1'),
     main = 'Dribbling of the Player',
     xlab = 'Dribbling')

#Boxplot
boxplot(fb$Dribbling,
        col = 'orangered1',
        horizontal = TRUE,
        main = 'Dribbling of the Player')

#Checking the outliers 
dr_lb = quantile(fb$Dribbling, 0.25)-1.5*IQR(fb$Dribbling)
length(fb$Dribbling[fb$Dribbling<dr_lb])
'1893 outliers'

dr_ub = quantile(fb$Dribbling, 0.75)+1.5*IQR(fb$Dribbling)
length(fb$Dribbling[fb$Dribbling>dr_ub])
'1 outliers'

#________________61 Curve
str(fb$Curve)
'int [1:17918] 93 81 88 21 85 83 85 86 74 13 ...'

summary(fb$Curve)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   34.00   49.00   47.22   62.00   94.00'

#Histogram
hist(fb$Curve,
     col = brewer.pal(8,'RdYlGn'),
     main = 'Curve of the Player',
     xlab = 'Curve')

#Boxplot
boxplot(fb$Curve,
        col = 'darkgreen',
        horizontal = TRUE,
        main = 'Curve of the Player')

#________________62 FKAccuracy
str(fb$FKAccuracy)
'int [1:17918] 94 76 87 19 83 79 78 84 72 14 ...'

summary(fb$FKAccuracy)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   31.00   41.00   42.88   57.00   94.00 '

#Histogram
hist(fb$FKAccuracy,
     col = brewer.pal(8,'Set2'),
     main = 'FKAccuracy of the Player',
     xlab = 'FKAccuracy')

#Boxplot
boxplot(fb$FKAccuracy,
        col = 'aquamarine2',
        horizontal = TRUE,
        main = 'FKAccuracy of the Player')

#________________63 LongPassing
str(fb$LongPassing)
'int [1:17918] 87 77 78 51 91 83 88 64 77 26 ...'

summary(fb$LongPassing)
'Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   9.00   43.00   56.00   52.72   64.00   93.00 '

#Histogram
hist(fb$LongPassing,
     col = brewer.pal(8,'PiYG'),
     main = 'LongPassing of the Player',
     xlab = 'LongPassing')

#Boxplot
boxplot(fb$LongPassing,
        col = 'deeppink3',
        horizontal = TRUE,
        main = 'LongPassing of the Player')

#Checking the outliers 
lp_lb = quantile(fb$LongPassing, 0.25)-1.5*IQR(fb$LongPassing)
length(fb$LongPassing[fb$LongPassing<lp_lb])
'17 outliers - Ignoring'

#________________64 BallControl
str(fb$BallControl)
'int [1:17918] 96 94 95 42 91 94 93 90 84 16 ...'

summary(fb$BallControl)
'Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   54.00   63.00   58.41   69.00   96.00'

#Histogram
hist(fb$BallControl,
     col = brewer.pal(8,'Accent'),
     main = 'BallControl of the Player',
     xlab = 'BallControl')

#Boxplot
boxplot(fb$BallControl,
        col = 'khaki1',
        horizontal = TRUE,
        main = 'BallControl of the Player')

#Checking the outliers 
bc_lb = quantile(fb$BallControl, 0.25)-1.5*IQR(fb$BallControl)
length(fb$BallControl[fb$BallControl<bc_lb])
'1994 outliers'

bc_ub = quantile(fb$BallControl, 0.75)+1.5*IQR(fb$BallControl)
length(fb$BallControl[fb$BallControl>bc_ub])
'12 outliers'

#________________65 Acceleration
str(fb$Acceleration)
'int [1:17918] 91 89 94 57 78 94 80 86 76 43 ...'

summary(fb$Acceleration)
'Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   12.0    57.0    67.0    64.6    75.0    97.0'

#Histogram
hist(fb$Acceleration,
     col = brewer.pal(8,'RdBu'),
     main = 'Acceleration of the Player',
     xlab = 'Acceleration')

#Boxplot
boxplot(fb$Acceleration,
        col = 'firebrick3',
        horizontal = TRUE,
        main = 'Acceleration of the Player')

#Checking the outliers 
ac_lb = quantile(fb$Acceleration, 0.25)-1.5*IQR(fb$Acceleration)
length(fb$Acceleration[fb$Acceleration<ac_lb])
'475 outliers - Ignore'

#________________66 SprintSpeed
str(fb$SprintSpeed)
'int [1:17918] 86 91 90 58 76 88 72 75 75 60 ...'

summary(fb$SprintSpeed)
' Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  12.00   57.00   67.00   64.72   75.00   96.00'

#Histogram
hist(fb$SprintSpeed,
     col = brewer.pal(8,'Paired'),
     main = 'SprintSpeed of the Player',
     xlab = 'SprintSpeed')

#Boxplot
boxplot(fb$SprintSpeed,
        col = 'coral',
        horizontal = TRUE,
        main = 'SprintSpeed of the Player')

#Checking the outliers 
ss_lb = quantile(fb$SprintSpeed, 0.25)-1.5*IQR(fb$SprintSpeed)
length(fb$SprintSpeed[fb$SprintSpeed<ss_lb])
'440 outliers - Ignore'

#________________67 Agility
str(fb$Agility)
'int [1:17918] 86 91 90 58 76 88 72 75 75 60 ...'

summary(fb$Agility)
' Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  12.00   57.00   67.00   64.72   75.00   96.00'

#Histogram
hist(fb$Agility,
     col = brewer.pal(8,'BrBG'),
     main = 'Agility of the Player',
     xlab = 'Agility')

#Boxplot
boxplot(fb$Agility,
        col = 'tan3',
        horizontal = TRUE,
        main = 'Agility of the Player')

#Checking the outliers 
ag_lb = quantile(fb$Agility, 0.25)-1.5*IQR(fb$Agility)
length(fb$Agility[fb$Agility<ag_lb])
'187 outliers - Ignore'

#________________68 Reactions
str(fb$Reactions)
'int [1:17918] 95 96 94 90 91 90 90 92 85 86 ...'

summary(fb$Reactions)
' Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  21.00   56.00   62.00   61.82   68.00   96.00 '

#Histogram
hist(fb$Reactions,
     col = brewer.pal(8,'Dark2'),
     main = 'Reactions of the Player',
     xlab = 'Reactions')

#Boxplot
boxplot(fb$Reactions,
        col = 'slategray4',
        horizontal = TRUE,
        main = 'Reactions of the Player')

#Checking the outliers 
rc_lb = quantile(fb$Reactions, 0.25)-1.5*IQR(fb$Reactions)
length(fb$Reactions[fb$Reactions<rc_lb])
'98 outliers'

rc_ub = quantile(fb$Reactions, 0.75)+1.5*IQR(fb$Reactions)
length(fb$Reactions[fb$Reactions>rc_ub])
'35 outliers'

#________________69 Balance
str(fb$Balance)
'int [1:17918] 95 70 84 43 77 94 94 83 66 49 ...'

summary(fb$Balance)
' Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  16.00   56.00   66.00   63.96   74.00   96.00 '

#Histogram
hist(fb$Balance,
     col = brewer.pal(8,'PRGn'),
     main = 'Balance of the Player',
     xlab = 'Balance')

#Boxplot
boxplot(fb$Balance,
        col = 'purple3',
        horizontal = TRUE,
        main = 'Balance of the Player')

#Checking the outliers 
bl_lb = quantile(fb$Balance, 0.25)-1.5*IQR(fb$Balance)
length(fb$Balance[fb$Balance<bl_lb])
'240 outliers'

#________________70 ShotPower
str(fb$ShotPower)
'int [1:17918] 85 95 80 31 91 82 79 86 79 22 ...'

summary(fb$ShotPower)
'  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00   45.00   59.00   55.49   68.00   95.00 '

#Histogram
hist(fb$ShotPower,
     col = brewer.pal(8,'Pastel1'),
     main = 'ShotPower of the Player',
     xlab = 'ShotPower')

#Boxplot
boxplot(fb$ShotPower,
        col = 'lightcyan2',
        horizontal = TRUE,
        main = 'ShotPower of the Player')

#Checking the outliers 
sh_lb = quantile(fb$ShotPower, 0.25)-1.5*IQR(fb$ShotPower)
length(fb$ShotPower[fb$ShotPower<sh_lb])
'15 outliers'

#________________71 Jumping
str(fb$Jumping)
'int [1:17918] 68 95 61 67 63 56 68 69 93 76 ...'

summary(fb$Jumping)
'  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.00   58.00   66.00   65.12   73.00   95.00 '

#Histogram
hist(fb$Jumping,
     col = brewer.pal(8,'Spectral'),
     main = 'Jumping of the Player',
     xlab = 'Jumping')

#Boxplot
boxplot(fb$Jumping,
        col = 'sienna3',
        horizontal = TRUE,
        main = 'Jumping of the Player')

#Checking the outliers 
ju_lb = quantile(fb$Jumping, 0.25)-1.5*IQR(fb$Jumping)
length(fb$Jumping[fb$Jumping<ju_lb])
'381 outliers'

#________________72 Stamina
str(fb$Stamina)
'int [1:17918] 72 88 81 43 90 83 89 90 84 41 ...'

summary(fb$Stamina)
'  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.00   58.00   66.00   65.12   73.00   95.00 '

#Histogram
hist(fb$Stamina,
     col = brewer.pal(8,'YlGnBu'),
     main = 'Stamina of the Player',
     xlab = 'Stamina')

#Boxplot
boxplot(fb$Stamina,
        col = 'deepskyblue2',
        horizontal = TRUE,
        main = 'Stamina of the Player')

#Checking the outliers 
st_lb = quantile(fb$Stamina, 0.25)-1.5*IQR(fb$Stamina)
length(fb$Stamina[fb$Stamina<st_lb])
'854 outliers'

#________________73 Strength
str(fb$Strength)
'int [1:17918] 59 79 49 64 75 66 58 83 83 78 ...'

summary(fb$Strength)
 ' Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  17.00   58.00   67.00   65.32   74.00   97.00 '

#Histogram
hist(fb$Strength,
     col = brewer.pal(8,'YlOrRd'),
     main = 'Strength of the Player',
     xlab = 'Strength')

#Boxplot
boxplot(fb$Strength,
        col = 'darkred',
        horizontal = TRUE,
        main = 'Strength of the Player')

#Checking the outliers 
str_lb = quantile(fb$Strength, 0.25)-1.5*IQR(fb$Strength)
length(fb$Strength[fb$Strength<str_lb])
'253 outliers'

#________________74 LongShots
str(fb$LongShots)
'int [1:17918] 94 93 82 12 91 80 82 85 59 12 ...'

summary(fb$LongShots)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   33.00   51.00   47.13   62.00   94.00'

#Histogram
hist(fb$LongShots,
     col = brewer.pal(11,'Set3'),
     main = 'LongShots of the Player',
     xlab = 'LongShots')

#Boxplot
boxplot(fb$LongShots,
        col = 'slategray2',
        horizontal = TRUE,
        main = 'LongShots of the Player')

#________________75 Aggression
str(fb$Aggression)
'int [1:17918] 48 63 56 38 76 54 62 87 88 34 ...'

summary(fb$Aggression)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  11.00   44.00   59.00   55.88   69.00   95.00 '

#Histogram
hist(fb$Aggression,
     col = brewer.pal(8,'Set1'),
     main = 'Aggression of the Player',
     xlab = 'Aggression')

#Boxplot
boxplot(fb$Aggression,
        col = 'yellow2',
        horizontal = TRUE,
        main = 'Aggression of the Player')

#________________76 Interceptions
str(fb$Interceptions)
' int [1:17918] 22 29 36 30 61 41 83 41 90 19 ...'

summary(fb$Interceptions)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   26.00   52.00   46.69   64.00   92.00'

#Histogram
hist(fb$Interceptions,
     col = brewer.pal(8,'RdGy'),
     main = 'Interceptions of the Player',
     xlab = 'Interceptions')

#Boxplot
boxplot(fb$Interceptions,
        col = 'gray50',
        horizontal = TRUE,
        main = 'Interceptions of the Player')

#________________77 Positioning
str(fb$Positioning)
'int [1:17918] 94 95 89 12 87 87 79 92 60 11 ...'

summary(fb$Positioning)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      2      39      55      50      64      95'

#Histogram
hist(fb$Positioning,
     col = brewer.pal(8,'PRGn'),
     main = 'Positioning of the Player',
     xlab = 'Positioning')

#Boxplot
boxplot(fb$Positioning,
        col = 'mediumseagreen',
        horizontal = TRUE,
        main = 'Positioning of the Player')

#________________78 Vision
str(fb$Vision)
'int [1:17918] 94 82 87 68 94 89 92 84 63 70 ...'

summary(fb$Vision)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  10.00   44.00   55.00   53.45   64.00   94.00 '

#Histogram
hist(fb$Vision,
     col = brewer.pal(8,'PiYG'),
     main = 'Vision of the Player',
     xlab = 'Vision')

#Boxplot
boxplot(fb$Vision,
        col = 'maroon3',
        horizontal = TRUE,
        main = 'Vision of the Player')

#Checking the outliers 
vis_lb = quantile(fb$Vision, 0.25)-1.5*IQR(fb$Vision)
length(fb$Vision[fb$Vision<vis_lb])
'60 outliers'

#________________79 Penalties
str(fb$Penalties)
'int [1:17918] 75 85 81 40 79 86 82 85 75 11 ...'

summary(fb$Penalties)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   39.00   49.00   48.54   60.00   92.00 '

#Histogram
hist(fb$Penalties,
     col = brewer.pal(8,'BrBG'),
     main = 'Penalties of the Player',
     xlab = 'Penalties')

#Boxplot
boxplot(fb$Penalties,
        col = 'darkcyan',
        horizontal = TRUE,
        main = 'Penalties of the Player')

#Checking the outliers 
pen_lb = quantile(fb$Penalties, 0.25)-1.5*IQR(fb$Penalties)
length(fb$Penalties[fb$Penalties<pen_lb])
'2 outliers'

pen_ub = quantile(fb$Penalties, 0.75)+1.5*IQR(fb$Penalties)
length(fb$Penalties[fb$Penalties>pen_ub])
'1 outlier'

#________________80 Composure
str(fb$Composure)
'int [1:17918] 96 95 94 68 88 91 84 85 82 70 ...'

summary(fb$Composure)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   51.00   60.00   58.66   67.00   96.00 '

#Histogram
hist(fb$Composure,
     col = brewer.pal(8,'Accent'),
     main = 'Composure of the Player',
     xlab = 'Composure')

#Boxplot
boxplot(fb$Composure,
        col = 'dodgerblue3',
        horizontal = TRUE,
        main = 'Composure of the Player')

#Checking the outliers 
com_lb = quantile(fb$Composure, 0.25)-1.5*IQR(fb$Composure)
length(fb$Composure[fb$Composure<com_lb])
'145 outliers'

com_ub = quantile(fb$Composure, 0.75)+1.5*IQR(fb$Composure)
length(fb$Composure[fb$Composure>com_ub])
'6 outlier'

#________________81 Marking
str(fb$Marking)
'int [1:17918] 33 28 27 15 68 34 60 62 87 27 ...'

summary(fb$Marking)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   30.00   53.00   47.26   64.00   94.00'

#Histogram
hist(fb$Marking,
     col = brewer.pal(11,'Set3'),
     main = 'Marking of the Player',
     xlab = 'Marking')

#Boxplot
boxplot(fb$Marking,
        col = 'coral',
        horizontal = TRUE,
        main = 'Marking of the Player')

#________________82 StandingTackle
str(fb$StandingTackle)
' int [1:17918] 28 31 24 21 58 27 76 45 92 12 ...'

summary(fb$StandingTackle)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00   27.00   55.00   47.68   66.00   93.00 '

#Histogram
hist(fb$StandingTackle,
     col = brewer.pal(11,'Dark2'),
     main = 'StandingTackle of the Player',
     xlab = 'StandingTackle')

#Boxplot
boxplot(fb$StandingTackle,
        col = 'gold3',
        horizontal = TRUE,
        main = 'StandingTackle of the Player')

#________________83 SlidingTackle
str(fb$SlidingTackle)
'int [1:17918] 26 23 33 13 51 22 73 38 91 18 ...'

summary(fb$SlidingTackle)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   24.00   52.00   45.64   64.00   91.00'

#Histogram
hist(fb$SlidingTackle,
     col = brewer.pal(8,'YlGnBu'),
     main = 'SlidingTackle of the Player',
     xlab = 'SlidingTackle')

#Boxplot
boxplot(fb$SlidingTackle,
        col = 'cyan4',
        horizontal = TRUE,
        main = 'SlidingTackle of the Player')

#________________84 GKDiving
str(fb$GKDiving)
' int [1:17918] 6 7 9 90 15 11 13 27 11 86 ...'

summary(fb$GKDiving)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    8.00   11.00   16.59   14.00   90.00'

#Histogram
hist(fb$GKDiving,
     col = brewer.pal(8,'Spectral'),
     main = 'GKDiving of the Player',
     xlab = 'GKDiving')

#Boxplot
boxplot(fb$GKDiving,
        col = 'brown1',
        horizontal = TRUE,
        main = 'GKDiving of the Player')

#Checking the outliers 
gk_ub = quantile(fb$GKDiving, 0.75)+1.5*IQR(fb$GKDiving)
length(fb$GKDiving[fb$GKDiving>gk_ub])
'1996 outlier'

#________________85 GKHandling
str(fb$GKHandling)
'int [1:17918] 11 11 9 85 13 12 9 25 8 92 ...'

summary(fb$GKHandling)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    8.00   11.00   16.37   14.00   92.00'

#Histogram
hist(fb$GKHandling,
     col = brewer.pal(8,'Pastel2'),
     main = 'GKHandling of the Player',
     xlab = 'GKHandling')

#Boxplot
boxplot(fb$GKHandling,
        col = 'burlywood2',
        horizontal = TRUE,
        main = 'GKHandling of the Player')

#Checking the outliers 
gkh_ub = quantile(fb$GKHandling, 0.75)+1.5*IQR(fb$GKHandling)
length(fb$GKHandling[fb$GKHandling>gkh_ub])
'1995 outlier'

#________________86 GKKicking
str(fb$GKKicking)
'int [1:17918] 15 15 15 87 5 6 7 31 9 78 ...'

summary(fb$GKKicking)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    8.00   11.00   16.21   14.00   91.00 '

#Histogram
hist(fb$GKKicking,
     col = brewer.pal(8,'Paired'),
     main = 'GKKicking of the Player',
     xlab = 'GKKicking')

#Boxplot
boxplot(fb$GKKicking,
        col = 'dodgerblue2',
        horizontal = TRUE,
        main = 'GKKicking of the Player')

#Checking the outliers 
gkk_ub = quantile(fb$GKKicking, 0.75)+1.5*IQR(fb$GKKicking)
length(fb$GKKicking[fb$GKKicking>gkk_ub])
'2002 outliers'

#________________87 GKPositioning
str(fb$GKPositioning)
' int [1:17918] 14 14 15 88 10 8 14 33 7 88 ...'

summary(fb$GKPositioning)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    8.00   11.00   16.36   14.00   90.00 '

#Histogram
hist(fb$GKPositioning,
     col = brewer.pal(8,'RdYlGn'),
     main = 'GKPositioning of the Player',
     xlab = 'GKPositioning')

#Boxplot
boxplot(fb$GKPositioning,
        col = 'coral2',
        horizontal = TRUE,
        main = 'GKPositioning of the Player')

#Checking the outliers 
gkp_ub = quantile(fb$GKPositioning, 0.75)+1.5*IQR(fb$GKPositioning)
length(fb$GKPositioning[fb$GKPositioning>gkp_ub])
'1998 Outliers'

#________________88 GKReflexes
str(fb$GKReflexes)
'int [1:17918] 8 11 11 94 13 8 9 37 11 89 ...'

summary(fb$GKReflexes)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    8.00   11.00   16.68   14.00   94.00 '

#Histogram
hist(fb$GKReflexes,
     col = brewer.pal(8,'PuOr'),
     main = 'GKReflexes of the Player',
     xlab = 'GKReflexes')

#Boxplot
boxplot(fb$GKReflexes,
        col = 'gold3',
        horizontal = TRUE,
        main = 'GKReflexes of the Player')

#Checking the outliers 
gkr_ub = quantile(fb$GKReflexes, 0.75)+1.5*IQR(fb$GKReflexes)
length(fb$GKReflexes[fb$GKReflexes>gkr_ub])
'1997 Outliers'

#________________89 Release.Clause
str(fb$Release.Clause)
' Factor w/ 1245 levels "","â,¬1.1M","â,¬1.2M",..: 295 84 296 106 234  ...'

table(fb$Release.Clause)

#Replace or substitute â,¬ with nothing
fb$Release.Clause = sub("â,¬","", fb$Release.Clause)
table(fb$Release.Clause)

#integer. A penalty to be applied when deciding to print numeric values in
#fixed or exponential notation. Positive values bias towards fixed and negative 
#towards scientific notation: fixed notation will be preferred unless it is 
#more than scipen digits wider.

options(scipen = 15)

#Removing M & K and keeping all the values to 1000's
'The grep R function searches for matches of certain character pattern in a 
vector of character strings and returns the indices that yielded a match.'
fb$Release.Clause[grep('K$',fb$Release.Clause)] = as.numeric(sub('K',"",fb$Release.Clause[grep('K$',fb$Release.Clause)]))
fb$Release.Clause[grep('M$',fb$Release.Clause)] = (as.numeric(sub('M',"",fb$Release.Clause[grep('M$',fb$Release.Clause)]))*1000000)/1000
fb$Release.Clause = as.numeric(fb$Release.Clause)

str(fb$Release.Clause)
'num [1:17918] 226500 127100 228100 138600 196400 ...'

summary(fb$Release.Clause)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's' 
     13     525    1100    4585    3500  228100    1275'

#Removing the Na's
fb = na.omit(fb)
dim(fb)
'16643 89
17918-16643 = 1275 obs'

summary(fb$Release.Clause)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     13     525    1100    4585    3500  228100'

#Histogram
hist(fb$Release.Clause,
     col = brewer.pal(8,'Set2'),
     main = 'Release.Clause of the Player',
     xlab = 'Release.Clause')

#Boxplot
boxplot(fb$Release.Clause,
        col = 'lightcoral',
        horizontal = TRUE,
        main = 'Release.Clause of the Player')

#Checking the outliers 
rc_ub = quantile(fb$Release.Clause, 0.75)+1.5*IQR(fb$Release.Clause)
length(fb$Release.Clause[fb$Release.Clause>rc_ub])
'2403 Outliers'

for (i in seq(rc_ub,max(fb$Release.Clause),25000)){
        j = length(fb$Release.Clause[fb$Release.Clause > i])
        print(paste('No of outliers with ub as',round(i,0), 'is',j))
}

'[1] "No of outliers with ub as 7962 is 2403"
[1] "No of outliers with ub as 32962 is 370"
[1] "No of outliers with ub as 57962 is 138"
[1] "No of outliers with ub as 82962 is 61"
[1] "No of outliers with ub as 107962 is 36"
[1] "No of outliers with ub as 132962 is 17"
[1] "No of outliers with ub as 157962 is 8"
[1] "No of outliers with ub as 182962 is 3"
[1] "No of outliers with ub as 207962 is 2"'

#no action taken on outliers
head(fb[order(fb$Release.Clause, decreasing = TRUE),][c(3,89)],10)

tail(fb[order(fb$Release.Clause, decreasing = TRUE),][c(3,89)],10)

#===============================================================================
"The file has lots of information which we do not need for clustering. 
Only player's attributes such as speed, strength, passing, finish, heading etc 
will be taken."

fb1 = fb[,c(3,4,8,9,12,14,16:18,27,28,55:89)]
#For easy calculation selected mainly numerical variables
names(fb) # 46
dim(fb1)
'16643    46'

write.csv(fb1, "C:/Users/Dr Vinod/Desktop/football_ kmean/fb1.csv")


str(fb1)

#Converting the Names column as row names
row.names(fb1) = fb1$Name
'There are duplicate rownames'

#Removing the duplicate names
duplicated(fb1$Name) # identifies which are duplicate
fb2 = fb1[!duplicated(fb1$Name),] # Selecting the rows which are not duplicated
row.names(fb2) = fb2$Name #Converting the names as row names/ index
fb2 = subset(fb2, select = -c(Name)) #Removing the name column
head(fb2,3)
dim(fb2)
'15773    45'
'16643-15773 = 870 observations removed'

write.csv(fb2, "C:/Users/Dr Vinod/Desktop/football_ kmean/fb2.csv")

#It is taking lot of time to execute on whole data so selecting only first
#5000 observations
fb_5000 = sample_n(fb2,5000)
str(fb_5000) 



#RCB, RB should not be there
#Scaling the data
fb_5000_scale = scale(fb_5000)

#================Model 1 - Kmeans using WSS method==============================
#Plotting to get the optimal number of clusters, kmeans by default checks upto 10

library(factoextra)

fviz_nbclust(fb_5000_scale, kmeans, method = "wss")

fviz_nbclust(fb_5000_scale, kmeans, method = "wss")+
  geom_vline(xintercept = 3,linetype=5,col='red')

# Compute k-means with k = 3
set.seed(123)
' we specify nstart = 25. This means that R will try 25 different random
starting assignments and then select the best results corresponding to the one 
with the lowest within cluster variation'
km_clusters <- kmeans(fb_5000_scale, 3, nstart = 25)

# Print the results
print(km_clusters)

km_clusters$size
'K-means clustering with 3 clusters of sizes 565, 2414, 2021'

km_clusters$betweenss/km_clusters$totss
'0.4897724'

#Adding the cluster index as a variable to the data frame
fb_5000 = cbind(fb_5000, cluster = km_clusters$cluster)
head(fb_5000)

#Checking the means of values in the fb_5000
aggregate(fb_5000, by=list(cluster=km_clusters$cluster), mean)

#=================Don't run the following code==================================
#Visualizing the clusters
fviz_cluster(km.res, data = fb_5000,
             palette = brewer.pal(5,'Dark2'),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())
#===============================================================================

#==========Kmeans - cluster visual output using 100 data points=================
#It is taking lot of time to execute on whole data so selecting only first
#100 observations
fb_100 = sample_n(fb2,100)

#Scaling
fb_100_scale = scale(fb_100)

#Plotting to get the optimal number of clusters, kmeans by default checks upto 10
fviz_nbclust(fb_100_scale, kmeans, method = "wss")+
  geom_vline(xintercept = 3,linetype=2, col='red')
  
# Compute k-means with k = 4
set.seed(123)
' we specify nstart = 25. This means that R will try 25 different random
starting assignments and then select the best results corresponding to the one 
with the lowest within cluster variation'
km.res_100 <- kmeans(fb_100_scale, 3, nstart = 25)

# Print the results
print(km.res_100)

km.res_100$size
'K-means clustering with 3 clusters of sizes 45, 12, 43'

km.res_100$betweenss/km.res_100$totss
'0.5188112'

#Adding the cluster index as a variable to the data frame
fb_100 = cbind(fb_100, cluster = km.res_100$cluster)
head(fb_100)

#Visualizing the clusters
fviz_cluster(km.res_100, data = fb_100,
             palette = brewer.pal(3,'Set1'),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())
#===============================================================================
#================Model 2 - Kmeans using Silhouette method=======================
#Plotting to get the optimal number of clusters, kmeans by default checks upto 10
fviz_nbclust(fb_5000_scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# K-means clustering
km.res1 <- eclust(fb_5000_scale, "kmeans", k = 2, nstart = 25, graph = FALSE)

#Silhouette coefficients
km.res1$silinfo

# Silhouette plot
fviz_silhouette(km.res1, palette = "jco",
                ggtheme = theme_classic())

#Visualize k-means clusters
fviz_cluster(km.res1, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

#===============================================================================
#>>>>>>>>>>>>>>>>>> NOW ON ENTIRE DATA
#Scaling the data
fb1_scale = scale(fb2)

#Plotting to get the optimal number of clusters, kmeans by default checks upto 10
#Method - Silhouette
fviz_nbclust(fb1_scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#Method - WSS - Taking long time=======WITHOUT BELOW CODE , WE CAN GO FORWARD=========================================
fviz_nbclust(fb1_scale, kmeans, method = "wss")+
  labs(subtitle = "WSS method")
#===============================================================================
# K-means clustering
km.res_fb <- eclust(fb1_scale, "kmeans", k = 2, nstart = 25, graph = FALSE)

#Silhouette coefficients
km.res_fb$silinfo

#Size of each cluster
km.res_fb$size
'K-means clustering with 2 clusters of sizes 13976  1797'

km.res_fb$betweenss/km.res_fb$totss
'0.3676574'

# Silhouette plot
fviz_silhouette(km.res_fb, palette = "jco",
                ggtheme = theme_classic())

#Visualize k-means clusters
fviz_cluster(km.res_fb, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

#Adding the cluster index as a variable to the data frame
fb3 = cbind(fb2, cluster = km.res_fb$cluster)
head(fb3)

write.csv(fb3, "C:/Users/Dr Vinod/Desktop/football_ kmean/fb3.csv")


#Checking the means of values in the fb_5000
c3 = aggregate(fb3, by=list(cluster=km.res_fb$cluster), mean)
write.csv(c3, "C:/Users/Dr Vinod/Desktop/football_ kmean/c3.csv")


