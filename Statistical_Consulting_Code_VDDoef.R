# libraries
library( tidyverse )
library( haven )
library( lme4 )
library( multcomp )
library( ClusterBootstrap )
library( ggplot2 )
library( ggthemes )
library( jtools )

# loading the data (Faraaz)
basisvragenlijst <- read_sav( 'C:/Users/lappy/Desktop/M&S/SC/analysis/basisvragenlijst 1e en 2e deel (alle respondenten anon - dagboek).sav' )
dagboek0 <- read_sav( 'C:/Users/lappy/Desktop/M&S/SC/analysis/dagboek 1e en 2e deel dag 1-28 (alle respondenten - gereduceerd).sav' )

# loading the data (Luuk)
basisvragenlijst <- basisvragenlijst_1e_en_2e_deel_alle_respondenten_anon_dagboek_ <- read_sav("C:/Users/Luuk/Desktop/Statistical consulting/basisvragenlijst 1e en 2e deel (alle respondenten anon - dagboek).sav")
dagboek0 <- dagboek_1e_en_2e_deel_dag_1_28_alle_respondenten_gereduceerd_ <- read_sav("C:/Users/Luuk/Desktop/Statistical consulting/dagboek 1e en 2e deel dag 1-28 (alle respondenten - gereduceerd).sav")

# extracting the relevant columns
basis <- basisvragenlijst[ , c( 1, 18, 20, 54, 55, 110 : 117, 118 : 128 ) ]
dagboek <- dagboek0[ , c( 1 : 9, 45, 46, 48 : 57 ) ]
colnames( dagboek )[ 1 : 11 ] <- c( 'ID', 'dag', 'pijn', 'level', 'unilat', 'pulsat', 'aura',
                          'nausea', 'sensitiv', 'prod', 'situatie' )

# WRFQ items: converting NAs to 9999
dagboek[ , c( 12 : 21 ) ] <- dagboek[ , c( 12 : 21 ) ] %>%
  replace( is.na( . ), 9999 )

# converting NAs to 0 in the rest of the data frame
dagboek <- dagboek %>%
  replace( is.na( . ), 0 )

# WRFQ items: converting 6 to 0
for ( i in 1 : nrow( dagboek[ , c( 12 : 21 ) ] ) ) {
  for ( j in 1 : ncol( dagboek[ , c( 12 : 21 ) ] ) ) {
    if ( dagboek[ , c( 12 : 21 ) ][ i, j ] == 6 ) {
      dagboek[ , c( 12 : 21 ) ][ i, j ] <- 0
    }
  }
}

# generating new variables
# migraine attack variable
migraine <- rep( 9999 , nrow( dagboek ) )
for ( i in 1 : nrow( dagboek ) ) {
  if ( dagboek$pijn[ i ] == 1 && dagboek$level[ i ] > 6 ) {
    if ( dagboek$unilat == 1 || dagboek$pulsat == 1 || dagboek$aura == 1 ||
         dagboek$nausea == 1 || dagboek$sensitiv == 1 ) {
      migraine[ i ] <- 1
    } else {
      migraine[ i ] <- 0
    }
  } else {
    migraine[ i ] <- 0
  }
}

# lower work ability variable
lowwork <- rep( 9999 , nrow( dagboek ) )
for ( i in 1: nrow( dagboek ) ) {
  if ( dagboek$situatie[ i ] == 4 && dagboek$prod[ i ] == 1 ) {
    lowwork[ i ] <- 1
  } else if ( dagboek$situatie[ i ] == 4 || dagboek$prod[ i ] == 1 ) {
    lowwork[ i ] <- 1
  } else {
    lowwork[ i ] <- 0
  }
}

# WRFQ sum score
WRFQscore <- ifelse( dagboek$Q132_1 != 9999, rowSums( dagboek[ , c( 12 : 21 ) ] ),
                     9999 )

# lower work ability 2 variable
mdn <- median( WRFQscore[ WRFQscore != 9999 ] )
lowwork2 <- rep( 9999, nrow( dagboek ) )
for ( i in 1 : nrow( dagboek ) ) {
  if ( dagboek$situatie[ i ] == 4 && dagboek$prod[ i ] == 1 && WRFQscore[ i ] > mdn
       && WRFQscore[ i ] < 9999 ) {
    lowwork2[ i ] <- 1
  } else if ( dagboek$situatie[ i ] == 4 && dagboek$prod[ i ] == 1 ) {
    lowwork2[ i ] <- 1
  } else if ( dagboek$situatie[ i ] == 4 && WRFQscore[ i ] > mdn
              && WRFQscore[ i ] < 9999 ) {
    lowwork2[ i ] <- 1
  } else if ( dagboek$prod[ i ] == 1 && WRFQscore[ i ] > mdn
              && WRFQscore[ i ] < 9999 ) {
    lowwork2[ i ] <- 1
  } else if ( dagboek$situatie[ i ] == 4 || dagboek$prod[ i ] == 1 ) {
    lowwork2[ i ] <- 1
  } else if ( WRFQscore[ i ] > mdn && WRFQscore[ i ] < 9999 ) {
    lowwork2[ i ] <- 1
  } else if ( dagboek$situatie[ i ] == 4 || dagboek$prod[ i ] == 1 ) {
    lowwork2[ i ] <- 1
  } else {
    lowwork2[ i ] <- 0
  }
}

# adding the variables to the data frame
dagboek1 <- cbind( dagboek, migraine, lowwork2, lowwork, WRFQscore )
dagboek2 <- dagboek1[ , c( 1, 2, 22, 23, 24, 25, 3 : 21 ) ]

# adding the flexibility variables to the diary data frame
dagboekvol <- left_join( dagboek2, basis, by = c( 'ID' = 'identcode' ) )
colnames( dagboekvol )[ c( 26, 27 ) ] <- c( 'flextijd', 'flexplek' )

dagboekvol$flextijd[ dagboekvol$ID == 144 ] <- 1

# flexibility of worktime: converting 5 to 1
for ( i in 1 : nrow( dagboekvol ) ) {
  if ( dagboekvol$flextijd[ i ] == 5 ) {
    dagboekvol$flextijd[ i ] <- 1
  }
}

# extra: data frame displaying how many das each participant responded
ndagen <- dagboekvol %>%
  group_by( ID ) %>%
  summarise( dagen = n() )
ndagen

# adding the prodromal day variable (the seperate phase variables will later be added together in
# one factor)
prodrome <- numeric()
for ( i in 1 : nrow( dagboekvol ) - 5 ) {
  if ( dagboekvol$migraine[ i + 1 ] == 1 &&
       dagboekvol$migraine[ i - 3 ] == 0 &&
       dagboekvol$migraine[ i - 2 ] == 0 &&
       dagboekvol$migraine[ i - 1 ] == 0 &&
       dagboekvol$migraine[ i + 4 ] == 0 &&
       dagboekvol$migraine[ i + 5 ] == 0 &&
       dagboekvol$migraine[ i ] == 0 &&
       dagboekvol$migraine[ i + 2 ] == 0 &&
       dagboekvol$migraine[ i + 3 ] == 0 &&
       dagboekvol$dag[ i - 1 ] == dagboekvol$dag[ i ] - 1 &&
       dagboekvol$dag[ i + 1 ] == dagboekvol$dag[ i ] + 1 &&
       dagboekvol$dag[ i + 2 ] == dagboekvol$dag[ i ] + 2 &&
       dagboekvol$dag[ i + 3 ] == dagboekvol$dag[ i ] + 3 ) {
    prodrome[ i ] <- 1
  } else {
    prodrome[ i ] <- 0
  }
}
prodrome[ 1014 : 1018 ] <- c( 0, 1, 0, 0, 0 )

dagboekvol2 <- cbind( dagboekvol, prodrome )
dagboekvol2 <- dagboekvol2[ , c( 1, 2, 49, 3 : 48 ) ]

for ( i in 1 : nrow( dagboekvol2 ) ) {
  if ( dagboekvol2$migraine[ i ] == 1 ) {
    dagboekvol2$prodrome[ i ] <- 0
  }
}

for ( i in 1 : nrow( dagboekvol2 ) - 1 ) {
  if ( dagboekvol2$prodrome[ i + 1 ] == 1 ) {
    dagboekvol2$prodrome[ i ] <- 1
  }
}
  
# generating the recovery day variable
recovery <- numeric()
for ( i in 1 : nrow( dagboekvol ) ) {
  if ( dagboekvol2$prodrome[ i - 2 ] == 1 &&
       dagboekvol2$prodrome[ i - 3 ] == 1 ) {
    recovery[ i ] <- 1
  } else {
    recovery[ i ] <- 0
  }
}

dagboekvol3 <- cbind( dagboekvol2, recovery )
dagboekvol3 <- dagboekvol3[ , c( 1 : 4, 50, 5 : 49 ) ]  

for ( i in 4 : nrow( dagboekvol3 ) ) {
  if ( dagboekvol3$prodrome[ i - 3 ] == 1 ) {
    dagboekvol3$recovery[ i ] <- 1
  }
}

# generating the phase variable
fase <- rep( 0, nrow( dagboekvol3 ) )
dagboekvol4 <- cbind( dagboekvol3, fase )
dagboekvol4 <- dagboekvol4[ , c( 1, 2, 51, 3 : 50 ) ]  

for ( i in 3 : nrow( dagboekvol4 ) - 1 ) {
  if ( dagboekvol4$prodrome[ i - 1 ] == 1 &&
       dagboekvol4$prodrome[ i - 2 ] == 1 ) {
    dagboekvol4$fase[ i ] <- 3
  } else if ( dagboekvol4$prodrome[ i ] == 1 &&
              dagboekvol4$prodrome[ i + 1 ] == 1) {
    dagboekvol4$fase[ i ] <- 1
  } else if ( dagboekvol4$prodrome[ i ] == 1 &&
              dagboekvol4$prodrome[ i - 1 ] == 1) {
    dagboekvol4$fase[ i ] <- 2
  } else if ( dagboekvol4$recovery[ i ] == 1 &&
              dagboekvol4$recovery[ i + 1 ] == 1) {
    dagboekvol4$fase[ i ] <- 4
  } else if ( dagboekvol4$recovery[ i ] == 1 &&
             dagboekvol4$recovery[ i - 1 ] == 1) {
    dagboekvol4$fase[ i ] <- 5
  }
}
dagboekvol4$fase[ 1018 ] <- 5

for ( i in 1 : nrow( dagboekvol4 ) ) {
  if( dagboekvol4$fase[ i ] == 0 &&
      dagboekvol4$migraine[ i ] == 1 ) {
    dagboekvol4$fase[ i ] <- 9999
  }
}

# observations per factor level
summary( as.factor( dagboekvol4$fase ) )

# eventual data frame (dagboekfinal: only participants with pure classification
# values of the phase variabele:
# 0 = no headache
# 1 = prodromal day 1
# 2 = prodromal day 2
# 3 = migraine attack day
# 4 = recovery day 1
# 5 = recovery day 2

dagboekfinal <- dagboekvol4
for ( i in 1 : nrow( dagboekfinal ) ) {
  if ( dagboekfinal$fase[ i ] == 9999 ) {
    dagboekfinal$fase[ i ] <- NA
  }
}
dagboekfinal <- dagboekfinal %>% 
  drop_na( fase )
dagboekfinal$fase <- as.factor( dagboekfinal$fase )
dagboekfinal$lowwork <- as.factor( dagboekfinal$lowwork )
dagboekfinal$flextijd <- as.numeric( dagboekfinal$flextijd )
dagboekfinal$flexplek <- as.numeric( dagboekfinal$flexplek )

# observations per level
summary( dagboekfinal$fase )

# observations with complete WRFQ score per level
summary( dagboekfinal$fase[ WRFQscore != 9999 ] )

# <additional data frame> new phase variabele: 1 day prodromal, 1 day recovery
# first making the prodromal day variable again
prodrome2 <- numeric()
for ( i in 1 : nrow( dagboekvol ) - 5 ) {
  if ( dagboekvol$migraine[ i + 1 ] == 1 &&
       dagboekvol$migraine[ i - 1 ] == 0 &&
       dagboekvol$migraine[ i + 3 ] == 0 &&
       dagboekvol$migraine[ i ] == 0 &&
       dagboekvol$migraine[ i + 2 ] == 0 &&
       dagboekvol$migraine[ i + 3 ] == 0 &&
       dagboekvol$dag[ i + 1 ] == dagboekvol$dag[ i ] + 1 &&
       dagboekvol$dag[ i + 2 ] == dagboekvol$dag[ i ] + 2 ) {
    prodrome2[ i ] <- 1
  } else {
    prodrome2[ i ] <- 0
  }
}
prodrome2[ 1014 : 1018 ] <- c( 0, 1, 0, 0, 0 )

dagboekvol2.2 <- cbind( dagboekvol, prodrome2 )
dagboekvol2.2 <- dagboekvol2.2[ , c( 1, 2, 49, 3 : 48 ) ]

# then the recovery day variable
recovery2 <- numeric()
for ( i in 1 : nrow( dagboekvol ) ) {
  if ( dagboekvol2.2$prodrome2[ i - 2 ] == 1 &&
       dagboekvol2.2$migraine[ i - 1 ] == 1 ) {
    recovery2[ i ] <- 1
  } else {
    recovery2[ i ] <- 0
  }
}

dagboekvol3.2 <- cbind( dagboekvol2.2, recovery2 )
dagboekvol3.2 <- dagboekvol3.2[ , c( 1 : 4, 50, 5 : 49 ) ]  

# finally the second phase variable
fase2 <- rep( 0, nrow( dagboekvol3.2 ) )
dagboekvol4.2 <- cbind( dagboekvol3.2, fase2 )
dagboekvol4.2 <- dagboekvol4.2[ , c( 1, 2, 51, 3 : 50 ) ]  

for ( i in 3 : nrow( dagboekvol4.2 ) - 2 ) {
  if ( dagboekvol4.2$prodrome2[ i ] == 1 &&
       dagboekvol4.2$migraine[ i + 1 ] == 1 &&
       dagboekvol4.2$recovery2[ i + 2 ] == 1 ) {
    dagboekvol4.2$fase2[ i ] <- 1
  } else if ( dagboekvol4.2$migraine[ i ] == 1 &&
              dagboekvol4.2$prodrome2[ i - 1 ] == 1 &&
              dagboekvol4.2$recovery2[ i + 1 ] == 1 ) {
    dagboekvol4.2$fase2[ i ] <- 2
  } else if ( dagboekvol4.2$recovery2[ i ] == 1 &&
              dagboekvol4.2$prodrome2[ i - 2 ] == 1 &&
              dagboekvol4.2$migraine[ i - 1 ] == 1) {
    dagboekvol4.2$fase2[ i ] <- 3
  }
}
dagboekvol4.2$fase2[ 1017 ] <- 3

# for normal days: phase = 0 and migraine = 0, else 9999
for ( i in 1 : nrow( dagboekvol4.2 ) ) {
  if( dagboekvol4.2$fase2[ i ] == 0 &&
      dagboekvol4.2$migraine[ i ] == 1 ) {
    dagboekvol4.2$fase2[ i ] <- 9999
  }
}

dagboekfinal2 <- dagboekvol4.2
for ( i in 1 : nrow( dagboekfinal2 ) ) {
  if ( dagboekfinal2$fase2[ i ] == 9999 ) {
    dagboekfinal2$fase2[ i ] <- NA
  }
}
dagboekfinal2 <- dagboekfinal2 %>% 
  drop_na( fase2 )
dagboekfinal2$fase2 <- as.factor( dagboekfinal2$fase2 )
dagboekfinal2$lowwork <- as.factor( dagboekfinal2$lowwork )
dagboekfinal2$flextijd <- as.numeric( dagboekfinal2$flextijd )
dagboekfinal2$flexplek <- as.numeric( dagboekfinal2$flexplek )

# 2nd data frame for the 2nd phase variable, levels:
# 0 = no headache
# 1 = prodromal day
# 2 = migraine attack day
# 3 = recovery day
# observations per level:
summary( dagboekfinal2$fase2 )

# observations with complete WRFQ scores:
summary( dagboekfinal2$fase2[ WRFQscore != 9999 ] )



######## data exploration ######## 



# WRFQ sum score distribution
WRFQss <- dagboekfinal2[ dagboekfinal2$WRFQscore != 9999, ]
ggplot( WRFQss, aes( x = WRFQscore ) ) +
  geom_histogram( ) +
  theme_apa( ) +
  xlab( 'WRFQ sum score' )

# number of observations per phase, phase long
summary( as.factor( dagboekfinal$fase ) )

# number of observations per phase, phase short
summary( as.factor( dagboekfinal2$fase2 ) )

# number of observations per phase, phase long, only with complete WRFQ sum score
summary( as.factor( dagboekfinal$fase[ dagboekfinal$WRFQscore != 9999 ] ) )

# number of observations per phase, phase short, only with complete WRFQ sum score
summary( as.factor( dagboekfinal2$fase2[ dagboekfinal$WRFQscore != 9999 ] ) )

# distribution lowwork, phase long
table( dagboekfinal$fase, dagboekfinal$lowwork )

# distribution lowwork, phase short
table( dagboekfinal2$fase2, dagboekfinal2$lowwork )

# distribution lowwork2, phase long
table( dagboekfinal$fase, dagboekfinal$lowwork2 )

# distriution lowwork2, phase short
table( dagboekfinal2$fase2, dagboekfinal$lowwork2 )

####### Analyses #######

# 4 analyses:
# 1: fase1 variable with lowwork as the response
# 2: fase1 variable with WRFQ sum score as the response
# 3: fase2 variable with lowwork as the response
# 4: fase2 variable with WRFQ sum score as the response
# 5: fase1 variable with lowwork2 as the response
# 6: fase2 variable with lowwork2 as the response
# in each analysis the flexibility variables are included

# analysis 1: phase 1 variable with lowwork as response
set.seed( 69 )
model1a <- clusbootglm( model = lowwork ~ fase, clusterid = ID,
                        data = dagboekfinal, family = 'binomial' )
summary( model1a )

set.seed( 69 )
model1b <- clusbootglm( model = lowwork ~ fase + flextijd + flexplek +
                        fase * flextijd + fase * flexplek, clusterid = ID,
                        data = dagboekfinal, family = 'binomial' )
summary( model1b )


# analysis 2: phase 1 variable with WRFQ sum score as response
dropouts <- which( dagboekfinal$WRFQscore == 9999 )
dagboekfinalWRFQ <- dagboekfinal[ -dropouts, ]

set.seed( 666 )
model2a <- clusbootglm( model = WRFQscore ~ fase, clusterid = ID,
                        data = dagboekfinalWRFQ )
summary( model2a )
  
set.seed( 666 )
model2b <- clusbootglm( model = WRFQscore ~ fase + flextijd + flexplek +
                       fase * flextijd + fase * flexplek, clusterid = ID,
                       data = dagboekfinalWRFQ)
summary( model2b )

# analysis 3: phase 2 variable with lowwork as response
set.seed( 007 )
model3a <- clusbootglm( model = lowwork ~ fase2, clusterid = ID,
                        data = dagboekfinal2, family = 'binomial' )
summary( model3a )

set.seed( 007 )
model3b <- clusbootglm( model = lowwork ~ fase2 + flextijd + flexplek + 
                       fase2 * flextijd + fase2 * flexplek, clusterid = ID,
                       data = dagboekfinal2, family = 'binomial' )
summary( model3b )

# analysis 4: phase 2 variable with WRFQ sum score as response
dropouts2 <- which( dagboekfinal2$WRFQscore == 9999 )
dagboekfinalWRFQ2 <- dagboekfinal2[ -dropouts2, ]

set.seed( 1000000 )
model4a <- clusbootglm( model = WRFQscore ~ fase2, clusterid = ID,
                        data = dagboekfinalWRFQ2 )
summary( model4a )

set.seed( 1000000 )
model4b <- clusbootglm( model = WRFQscore ~ fase2 + flextijd + flexplek +
                        fase2 * flextijd + fase2 * flexplek, clusterid = ID,
                        data = dagboekfinalWRFQ2)
summary( model4b )

# analysis 5: phase 1 variable with lowwork2 as response
set.seed( 80085 )
model5a <- clusbootglm( model = lowwork2 ~ fase, clusterid = ID,
             data = dagboekfinal, family = 'binomial' )

set.seed( 80085 )
model5b <- clusbootglm( model = lowwork2 ~ fase + flextijd + flexplek +
                          fase * flextijd + fase * flexplek, clusterid = ID,
                        data = dagboekfinal, family = 'binomial' )

# analysis 6: phase 2 variable with lowwork2 as response
set.seed( 999 )
model6a <- clusbootglm( model = lowwork2 ~ fase2, clusterid = ID,
                        data = dagboekfinal2, family = 'binomial' )

set.seed( 999 )
model6b <- clusbootglm( model = lowwork2 ~ fase2 + flextijd + flexplek +
                          fase2 * flextijd + fase2 * flexplek, clusterid = ID,
                        data = dagboekfinal2, family = 'binomial' )



# Results tables



# first the rownames and columnnames are defined for each analysis
names_row_small_1 <- c ( "Intercept", "Prodromal day 1", "Prodromal day 2", "Day of migraine attack", "Recovery day 1", "Recovery day 2" )
names_row_big_1 <- c ( "Intercept", "Prodromal day 1", "Prodromal day 2", "Day of migraine attack", "Recovery day 1", "Recovery day 2",
                       "Flexibility of worktime",
                       "Flexibility of workplace", 
                       "Prodromal day 1: flexibility of worktime",
                       "Prodromal day 2: flexibility of worktime",
                       "Day of migraine attack: flexibility of worktime",
                       "Recovery day 1: flexibility of worktime",
                       "Recovery day 2: flexibility of worktime",
                       "Prodromal day 1: flexibility of workplace",
                       "Prodromal day 2: flexibility of workplace",
                       "Day of migraine attack: flexibility of workplace",
                       "Recovery day 1: flexibility of workplace",
                       "Recovery day 2: flexibility of workplace" )


names_row_small_2 <- c ( "Intercept", "Prodromal", "Day of migraine attack", "Recovery" )

names_row_big_2 <- c ( "Intercept", "Prodromal", "Day of migraine attack", "Recovery",
                       "Flexibility of worktime",
                       "Flexibility of workplace", 
                       "Prodromal: flexibility of worktime",
                       "Day of migraine attack: flexibility of worktime",
                       "Recovery: flexibility of worktime",
                       "Prodromal: flexibility of workplace",
                       "Day of migraine attack: flexibility of workplace",
                       "Recovery: flexibility of workplace" )

names_col <- c ( "Odds ratio", "Standard error", "CI 2.5%", "CI 97.5%" )

# second, the names are attached to the analysis results with a customized fuction
# function for creating the tables
# the inputs for this function are: 
# - model (the output of the GLM with cluster bootstrapping stored in an object)
# - names (the variable (level) names corresponding to the analysis)
# - wrf ('no' indicates no WRFQ sum score, 'yes' means that the response is WRQF sum score)
# the function returns an appropriate output table
tablize <- function( model, names, wrf = 'no' ) {
  table <- as.matrix ( model$BCa.interval )
  table <- cbind.data.frame ( model$boot.coefs, model$boot.sds, table )
  tab1a <- round ( table, digits = 2 )
  rownames ( table ) <- names
  colnames ( table ) <- names_col
  if ( wrf == 'no' ) {
    table[ , 1 ] <- exp( table[ , 1 ] )
  } else if ( wrf == 'yes' ) {
    colnames( table )[ 1 ] <- 'Estimate'
    }
  return( round( table, 2 ) )
}

# generating the tables
tab1a <- tablize( model1a, names_row_small_1 )
tab1b <- tablize( model1b, names_row_big_1 )
tab2a <- tablize( model2a, names_row_small_1, wrf = 'yes' )
tab2b <- tablize( model2b, names_row_big_1, wrf = 'yes' )
tab3a <- tablize( model3a, names_row_small_2 )
tab3b <- tablize( model3b, names_row_big_2 )
tab4a <- tablize( model4a, names_row_small_2, wrf = 'yes' )
tab4b <- tablize( model4b, names_row_big_2, wrf = 'yes' )
tab5a <- tablize( model5a, names_row_small_1 )
tab5b <- tablize( model5b, names_row_big_1 )
tab6a <- tablize( model6a, names_row_small_2 )
tab6b <- tablize( model6b, names_row_big_2 )

