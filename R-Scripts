#install the appropriate package for Structural Equation Model and improt data ----

install.packages("lavaan", type= "source")
library(lavaan)
motiv_data <- read.csv("path/to/file", header=T , sep=",")
colnames(motiv_data)

#Confirmatory factor analysis ----
Motiv_Model_CFA <- 'F1 =~ m1 + m2 + m3 + m4 + m5
                    F2 =~ m11 + m13 + m17 + m19 + m23
                    F3 =~ m4 + m6 + m8
                    F4 =~ m10 + m12
                    F5 =~ m14 + m16 + m18
                    F6 =~ m9 + m15 + m21 + m25 + m27
                    F7 =~ m20 + m22 + m24 + m26 + m28'

Motiv_CFA_Results <- cfa(Motiv_Model_CFA, data = motiv_data)
summary(Motiv_CFA_Results, fit.measures= T)

#Structural Equation Model ----
Motiv_Model_SEM <- 'F1 =~ m1 + m2 + m3 + m4 + m5
                    F2 =~ m11 + m13 + m17 + m19 + m23
                    F3 =~ m4 + m6 + m8
                    F4 =~ m10 + m12
                    F5 =~ m14 + m16 + m18
                    F6 =~ m9 + m15 + m21 + m25 + m27
                    F7 =~ m20 + m22 + m24 + m26 + m28

                    SO1 =~ F1 + F2
                    SO2 =~ F3 + F4 + F5
                    SO3 =~ F6 + F7
                                   
                    SO1 ~ F1 + F2
                    SO2 ~ F3 + F4 + F5
                    SO3 ~ F6 + F7
                    
                    F1 ~~ F6
                    F2 ~~ F7'

Motiv_SEM_Results <- sem(Motiv_Model_SEM, data = motiv_data)
summary(Motiv_SEM_Results, fit.measures= T, standardized=T)
library("psych")
lavaan.diagram(fit = Motiv_SEM_Results, main = "SEM")
