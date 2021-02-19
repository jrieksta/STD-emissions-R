#######################################################
# formulas for Standartized emission calculation for 
#MTs, HTs, SQTs and Isoprene##
#Jolanta Rieksta 
#adapted from Tao Li
#19022021
######################################################

############# Isoprene normalised to Temp and Par ##################

#1.5*(60/20) 
              # 1.5 is my case was 1.5 (inflow / outflow), change to what was the ratio between inflow and outflow rate
              #60/20 i measured vocs 20 min, you change to what you had

#isoprene/predictedleafarea = isoprene in nanograms / the leaf area or dryweight of the plants 
#Par.average = Average photosynthetically active radiation
#Temp.bag = temperature in the branch enclosure /bag


iso.ER <- iso.ng %>% 
  mutate(Isoprene.ER = 1.5*(60/20)*isoprene/predictedleafarea/1000) %>% 
  mutate(Cl = (0.0027* 1.066*Par.average)/
           (sqrt(1+0.0027^2*Par.average^2)),
         Ct = exp((95000*((Temp.bag+273.15)-303.15))/(8.31*303.15*(Temp.bag+273.15)))/
           (1+exp((230000*((Temp.bag+273.15)-314))/(8.31*303.15*(Temp.bag+273.15))))) %>% 
  mutate(Isoprene.nER = Isoprene.ER/(Cl*Ct)) #nER means Temp and Par normalized ER


######### MTs , HTs, SQTS normalized to temperature ###################

#ER_MT_HT = calculated emission rate for MTs and HTs
#ER_SQT = calculated emission rate for SQTs
#Temp.bag = temperature in the branch enclosure /bag
#Beta value for MT and HTs = 0.09
#Beta value for SQTs = 0.18

ER_MT_HT=ER_mt_ht/exp(0.09*(ER_mt_ht$Temp.bag-30)) #mt ht
ER_SQT=ER_sqt/exp(0.18*(ER_sqt$Temp.bag-30)) #sqt

