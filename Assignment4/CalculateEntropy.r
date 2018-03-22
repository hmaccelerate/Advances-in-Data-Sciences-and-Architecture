#dcompute the entropy of conversion  1.416606
entropyConversion<- -((5/16)*log2(5/16)+(5/16)*log2(5/16)+(6/16)*log2(6/16))
# comopute the root node
#compute the information gian of demand
eDHeavy<- -((3/7)*log2(3/7)+(2/7)*log2(2/7)+(2/7)*log2(2/7))
eDModerate<- -((1/4)*log2(1/4)+(1/4)*log2(1/4)+(2/4)*log2(2/4))
eDlow<- -(0+(2/5)*log2(2/5)+(3/5)*log2(3/5))
entropyDemand<- (7/16)*eDHeavy+(4/16)*eDModerate+(5/16)*eDlow
gainDemand<- entropyConversion-entropyDemand

#compute the information gian of strategic
eSyes<- -((3/9)*log2(3/9)+(3/9)*log2(3/9)+(3/9)*log2(3/9))
eSno<- -((2/7)*log2(2/7)+(2/7)*log2(2/7)+(3/7)*log2(3/7))
entropyStrategic<- (9/16)*eSyes+(7/16)*eSno
gainStrategic<- entropyConversion-entropyStrategic

#compute the information gian of compaign
eCagressive<--((4/9)*log2(4/9)+(4/9)*log2(4/9)+(1/9)*log2(1/9))
eClowkey<- -((1/7)*log2(1/7)+(1/7)*log2(1/7)+(5/7)*log2(5/7))
entropyCompaign<-(9/16)*eCagressive+(7/16)*eClowkey
gainCompaign<-entropyConversion-entropyCompaign

# comopute the root node
#compute the information gian of demand
#when compaign=aggressive
eADHeavy<- -((3/6)*log2(3/6)+(2/6)*log2(2/6)+(1/6)*log2(1/6))
eADModerate<- -((1/2)*log2(1/2)+(1/2)*log2(1/2)+0)
eADlow<- -(0+(1/1)*log2(1/1)+0)
entropyADemand<- (6/9)*eADHeavy+(2/9)*eADModerate+(1/9)*eADlow
gainADemand<- entropyConversion-entropyADemand
#when compaign=lowkey


#compute the information gian of demand
#when compaign=aggressive
eASyes<- -((3/7)*log2(3/7)+(3/7)*log2(3/7)+(1/7)*log2(1/7))
eASno<- -((1/2)*log2(1/2)+(1/2)*log2(1/2)+0)
entropyAStrategic<- (7/9)*eASyes+(2/9)*eASno
gainAStrategic<- entropyConversion-entropyAStrategic

#when compaign=lowkey








