* Encoding: UTF-8.

compute samplecut=sample.
if sample > 1000 samplecut=1000.
compute samplecut2=sample.
if sample > 10000 samplecut2=10000.
compute samplecut3=sample.
if sample > 500 samplecut3=500.
EXECUTE.

corr  selfrepx online samplecut3.

*** journal 1-4.

compute j1=0-1.
compute j2=0.
compute j3=0.
EXECUTE.
if journal=2 j1=1.
if journal=4 j1=1.
If journal=1 j2=0-1.
If journal=3 j2=1.
if journal =2 j3=0-1.
if journal =4 j3=1.
EXECUTE.


*** year 0-3.

compute y1=0.
compute y2=0.
compute y3=1.
execute.
if jahr=1 y1=0-1.
if jahr=2 y1=1.
If jahr=0 y2=0-1.
If jahr=3 y2=1.
if jahr =2 y3=0-1.
if jahr =1 y3=0-1.
EXECUTE.

compute jy1=j1*y1.
compute jy2=j2*y1.
compute jy3=j3*y1.
EXECUTE.

compute jy4=0.
if journal=1 and jahr=0 jy4=1.
if journal=1 and jahr=3 jy4=0-1.
if journal=3 and jahr=0 jy4=0-1.
if journal=3 and jahr=3 jy4=1.

compute jy5=0.
if journal=1 and jahr=0 jy5=1.
if journal=1 and jahr=1 jy5=0-1.
if journal=1 and jahr=2 jy5=0-1.
if journal=1 and jahr=3 jy5=1.
if journal=3 and jahr=0 jy5=0-1.
if journal=3 and jahr=1 jy5=1.
if journal=3 and jahr=2 jy5=1.
if journal=3 and jahr=3 jy5=0-1.

EXECUTE.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE CI
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT studynum
  /METHOD=ENTER j1 j2 j3 y1 y2 y3  jy1 jy2 jy3 jy4 jy5  .

