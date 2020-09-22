function dummy = p_test(t)
global PQ
T1=25;
T2=50;
a=0.6;b=1.3;BUS=5:15;
initial=[         0         0
         0         0
         2.3380    0.8400
         5.2200    1.7600
         0         0
         0         0
         0         0
         0.0850    0.8800
         0         0
         0         0
         3.2000    1.5300];
if (t>T1 & t<T2)
PQ.con(BUS,[4 5]) =((b-a)*rand+a)* initial;
else
PQ.con(BUS,[4 5]) = initial;

end


