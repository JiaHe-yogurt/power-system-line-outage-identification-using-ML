 %% 39 bus. Dynamic load
clear all
closepsat
initpsat
clpsat.readfile=0
runpsat('d039_dyn_copy.m','/path','data')
runpsat('pf')
runpsat('p_test_39.m','/path','pert')
 
% no outage
initial=PQ.con(:,[4,5]);
x=[];
Settings.tf=200;
num_line=46;
 
% outage 
outage_time=100;
test_time=15 
 
 
for i=1:num_line
for j=1: test_time
     runpsat('pf')
     Breaker.t1=Settings.tf*ones(num_line,1)+rand;
     Breaker.t2=Breaker.t1;
     Breaker.t1(i)=outage_time;
     runpsat('td')
     if (length(Varout.t)>Settings.tf*8)
       a= Varout.vars(:,110:148);
      save(['/path',strcat(num2str(i),'.',num2str(j),'.mat')],'a')
     end
end
end



%%%%118 dynamic load

clear all
initpsat
clpsat.readfile=0
runpsat('bus118.m','/path','data')
runpsat('pf')
runpsat('p_118_aro.m','/path','pert')
Settings.tf=100;      %%%%  remember to change TIME in pert file also

test_time=600 %number of trial

%% outage
outage_time=50
num_line=size(Line.con,1);
initial=PQ.con(:,[4,5]);

i=1:10
for j=16: test_time
PQ.store(:,[4,5]) = (0.8+rand*0.2)*initial;
runpsat('pf')
Breaker.t1=Settings.tf*ones(num_line,1)+rand;
Breaker.t2=Breaker.t1;
Breaker.t1(i)=outage_time;
runpsat('td')
if (length(Varout.t)>Settings.tf*8)
a= Varout.vars(:,370:487);
save(['/path',strcat(num2str(i),'.',num2str(j),'.mat')],'a')
end
end
