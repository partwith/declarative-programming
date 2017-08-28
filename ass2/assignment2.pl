%File     : assignment2.pl 
%Author   : Jizhe Hou
%Purpose  : Assignment1 project
%Funtion  : replace zip sublist
%Date     :  10/10/2016

%keep the rest of L2's element are same with the L1
%only change the element which want to change 
replace(Old, [Old|Olds], New, [New|Olds]).
replace(Old, [E|Olds], New, [E|News]):-
    replace(Old, Olds, New, News).

%if they have diffent length and there no have a fact deal with this
%so if diffent length will occur fail, else just one by one generate the result
zip([],[],[]).
zip([H|T],[H1|T1],[H-H1|T2]):-
    zip(T,T1,T2).

%if the rest of the L1 is sublist of L2, 
%each list add one same element also be the sublist
sublist( [], _ ).
sublist( [X|XS], [X|XSS] ) :- 
    sublist( XS, XSS ).
%check if the L1 is the sublist of the L2 which without the first element
sublist( [X|XS], [_|XSS] ) :- 
    sublist( [X|XS], XSS ).