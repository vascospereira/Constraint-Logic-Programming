% Constraint Logic Programming
% 1 - Declare variables and domains
% 2 - Put some restrictions to the problem
% 3 - Search solutions
% To use constraints -> :- use_module(library(clpfd)).

% PRIME NUMBER
prime(Num):-
	findall(I, (Num mod I #= 0, I in 1..Num, indomain(I)),Ns),
	length(Ns,2). 

puzzle(1,[D,O,N,A,L,D],[G,E,R,A,L,D],[R,O,B,E,R,T]).
puzzle(2,[0,C,R,O,S,S],[0,0,R,O,A,D],[D,A,N,G,E,R]).
puzzle(3,[0,S,E,N,D],[0,M,O,R,E],[M,O,N,E,Y]).

puzzle([S,E,N,D,M,O,R,Y]) :-
	domain([S,E,N,D,M,O,R,Y], 0, 9), % passo 1
	S #> 0, M #> 0,
	all_different([S,E,N,D,M,O,R,Y]), % passo 2
	sum(S,E,N,D,M,O,R,Y),
	labeling([], [S,E,N,D,M,O,R,Y]). % passo 3
sum(S, E, N, D, M, O, R, Y) :-
	1000 * S + 100 * E + 10 * N + D + 1000 * M + 100 * O + 10 * R + E
	#= 10000 * M + 1000 * O + 100 * N + 10 * E + Y.

% PROBLEM 14 golf clubs:
% 3 woods, 4 hybrids 5 irons 1 wedge 1 putter
% first and last clubs are equal
% second and penultimate clubs are equal
% fifth is a wedge
% every group of three clubs are different
% occurs only once the sequence wood(1)-hybrid(2)-iron(3)-wedge(4)-putter(5)

triDiffClubs([C1,C2,C3|Cs]):-
	all_distinct([C1,C2,C3]),
	triDiff([C2,C3|Cs]).
triDiff([_,_]).

oneSeqClubs([C1,C2,C3,C4,C5|Cs],[B|Bs]):-
	C1 #= 1 #/\ C2 #= 2 #/\ C3 #= 3 #/\ C4 #= 4 #/\ C5 #= 5 #<=> B,
	oneSeqClubs([C2,C3,C4,C5|Cs],Bs).
oneSeqClubs([_,_,_,_],[]).

seqOf14(Clubs):-
	length(Clubs,14),
	global_cardinality(Clubs,[1-3,2-4,3-5,4-1,5-1]),
	element(1,Clubs,SameClub),
	element(14,Clubs,SameClub),
	element(2,Clubs,SecondClub),
	element(13,Clubs,BeforeLastClub),
	element(5,Clubs,4),
	triDiffClubs(Clubs),
	oneSeqClubs(Clubs,Bs),
	count(1,Bs,#=,1),
	labeling([],Clubs).
	
%GOLF tournament(Name,Distance,Prize)
tournament('Open Championship',2120,1608404).
tournament('Masters Tournament',6350,1838116).
tournament('WGC-Mexico Championship',8636,1563530).
tournament('US PGA Championship',6150,1597660).
tournament('US Open',6281,1934271).
tournament('WGC-HSBC Champions',10474,1399804).
tournament('Turkish Airlines Open',4500,985495).
tournament('DP World Tour Championship',7902,1175051).
tournament('WGC-Dell Technologies Match Play',7780,1539603).
tournament('WGC-Bridgestone Invitational',5938,1409407).
	
% Maximaze first place total prize in some number of days with a max distance
tournaments(NDays, MaxDist, PlayedTournaments, TotalDistance, TotalPrize) :-
	findall(Name-Distance-Prize, tournament(Name,Distance,Prize), TourList),
	length(TourList, NTournaments),
	length(Played,NTournaments),
	global_cardinality(Played,[1-NPlayed,0-_]),
	NPlayed #=< NDays,
	% Total Distance
	findall(Distances,member(_-Distances-_, TourList), DistancesList),
	scalar_product(DistancesList,Played,#=,TotalDistance),
	TotalDistance #=< MaxDist,
	% Total Prize
	findall(Prizes,member(_-_-Prizes,TourList), PrizesList),
	scalar_product(PrizesList,Played, #=,TotalPrize),
	labeling([ffc, bisect, down, maximize(TotalPrize)], Played),
	getPlayedTournaments(Played,TourList,PlayedTournaments).

getPlayedTournaments([1|R],[TournamentName-_-_|RTours1],[TournamentName|RTours2]):-
	getPlayedTournaments(R,RTours1,RTours2).
getPlayedTournaments([0|R],[_|RTours1],RTours2):-
	getPlayedTournaments(R,RTours1,RTours2).
getPlayedTournaments([],[],[]).

%N Queens
nqueens(N,Cols):-
	length(Cols,N),
	domain(Cols,1,N),
	all_distinct(Cols),
	constrain(Cols),
	labeling([],Cols).

constrain([A1|RAs]):- 
	safe(A1,RAs,1), 
	constrain(RAs).
constrain([]).

safe(A1,[A2|RAs],K):-
	no_attack(A1,A2,K),
	K1 is K + 1,
	safe(A1,RAs,K1).
safe(_,[],_).

no_attack(A1,A2,K):-
	A1 #\= A2 + K, A1 #\= A2 - K.
	
%MAGIC SQUARE	
sum_max_nine(A,B,C,D,F,G,H,I,J):-
	A+D+H #= 15, B+F+I #= 15, C+G+J #= 15,
	A+B+C #= 15, D+F+G #= 15, H+I+J #= 15,
	A+F+J #= 15, C+F+H #= 15.
magic_square(Nums):-
	Nums = [A,B,C,D,F,G,H,I,J],
	domain(Nums,1,9),
	all_distinct(Nums),
	sum_max_nine(A,B,C,D,F,G,H,I,J),
	labeling([],Nums).
	
exactly(_,[],0).
exactly(X,[Y|L],N) :-
	X #= Y #<=> B,
	N #= M + B,
	exactly(X,L,M).
l:- exactly(3,[1,3,5,3],2).

% UPS AND DOWNS
ups_downs(Min,Max,N,L):-
	length(L,N),
	domain(L,Min,Max),
	check_up_down(L),
	labeling([],L).
check_up_down([E1,E2,E3|R]):-
	(E1 #< E2 #/\ E2 #> E3) #\/ (E1 #> E2 #/\ E2 #< E3),
	check_up_down([E2,E3|R]).
check_up_down([E1,E2]):- 
	E1 #\= E2.
check_up_down([_]).

%SCHEDULE
schedule(Times):-
	Times = [WakeUp, TakeBus1, StartWork, TakeBus2, TurnTVOn, FallASleep],
	domain(Times,1,24),
	WakeUp #>= 6,
	TakeBus1 #= WakeUp+1,
	StartWork #= TakeBus1+1,
	TakeBus2 #= StartWork+8,
	TurnTVOn #= TakeBus2+1,
	FallASleep #= TurnTVOn+3,
	labeling([],Times).
	
build_lego(Budget,NLegos,ObjectCosts,ObjectLegos,Objects,UsedLegos):-
	length(Objects,3),
	length(ObjectLegos,NObjects),
	domain(Objects,1,NObjects),
	all_distinct(Objects),
	getValue(Objects,ObjectCosts,Costs),
	Costs #=< Budget,
	getValue(Objects,ObjectLegos,UsedLegos),
	UsedLegos #=< NLegos,
	labeling([maximize(UsedLegos)],Objects).
	
getValue([I|Is],ObjectInfo,Value):-
	element(I,ObjectInfo,V),
	Value #= V + Value1,
	getValue(Is,ObjectInfo,Value1).
getValue([],_,0).
