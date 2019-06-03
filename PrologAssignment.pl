
/** this is for the menu  */
menu :-
write(' +----------------------------------------------------------------------+ \n'),
nl,
write(' | >> 1. Insert a new consumption                                       | '),
nl,
write(' | >> 2. Get the Latex format invoice                                   | '),
nl,
write(' | >> 3. Check average consumption of a year                            | '),
nl,
write(' | >> 4. Check total payment made in an year                            | '),
nl,
write(' | >> 5. Check over 5 cubicmeter consumption by all clients in a year   | '),
nl,
write(' | >> 6. Check the graph                                                | '),
nl,
write(' +----------------------------------------------------------------------+ \n'),
nl,
read(A),
menu_selection(A).

/** this is for the menu selections simple switch */

menu_selection(A):- (A==1, insert_new_consumption; A==2, latex_format_invoice; A==3, average_consumption_per_year; A==4, check_all_payments_of_year; A==5, check_over5_cubicmeter_consumption; A==6, dot_graph).

/** Inserting the Entries **/
:- dynamic consumption/8.
:- dynamic invoice/4.

/** the consumptio examples (instances of the object) **/
consumption(id-client(1), name('client1'), address('address1'), n-persons('5'), cubicmeters0_5(1), cubicmeters6_15(0), month(1), year(2019)).
consumption(id-client(1), name('client1'), address('address1'), n-persons('5'), cubicmeters0_5(2), cubicmeters6_15(0), month(2), year(2019)).
consumption(id-client(1), name('client1'), address('address1'), n-persons('5'), cubicmeters0_5(1), cubicmeters6_15(0), month(3), year(2019)).

consumption(id-client(2), name('client2'), address('address2'), n-persons('3'), cubicmeters0_5(5), cubicmeters6_15(6), month(1), year(2019)).
consumption(id-client(3), name('client3'), address('address3'), n-persons('7'), cubicmeters0_5(5), cubicmeters6_15(8), month(2), year(2019)).
consumption(id-client(3), name('client3'), address('address3'), n-persons('7'), cubicmeters0_5(4), cubicmeters6_15(0), month(3), year(2019)).

insert_new_consumption:-
write('Id:'),
read(I),
write('Name:'),
read(N),
write('Address:'),
read(Add),
write('Number of Persons:'),
read(NP),
write('cubicmeters0_5:'),
read(C5),
write('cubicmeters6_15:'),
read(C15),
write('month:'),
read(M),
write('year:'),
read(Y), !,
assert(consumption(id-client(I), name(N), address(Add), n-persons(NP), cubicmeters0_5(C5),cubicmeters6_15(C15), month(M),year(Y))),
water_total_bill(C5, C15, NP, B),
assert(invoice(id-client(I),
water_bill(B),
month(M),
year(Y))).


bill_for_cubicmeter0_5(X,Y):- Y is X*1000*0.5.
bill_for_cubicmeter6_15(X,Y):- X>5, !, Y is (X-5)*1000 .
bill_for_cubicmeter6_15(_, Y):- Y is 0 .
bill_for_sanitation(N,Y):- Y is N*1.5 .

water_total_bill(C,C6,N, B):- bill_for_cubicmeter0_5(C,Y), bill_for_cubicmeter6_15(C6,Z), bill_for_sanitation(N,X), B is Y+Z+X.


invoice(id-client(1), water_bill(507.5), month(1), year(2019)).
invoice(id-client(1), water_bill(1003), month(3), year(2019)).


/** calculate the average consumption of a client in an year **/

sumList([],0).
sumList([X|Y], A):- sumList(Y, A1), A is A1+X.

all_consumption_of_0_5(I,Y,X):- findall(P, consumption(id-client(I),_,_,_, cubicmeters0_5(P),_,_, year(Y)), S), sumList(S, X).

all_consumption_of_6_15(I,Y,X):- findall(P, consumption(id-client(I),_,_,_,_, cubicmeters6_15(P),_, year(Y)), S), sumList(S, X).

average_of_both_consumption(I,Y):- all_consumption_of_0_5(I,Y,X), all_consumption_of_6_15(I,Y,Z),A is X+Z, write('Average Consumption of the year '), write(Y), write(' is '), write(A), write(' cubicmeters.').

average_consumption_per_year:- write('Enter Client Id : '),
read(I),
write('Enter the year you want to check average for : '),
read(Y),
average_of_both_consumption(I,Y).



/** total payment of a client in an year **/

all_bills_of_year(I,Y):- findall(T, invoice(id-client(I), water_bill(T), _, year(Y)), S), sumList(S,X), write('the payment made in the year '), write(Y), write(' is: '), write(X).

check_all_payments_of_year:- write('Enter client id : '),
read(I),
write('Enter the year : '),
read(Y),
all_bills_of_year(I,Y).


/** total number of cubicmeters consummated over 5 cubicmeters of all clients in the last year **/


replacelist(X,Y, [X|L1], [Y|L2]):- replacelist(X,Y,L1, L2).
replacelist(X,Y, [Z|L1], [Z|L2]):- replacelist(X,Y,L1, L2).
replacelist(X,Y, [Y|L1], [Y|L2]):- replacelist(X,Y,L1, L2).
replacelist(_,_, [], []).


check_cubicmeter(Y):- findall(P, consumption(_,_,_,_,_, cubicmeters6_15(P), _, year(Y)), S), sumList(S,A), write('total number of cubicmeters consummated over 5 cubicmeters of all clients in the year '), write(Y), write(' is :'), write(A).

check_over5_cubicmeter_consumption:- write('Enter the year to check the consumption : '),
read(Y),
check_cubicmeter(Y).

/** Latex format invoice **/

header_config:- write('%----------------------------------------------------------------------------------------'),
nl,
write('%    PACKAGES AND OTHER DOCUMENT CONFIGURATIONS'),
nl,
write('%----------------------------------------------------------------------------------------'),
nl,
write('documentclass[fleqn,10pt]{SelfArx} % Document font size and equations flushed left'),
nl,
nl,
write('usepackage[english]{babel} % Specify a different language here - english by default'),
nl,
write('usepackage{lipsum} % Required to insert dummy text. To be removed otherwise').

article_info(M):- write('%----------------------------------------------------------------------------------------'),
nl,
write('%    ARTICLE INFORMATION'),
nl,
write('%----------------------------------------------------------------------------------------'),
nl,
write('JournalInfo{Prolog Invoice} % Journal information'),
nl,
write('Archive{Work 2} % Additional notes (e.g. copyright, DOI, review/research article)'),
nl,
write('PaperTitle{Water Bill Invoice} % Article title'),
nl,
write('Authors{'), write('For Month :'), write(M), write('} % Authors').

abstract:- write('%----------------------------------------------------------------------------------------'),
nl,
write('%    ABSTRACT'),
nl,
write('%----------------------------------------------------------------------------------------'),
nl,
write('Abstract{This invoice is generated using the latex format in the prolog language}').

document(I,N,A,M,Y):-

write('begin{document}'),
nl,
write('flushbottom % Makes all text pages the same height'),
nl,
write('maketitle % Print the title and abstract box'),
nl,
write('%----------------------------------------------------------------------------------------'),
nl,
write('%    ARTICLE CONTENT'),
nl,
write('%----------------------------------------------------------------------------------------'),
nl,
write('section{Invoice}'),
nl,
nl,
write('Client - ID -> '), write(I),
nl,
write('Client - Name -> '), write(N),
nl,
write('Client Address -> '), write(A),
nl,
write('Billing Month -> '), write(M),
nl,
write('Water Bill -> '), invoice(id-client(I), water_bill(X), month(M), year(Y)), write(X),
nl,
nl,
write('end{document}').

latex_format_invoice:-
write('Enter the client id : '),
read(I),
write('Enter the month : '),
read(M),
write('Enter the year : '),
read(Y),
consumption(id-client(I), name(N), address(A), _, _, _, _, _),
header_config,
article_info(M),
abstract,
document(I,N,A,M,Y).


/** Make graph **/

get_months_and_bills_object(C, Y):- findall(P-A, invoice(id-client(C), water_bill(A), month(P), year(Y)), S), write_graph_obj(S).

write_graph_obj([]):- write('"end"').
write_graph_obj([X|Y]):-
write('"'),
write(X),
write('"'),
write(' -> '),
write_graph_obj(Y).

dot_graph:-
write('Enter client id for the graph : '),
read(C),
write('Enter the year for the graph : '),
read(Y),
nl,
write('digraph G {'),
nl,
write('rankdir = LR'),
nl,
write('"Start"'),
write(' -> '),
get_months_and_bills_object(C, Y).


/** for testing purpose **/

see_invoice:- findall(T, invoice(_, water_bill(T), _, _), S), write(S).
