:- dynamic article/6.
:- dynamic inproceedings/6.
:- dynamic book/6.


article(title("title1") , author("author1") , journal("journal1") , volume("volume1") , number(1) , pages("44-55") , month("August") , year("1995") , annote("Software Maintenance")).
article(title("title2") , author("author2") , journal("journal2") , volume("volume2") , number(2) , pages("44-55") , month("August") , year("1995") , annote("Software Maintenance")).
article(title("title3") , author("author3") , journal("journal3") , volume("volume3") , number(3) , pages("44-55") , month("August") , year("1995") , annote("Software Maintenance")).
article(title("title4") , author("author4") , journal("journal4") , volume("volume4") , number(4) , pages("44-55") , month("August") , year("1995") , annote("Software Maintenance")).

inproceedings(title("title1") , author("author1") , booktitle("International") , address("adress") , month("june") , year("1995") , annote("Code Analysis") ) .
inproceedings(title("title2") , author("author2") , booktitle("International") , address("adress") , month("june") , year("1995") , annote("Code Analysis") ) .
inproceedings(title("title3") , author("author3") , booktitle("International") , address("adress") , month("june") , year("1995") , annote("Code Analysis") ) .
inproceedings(title("title4") , author("author4") , booktitle("International") , address("adress") , month("june") , year("1995") , annote("Code Analysis") ) .

book(title("title1") , author("author1") , series("Texts and Monographs in C") , year(1995) , publisher("sv") , annote("Compilers") ) .
book(title("title2") , author("author2") , series("Texts and Monographs in C") , year(1995) , publisher("sv") , annote("Compilers") ) .
book(title("title3") , author("author3") , series("Texts and Monographs in C") , year(1995) , publisher("sv") , annote("Compilers") ) .
book(title("title4") , author("author4") , series("Texts and Monographs in C") , year(1995) , publisher("sv") , annote("Compilers") ) .




			  
start :-menu .
menu :-
	write(' ---------------------------------------------------- \n'),
    nl,
    write(' --- >> 1. List bibliography                        --- '),
    nl,
    write(' --- >> 2. Insert article                           --- '),
	nl,
	write(' --- >> 3. Insert inproceedig                       --- '),
	nl,
	write(' --->> 4. Insert book                               --- '),
	nl,
	write(' --- >> 5. Search by year                           --- '),
	nl,
	write(' --- >> 6. Search by author                         --- '),
	nl,
	write(' --- >> 7. Search by title                          --- '),
	nl,
	write(' --- >> 8. Search by Author AND title                --- '),
	nl,
	write(' --- >> 9. Generate graph                           --- '),
	nl,
	write(' --- >> 10. Latex format                            --- '),
    nl,
	write(' --- >> 0. End of session , exit                    --- '),
	nl,
	write(' ----------------------------------------------------- \n'),
	nl,
	read(A),
	menu_selection(A).
	
	menu_selection(A) :-(A==1,not(listal_biblio), nl, menu;
	   A==2,insert_art,nl, menu;   
	   A==3,insert_proc, nl, menu;
	   A==4, insert_book, nl, menu;
	   A==5, write('Type of the refrence : 1- article 2- inproceedings 3- book '), read(D), searchbyyear(D) , nl, menu ; 
	   A==6, write('Type of the refrence : 1- article 2- inproceedings 3- book '), read(D), searchbyauthor(D) , nl, menu ;   
	   A==7, write('Type of the refrence : 1- article 2- inproceedings 3- book '), read(D), searchbytitle(D) , nl, menu;   
	   A==8,write('Type of the refrence : 1- article 2- inproceedings 3- book '), read(D), searchbyAuthTitle(D) ,nl,menu ;  
	   A==9,write('Type of the refrence : 1- article 2- inproceedings 3- book '), read(D) ,dot_graph(D),nl,menu;
	   A==10 ,latexformat(),nl,menu;
	   A==0,gravar, true
	).


% List a biblio  -----------------------------------------------------
listal_biblio :- biblio(art(title(W),author(R),journal(T),volume(Z),number(U),pages(O),month(P),year(F),annote(G))),
    write(' article\n'),
	write(' title: '),write(W),write(' author: '),write(R),write(' journal: '),
	write(T),write(' volume: '),write(Z),write(' number: '),write(U),
	write(' pages: '),write(O),write(' month: '),write(P),write(' year: '),write(F),
	write(' annote: '),write(G),nl,fail.
% List a biblio  -----------------------------------------------------


init:-assert(biblio(art([]),proc([]),book([]))).

% Insert an article   -----------------------------------------------------
	insert_art :-
	write('title:'),
	read(A),
	write('author:'),
	read(B),
	write('journal:'),
	read(C),
	write('volume:'),
	read(D),
	write('number:'),
	read(E),
	write('pages:'),
	read(F),
	write('month:'),
	read(G),
	write('year:'),
	read(H),
	write('annote:'),
	read(I), !,
	assert(biblio(art(title(A),author(B),journal(C),volume(D),number(E),pages(F),month(G),year(H),annote(I)))) ,
	assert(article(title(A) , author(B) , journal(C) , volume(D) , number(E) , pages(F) , month(G) , year(H) , annote(I)) ).
% Insert an article   -----------------------------------------------------

% Insert an inproceedig   ------------------------------------------------------------  
    insert_proc :-
	write('title:'),
	read(A),
	write('author:'),
	read(B),
	write('booktitle:'),
	read(C),
	write('address:'),
	read(D),
	write('month:'),
	read(E),
	write('year:'),
	read(F),
	write('annote:'),
	read(G), !,
	assert(biblio(proc(title(A),author(B),booktitle(C),address(D),month(E),year(F),annote(G)))) ,
	inproceedings(title(A) , author(B) , booktitle(C) , address(D) , month(E) , year(F) , annote(G) ) .
% Insert an inproceedig   ------------------------------------------------------------  

% Insert an inproceedig   -------------------------------------------------------------
	insert_book :-
	write('title:'),
	read(A),
	write('author:'),
	read(B),
	write('Series:'),
	read(C),
	write('year:'),
	read(D),
	write('publisher:'),
	read(E),
	write('annote:'),
	read(F), !,
	assert(biblio(book(title(A),author(B),series(C),year(D),publisher(E),annote(F)))) , 
	book(title(A) , author(B) , series(C) , year(D) , publisher(E) , annote(F) ) .
% Insert an inproceedig   -------------------------------------------------------------


%Search by Year ---------------------------------------------------------
 searchbyyear(D):- (D==1, write('Set the year  '), read(Y), searcharticlebyyear(Y);
	   D==2,write('Set the year  '), read(Y), searchinproceedingsbyyear(Y); 
	   D==3,write('Set the year  '), read(Y),searchbookbyyear(Y)).

 searcharticlebyyear(Y):- findall(P,article(_,_,_,_,_,_,_,year(Y),_), S),write(S) .
 searchinproceedingsbyyear(Y):-findall(P,inproceedings(_,_,_,_,_, year(Y) ,_), S) ,write(S).
 searchbookbyyear(Y):- findall(P,book(_,_,_, year(Y) ,_,_), S) , write(S).
%Search by Year ---------------------------------------------------------

%Search by author ---------------------------------------------------------

 searchbyauthor(D):- (D==1, write('Set the Author name  '), read(N), searcharticlebyauthor(N),
	   D==2,write('Set the author name   '), read(N), searchinproceedingsbyauthor(N),   
	   D==3,write('Set the author name   '), read(N),searchbookbyyearbyauthor(N)).


 searcharticlebyauthor(N):- findall(P,article(_,author(N),_,_,_,_,_,_,_), S) ,write(S) .
 searchinproceedingsbyauthor(N):-findall(P,inproceedings(_,author(N),_,_,_,_,_), S) ,write(S).
 searchbookbyyearbyauthor(N):- findall(P,book(_,author(N),_,_,_,_), S) , write(S).
%Search by author ---------------------------------------------------------


%Search by Title ---------------------------------------------------------
searchbytitle(D):- (D==1, write('Set the Title  '), read(T), searcharticlebytitle(T);
	   D==2,write('Set the title   '), read(T), searchinproceedingsbytitle(T);   
	   D==3,write('Set the title   '), read(T),searchbookbyyearbytitle(T) ).


 searcharticlebytitle(T):- findall(P,article(title(T),_,_,_,_,_,_,_,_), S) ,write(S) .
 searchinproceedingsbytitle(T):- findall(P,inproceedings(title(T),_,_,_,_,_,_), S) , write(S).
 searchbookbyyearbytitle(T):- findall(P,book(title(T),_,_,_,_,_), S) , write(S).

%Search by Title ---------------------------------------------------------

%Search by author an Title ---------------------------------------------------------
searchbyAuthTitle(D)  :- (D==1, write('Set the Title'), read(T3),  write('Set the Author'), read(A3) , searcharticlebyauthtile(A3,T3);
	   D==2,write('Set the title'), read(T2), write('Set the Author'), read(A2), searchinproceedingsbyauthtile(A2,T2);   
	   D==3,write('Set the title'), read(T1), write('Set the Author'), read(A1) ,
	   searchbookbyyearbyauthtile(A1,T1)).

 searcharticlebyauthtile(A,T):- findall(P,article(title(T),author(A),_,_,_,_,_,_,_), S),write(S) .
 searchinproceedingsbyauthtile(A,T):- findall(P,inproceedings(title(T),author(A),_,_,_,_,_), S) ,write(S).
 searchbookbyyearbyauthtile(A,T):- findall(P,book(title(T),author(A),_,_,_,_), S) , write(S).

%Search by author an Title ---------------------------------------------------------





%Make graph ---------------------------------------------------------


dot_graph(D):-(D==1,write('Enter the keyword '),read(C),nl,write('digraph G {'),nl,write('rankdir = LR'),nl,write('"Start"'),write(' -> '),get_referencesArticle(C);
	   D==2,write('Enter the keyword '),read(C),nl,write('digraph G {'),nl,write('rankdir = LR'),nl,write('"Start"'),write(' -> '),get_referencesinproceedings(C);   
	   D==3,write('Enter the keyword '),read(C),nl,write('digraph G {'),nl,write('rankdir = LR'),nl,write('"Start"'),write(' -> '),get_referencesinproceedings(C) ).


get_referencesArticle(C):- findall(P-A,article(_,_,_,_,_,_,_,_,annote(C)), S), write_graph_obj(S).
get_referencesinproceedings(C):- findall(P-A,inproceedings(_,_,_,_,_,_,annote(C) ), S), write_graph_obj(S).
get_referencesinproceedings(C):- findall(P-A,book(_,_,_,_,_,annote(C) ), S) , write_graph_obj(S).

write_graph_obj([]):- write('"end"').
write_graph_obj([X|Y]):-
write('"'),
write(X),
write('"'),
write(' -> '),
write_graph_obj(Y).
%Make graph ----------------------------------------------------------------------------



%latexformat ----------------------------------------------------------------------------

latexformat():- 
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



%latexformat ----------------------------------------------------------------------------