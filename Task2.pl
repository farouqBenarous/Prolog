init:-assert(biblio(art([]),proc([]),book([]))).

listal_biblio :- biblio(art(title(W),author(R),journal(T),volume(Z),number(U),pages(O),month(P),year(F),annote(G))),
    write(' article\n'),
	write(' title: '),write(W),write(' author: '),write(R),write(' journal: '),
	write(T),write(' volume: '),write(Z),write(' number: '),write(U),
	write(' pages: '),write(O),write(' month: '),write(P),write(' year: '),write(F),
	write(' annote: '),write(G),nl,fail.

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
	assert(biblio(art(title(A),
		      author(B),
		      journal(C),
		      volume(D),
		      number(E),
              pages(F),
		      month(G),
		      year(H),
		      annote(I)))).
			  
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
	assert(biblio(proc(title(A),
		      author(B),
		      booktitle(C),
		      address(D),
		      month(E),
              year(F),
		      annote(G)))).
			  
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
	assert(biblio(book(title(A),
		      author(B),
		      series(C),
		      year(D),
		      publisher(E),
		      annote(F)))).
			  
    print_years:-findall(T,biblio(_(name(T),_,_)),S),write(S).
	print_authors:-findall(T,biblio(_(name(T),_,_)),S),write(S).
    print_titles:-findall(T,biblio(_(titles(T),_,_)),S),write(S).

			  
start :-menu.

    menu :-
	write(' #################################################### \n'),
    nl,
    write(' ### >> 1. List bibliography                        ### '),
    nl,
    write(' ### >> 2. Insert article                      ### '),
	nl,
	write(' ### >> 3. Insert inproceedig\t         ### '),
	nl,
	write(' ### >> 4. Insert book\t                        ### '),
	nl,
	write(' ### >> 5. Search by year       ### '),
	nl,
	write(' ### >> 6. Search by author  ### '),
	nl,
	write(' ### >> 7. Search by title      ### '),
	nl,
	write(' ### >> 8. Determinar possíveis acasalamentos\t ### '),
	nl,
	write(' ### >> 9. Gerar grafo\t                         ### '),
	nl,
	write(' ### >> 0. Terminar sessăo                        ### '),
	nl,
	write(' #################################################### \n'),
	nl,
	read(A),
	choice(A).
	
	choice(A) :-
	   (A==1,
        not(listal_biblio),
        nl,
        menu
    ;   A==2,
	    insert_art,
	    nl,
	    menu
	;   A==3,
	    insert_proc,
	    nl,
	    menu
	;   A==4,
	    insert_book,
	    nl,
	    menu
	;   A==5,
	    write('Mother: '),
	    read(B),
	    count_child_mother(B, C),
	    write('N:'),
	    write(C),
	    nl,
	    menu
	;   A==6,
	    or_list,
	    nl,
	    menu
	;   A==7,
	    write('X: '),
	    read(D),
	    write('Y: '),
	    read(E),
	    degree(D, E),
	    nl,
	    menu
	;   A==8,
	    mate,
	    nl,
	    menu
	;   A==9,
	    dot,
	    nl,
	    menu
	;   A==0,
	    gravar,
	    true
	).


