:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
initialState([[['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
              ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
              ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', '']], []]) :-true.

% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).
getBoards([[], _], []).
getBoards([Board, _], Board).

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT,
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.
getBoard([UTTT, _], Upos, Board) :- positions(S),                                                           % in S se va gasi :([nw, n, ne, w, c, e, sw, s, se])
                                    nth0(Index, S, Upos),                                                   % indexul coreclat cu Upos
                                    nth0(Index, UTTT, Board).                                               % lista corespunzatoare MiniTable din UTTT de la pozitia index

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).
getUBoard([[], _], []).
getUBoard([[Head | Tail], _], MyUboardState) :- player_wins(Winner1, Head),                     % gasirea castigatorului pentru MiniBoard-ul curent (daca exista, altfel trece mai jos)
                                                getUBoard([Tail, _], L1),                       % apel recursiv pentru celelalte boarduri
                                                append([Winner1], L1, MyUboardState), !.        % daca nu a esuat => va adauga rezultatul la finalState
 
getUBoard([[Head | Tail], _], MyUboardState) :- findall(X, (member(X, Head), X = ''), Acc),
                                             length(Acc, 0),
                                             getUBoard([Tail, _], L1),                        % apel recursiv pentru iterare
                                             append([r], L1, MyUboardState), !.                 % daca a ajuns aici => nu mai sunt pozitii libere => remiza
 
getUBoard([[_ | Tail], _], MyUboardState) :- getUBoard([Tail, _], L1),                        % apel recursiv pentru iterare
                                             append([''], L1, MyUboardState).                 % daca a ajuns aici => nu a fost niciun castigator
 

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos([Board, _], UPos, Pos, Cell) :- positions(S),                   % in S se va gasi :([nw, n, ne, w, c, e, sw, s, se])
                                        nth0(Index1, S, UPos),            % se determina indexul corespondent UPos
                                        nth0(Index1, Board, MiniBoard),   % se extrage tabla individuala de la pozitia index
                                        nth0(Index2, S, Pos),             % se determina indexul corespunzator Pos
                                        nth0(Index2, MiniBoard, Cell).    % celula de pe pozitia Pos din tabla individuala

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos,
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz
% în care Cell poate fi și r.
getPos(Board, Pos, Cell) :- positions(S),                   % in S se va gasi :([nw, n, ne, w, c, e, sw, s, se])
                            nth0(Index, S, Pos),            % gaseste indexul din lista S, care face match cu Pos
                            nth0(Index, Board, Cell).       % gaseste celula de pe pozitia index din lista Board

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..
% se verifica de ce tip e patratica - adun pentru patraica X
checkSquareX(x, 1).
checkSquareX(0, 0).
checkSquareX('', 0).

% se numara cate patratele de X sunt intr-o lista cu elemente x si 0 - dintr-un MiniBoard
countSquaresX([], 0).
countSquaresX([Head | Tail], Number) :- countSquaresX(Tail, Number1),
                                        checkSquareX(Head, Res),
                                        Number is Number1 + Res.

% se verifica de ce tip e patratica -  adun pentru patraica 0
checkSquare0(0, 1).
checkSquare0(x, 0).
checkSquare0('', 0).

% se numara cate patratele de 0 sunt intr-o lista cu elemente x si 0 - dintr-un MiniBoard
countSquares0([], 0).
countSquares0([Head | Tail], Number) :- countSquares0(Tail, Number1),
                                        checkSquare0(Head, Res),
                                        Number is Number1 + Res.

% se contorizeaza numarul total de X din liste ce contin liste de x si 0 - din tot Boardul
countAllSquaresX([], 0).
countAllSquaresX([Head | Tail], Allx) :- countAllSquaresX(Tail, Result),
                                        countSquaresX(Head, Number),
                                        Allx is Result + Number.

% se contorizeaza numarul total de 0 din liste ce contin liste de x si 0 - din tot Boardul
countAllSquares0([], 0).
countAllSquares0([Head | Tail], All0) :- countAllSquares0(Tail, Result),
                                         countSquares0(Head, Number),
                                         All0 is Result + Number.


getNextPlayer([Board, _], NextPlayer) :- countAllSquares0(Board, All0),
                                         countAllSquaresX(Board, Allx),
                                         All0 < Allx,
                                         NextPlayer is 0, !.
getNextPlayer(_, x).

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile
% din UBoard ale tablelor disponibile pentru următoarea mutare.
selectAvailable([], _, []).
selectAvailable([''|Tail], Pos, Res) :- Pos1 is Pos + 1,
                                        selectAvailable(Tail, Pos1, ToAdd),
                                        positions(L),
                                        nth0(Pos, L, Move),
                                        append([Move], ToAdd, Res), !.

selectAvailable([_|Tail], Pos, Res) :- Pos1 is Pos + 1,
                                       selectAvailable(Tail, Pos1, Res).

% Caz particular pentru State = Initial State
getNextAvailableBoards(State, AllPositions) :-   initialState(State),
                                                 positions(AllPositions), !.

% Daca MiniBoardul de pe pozitia Pos este finalizat => pot sa ma duc unde vreau eu
getNextAvailableBoards([Board, [Pos]], [Pos]) :-  getUBoard([Board, [Pos]], MyUboardState),
                                                  getPos(MyUboardState, Pos, MiniBoard),
                                                  MiniBoard = '' ,!.

% prima lista este MyUboardState, resultatul contine indexii necastigati din tabla                  
getNextAvailableBoards([Board, [Pos]], AllPositions) :- getUBoard([Board, [Pos]], MyUboardState),
                                                        selectAvailable(MyUboardState, 0, AllPositions).

% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s-a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s-au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
checkCell(x, 1).
checkCell(0, 1).
checkCell(_, 0).

% Rezultatul Cells - numarul total de casute goale
checkBoard([], 0).
checkBoard([Head | Tail], Cells) :- checkBoard(Tail, Cells1),
                                    checkCell(Head, Res),
                                    Cells is Cells1 + Res.

% Catigator X sau 0 pe MiniBoard.
getBoardResult(MiniBoard, Winner) :- player_wins(Winner, MiniBoard) ,!.
% Remiza deoarece toate casutele sunt completate.
getBoardResult(MiniBoard, r) :- checkBoard(MiniBoard, Cells),
                                     Cells = 9 ,!.
% Meci in desfasurare  
getBoardResult(_, '').

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.
buildState(Boards, PreviousPos, [Boards, [PreviousPos]]).

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.
checkWinner('').
checkWinner(_) :- false.
validMove(State, Move) :- getUBoard(State, B),
                          getBoardResult(B, R),
                          checkWinner(R),
                          getNextAvailableBoards(State, NextBoards),
                          length(NextBoards, 1),
                          nth0(0, NextBoards, MiniBoard),
                          getPos(State, MiniBoard, Move, ''),!.
validMove(State, (UPos, Pos)) :- getUBoard(State, B),
                                 getBoardResult(B, R),
                                 checkWinner(R),
                                 getNextAvailableBoards(State, NextBoards),
                                 member(UPos, NextBoards),
                                 getPos(State, UPos, Pos, '').

% moveOnMiniBoard(+List, +Pos, +Player, -UpdatedBoard)
% Se modifica celula de pe pozitia Pos din List cu Player
% Pos scade pana cand prin recursivitate se ajunge pe celula Pos
% Restul celulelor raman neschimbate
moveOnMiniBoard([], _, _, []).
moveOnMiniBoard([_ | Tail], Pos, Player, UpdatedBoard) :- Pos1 is Pos - 1,
                                                        moveOnMiniBoard(Tail, Pos1, Player, UpdatedBoard1),
                                                        Pos = 0,
                                                        append([Player], UpdatedBoard1, UpdatedBoard), !.
moveOnMiniBoard([Head | Tail], Pos, Player, UpdatedBoard) :- Pos1 is Pos - 1,
                                                        moveOnMiniBoard(Tail, Pos1, Player, UpdatedBoard1),
                                                        append([Head], UpdatedBoard1, UpdatedBoard), !.
                                                             

% moveOnBoard(+Board, (+UPos, +Pos), +Player, -UpdatedBoard)
% Se modifica boardul de pe pozitia UPos
% De unde se apeleaza moveOnMiniBoard pentru schimbarea celulei Pos
% Restul boardurilor ramane neschimbate in NewBoard
moveOnBoard([], _, _, []).
moveOnBoard([Head | Tail], (UPos, Pos), Player, NewBoard) :- UPos1 is UPos - 1,
                                                             moveOnBoard(Tail, (UPos1, Pos), Player, NewBoard1),
                                                             UPos = 0,
                                                             moveOnMiniBoard(Head, Pos, Player, MiniBoard),
                                                             append([MiniBoard], NewBoard1, NewBoard), !.

moveOnBoard([Head | Tail], (UPos, Pos), Player, NewBoard) :- UPos1 is UPos - 1,
                                                        moveOnBoard(Tail, (UPos1, Pos), Player, NewBoard1),
                                                        append([Head], NewBoard1, NewBoard).                                                     


% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.
makeMove(S, Pos, NewState) :-   validMove(S, Pos),                      % Se verifica validarea miscarii
                                getBoards(S, Board),                    % Se extrage Boardul
                                getNextAvailableBoards(S, NextBoards),  % Se extrag pozitiile libere
                                getNextPlayer(S, Player),               % Se extrage next player
                                length(NextBoards, 1),              
                                positions(L),
                                nth0(0, NextBoards, PrevPos),                               % Pozitia anteriorara de forma:[u, w, e ...]
                                nth0(UPosInd, L, PrevPos),                                  % Indicele pozitiei anterioare
                                nth0(PosInd, L, Pos),                                       % Indicele pozitiei curente
                                moveOnBoard(Board, (UPosInd, PosInd), Player, NewBoard),    % Se face modifica MiniBoardul de la pozitia UPosInd si celula de la pozitia PosInd
                                buildState(NewBoard, Pos, NewState), !.                     % Se contruieste starea

makeMove(S, (UPos, Pos), NewState) :- validMove(S, (UPos, Pos)),                 
                                    getBoards(S, Board),
                                    positions(L),
                                    nth0(UPosInd, L, UPos),
                                    nth0(PosInd, L, Pos),
                                    getNextPlayer(S, Player),
                                    moveOnBoard(Board, (UPosInd, PosInd), Player, NewBoard),
                                    buildState(NewBoard, Pos, NewState), !.

% ======== Etapa 2

% movePriority/4
% movePriority(+Player, +Board, +Move, -Priority)
% Calculează prioritatea mutării Move pentru jucătorul Player, într-o
% tablă individuală Board. Vezi enunț.


priorityZero(Player, Board, Pos, 0):-   moveOnMiniBoard(Board, Pos, Player, UpdatedBoard),   % simulez mutarea
                                        player_wins(Winner, UpdatedBoard),                   % gasesc castigatorul
                                        Winner = Player.                                     % verific daca jucatorul meu a castigat jocul => prioritate 0
                                     
priorityOne(Player, Board, Pos, 1) :-   nextPlayer(NextPlayer, Player),                          % mut cu adversarul
                                        moveOnMiniBoard(Board, Pos, NextPlayer, UpdatedBoard),   % simulez mutarea
                                        player_wins(Winner, UpdatedBoard),                       % gasesc castigatorul
                                        Winner = NextPlayer.                                     % miscarea mea curenta l-ar face pe adeversar sa castige => prioritate 1

priorityTwo(_, Board, Pos, 2) :- empty_board(Board), (Pos = 0; Pos = 2; Pos = 6 ; Pos = 8).      % tabla este goala, pozitia este pe colt

priorityThree(Player, Board, Pos, 3) :- nextPlayer(Player, NextPlayer),
                                        findall(X, (member(X, Board), X = Player), Acc),                                         % verific daca jucatorul meu a mutat in tabla
                                        getPos(Board, c, Cell),                                                                  % extrag celula Cell din centru
                                        length(Acc, Len),                                                                      
                                        (((Len = 0, Cell = NextPlayer), (Pos = 0; Pos = 2; Pos = 6 ; Pos = 8)) ;                 % daca jucatorul meu nu a mutat pe tabla SI adversarul e in mijloc SI pozitia curenta e pe colturi => prioritate 3
                                        (Len = 0, Cell \= NextPlayer, Pos = 4)).                                                 % SAU jucatorul curent nu a mutat in tabla, 


priorityFour(Player, Board, Pos, 4) :- moveOnMiniBoard(Board, Pos, Player, UpdatedBoard),                            % simulez mutarea
                                       positions(List), 
                                       findall(X, (member(X, List), getPos(UpdatedBoard, X, '')), Acc),              % gasesc toate pozitile disponibile de pe tabla
                                       findall(X, (member(X, Acc), nth0(MyPos, List, X), moveOnMiniBoard(UpdatedBoard, MyPos, Player, FinalBoard), player_wins(Player, FinalBoard)), WinPositions),
                                       length(WinPositions, Len),                                                    % verific daca as muta pe pozitiile disponibile, oare as castiga?
                                       Len \= 0.                                                                     % nu am castigat pentru nicio pozitie disponibila

priorityOthers(_, _, Pos, 5) :- (Pos = 0; Pos = 2; Pos = 6 ; Pos = 8),!.
priorityOthers(_, _, _, 6).

movePriority(Player, Board, Move, Priority):-  positions(L), nth0(Pos, L, Move),                             % extrag pozitiile si asociez mutarii pozitia corespunzatoare
                                               (priorityZero(Player, Board, Pos, Priority) ;
                                               priorityOne(Player, Board, Pos, Priority) ;
                                               priorityTwo(Player, Board, Pos, Priority)  ;
                                               priorityThree(Player, Board, Pos, Priority)  ;
                                               priorityFour(Player, Board, Pos, Priority)  ;
                                               priorityOthers(Player, Board, Pos, Priority)).

% bestIndividualMoves/3
% bestIndividualMoves(+P, +Board, -Moves)
% Leagă Moves la o listă cu toate mutările disponibile, în ordinea
% priorității lor.
%
% Hint: construiți o listă de perechi (prioritate, mutare) și folosiți
% sortMoves/2 pentru a obține lista de mutări, în ordinea priorității.
bestIndividualMoves(Player, Board, Moves) :- positions(ListPositions),
                                             findall(X, (member(X, ListPositions), getPos(Board, X, '')), Acc),                                      % Acc = lista cu toate pozitiile disponibile din Board
                                             findall((Priority, Move), (member(Move, Acc), movePriority(Player, Board, Move, Priority)), AllMoves),  % allMoves = lista de perechi (Move's Priority, Move)
                                             sortMoves(AllMoves, Moves).            % se sorteaza mutarile
% narrowGreedy/2
% narrowGreedy(+State, -Move)
% Strategie care întotdeauna ia cea mai bună mutare individuală.
% Dacă sunt mai multe table disponibile, ia tabla care este cea mai bună
% mutare individuală în raport cu U-board.


% narrowGreedyHelper(+State, -Board, -Player, Positions)
% Player = jucatorul curent
% Positions = Pozitiile MiniBoardurilor disponibile
narrowGreedyHelper(State, Board, Player, Positions) :- getBoards(State, Board),                      % extrag Boardul jocului de UTTT
                                                    getNextAvailableBoards(State, Positions),        % pozitiile disponibile din tabla
                                                    getNextPlayer(State, Player).             % caut urmatorul jucator


narrowGreedy(State, BestMove) :- narrowGreedyHelper(State, _, Player, Positions),
                                length(Positions, Len),                                % cate Miniboarduri disponibile am
                                Len = 1,                                               % un singur MiniBoard disponibil
                                nth0(0, Positions, Index),                             % Indexul unde se va realiza miscarea
                                getBoard(State, Index, MiniBoard),                     % MiniBoardul unde se va juca runda
                                bestIndividualMoves(Player, MiniBoard, Moves),         % Moves = lista ce contine in ordine mutarile disponibile din MiniBoard
                                nth0(0, Moves, BestMove) ,!.                           % BestMove = cea mai buna mutare

narrowGreedy(State, (UPos, Pos)) :- narrowGreedyHelper(State, _, Player, Positions),                  % Positions = lista cu toate MiniBoardurile DISPONIBILE SI ACCEPTATE (n, s, e, w..)
                                    getUBoard(State, MyUboardState),                                      % Transform tabla de UTTT intr-o "tabla de joc" de forma ('', 0, x, ...)
                                    bestIndividualMoves(Player, MyUboardState, AllMoves),                 % Moves = lista TUTUROR mutarilor in tabla de UTTT ORDONATE in functie de prioritate
                                    findall(X, (member(X, AllMoves), member(X, Positions)), ValidMoves),  % ValidMoves = lista tuturor mutarilor valide (disponibile si acceptate) dar si ordonate in functie de prioritate
                                    nth0(0, ValidMoves, UPos),                                            % UPos = indicele celei mai bune mutari posibile (mutare din tip MiniBoard)
                                    getBoard(State, UPos, MiniBoard),                                     % MiniBoard = aici se va juca jocul
                                    bestIndividualMoves(Player, MiniBoard, MovesCells),                   % MovesCells = mutarile de tip celula
                                    nth0(0, MovesCells, Pos).                                             % Cea mai buna mutare este pe pozitia ce trebuie returnata Pos

% bestMoves/2
% bestMoves(+State, -Moves)
% Leagă Moves la o listă care conține toate mutările disponibile, în
% ordinea priorității lor, după ordonarea prezentată în enunț.





% Len = numarul de posibilitati la mutare distanta de a castigat Playerul curent
priorityHelper2(NewState, Player, _, Pos, Len, NewUPos):- getBoard(NewState, Pos, MiniBoard),
                                                        getBoardResult(MiniBoard, Winner),
                                                        Winner = '',
                                                        positions(AllPositions),
                                                        findall(X, (member(X, AllPositions), getPos(MiniBoard, X, '')), Acc),     % extrag pozitiile libere
                                                        findall(X, (member(X, Acc), nth0(MyPos, AllPositions, X), moveOnMiniBoard(MiniBoard, MyPos, Player, FinalBoard), player_wins(Player, FinalBoard)), WinPositions),
                                                        length(WinPositions, Len1),
                                                        Len is Len1,
                                                        nth0(0, WinPositions, NewUPos). 

% - Len -> the number of opponent'squares          
priorityHelper(NewState, Player, _, Pos, Len) :- getBoard(NewState, Pos, MiniBoard),                                             % se extrage Minitabelul de la pozitia Pos
                                                getBoardResult(MiniBoard, Winner),
                                                Winner = '',                                                  
                                                positions(AllPositions),                                                            % pozitiile din Minitable
                                                nextPlayer(Player, NextPlayer),                                                     % se extrage oponentul
                                                findall(X, (member(X, AllPositions),  getPos(MiniBoard, X, NextPlayer)), Acc),      % verific daca oponentul nu a mutat deloc in acest MiniBoard
                                                length(Acc, Len1),
                                                Len is Len1.

% Prioritate 0
priority2(State, Player, UPos, Pos, 0) :-   makeMove(State, (UPos, Pos), NewState),
                                                getUBoard(NewState, MyUboardState),     % extrag Starea jocului -> unde Board-ul est [0, 1, ' ', ...] in functie de winn-uri
                                                player_wins(Winner, MyUboardState),
                                                Winner = Player,!.

% Prioritate 1, daca oponentul are 0 patratele 
priority2(State, Player, UPos, Pos, 1):- makeMove(State, (UPos, Pos), NewState), 
                                        priorityHelper(NewState, Player, UPos, Pos, Len),
                                        Len = 0,
                                        \+ priorityHelper2(NewState, Player, _, Pos, Len2, _),
                                        Len2 = 0,!.

% Prioritate 2, daca oponentul are 1 patratele 
priority2(State, Player, UPos, Pos, 2):-  makeMove(State, (UPos, Pos), NewState),
                                            priorityHelper(NewState, Player, UPos, Pos, Len),
                                            Len = 1,!.                                           

% Prioritate 3, daca oponentul a mutat de cel putin 2 ori, unde jucatorul are mai multe mutari
priority2(State, Player, UPos, Pos, 3):- makeMove(State, (UPos, Pos), NewState),
                                                priorityHelper(NewState, Player, UPos, Pos, LenOponent),
                                                nextPlayer(Player, NextPlayer),
                                                priorityHelper(NewState, NextPlayer, UPos, Pos, LenPlayer),
                                                (LenOponent >= 2, LenPlayer > LenOponent),
                                                priorityHelper2(NewState, Player, _, Pos, Len, NewUPos),
                                                Len = 0,
                                                priorityHelper2(NewState, NextPlayer, _, Pos, Len2, NewUPos),
                                                Len2 = 0,!.

% Prioritate 5, daca mutarea duce oponentul intr-un MiniBoard in care jucatorul curent este la o mutare de a castiga
priority2(State, Player, UPos, Pos, 5):- makeMove(State, (UPos, Pos), NewState),
                                             priorityHelper2(NewState, Player, UPos, Pos, Len, _),
                                             Len \= 0,!.


% Prioritate 7
priority2(State, Player, UPos, Pos, 7):- makeMove(State, (UPos, Pos), NewState),
                                            nextPlayer(Player, NextPlayer),
                                            priorityHelper2(NewState, NextPlayer, UPos, Pos, Len, NewUPos),                         % NewUPos duce jucatorul curent in urmatoarea mutare
                                            Len \= 0,
                                            getBoard(NewState, NewUPos, MiniBoard),                                                 % extrag MiniBoardul de la NewUPos
                                            getBoardResult(MiniBoard, Winner),
                                            priorityHelper2(NewState, Player, _, NewUPos, Len2, _),
                                            (Winner \= Player; Len2 = 0),!.

% Prioritate 6, mutarea duce oponentul intr-un MiniBoard in care jucatorul curent este la o mutare de a castiga
priority2(State, Player, UPos, Pos, 6):- makeMove(State, (UPos, Pos), NewState),
                                            nextPlayer(Player, NextPlayer),
                                            priorityHelper2(NewState, NextPlayer, UPos, Pos, Len, NewUPos),                         % NewUPos duce jucatorul curent in urmatoarea mutare
                                            Len \= 0,
                                            getBoard(NewState, NewUPos, MiniBoard),                                                 % extrag MiniBoardul de la NewUPos
                                            getBoardResult(MiniBoard, Winner),
                                            priorityHelper2(NewState, Player, _, NewUPos, Len2, _),
                                            (Winner \= ''; Len2 \= 0),!.                                                              
% Prioritate 8
priority2(State, _, UPos, Pos, 8):- makeMove(State, (UPos, Pos), NewState),
                                    getBoard(NewState, Pos, MiniBoard),
                                    getBoardResult(MiniBoard, Winner),
                                    Winner \= '',!.

% Prioritate 9
priority2(State, Player, UPos, Pos, 9) :- makeMove(State, (UPos, Pos), NewState),
                                            getBoard(NewState, Pos, MiniBoard),
                                            nextPlayer(Player, NextPlayer),
                                            positions(AllPositions),
                                            findall(X, (member(X, AllPositions), getPos(MiniBoard, X, '')), Acc),     % extrag pozitiile libere
                                            findall(X, (member(X, Acc), nth0(MyPos, AllPositions, X), moveOnMiniBoard(MiniBoard, MyPos, NextPlayer, FinalBoard), player_wins(NextPlayer, FinalBoard)), WinPositions),
                                            length(WinPositions, LenWin),
                                            LenWin \= 0,
                                            nth0(0, WinPositions, Win),
                                            makeMove(NewState, (Pos, Win), FinalState),
                                            getUBoard(FinalState, BoardState),
                                            getBoardResult(BoardState, FinalResult),
                                            FinalResult = NextPlayer,!.

% Prioritate 4, mutări care nu se încadrează în alte cazuri din această listă
priority2(_, _, _, _, 4).

overallPriority(State, UPos, Pos, Prio) :- getNextPlayer(State, NextPlayer),
                                    priority2(State, NextPlayer, UPos, Pos, Prio).


bestMoves(State, MovesLikeJagger) :- getNextAvailableBoards(State, MiniPos),
                                length(MiniPos, Len),
                                Len = 1,
                                getNextPlayer(State, NextPlayer),
                                nth0(0, MiniPos, Pos),
                                getBoard(State, Pos, MiniBoard),
                                bestIndividualMoves(NextPlayer, MiniBoard, FinalMoves),
                                findall((P, Move), (member(Move, FinalMoves), overallPriority(State, Pos, Move, P)), Acc),
                                sortMoves(Acc, MovesLikeJagger) ,!.


bestMoves(State, MovesLikeJagger) :- getNextPlayer(State, NextPlayer),
                                    getUBoard(State, UBoardState),
                                    bestIndividualMoves(NextPlayer, UBoardState, SortedMiniPos),
                                    findall((UPos, Pos), (member(UPos, SortedMiniPos), getBoard(State, UPos, MiniBoard), bestIndividualMoves(NextPlayer, MiniBoard, Moves), member(Pos, Moves)), FinalMoves),
                                    findall((P, (UPos, Pos)), (member((UPos, Pos), FinalMoves), overallPriority(State, UPos, Pos, P)), Acc),
                                    sortMoves(Acc, MovesLikeJagger).

% greedy/2
% greedy(+State, -Move)
% Strategie care alege cea mai bună mutare, bazat pe rezultatul lui
% bestMoves/2.
greedy(State, Move) :- bestMoves(State, Moves),
                        nth0(0, Moves, Move).
