:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.

% Starea initiala contine o lista cu:
%  - o lista de liste => Board0ul
%  - o lista cu toate mutarile efectuate
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

getBoard([UTTT, _], Upos, Board) :- positions(S),                    % in S se va gasi :([nw, n, ne, w, c, e, sw, s, se])
                                    nth0(Index, S, Upos),            % indexul coreclat cu Upos
                                    nth0(Index, UTTT, Board).        % lista corespunzatoare MiniTable din UTTT de la pozitia index

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).
%getUBoard([], []).
%getUBoard([Head | Tail], MyUboardState) :- getUBoard(Tail, [Winner1 | MyUboardState]),
                                           %player_wins(Winner1, Head) ,!.
%getUBoard([_ | Tail], MyUboardState) :- getUBoard(Tail, ['' | MyUboardState]).

getUBoard([[], _], []).
getUBoard([[Head | Tail], _], MyUboardState) :- getUBoard([Tail, _], L1),                     % apel recursiv pentru iterarea si determinarea castigatorilor
                                                player_wins(Winner1, Head) ,                  % gasirea castigatorului pentru MiniBoard-ul curent
                                                append([Winner1], L1, MyUboardState),         % daca nu a esuat => va adauga rezultatul la finalState
                                                !.

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

getNextAvailableBoards(State, AllPositions) :-   initialState(State),
                                                 positions(AllPositions), !.
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
%buildState(Boards, _, State):- getBoards(Boards, State).
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
                                                

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).


% iteratePositions(+MiniBoard, +AllPositions, -Pos)
% Param1 -MiniBord
% Param2- Positions e de tipul [nw, n, ne, ...]
% Param3 - Prima pozitie libera gasita

iteratePositions(MiniBoard, Pos, Move) :- member(Move, Pos), getPos(MiniBoard, Move, '').

% Am ajuns la o pozitie libera
% iteratePositions(MiniBoard, [Head | _], Head) :- getPos(MiniBoard, Head, ''), !.
% Iterez pana la pozitia libera
% iteratePositions(MiniBoard, [_ | Tail], Pos) :- iteratePositions(MiniBoard, Tail, Pos).

dummy_first(State, Pos) :-  positions(S),
                            getNextAvailableBoards(State, AllLocations),      % Se extrag boardurile disponibile din UTTT
                            length(AllLocations, 1),                          % Daca este disponibila o singura locatie => Se va returna Pos                
                            nth0(0, AllLocations, Location),                  % Se extrage prima locatie libera (locatia este de tipul: n / s /e ..)                
                            getBoard(State, Location, MiniBoard),             % Se extrage MiniBoardul de la locatie
                            iteratePositions(MiniBoard, S, Pos),!.            % Se determina prima pozitie libera din MiniBoard                   

% Se ajunge aici in cazul in care:
% Lungimea listei AllLocations nu este 1
% => este necesara si trimiterea parametrului UPos pe langa Pos
dummy_first(State, (Location, Pos)) :- positions(S),
                                    % getBoards(State, Boards),
                                    getNextAvailableBoards(State, AllLocations),                    
                                    nth0(0, AllLocations, Location),                            
                                    getBoard(State, Location, MiniBoard),
                                    iteratePositions(MiniBoard, S, Pos),!.                          
                                                     

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).
dummy_last(State, Pos) :- positions(S),
                        getBoards(State, Boards),                      % Se extrage Boardul jocului
                        getNextAvailableBoards(State, AllLocations),   % Se extrag boardurile disponibile din UTTT
                        reverse(AllLocations, AllReversed),            % Se inverseaza locatiile pentru optiunea dummy_last
                        length(AllReversed, 1),                        % Daca este disponibila o singura locatie => Se va returna Pos          
                        nth0(0, AllReversed, Location),                % Se extrage ultima locatie libera (locatia este de tipul: n / s /e ..)                 
                        nth0(UPos, S, Location),                       % Se determina indexul Boardului corespunzator carecterului locatiei
                        nth0(UPos, Boards, MiniBoard),                 % Se extrage lista specifica indexului din UTTT (MiniBoard)
                        reverse(S, SReversed),                         % Se inverseaza lista MiniBoardului
                        iteratePositions(MiniBoard, SReversed, Pos),!. % Se determina ultima pozitie libera din MiniBoard


% Se ajunge aici in cazul in care:
% Lungimea listei AllLocations nu este 1
% => este necesara si trimiterea parametrului UPos pe langa Pos
dummy_last(State, (Location, Pos)) :-  positions(S),
                                       getBoards(State, Boards),
                                       getNextAvailableBoards(State, AllLocations),
                                       reverse(AllLocations, AllReversed),
                                       nth0(0, AllReversed, Location),                                 
                                       nth0(IndexMini, S, Location),
                                       nth0(IndexMini, Boards, MiniBoard),
                                       reverse(S, SReversed),                              
                                       iteratePositions(MiniBoard, SReversed, Pos),!.
