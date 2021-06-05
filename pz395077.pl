%   Piotr Żuber 395077

:- ensure_loaded(library(lists)).

%%  Data structures

%%%%
%   newArray(+N, -Result)
%   N - array size
%   Result - created array
%%%%
newArray(0, []).
newArray(N, [H | T]) :-
    N > 0,
    N1 is N - 1,
    H = 0,
    newArray(N1, T).

%%%%
%   setArrayElem(+InArr, +Index, +NewVal, -Result)
%   InArr - the array to be modified
%   Index - index of array element to be changed
%   NewVal - value to be set in InArr[Index]
%   Result - changed array
%%%%
setArrayElem(InArr, Index, NewVal, Result) :-
    nth0(Index, InArr, _, Rest),
    nth0(Index, Result, NewVal, Rest).

%%%%
%   newMap(+Keys, +InitVal, -Result)
%   Keys - set of map keys
%   InitVal - initial value for each entry
%   Result - created map
%%%%
newMap([], _, []).
newMap([Key | Keys], InitVal, [H | T]) :-
    H = entry(Key, InitVal),
    newMap(Keys, InitVal, T).

%%%%
%   setMapElem(+InMap, +Key, +NewVal, -Result)
%   InMap - the map to be modified
%   Key - key of map element to be changed
%   NewVal - value to be set in InMap[Key]
%   Result - changed map
%%%%
setMapElem([entry(Key, _) | T], Key, NewVal, Result) :-
    Result = [entry(Key, NewVal) | T].
setMapElem([H | T], Key, NewVal, [RH | RT]) :-
    RH = H,
    setMapElem(T, Key, NewVal, RT).

%%  State

%%%%
%   ArrEnv - map ArrayName -> ArrayValues
%     ArrName - name of array
%     ArrValues - list of size N containing values of ArrName
%   VarEnv - map VarId -> VarVal with VarVals initialized to 0
%     VarId - name of variable
%     VarVal - value assigned to the variable with name VarId (initially 0)
%   InsEnv - the list of size N (the number of processes), storing the number 
%     of last executed instruction for each process
%   ProcessInterlacing - initially empty list used to store the process 
%     interlacing in the form of (pid, instruction) pairs
%   Prog - code of program to be executed
%%%%
%%%%
%   initState(+Program, +N, -InitState)
%   Program - program(Vars, Arrs, Prog):
%     - Vars - list of variable names;
%     - Arrs - list of array names;
%     - Prog - list of program Prolog term instructions
%   N - number of running processes
%   InitState - obtained initial system state
%%%%
initState(program(Vars, Arrs, _), N, 
        sysState(ArrEnv, VarEnv, InsEnv)) :-
    initInsEnv(N, InsEnv),
    initVarEnv(Vars, VarEnv),
    initArrEnv(Arrs, N, ArrEnv).

%%  Initialization

%%%%
%   initInsEnv(+N, -InsEnv)
%   N - number of processes
%   InsEnv - the environment of instructions
%%%%
initInsEnv(N, InsEnv) :-
    newArray(N, InsEnv).

%%%%
%   initVarEnv(+Vars, -VarEnv)
%   Vars - list of variable names
%   VarEnv - the environment of variables
%%%%
initVarEnv(Vars, VarEnv) :-
    newMap(Vars, 0, VarEnv).

%%%%
%   initArrEnv(+Arrs, +N, -ArrEnv)
%   Arrs - list of array names
%   N - number of processes
%   ArrEnv - the environment of arrays
%%%%
initArrEnv(Arrs, N, ArrEnv) :-
    newArray(N, ZeroArr),
    newMap(Arrs, ZeroArr, ArrEnv).

%%  State operations

%%%%
%   getArrVarValue(+ArrId, +Index, +SysState, -Result)
%   ArrId - name of desired array
%   Index - index of desired array element
%   SysState - current system state
%   Result - ArrId[Index] value
%%%%
getArrVarValue(ArrId, Index, sysState(ArrEnv, _, _), Result) :-
    member(entry(ArrId, Arr), ArrEnv),
    nth0(Index, Arr, Result).

%%%%
%   setArrVarValue(+ArrId, +Index, +NewValue, +SysState, -NewState)
%   ArrId - name of desired array
%   Index - index of desired array element
%   NewValue - value to be assigned to ArrId[Index]
%   SysState - current system state
%   NewState - system state after assignment
%%%%
setArrVarValue(ArrId, Index, NewValue, 
        sysState(ArrEnv, VEnv, IEnv),
        sysState(NewArrEnv, VEnv, IEnv)) :-
    member(entry(ArrId, Arr), ArrEnv),
    setArrayElem(Arr, Index, NewValue, RetArr),
    setMapElem(ArrEnv, ArrId, RetArr, NewArrEnv).

%%%%
%   getVarValue(+VarId, +SysState, -VarVal)
%   VarId - name of desired variable
%   SysState - current system state
%   VarVal - value of VarId variable
%%%%
getVarValue(VarId, sysState(_, VarEnv, _), VarVal) :-
    member(entry(VarId, VarVal), VarEnv).

%%%%
%   setVarValue(+VarId, +NewValue, +SysState, -NewSysState)
%   VarId - name of desired variable
%   NewValue - value to be assigned to VarId
%   SysState - current system state
%   NewSysState - system state after assignment
%%%%
setVarValue(VarId, NewValue, 
        sysState(AEnv, VarEnv, IEnv),
        sysState(AEnv, NewVarEnv, IEnv)) :-
    setMapElem(VarEnv, VarId, NewValue, NewVarEnv).

%%%%
%   getProcInsPtr(+Pid, +SysState, -InsPtr)
%   Pid - process id
%   SysState - current system state
%   InsPtr - instruction pointer of pid process
%%%%
getProcInsPtr(Pid, sysState(_, _, InsEnv), InsPtr) :-
    nth0(Pid, InsEnv, InsPtr).

%%%%
%   setProcInsPtr(+Pid, +SysState, +NewInsPtr, -NewSysState)
%   Pid - process id
%   NewInsPtr - instruction pointer to be set
%   SysState - current system state
%   NewSysState - system state after InsPtr change
%%%%
setProcInsPtr(Pid, NewInsPtr, 
        sysState(AEnv, VEnv, InsEnv),
        sysState(AEnv, VEnv, NewInsEnv)) :-
    setArrayElem(InsEnv, Pid, NewInsPtr, NewInsEnv).

%%  Evaluating expressions

%%%%
%   arithmeticExpVal(+AritExp, +Pid, +SysState, -ExpValue)
%   AritExp - expression to be evaluated
%   Pid - process evaluating expression
%   SysState - current system state
%   ExpValue - the value of expression
%%%%

%%%%
%   booleanExpVal(+BoolExp, +Pid, +SysState)
%   BoolExp - expression to be evaluated
%   Pid - process evaluating expression
%   SysState - current system state
%   It's true iff. BoolExp evaluates to true.
%%%%

%   Simple expressions

arithmeticExpVal(Number, _, _, Number) :-
    integer(Number).

arithmeticExpVal(pid, Pid, _, ExpValue) :-
    Pid = ExpValue.

arithmeticExpVal(VarId, _, SysState, VarVal) :-
    atom(VarId),
    getVarValue(VarId, SysState, VarVal).

arithmeticExpVal(array(ArrId, IExp), Pid, SysState, ArrVal) :-
    arithmeticExpVal(IExp, Pid, SysState, Index),
    getArrVarValue(ArrId, Index, SysState, ArrVal).

%   Arithmetic expressions

arithmeticExpVal(AritExp, Pid, SysState, ExpVal) :-
    AritExp =.. [Oper, L, R],
    arithmeticExpVal(L, Pid, SysState, LValue),
    arithmeticExpVal(R, Pid, SysState, RValue),
    IntegerExp =.. [Oper, LValue, RValue],
    ExpVal is IntegerExp.

%   Boolean expressions

booleanExpVal(BoolExp, Pid, SysState) :-
    BoolExp =.. [OperRel, L, R],
    arithmeticExpVal(L, Pid, SysState, LValue),
    arithmeticExpVal(R, Pid, SysState, RValue),
    call(OperRel, LValue, RValue).

%   <> operator definition

:- op(700, 'xfx', '<>').
L <> R :- L \= R.
    
%%  Statements handling

%%%%
%   proceed(+Pid, +SysState, -NewSysState)
%   Pid - process id
%   SysState - current system state
%   NewSysState - system state after proceeding
%%%%
proceed(Pid, SysState, NewSysState) :-
    getProcInsPtr(Pid, SysState, InsPtr),
    NewInsPtr is InsPtr + 1,
    setProcInsPtr(Pid, NewInsPtr, SysState, NewSysState).

%%%%
%   perform(+Instruction, +Pid, +SysState, -NewSysState)
%   Instruction - Instruction to be performed
%   Pid - process pid
%   SysState - current system state
%   NewSysState - system state after executing Instruction
%%%%
perform(assign(VarId, AritExp), Pid, SysState, NewSysState) :-
    arithmeticExpVal(AritExp, Pid, SysState, NewValue),
    setVarValue(VarId, NewValue, SysState, NewVarValSysState),
    proceed(Pid, NewVarValSysState, NewSysState).

perform(assign(array(ArrId, IExp), AritExp), Pid, SysState, NewSysState) :-
    arithmeticExpVal(AritExp, Pid, SysState, NewValue),
    arithmeticExpVal(IExp, Pid, SysState, Index),
    setArrVarValue(ArrId, Index, NewValue, SysState, NewArrValSysState),
    proceed(Pid, NewArrValSysState, NewSysState).

perform(goto(InsPtr), Pid, SysState, NewSysState) :-
    NewInsPtr is InsPtr - 1,
    setProcInsPtr(Pid, NewInsPtr, SysState, NewSysState).

perform(condGoto(BoolExpr, InsPtr), Pid, SysState, NewSysState) :-
    (   booleanExpVal(BoolExpr, Pid, SysState) ->
        perform(goto(InsPtr), Pid, SysState, NewSysState)
    ;   proceed(Pid, SysState, NewSysState)
    ).
 
perform(sekcja, Pid, InsEnv, RetEnv) :-
    proceed(Pid, InsEnv, RetEnv).

%%%%
%   step(+Program, +SysState, +Pid, -NewSysState)
%   Program - list of program instructions
%   SysState - current system state
%   Pid - process that performs a step
%   NewSysState - system state after step execution
%%%%
step(Program, SysState, Pid, NewSysState) :-
    getProcInsPtr(Pid, SysState, InsPtr),
    NextInsPtr is InsPtr + 1,
    nth1(NextInsPtr, Program, Instruction),
    perform(Instruction, Pid, SysState, NewSysState).

%%  Security validation

%%%%
%   procInSection(+InsPtr, +Program)
%   InsPtr - instruction pointer
%   Program - list of program instructions
%%%%
procInSection(InsPtr, Program) :-
    CurrIns is InsPtr + 1,
    nth1(CurrIns, Program, sekcja).

%%%%
%   sectionUsers(+Program, +SysState, -Result)
%   Program - list of program instructions
%   SysState - current system state
%   Result - list of processes in section
%%%%
sectionUsers(Program, sysState(_, _, InsEnv), Result) :-
    sectionUsers(Program, InsEnv, 0, [], Result).

%%%%
%   sectionUsers(+Program, +InsEnv, +Pid, +Users, -Result)
%   Users - current section users
%   InsEnv - list of instruction pointers
%   Pid - id of process
%%%%
sectionUsers(_, [], _, Users, Result) :-
    Users = Result.

sectionUsers(Program, [InsPtr | InsPtrs], Pid, Users, Result) :-
    NextProc is Pid + 1,
    (   procInSection(InsPtr, Program) ->
        sectionUsers(Program, InsPtrs, NextProc, [Pid | Users], Result)
    ;   sectionUsers(Program, InsPtrs, NextProc, Users, Result)    
    ).

%%%%
%   securityViolation(+Program, +SysState, -SectionUsers)
%   Program - list of program instructions
%   SysState - current system state
%   SectionUsers - list of processes in critical section
%   It's true iff current interlacing violates the security 
%   (there are at least 2 processes in critical section)
%%%%
securityViolation(Program, SysState, SectionUsers) :-
    sectionUsers(Program, SysState, SectionUsers),
    length(SectionUsers, SectionOccupation),
    SectionOccupation >= 2.

%%%%
%   violationSearchWrapper(+N, +Program, +SysState, -SearchResult)
%   N - number of processes
%   Program - list of program instructions
%   SysState - current system state
%   SearchResult - queue of calls leading to unsafe system state if possible
%%%%
violationSearchWrapper(N, Program, SysState, Res) :-
    violationSearch(N, Program, [], SysState, _, Res).

%%%%
%   violationSearch(+N, +Program, +CheckedNodes, +SysState, 
%               -NewChecked, -SearchResult)
%   N - number of processes
%   Program - list of program instructions
%   CheckedNodes - currently checked states
%   SysState - current system state
%   NewChecked - new checked states after BFS step
%   SearchResult - id of unsafe system state, security breaking processes,
%     process interlacing, if program is safe, SearchResult is res(ok)
%%%%
violationSearch(N, Program, CheckedNodes, SysState, NewChecked, Res) :-
    (   member(SysState, CheckedNodes) ->
        NewChecked = CheckedNodes,
        Res = res(ok)
    ;   CurrNodes = [SysState | CheckedNodes],
        (   securityViolation(Program, SysState, SectionUsers) ->
            length(CurrNodes, SysStateId),
            Res = res(SysStateId, SectionUsers, [])
        ;   performSteps(0, N, Program, CurrNodes, SysState, NewChecked, Res)    
        )
    ).

%%%%
%   performSteps(+Pid, +N, +Program, +CheckedNodes, +SysState, 
%              -NewChecked, -SearchResult)
%   Pid - id of process executing step
%   N - number of processes
%   Program - list of program instructions
%   CheckedNodes - currently checked states
%   SysState - current system state
%   NewChecked - new checked states after BFS step
%   SearchResult - id of unsafe system state, security breaking processes,
%     process interlacing, if program is safe, SearchResult is res(ok)
%%%%
performSteps(N, N, _, CheckedNodes, _, NewChecked, Res) :-
    CheckedNodes = NewChecked,
    Res = res(ok).

performSteps(Pid, N, Program, CheckedNodes, SysState, NewChecked, Res) :-
    step(Program, SysState, Pid, NewSysState),
    getProcInsPtr(Pid, NewSysState, InsPtr),
    violationSearch(N, Program, CheckedNodes, NewSysState, CurrNodes, AuxRes),
    (   AuxRes = res(SysStateId, SectionUsers, Interlacing) ->
        Res = res(SysStateId, SectionUsers, [step(Pid, InsPtr) | Interlacing])
    ;   NextPid is Pid + 1,
        performSteps(NextPid, N, Program, CurrNodes, SysState, NewChecked, Res)   
    ).

%   Program verification

verify(N, ProgramPath) :-
    (   integer(N), N > 0 ->
        set_prolog_flag(fileerrors, off),
        (   see(ProgramPath) ->
        read(variables(Variables)),
        read(arrays(Arrays)),
        read(program(Program)),
        seen,
        initState(program(Variables, Arrays, Program), N, SysState),
        violationSearchWrapper(N, Program, SysState, SearchResult),
        showResultMsg(SearchResult)
        ;   format('Error: brak pliku o nazwie - ~w~n', [ProgramPath])
        )   
    ;   format('Error: parametr ~w powinien byc liczba > 0~n', [N])
    ).

verify() :-
    current_prolog_flag(argv, [N1, ProgramPath | _]),
    (   atom_number(N1, N) ->
        verify(N, ProgramPath)
    ;   format('Error: parametr ~w powinien byc liczba > 0~n', [N1])
    ).

%%  IO

%%%%
%   showInterlacing(+Interlacing)
%   Interlacing - list of consecutive processes' steps
%%%%
showInterlacing([]).
showInterlacing([step(Pid, InsPtr) | Steps]) :-
    format('   Proces ~d: ~d~n', [Pid, InsPtr]),
    showInterlacing(Steps).

%%%%
%   showSectionUsersTail(+SectionUsers)
%   SectionUsers - list of processes in critical section
%%%%
showSectionUsersTail([]).
showSectionUsersTail([SectionUser | SectionUsers]) :-
    format(', ~d', [SectionUser]),
    showSectionUsersTail(SectionUsers).

%%%%
%   showResultMsg(+SearchResult)
%   SearchResult - id of unsafe system state, security breaking processes,
%     process interlacing, if program is safe, SearchResult is res(ok)
%%%%
showResultMsg(res(ok)) :-
    writeln('Program jest poprawny (bezpieczny).').

showResultMsg(res(SysStateId, SectionUsers, Interlacing)) :-
    format('Program jest niepoprawny: stan nr ~d nie jest bezpieczny.~n', 
            [SysStateId]),
    writeln('Niepoprawny przeplot:'),
    showInterlacing(Interlacing),
    [SectionUser | SectionUsersT] = SectionUsers,
    format('Procesy w sekcji: ~d', [SectionUser]),
    showSectionUsersTail(SectionUsersT),
    writeln('.').
