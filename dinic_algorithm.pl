% -----------------------------------------
% Zapoctovy program
% Neproceduralni programovani
% Petr Siegl LS 2016
% Dinic's Algorithm
%------------------------------------------
% Start with dinic(+Source,+Sink,+Graph,-MaxFlowValue,-FlowGraph)
% You have to enter even the vertices with zero neighbors as v-[] for
% correct functioning.
% Graph is represented as [vertex-[neighbor-capacity,...],...] where
% vertex-neighbor is edge. First the algorithm creates residual graph
% (at the beginning all residual capacities equals capacity of the
% corresponding edges) from input graph, while removing edges with zero
% capacity, because they are useless for our algorithm, and asserts all
% edges in graph e(From,To, Capacity) for later use. The algorithm then
% searches the residual graph for path from the source to the sink.
% During search current minimal residual value on the path is kept
% updated. When path is found, the algorithm adds newly found blocking
% flow to the current residual graph and with that new residual graph it
% stars the path search again. If there is none anymore, it returns
% current residual graph a calculates maximum value as a sum of flow
% values going from the source(there are none going to the source
% because of the way algorithm proceeds...). More info on algorithm's
% sections below in comments.

% e(From,To,Capacity).
% Helps find capacity of the edge From-To
:-dynamic e/3.

% init(+Graph,+FlowGraph)
% assert e(From,To,Capacity) for every edge
% remove edges with capacity 0, they're useless for the algorithm
init(Graph,ResidualGraph):-
	init(Graph,[],ResidualGraph).
init([],G,G).
init([From-ToList|Edges],A,ResidualGraph):-
	initEdges(From,ToList,[],NewList),
	init(Edges,[From-NewList|A],ResidualGraph).

% initEdges(+From,+ToList,+Accumulator,-NewList)
% Remove all edges with zero capacity from ToList, assert the rest and
% put them in the NewList.
initEdges(_,[],A,A).
initEdges(From,[To-Capacity|ToList],A,NewList):-
	( Capacity > 0 ->
	asserta(e(From,To,Capacity)),
	initEdges(From,ToList,[To-Capacity|A],NewList)
	;initEdges(From,ToList,A,NewList)).


% dinic(+Source,+Sink,+Graph,-MaxFlowValue,-FlowGraph)
% Starts the dinic algorithm for the Graph with source Source and sink
% Sink. Returns maximum flow value in MaxFlow and all flow values in the
% FlowGraph(The order of vertices can be different from the input
% graph).
dinic(Source,Sink,Graph,MaxFlow,FlowGraph):-
	init(Graph,CurrentResGraph),
	dinicSearch(Source,Sink,CurrentResGraph,MaxFlow,FlowGraph),!.

% dinicSearch(+Source,+Sink,+CurrentResGraph,-MaxFlow,-FlowGraph)
% While there is a path in the graph from the Source to the Sink (Path
% isn't []) the algorithm finds path(blockingFlow) in the graph and
% adds the flow to the residual graph(BlockedResGraph).
dinicSearch(Source,Sink,CurrentResGraph,MaxFlow,FlowGraph):-
	findPath(Source,Sink,CurrentResGraph,Path),
	(Path=[] ->
	getFlowGraph(CurrentResGraph,[],FlowGraph),
	countMax(Source,FlowGraph,MaxFlow)
	;
	blockFlow(CurrentResGraph,Path,BlockedResGraph),
	dinicSearch(Source,Sink,BlockedResGraph,MaxFlow,FlowGraph)).

%getFlowGraph(+ResidualGraph,-FlowGraph)
% Calculates the flow graph from the residual one.
getFlowGraph([],A,A).
getFlowGraph([V-ToList|ResidualGraph],A,FlowGraph):-
	getFlowEdges(V,ToList,[],NewToList),
	getFlowGraph(ResidualGraph,[V-NewToList|A],FlowGraph).

%getFlowEdges(+EdgesList,+FlowEdgesList)
% Calculates the flow value from the residual one for every edge in the
% list.
getFlowEdges(_,[],A,A).
getFlowEdges(From,[V-Res|ToList],A,NewToList):-
	e(From,V,Capacity),
	Flow is Capacity - Res,
	getFlowEdges(From,ToList,[V-Flow|A],NewToList).

% blockFlow(+CurrentResidualGraph,+Path,-BlockedResGraph)
% Adss the blocking flow, represented by the Path, to the
% CurrentResidualGraph.
blockFlow(CurrentResGraph,[MinRes-V|Path],BlockedResGraph):-
	blockFlowAcu(CurrentResGraph,MinRes,[V|Path],[],BlockedResGraph).

% Just one element in the Path -> finished with blocking flow, add the
% rest of the vertices in the CurrentResGraph.
blockFlowAcu(CurrentResGraph,_,[_|E],A,BlockedResGraph):-
	E=[],
	completeFlowGraph(CurrentResGraph,A,BlockedResGraph).
blockFlowAcu(CurrentResGraph,MinRes,[To,From|Path],A,BlockedResGraph):-
	member(From-ToList,CurrentResGraph),
	addFlow(ToList,To,MinRes,[],NewToList),
	blockFlowAcu(CurrentResGraph,MinRes,[From|Path],[From-NewToList|A],BlockedResGraph).

% completeFlowGraph(+CurrentResidualGraph,+Accumulator,-BlockedResidualGraph).
% Adds the rest of the vertices in the CurrentResidualGraph.
completeFlowGraph([],A,A).
completeFlowGraph([V-ToList|ResidualGraph],A,BlockedResGraph):-
	(member(V-_,A) -> % Is the vertex already in the Blocked one?
	completeFlowGraph(ResidualGraph,A,BlockedResGraph)
	;
	completeFlowGraph(ResidualGraph,[V-ToList|A],BlockedResGraph)).


% addFlow(+ToListOfFromVertex,+ToVertexInEdge,+MinimalResidualOnPath,+Accumulator,-NewToList)
% When adding the blocking flow, there are From and To vertices on the
% path. This term adds all vertices to the new list and for the edge on
% the path calculates new residual value.
addFlow([],_,_,A,A).
addFlow([V-Res|ToList],To,MinRes,A,NewToList):-
	V\=To,
	addFlow(ToList,To,MinRes,[V-Res|A],NewToList).

addFlow([To-Res|ToList],To,MinRes,A,NewToList):-
	NewRes is Res - MinRes,
	addFlow(ToList,To,MinRes,[To-NewRes|A],NewToList).


% countMax(+Sink,+FlowGraph,-MaxFlowValue)
% Returns max flow value. Just counts all the outgoing flows from the
% Source.
countMax(Source,FlowGraph,MaxFlowValue):-
	member(Source-ToList,FlowGraph),
	count(ToList,0,MaxFlowValue).

count([],Sum,Sum).
count([_-Flow|ToList],Sum,MaxFlowValue):-
	NewSum is Sum+Flow,
	count(ToList,NewSum,MaxFlowValue).


% findPath(+Source,+Sink,+Graph,-Path)
% Looks for the path from the Source to the Sink in the Graph.
% In Path is [] if there is none.
% Otherwise Path is of format
% [MinimalResidualCapacityOnPath-Sink,v1,...,Source]
findPath(Source,Sink,Graph,Path):-
	Source\=Sink,
	bfs(Sink,Graph,[],[[9999-Source]|X]-X,Path). % 9999 some high start MinimalResidual.

%bfs(+Sink,+Graph,+Closed,+Open,-Path)
% Finds shortest path to the Sink.
bfs(_,_,_,[X]-X,[]). % Empty Open list, path not found.

bfs(Sink,Graph,Closed,[[MinRes-V|PathTo]|Open]-X,Path):-
	% All neighbors
	vertexNeighbors(Graph,V,AllNeighbors),
	% remove ones with 0 residual capacity
	filterNeighbors(AllNeighbors,[],Neighbors),
	(member(Sink-Res,Neighbors) -> % Is neighbor Sink?
	NewMinRes is min(MinRes,Res),
	Path=[NewMinRes-Sink,V|PathTo] % Found my path.
	;
	removeClosed(Neighbors,Closed,OpenNB), % remove closed vertices
	merge(MinRes,PathTo,V,OpenNB,Open-X,NewOpen), % join with the rest of the open vertices
	bfs(Sink,Graph,[V|Closed],NewOpen,Path)).


% removeClosed(+ListOfVertices,+Closed,-ListOfNonClosedVertices)
% Remove already closed vertices from the list.
removeClosed(Neighbors,Closed,OpenNB):- removeClosed(Neighbors,Closed,[],OpenNB).
removeClosed([],_,OpenNB,OpenNB).
removeClosed([V-Value|Neighbors],Closed,Open,OpenNB):-
	(member(V,Closed) -> NewOpen=Open ; NewOpen=[V-Value|Open]),
	removeClosed(Neighbors,Closed,NewOpen,OpenNB).

% merge(+ListOfVertices,+OldOpenVertices,-AllOpenVertices)
% Join newly opened vertices to the old ones.
merge(_,_,_,[],Old,Old).
merge(MinRes,PathTo,V,[Current-Res|OpenNB],OldOpen,NewOpen):-
	NewMinRes is min(MinRes,Res),
	appendSingle(OldOpen,[NewMinRes-Current,V|PathTo],Open),
	merge(MinRes,PathTo,V,OpenNB,Open,NewOpen).

%filterNeighbors(+Neighbors,-FilteredNeighbors)
% Remove vertices with zero residual capacity from the list.
filterNeighbors([],A,A).
filterNeighbors([V-Res|Neighbors],A,FilteredNeighbors):-
	(Res > 0 ->
	filterNeighbors(Neighbors,[V-Res|A],FilteredNeighbors)
	;
	filterNeighbors(Neighbors,A,FilteredNeighbors)).

% neighbors(+Graph,+Vertex,-NeighborsOfVertex)
% Return all neighbors of the Vertex in Graph.
vertexNeighbors(Graph,V,NB):-
   member(V-NB,Graph).

% appendsingle(+DiffList,+Element,-DiffListWithElementAsLast)
% Append element to the end of the DiffList.
appendSingle(X-[Y|Zt],Y,X-Zt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%TESTS%%%%%%%%%%%%%%%%%
% No path
test1(MaxFlowValue,FlowGraph):-
	Graph = [source-[],sink-[]],
	print(Graph),
	dinic(source,sink,Graph,MaxFlowValue,FlowGraph).
% Streight from the source to the sink.
test2(MaxFlowValue,FlowGraph):-
	Graph = [source-[sink-1],sink-[]],
	print(Graph),
	dinic(source,sink,Graph,MaxFlowValue,FlowGraph).
% One branch
test3(MaxFlowValue,FlowGraph):-
	Graph = [source-[v1-2],v1-[sink-1],sink-[]],
	print(Graph),
	dinic(source,sink,Graph,MaxFlowValue,FlowGraph).
% Two independent branches
test4(MaxFlowValue,FlowGraph):-
	Graph = [source-[v1-2,v2-2],v1-[sink-2],v2-[sink-2],sink-[]],
	print(Graph),
	dinic(source,sink,Graph,MaxFlowValue,FlowGraph).

% Two independent branches, one longer.
test5(MaxFlowValue,FlowGraph):-
	Graph = [source-[v1-2,v3-2],v1-[v2-2],v3-[sink-1],v2-[sink-2],sink-[]],
	print(Graph),
	dinic(source,sink,Graph,MaxFlowValue,FlowGraph).

% Empty branch
test6(MaxFlowValue,FlowGraph):-
	Graph = [source-[v1-3,v2-4],v2-[v3-2],v3-[],v1-[sink-2],sink-[]],
	print(Graph),
	dinic(source,sink,Graph,MaxFlowValue,FlowGraph).

% One cross branches
test7(MaxFlowValue,FlowGraph):-
	Graph = [source-[v1-3,v2-4],v1-[sink-2,v2-4],v2-[sink-5],sink-[]],
	print(Graph),
	dinic(source,sink,Graph,MaxFlowValue,FlowGraph).
% Two Cross branches
test8(MaxFlowValue,FlowGraph):-
	Graph = [source-[v1-3,v2-4],v1-[sink-2,v2-4],v2-[v1-2,sink-5],sink-[]],
	print(Graph),
	dinic(source,sink,Graph,MaxFlowValue,FlowGraph).










