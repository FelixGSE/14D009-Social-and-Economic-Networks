nodes = csvread('flickr-nodes.csv')
edges = csvread('flickr-edges.csv')

EdgeTable = table([edges(:,1), edges(:,2)],'VariableNames',{'EndNodes'})
NodeTable = table([nodes])

get_graph = graph(EdgeTable, NodeTable)

adjacency = adjacency(get_graph)

network = sparse(adjacency)

global network

mod = modularity(500)

csvwrite('mod_matrix.csv', mod)