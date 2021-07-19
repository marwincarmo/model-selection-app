library(DiagrammeR)

DiagrammeR::grViz("digraph {

                  graph[layout = dot]
                  node[shape = rectangle]
                  A [label = '@@1']
                  B [label = '@@2']
                  C [label = '@@3']
                  
              A -> B -> C
              }
              
              [1]: 'Estimate model'
              [2]: 'Select a model'
              [3]: 'Compute CIs'
              ")
