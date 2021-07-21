library(DiagrammeR)

DiagrammeR::grViz("digraph {

                  graph[layout = dot]
                  
                  node[shape = rectangle,
                      fixedsize = true,
                      width = 1.7]
                  
                  A [label = '@@1', fontsize = 12]
                  B [label = '@@2', fontsize = 12]
                  C [label = '@@3', fontsize = 10]
                  
              A -> B -> C
              }
              
              [1]: 'Generate data'
              [2]: 'Select a model'
              [3]: 'Compute parameter estimates \\nand CIs coverage'
              ")
