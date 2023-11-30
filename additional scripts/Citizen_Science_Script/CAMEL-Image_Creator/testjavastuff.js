let queueCreator = () => {
    const queue = []
    return {
      add(x) {
        queue.unshift(x)
        //console.log("aaa:", queue)
      },
      remove() {
        if (queue.length === 0) {
          return undefined
        }
        return queue.pop()
      },
      empty() {
        return queue.length === 0
      }
    }
  }
  
  let nodeCreator = (id) => {
    const neighbors = []
    return {
      id,
      neighbors,
      addNeighbors(node) {
        neighbors.push(node)
      }
    }
  }
  
  let graphCreator = (uni = false) => {
    const nodes = []
    const edges = []
    return {
      uni,
      nodes,
      edges,
      addNode(id) {
        nodes.push(nodeCreator(id))  
      },
      addEdge(idOne, idTwo) {
        const a = this.searchNode(idOne)
        const b = this.searchNode(idTwo)
  
        a.addNeighbors(b)
        if (!uni) {
          b.addNeighbors(a)
        }
        edges.push(`${idOne}${idTwo}`)
      },

      
      searchNode(id) {
        return nodes.find(n => n.id === id)
      },

      breadthFirst(startingNode, neighborVisit) {
        const firstNode = this.searchNode(startingNode)
        const visitedNode = nodes.reduce((sum, node) => {
          sum[node.id] = false
          return sum
        }, {})
        // console.log(visitedNode);
        const queue = queueCreator()
        queue.add(firstNode) 
       

        while (!queue.empty()) {
          const temp = queue.remove()
          if (!visitedNode[temp.id]) {
            neighborVisit(temp)
            visitedNode[temp.id] = true
          }
          temp.neighbors.forEach(node => {
            if(!visitedNode[node.id]) {
              queue.add(node)
            }
          })
    
        }
       // console.log(visitedNode[firstNode.id])
        // visitedNode[firstNode.id] = true
       // console.log(visitedNode);

        return visitedNode;
   

      }  
     
    }
  }


  const graph = graphCreator(true)
  graph.addNode('a')
  graph.addNode('b')
  graph.addNode('c')

  graph.addEdge('a', 'b')
  graph.addEdge('a', 'c')
  graph.addEdge('b', 'c')



  graph.addNode('d')
  graph.addNode('e')
  graph.addNode('f')
  graph.addEdge('d', 'e')
  graph.addEdge('d', 'f')


console.log(graph.nodes[0]);



  var logArray = []; 

  logArray[0] = graph.breadthFirst('a', node => {
    console.log(node.id);
  })

  var tmpResult = Object.keys(logArray[0])
  .reduce((o, key) => {
    logArray[0][key] === false && (o[key] = logArray[0][key]);
    return o;
  }, {});
  console.log(Object.keys(tmpResult));


  logArray[1] = graph.breadthFirst(Object.keys(tmpResult)[0], node => {
    console.log(node.id);
  })


  tmpResult = Object.keys(logArray[1])
  .reduce((o, key) => {
    logArray[1][key] === false && (o[key] = logArray[1][key]);
    return o;
  }, {});
  console.log(Object.keys(tmpResult));