
function draw() {


    var nodes = CAM.nodes;
    var edges = CAM.connectors.map(elt => {
        var relt = {};
        relt["source"] = elt.source;
        relt["target"] = elt.target;
        relt["value"] = elt.value;
        relt["left"] = true;
        return relt;
    });
    
    svg.append("rect")
        .attr("x", 0)
        .attr("y", 0)
        .attr("width", 1000)
        .attr("height", 900) 
        .attr("fill", "white")  
        //.on("mousedown", (node) => {test(node)})
        .on("mousedown", (node) => {
            var pos = d3.pointer(node);
            mouseDownPosition = {x:pos[0], y:pos[1]};
        })



    svg.append("g")
        .selectAll("line")
        .data(edges)
        .enter()
            .append("line")
            .style("stroke", "red")
            .style("stroke-dasharray", (d) => {return d.value > 0 ? "0,0" : "5,5"})
            .attr("x1", function(d) { return getPosition(d.source).x; })
            .attr("y1", function(d) { return getPosition(d.source).y; })
            .attr("x2", function(d) { return getPosition(d.target).x; })
            .attr("y2", function(d) { return getPosition(d.target).y; });




    svg.append('g')
        .selectAll("dot")
        .data(nodes)
        .enter()
        .append("ellipse")
          .attr("cx", function (node) { return (node.position.x); } )
          .attr("cy", function (node) { return (node.position.y); } )
          .attr("rx", 60)
          .attr("ry", 40)
          .attr("id", function (node) { return (node.id); } )
          .attr("stroke", function(node) {return  !node.selected ? getColourNodeStroke(node) : "black";})
          .attr("stroke-width", function(node) {return  Math.abs(node.value) * 2})
          .attr("fill", function(node) {return  getColourNodeFill(node)})
          .on("click", click)
          .on("mousemove", (node) =>{
            console.log(d3.pointer(node));
            dragLine.style('marker-end', 'url(#end-arrow)')
            .classed('hidden', false)
            .attr('d', `M${0},${0}L${123},${234}`);
    
        })

    
    svg.append('g')
        .selectAll("text")
        .data(nodes)
        .enter()
          .append("text")
          .attr("x", function (node) { return (node.position.x); } )
          .attr("y", function (node) { return (node.position.y); } )
          .attr("dx", function(node){return -100})
          .text(function(node){return node.text})
          .attr("id", function (node) { return (node.id); } )
        .on("click", click)



    svg.append('svg:defs').append('svg:marker')
        .attr('id', 'end-arrow')
        .attr('viewBox', '0 -5 10 10')
        .attr('refX', 6)
        .attr('markerWidth', 3)
        .attr('markerHeight', 3)
        .attr('orient', 'auto')
      .append('svg:path')
        .attr('d', 'M0,-5L10,0L0,5')
        .attr('fill', '#000');
    
    svg.append('svg:defs').append('svg:marker')
        .attr('id', 'start-arrow')
        .attr('viewBox', '0 -5 10 10')
        .attr('refX', 4)
        .attr('markerWidth', 3)
        .attr('markerHeight', 3)
        .attr('orient', 'auto')
      .append('svg:path')
        .attr('d', 'M10,-5L0,0L10,5')
        .attr('fill', '#000');

    

}




function test(node) {
    let pos = d3.pointer(node);
    console.log(pos);
    CAM.addElement(new NodeCAM(0, "", {
        x: pos[0],
        y: pos[1]
    }, 1, 1, 1));
}

function getColourNodeStroke(node) {
    const colourPaletteRed = ["white", COLOUR.red1, COLOUR.red2, COLOUR.red3];
    const colourPaletteGreen = ["white", COLOUR.green3, COLOUR.green2, COLOUR.green1];

    if (node.value == 0) {
        return COLOUR.neutralLine;
    }
    if (node.value == 10) {
        return COLOUR.ambivalentLine;
    }
    if (node.value > 0) {
        return colourPaletteGreen[node.value];
    }
    if (node.value < 0) {
        return colourPaletteRed[-node.value];
    }
}

function getColourNodeFill(node) {

    if (node.value == 0) {
        return COLOUR.neutralNode;
    }
    if (node.value == 10) {
        return COLOUR.ambivalentNode;
    }
    if (node.value > 0) {
        return COLOUR.green3;
    }
    if (node.value < 0) {
        return COLOUR.red3;
    }

}


function click() {
    console.log(this);
    CAM.nodes.forEach(node => {
        node.selected = false;
    });
    CAM.getNodeById(this.id).selected = true;
    draw(CAM);
    console.log(CAM.getNodeById(this.id).value);
}

function clickBackground(node) {


}

function getPosition(elt) {
    const node = CAM.getNodeById(elt);
    return {
        x: node.position.x,
        y: node.position.y
    }
}