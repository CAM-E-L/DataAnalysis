class Elements {
    constructor() {
        this.idCAM = uuid.v4();
        this.creator = uuid.v4(); // id of the maker
        this.projectCAM = config.CAMproject;
        this.defocusCAM = null;
        this.date = new Date().getTime(); // representing the milliseconds elapsed between 1 January 1970 00:00:00 UTC and the given date
        this.nodes = [];
        this.connectors = [];
        this.currentID = null;
        this.currentNode = null;
        this.hasSelectedNode = false;
        this.currentConnector = null;
        this.hasSelectedConnector = false;
        this.readyToMove = false;
        this.isIncoming = false;
    }

    addElement(newElement) {
        const kind = newElement.getKind();
        switch (kind) {
            case "Node":
                var { success, node } = this.addNode(newElement);
                if (success & WEBSOCKET)
                    sendMessage("Add", null, "Node", null, node);
                break;
            case "Connector":
                var { success, connector } = this.addConnector(newElement);
                if (success & WEBSOCKET)
                    sendMessage("Add", null, "Connector", null, connector);
                break;
        }
    }

    updateElement(kind, field, value) {
        switch (kind) {
            case "Node":
                if (this.currentNode != null) {
                    this.currentNode.updateNode(field, value);
                }
                break;
            case "Connector":
                if (this.currentConnector != null) {
                    this.currentConnector.updateConnector(field, value);
                }
                break;
        }
        if (WEBSOCKET)
            sendMessage("Update", this.currentID, kind, field, value);
        this.draw();
    }

    deleteElement() {
        if (this.hasSelectedNode) {
            const tmp_id = this.currentID;
            var success = this.deleteNode();
            if (success & WEBSOCKET)
                sendMessage("Update", tmp_id, "Node", "active", false);
        }

        if (this.hasSelectedConnector) {
            const tmp_id = this.currentID;
            var success = this.deleteConnector();
            if (success & WEBSOCKET)
                sendMessage("Update", tmp_id, "Connector", "active", false);
        }
        this.draw();
    }

    addConnector(connector) {
        if (this.isConnectorIn(connector) == false) {
            if (config.BidirectionalDefault) {
                connector.setBidirectional(true);
            }

            this.connectors.push(connector);
            console.log("Connector has been added.");
            return {
                success: true,
                connector,
            };
        }
        var connector = this.findConnector(connector);
        connector.isActive = true;
        return false;
    }

    isConnectorIn(connector) {
        const st = this.findConnector(connector);
        if (st == undefined) return false;
        return true;
    }

    findConnector(connector) {
        const connector1 = this.connectors.filter(
            (elt) =>
                (elt.target === connector.target &&
                    elt.source === connector.source) ||
                (elt.target === connector.source &&
                    elt.source === connector.target)
        );
        return connector1[0];
    }
    getConnectorById(id) {
        for (var elt of this.connectors) {
            if (elt.id === id) return elt;
        }
        return null;
    }

    deleteConnector() {
        if (!this.currentConnector.getIsDeletable()) {
            console.log("This element cannot be deleted.");

            toastr.info(
                languageFileOut.edw_01notDeleteConnector,
                languageFileOut.edw_02notDeleteConnector,
                {
                    closeButton: true,
                    timeOut: 2000,
                    positionClass: "toast-top-center",
                    preventDuplicates: true,
                }
            );
            return false;
        }
        this.currentConnector.deleteConnection();
        this.unselectConnection();

        this.currentConnector = null;
        this.hasSelectedConnector = false;

        return true;
    }

    addNode(node) {
        const elt = this.getNodeById(node.id);
        if (elt != null) {
            console.log("Already existing element.");
            return false;
        }
        this.nodes.push(node);
        console.log("Node has been added.");
        return {
            success: true,
            node,
        };
    }

    deleteNode() {
        const nodeID = this.currentNode.id;

        if (!this.currentNode.getIsDeletable()) {
            console.log("This element cannot be deleted.");

            toastr.info(
                languageFileOut.edw_01notDeleteNode,
                languageFileOut.edw_02notDeleteNode,
                {
                    closeButton: true,
                    timeOut: 2000,
                    positionClass: "toast-top-center",
                    preventDuplicates: true,
                }
            );

            return false;
        }

        this.connectors.forEach((connector) => {
            if (connector.target === nodeID || connector.source === nodeID) {
                connector.deleteConnection();
            }
        });

        this.currentNode.updateNode("active", false);
        this.unselectNode();

        this.currentNode = null;
        this.hasSelectedNode = false;
        return true;
    }

    getIndex(id, kind) {
        switch (kind) {
            case "Node":
                for (var index = 0; index < this.nodes.length; index++) {
                    if (this.nodes[index].id === id) return index;
                }
                break;
            case "Connector":
                for (var index = 0; index < this.connectors.length; index++) {
                    if (this.connectors[index].id === id) return index;
                }
        }
        return -1;
    }

    selecteNode(id) {
        const index = this.getIndex(id, "Node");
        if (
            this.currentNode != null &&
            this.nodes[index].id != this.currentNode.id
        ) {
            var connector = new ConnectorCAM();
            connector.establishConnection(
                this.currentNode,
                this.nodes[index],
                IncreaseSliderIntensity,
                true
            );
            this.addElement(connector);
            this.unselectNode();
            return;
        }

        this.hasSelectedNode = true;
        this.currentNode = this.nodes[index];
        this.currentID = this.currentNode.id;
        this.currentNode.updateNode("selected", !this.currentNode.isSelected);
    }

    unselectNode() {
        this.nodes.map((node) => {
            node.updateNode("selected", false);
        });
        this.currentNode = null;
        this.currentID = null;
        this.hasSelectedNode = false;
    }

    getNodeById(id) {
        for (var elt of this.nodes) {
            if (elt.id === id) return elt;
        }
        return null;
    }

    selectConnection(id) {
        const index = this.getIndex(id, "Connector");
        this.currentConnector = this.connectors[index];
        this.currentConnector.isSelected = true;
        this.currentID = this.currentConnector.id;
        this.hasSelectedConnector = true;

        const source = this.getNodeById(this.currentConnector.source);
        const target = this.getNodeById(this.currentConnector.target);

        target.updateNode("connector", true);
        source.updateNode("connector", true);
    }

    unselectConnection() {
        const source = this.getNodeById(this.currentConnector.source);
        const target = this.getNodeById(this.currentConnector.target);

        target.updateNode("connector", false);
        source.updateNode("connector", false);

        this.currentConnector.isSelected = false;

        this.currentID = null;
        this.currentConnector = null;
        this.hasSelectedConnector = false;
    }

    draw() {
        draw(this);
    }

    importElement(element) {
        console.log(element);
        if (element.kind === "Node") {
            var node = new NodeCAM(
                0,
                "",
                {
                    x: 0,
                    y: 0,
                },
                false,
                false
            );
            node.setNodeImport(element);
            console.log("A node has been imported.");
            this.nodes.push(node);
        }

        if (element.kind === "Connector") {
            const source = this.getNodeById(element.source);
            const target = this.getNodeById(element.target);
            var connector = new ConnectorCAM();
            connector.establishConnection(
                source,
                target,
                element.intensity,
                element.agreement
            );
            connector.id = element.id;
            connector.isDeletable = element.isDeletable;

            this.connectors.push(connector);
        }
        this.draw();
    }
}
