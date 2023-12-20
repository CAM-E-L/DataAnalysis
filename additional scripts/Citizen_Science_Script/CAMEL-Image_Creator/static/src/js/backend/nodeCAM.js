class NodeCAM {

    constructor(value, text, position, isDraggable = true, isDeletable = true, isTextChangeable = true) {
        this.id = uuid.v4();
        this.value = value;
        this.text = text;
        this.comment = "";
        this.position = position;
        this.isActive = true;
        this.date = (new Date).getTime(); // representing the milliseconds elapsed between 1 January 1970 00:00:00 UTC and the given date
        this.kind = "Node";
        this.isSelected = false;
        this.isConnectorSelected = false;
        this.isDraggable = isDraggable;
        this.isDeletable = isDeletable;
        this.hasElementMoved = false;
        this.eventLog = [];
        this.isTextChangeable = isTextChangeable;

        this.enterLog({
            type: "create node",
            value: value
        });
    }

    setNodeImport(node) {
        this.id = node.id;
        this.value = node.value;
        this.text = node.text;
        this.comment = node.comment;
        this.position = node.position;
        this.isActive = node.isActive;
        this.date = node.date;
        this.kind = node.kind;
        this.isSelected = node.isSelected;
        this.isConnectorSelected = node.isConnectorSelected;
        this.isDraggable = node.isDraggable;
        this.isDeletable = node.isDeletable;
        this.eventLog = node.eventLog;
        this.isTextChangeable = node.isTextChangeable;
    }

    /* set functions */
    setValue(newValue) {
        this.value = newValue;
    }
    setText(newText) {
        this.text = newText;
    }
    setComment(newComment) {
        this.comment = newComment;
    }
    setPosition(newPosition) {
        this.position = this.isDraggable ? newPosition : this.position;
    }
    setIsActive(val) {
        this.isActive = this.isDeletable ? val : this.isActive;
    }
    setIsSelected(val) {
        this.isSelected = val;
    }
    setIsConnectorSelected(val) {
        this.isConnectorSelected = val;
    }
    setIsDeletable(val) {
        this.isDeletable = val;
    }
    setIsDraggable(val) {
        this.isDraggable = val;
    }
    setIsTextChangeable(val) {
        this.isTextChangeable = val;
    }



    /* get functions */

    getValue() {
        return this.value;
    }
    getText() {
        return this.text;
    }
    getComment() {
        return this.comment;
    }
    getPosition() {
        return this.position;
    }
    getIsActive() {
        return this.isActive;
    }
    getIsSelected() {
        return this.isSelected;
    }
    getIsConnectorSelected() {
        return this.isConnectorSelected;
    }
    getKind() {
        return this.kind;
    }
    getIsDeletable() {
        return this.isDeletable;
    }
    getIsDraggable() {
        return this.isDraggable;
    }
    getIsTextChangeable() {
        return this.isTextChangeable;
    }




    updateNode(field, value) {
        if (field === "text") this.setText(value);
        if (field === "position") this.setPosition(value);
        if (field === "value") this.setValue(value);
        if (field === "comment") this.setComment(value);
        if (field === "active") this.setIsActive(value);
        if (field === "selected") this.setIsSelected(value);
        if (field === "connector") this.setIsConnectorSelected(value);
    }

    enterLog(log) {
        this.eventLog.push({
            time: (new Date).getTime(),
            type: log.type,
            value: log.value
        });
    }

    deletenode() {
        this.active = false;
    }

    getId() {
        return this.id;
    }

    isNode() {
        return true;
    }

}


