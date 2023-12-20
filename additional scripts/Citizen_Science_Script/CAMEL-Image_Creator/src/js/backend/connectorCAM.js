/**
 * Brief description of the class here
 * @extends ParentClassNameHereIfAny
 */
class ConnectorCAM {
    constructor(isDeletable = true) {
        this.id = uuid.v4(); // uuidv4.v4(); // to run "npm run test"
        this.value = null;
        this.source = null;
        this.target = null;
        this.agreement = null; // shape of line
        this.isActive = null;
        this.date = new Date().getTime(); // representing the milliseconds elapsed between 1 January 1970 00:00:00 UTC and the given date
        this.kind = "Connector"; // information for drawing edges / nodes
        this.isSelected = false;
        this.intensity = IncreaseSliderIntensity; // 3
        this.isDeletable = isDeletable;
        this.isOver = false;
        this.isBidirectional = true;
        this.eventLog = [];
    }

    setValue(newValue) {
        this.value = newValue;
    }
    getIntensity() {
        return this.intensity;
    }

    getKind() {
        return this.kind;
    }

    setIsSelected(val) {
        this.isSelected = val;
    }

    getSelected() {
        return this.isSelected;
    }

    getIsActive() {
        return this.isActive;
    }

    setIsActive(val) {
        if (this.isDeletable) {
            this.isActive = val;
        }
    }

    setAgreement(val) {
        this.agreement = val;
    }

    setIsDeletable(val) {
        this.isDeletable = val;
    }

    setBidirectional(val) {
        this.isBidirectional = val;
    }

    setDirectional() {
        this.isBidirectional = false;

        const tmp_source = this.source;
        this.source = this.target;
        this.target = tmp_source;
    }

    getId() {
        return this.id;
    }

    getIsDeletable() {
        return this.isDeletable;
    }

    setShape() {
        if (this.value < 0) this.shape = "negative";
        if (this.value > 0) this.shape = "positive";
        if (this.value == 0) this.shape = "neutral";
    }

    updateConnector(field, value) {
        this.enterLog({
            type: field,
            value: value,
        });
        if (field === "value") this.setValue(value);
        if (field === "active") this.setIsActive(value);
        if (field === "selected") this.setIsSelected(value);
        if (field === "agreement") this.setAgreement(value);
        if (field === "bidirection") this.setBidirectional(value);
        if (field === "direction") this.setDirectional();
    }

    /**
     * This function establishes a connection between a source and a target with a given intensity and agreement value.
     * @param {Node} source - Node corresponding to the source.
     * @param {Node} target - Node corresponding to the target.
     * @param {int} intensity - Intensity of the connection.
     * @param {boolean} agreement - Is the connection an agreement.
     */
    establishConnection(source, target, intensity, agreement) {
        this.agreement = agreement;
        this.date = new Date().getTime();
        this.source = source.id;
        this.target = target.id;
        this.isActive = true;
        this.agreement = agreement;
        this.intensity = intensity;
        this.isBidirectional = false;

        this.enterLog({
            type: "create connection",
            value: null,
        });
    }

    makeBidirectionalConnection() {
        this.enterLog({
            type: "create bidirectional connection",
            value: null,
        });
        this.isBidirectional = true;
    }

    makeUnidirectionalConnection() {
        this.enterLog({
            type: "create unidirectional connection",
            value: null,
        });

        this.isBidirectional = false;

        const tmp_source = this.source;
        this.source = this.target;
        this.target = tmp_source;
    }

    deleteConnection() {
        this.enterLog({
            type: "active",
            value: false,
        });

        this.isActive = false;
        console.log("The connection has been destroyed.");
        return true;
    }

    enterLog(log) {
        this.eventLog.push({
            time: new Date().getTime(),
            type: log.type,
            value: log.value,
        });
    }
}
