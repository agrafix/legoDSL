const OUTB = 1;
const OUTC = 2;

const IN3 = 2;

const black = 45.0;
const white = 65.0;

libDef void:OnFwd(int, float);
libDef void:SetSensorLight(int);
libDef float:Sensor(int);
libDef void:Wait(int);

function setMotorVal(sensorVal) {
    var motorVal = 0.0;
    var range = white-black;

    if (sensorVal<black) { motorVal = 0; }
    else if (sensorVal>white) { motorVal = 1; }
    else { motorVal = (sensorVal-black) / range; }

    var left = 0.0;
    var right = 0.0;

    left = motorVal;
    right = -1.0 * motorVal + 1.0;

    OnFwd(OUTB, left * 0.75);
    OnFwd(OUTC, right * 0.75);
}

function follow(old) {
    var n = Sensor(IN3);
    if (old != n) {
        setMotorVal(n);
    }
    Wait(50);
    return n;
}

function main() {
    SetSensorLight(IN3);
    var curVal = Sensor(IN3);
    while (true) {
        curVal = follow(curVal);
    }
}
