import TSim.*;

import java.util.concurrent.Semaphore;


public class Lab1 {

    private final Switch north = new Switch(17, 7, true);
    private final Switch midWest = new Switch(4, 9, false);
    private final Switch midEast = new Switch(15, 9, true);
    private final Switch south = new Switch(3, 11, false);
    private final Semaphore start = new Semaphore(1);
    private final Semaphore midWestCS = new Semaphore(1);
    private final Semaphore midUpperSection = new Semaphore(1);
    private final Semaphore midEastCS = new Semaphore(1);
    private final Semaphore northStart = new Semaphore(1);

    public Lab1(int speed1, int speed2) {

        //setupSemaphores();

        Thread train1 = new Thread(new Train(speed1, 1, false));
        Thread train2 = new Thread(new Train(speed2, 2, true));
        // loop
        // sensorcheck
        // i sensorcheck kolla vilken sensor, kolla riktning, agera därefter
        train1.start();
        train2.start();


    }

    public class Train implements Runnable {
        TSimInterface tsi = TSimInterface.getInstance();
        int id;
        int speed;
        int initialspeed;
        boolean goingNorth;
        boolean goingForward = true;


        public Train(int speed, int id, boolean goingNorth) {
            this.speed = speed;
            this.id = id;
            this.goingNorth = goingNorth;
            this.initialspeed = speed;
        }

        @Override
        public void run() {
            if (id == 2) {
                try {
                    start.acquire();
                    System.out.println("Start acquired!");
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            try {
                tsi.setSpeed(id, speed);
                while (true) {
                    updateSensors();
                }
            } catch (CommandException e) {

                e.printStackTrace();
            }
        }

        private void updateSensors() {


            // Hantera goingNorth
            // if can take then take
            // else (1)

            try {
                SensorEvent se = tsi.getSensor(this.id);
                updateOnActive(se);
                updateOnInactive(se);
                shortestPathSwitches(se);


            } catch (CommandException | InterruptedException e) {
                e.printStackTrace();
            }
        }

        private void updateOnInactive(SensorEvent se) {


            if (passedSensorInactive(se, 16, 8)) {
                if (goingSouth()) {
                    northStart.release();
                    System.out.println("NorthStart released, train id: " + id);
                }


            }
            if (passedSensorInactive(se, 13, 9)) {
                if (goingNorth) {
                    midUpperSection.release();
                    System.out.println("midUpperSection released, train id: " + id);

                }

            }
            if (passedSensorInactive(se, 5, 9)) {
                if (goingSouth()) {
                    midUpperSection.release();
                    System.out.println("midUpperSection released, train id: " + id);
                }

            }
            if (passedSensorInactive(se, 4, 11)) {
                if (goingNorth) {
                    start.release();
                    System.out.println("start released, train id: " + id);

                }
            }
            if (passedSensorInactive(se, 14, 10)) {
                if (goingSouth()) {
                    midEastCS.release();
                }
            }

        }

        private void updateOnActive(SensorEvent se) throws InterruptedException {

            if (passedSensor(se, 5, 10)) {
                if (goingNorth) {
                    System.out.println("midWestCS released, train id: " + id);
                    midWestCS.release();
                } else {
                    tryTakeAndGo("midWestCS", midWestCS, midWest, false);
                }


            }
            if (passedSensor(se, 13, 9)) {
                if (goingSouth()) {
                    midEastCS.release();
                } else {
                    tryTakeAndGo("midEastCS", midEastCS, midEast, true);
                }
            }
            if (passedSensor(se, 15, 7)) {
                if (goingSouth()) {
                    tryTakeAndGo("midEastCS", midEastCS, north, false);
                } else {
                    midEastCS.release();
                    System.out.println("midEastCS released, train id: " + id);
                }
            }
            if (passedSensor(se, 18, 7)) {
                if (goingNorth) {
                    if (northStart.tryAcquire()) {
                        updateSwitch(north, true);
                        System.out.println("NorthStart acquired, train id: " + id);

                    } else {
                        updateSwitch(north, false);
                    }
                } else {

                }
            }
            if (passedSensor(se, 16, 8)) {
                if (goingNorth) {
                    System.out.println("midEastCS released, train id: " + id);
                    midEastCS.release();
                } else {
                    tryTakeAndGo("midEastCS", midEastCS, north, true);
                }
            }
            if (passedSensor(se, 17, 9)) {
                if (goingSouth()) {
                    checkAndSwitch(midUpperSection, midEast, "midEastCS");
                }
            }
            if (passedSensor(se, 5, 9)) {
                if (goingNorth) {
                    System.out.println("midWestCS released, train id: " + id);
                    midWestCS.release();
                } else {
                    tryTakeAndGo("midWestCS", midWestCS, midWest, true);
                }
            }
            if (passedSensor(se, 2, 11)) {
                if (goingSouth()) {
                    checkAndSwitch(start, south, "startCS");
                }
            }
            if (passedSensor(se, 4, 11)) {
                if (goingNorth) {
                    tryTakeAndGo("midWestCS", midWestCS, south, true);
                } else {
                    System.out.println("midWestCS released, train id: " + id);
                    midWestCS.release();
                }
            }
            if (passedSensor(se, 3, 12)) {
                if (goingNorth) {
                    tryTakeAndGo("midWestCS", midWestCS, south, false);
                } else {
                    System.out.println("midWestCS released, train id: " + id);
                    midWestCS.release();
                }
            }

            if (passedSensor(se, 3, 9)) {
                if (goingNorth) {
                    checkAndSwitch(midUpperSection, midWest, "midUpperCS");
                }
            }
            if (passedSensor(se, 14, 10)) {
                if (goingNorth) {
                    tryTakeAndGo("midEastCS", midEastCS, midEast, false);
                } else {
                    System.out.println("midWestCS released, train id: " + id);
                }
            }
        }


        private void checkAndSwitch(Semaphore sem, Switch s, String name) {
            if (!sem.tryAcquire()) {
                System.out.println(name + " not acquired, train id: " + id);
                updateSwitch(s, false);
            } else {
                System.out.println(name + " acquired, train id: " + id);
                updateSwitch(s, true);
            }
        }


        private void tryTakeAndGo(String name, Semaphore cs, Switch sw, boolean shortestPath) throws InterruptedException {
            if (!cs.tryAcquire()) {
                System.out.println(name + " not acquired, train id: " + id);
                halt();
                cs.acquire();
                System.out.println(name + " acquired, train id: " + id);
                go();
            } else {
                System.out.println(name + " acquired, train id: " + id);
            }
            updateSwitch(sw, shortestPath);
        }

        private void shortestPathSwitches(SensorEvent se) throws CommandException, InterruptedException {
            if (passedSensor(se, 15, 5) && goingNorth) {
                switchDirection();
            }
            if (passedSensor(se, 16, 11) && !goingNorth) {
                switchDirection();
            }
            if (passedSensor(se, 15, 13) && !goingNorth) {
                switchDirection();
            }
            if (passedSensor(se, 14, 3) && goingNorth) {
                switchDirection();
            }
        }

        private void go() {
            try {
                tsi.setSpeed(id, goingForward ? initialspeed : -initialspeed);
            } catch (CommandException e) {
                e.printStackTrace();
            }
        }

        private void halt() {
            try {
                tsi.setSpeed(id, 0);
            } catch (CommandException e) {
                e.printStackTrace();
            }
        }

        private void switchDirection() throws CommandException, InterruptedException {
            System.out.println("Current speed " + speed);
            tsi.setSpeed(this.id, 0);
            Thread.sleep(2000);
            goingForward = !goingForward;
            go();
            System.out.println(initialspeed);
            this.goingNorth = !goingNorth;
        }

        private boolean passedSensor(SensorEvent se, int x, int y) {
            return se.getXpos() == x && se.getYpos() == y && se.getStatus() == se.ACTIVE;
        }

        private boolean passedSensorInactive(SensorEvent se, int x, int y) {
            return se.getXpos() == x && se.getYpos() == y && se.getStatus() == se.INACTIVE;
        }

        private void updateSwitch(Switch s, boolean shortestPath) {
            if (goingNorth) {
                updateNorthGoingSwitches(s, shortestPath);
            } else {
                updateSouthGoingSwitches(s, shortestPath);
            }
        }

        private void updateSouthGoingSwitches(Switch s, boolean shortestPath) {
            if (s.equals(north)) {
                if (shortestPath) {
                    s.setSwitchLeft();
                } else {
                    s.setSwitchRight();
                }
            } else if (s.equals(midEast)) {
                if (shortestPath) {
                    s.setSwitchRight();
                } else {
                    s.setSwitchLeft();
                }
            } else if (s.equals(midWest)) {
                if (shortestPath) {
                    s.setSwitchLeft();
                } else {
                    s.setSwitchRight();
                }
            } else {
                if (shortestPath) {
                    s.setSwitchLeft();
                } else {
                    s.setSwitchRight();
                }
            }
        }


        private void updateNorthGoingSwitches(Switch s, boolean shortestPath) {
            if (s.equals(north)) {
                if (shortestPath) {
                    s.setSwitchLeft();
                } else {
                    s.setSwitchRight();
                }
            } else if (s.equals(midEast)) {
                if (shortestPath) {
                    s.setSwitchRight();
                } else {
                    s.setSwitchLeft();
                }
            } else if (s.equals(midWest)) {
                if (shortestPath) {
                    s.setSwitchLeft();
                } else {
                    s.setSwitchRight();
                }
            } else {
                if (shortestPath) {
                    s.setSwitchLeft();
                } else {
                    s.setSwitchRight();
                }
            }
        }

        private boolean goingSouth() {
            return !goingNorth;
        }

    }
}


class Switch {
    private final Position pos;
    private int status;
    private boolean leftTurn;

    Switch(int x, int y, boolean leftTurn) {
        this.pos = new Position(x, y);
        this.status = 0;
        this.leftTurn = leftTurn;
    }

    void setSwitchLeft() {
        try {
            TSimInterface.getInstance().setSwitch(pos.x, pos.y, TSimInterface.SWITCH_LEFT);
        } catch (CommandException e) {
            //  e.printStackTrace();
        }
    }

    void setSwitchRight() {
        try {
            TSimInterface.getInstance().setSwitch(pos.x, pos.y, TSimInterface.SWITCH_RIGHT);
        } catch (CommandException e) {
            // e.printStackTrace();
        }

    }


    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Switch)) return false;
        Switch s = (Switch) obj;
        return this.pos.x == s.pos.x && this.pos.y == s.pos.y;
    }


}

class Position {
    final int x;
    final int y;

    Position(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return this.x;
    }

    public int getY() {
        return this.y;
    }
}






