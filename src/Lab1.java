import TSim.*;

import java.util.concurrent.Semaphore;
import java.util.ArrayList;
import java.util.List;


public class Lab1 {

    List<Semaphore> semaphores = new ArrayList();
    private final TSimInterface tsi = TSimInterface.getInstance();
    private final Switch north = new Switch(17, 7, true);
    private final Switch midWest = new Switch(4, 9, false);
    private final Switch midEast = new Switch(15, 9, true);
    private final Switch south = new Switch(3, 11, false);

    private final Semaphore start = new Semaphore(1);
    private final Semaphore korsning = new Semaphore(1);
    private final Semaphore intersection2 = new Semaphore(1);

    public Lab1(int speed1, int speed2) {

        //setupSemaphores();

        Thread train1 = new Train(speed1, 1, false);
        Thread train2 = new Train(speed2, 2, true);


        // loop
        // sensorcheck
        // i sensorcheck kolla vilken sensor, kolla riktning, agera därefter
        train1.start();
        train2.start();


    }


    private void addSemaphores(List<Semaphore> semaphores) {

        semaphores.add(new Semaphore(1)); // NE crossroads into CS.
        semaphores.add(new Semaphore(1));


    }

    public class Train extends Thread {
        TSimInterface tsi = TSimInterface.getInstance();
        int id;
        int speed;
        int initialspeed;
        boolean goingNorth;
        boolean goingForward = true;
        List<Semaphore> holdingsemaphores = new ArrayList();
        boolean ack;


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
                while (!this.isInterrupted()) {
                    updateSensors();
                }
            } catch (CommandException e) {

                e.printStackTrace();
            }
        }

        private  void updateSensors() {

            try {
                SensorEvent se = tsi.getSensor(this.id);
                if (passedSensor(se, 14, 9)) {
                    updateSwitch(midEast, true);
                }
                if (passedSensor(se, 16, 7)) {
                    updateSwitch(north, true);
                }

                if (passedSensor(se, 16, 9)) {
                    updateSwitch(midEast, true);
                }
                if (passedSensor(se, 2, 11)) {
                    if (!start.tryAcquire()) {
                        south.setSwitchRight();
                    } else {
                        updateSwitch(south, true);
                    }
                }
                if (passedSensor(se, 4, 11)) {
                    while (!korsning.tryAcquire()) {
                        tsi.setSpeed(this.id, 0);
                        wait();
                    }
                    updateSwitch(south, true);
                    start.release();
                    go();
                }

                if (passedSensor(se, 3, 12)) {
                    if (!goingNorth) {
                        korsning.release();
                    } else {
                        if (!korsning.tryAcquire()) {
                            tsi.setSpeed(this.id, 0);
                            korsning.acquire();
                            go();
                        }
                        updateSwitch(south, false);
                    }
                }

                if (passedSensor(se, 3, 9)) {
                    if (goingNorth) {
                        korsning.release();
                        updateSwitch(midWest, intersection2.tryAcquire());
                    } else {
                        // TODO do stuff
                    }
                }
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

            } catch (CommandException | InterruptedException e) {
                e.printStackTrace();
            }
        }

        private void go() {
            try {
                tsi.setSpeed(this.id, goingForward ? initialspeed : -initialspeed);
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
            return se.getXpos() == x && se.getYpos() == y;
        }

        private void updateSwitch(Switch s, boolean shortestPath) {
            if (goingNorth) {
                updateNorthGoingSwitches(s, shortestPath);
            } else {
                updateSouthGoingSwitches(s, shortestPath);
            }
        }

        private void updateSouthGoingSwitches(Switch s, boolean shortestPath) {
            if (shortestPath) {
                s.setSwitchRight();
            } else {
                s.setSwitchLeft();
            }
            /*
            if (s.equals(north)) {

            } else if (s.equals(midEast)) {
                s.setSwitchRight();
            } else if (s.equals(midWest)) {
                s.setSwitchRight();
            } else {
                s.setSwitchRight();
            }*/
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


    }
}


class SensorObject {
    private final Position pos;
    private int status;

    SensorObject(int x, int y, int status) {
        this.pos = new Position(x, y);
        this.status = status;
    }

    public int getX() {
        return this.pos.getX();
    }

    public int getY() {
        return this.pos.getY();
    }

    public int getStatus() {
        return this.status;
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
            e.printStackTrace();
        }
    }

    void setSwitchRight() {
        try {
            TSimInterface.getInstance().setSwitch(pos.x, pos.y, TSimInterface.SWITCH_RIGHT);
        } catch (CommandException e) {
            e.printStackTrace();
        }

    }

    boolean isLeftTurn() {
        return leftTurn;
    }

    boolean isRightTurn() {
        return !leftTurn;
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






