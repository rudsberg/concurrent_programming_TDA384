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
    private final Semaphore northIntersection = new Semaphore(1);

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
            try {
                SensorEvent se = tsi.getSensor(this.id);
                updateOnActive(se);
                releaseOnInactive(se);
                shortestPathSwitches(se);


            } catch (CommandException | InterruptedException e) {
                e.printStackTrace();
            }
        }

        private void releaseOnInactive(SensorEvent se) {
        	releaseIfNeededFor(se, 5, 11, true, start);
        	releaseIfNeededFor(se, 16, 8, false, northStart);
        	
        	releaseIfNeededFor(se, 12, 9, true, midUpperSection);
        	releaseIfNeededFor(se, 7, 9, false, midUpperSection);
        	
        	releaseIfNeededFor(se, 10, 7, false, northIntersection);
        	releaseIfNeededFor(se, 9, 8, false, northIntersection);
        	releaseIfNeededFor(se, 8, 5, true, northIntersection);
        	releaseIfNeededFor(se, 6, 7, true, northIntersection);

        	releaseIfNeededFor(se, 14, 10, false, midEastCS);
        	releaseIfNeededFor(se, 12, 9, false, midEastCS);
        	releaseIfNeededFor(se, 16, 8, true, midEastCS);
        	releaseIfNeededFor(se, 15, 7, true, midEastCS);

        	releaseIfNeededFor(se, 5, 10, true, midWestCS);
        	releaseIfNeededFor(se, 7, 9, true, midWestCS);
        	releaseIfNeededFor(se, 5, 11, false, midWestCS);
        	releaseIfNeededFor(se, 3, 13, false, midWestCS);
        }
        
    	private void releaseIfNeededFor(SensorEvent se, int x, int y, boolean forGoingNorth, Semaphore sem) {
			if (passedSensorInactive(se, x, y)) {
				if (forGoingNorth) {
					if (goingNorth) {
						sem.release();
					}
				} else {
					if (goingSouth()) {
						sem.release();
					}
				}
            }
		}

        private void updateOnActive(SensorEvent se) throws InterruptedException {

            handleIntersection(se);

            if (passedSensor(se, 5, 10)) {
                if (goingSouth()) {
                	takeThenGo(midWestCS);
                	updateSwitch(midWest, false);
                }
            }

            if (passedSensor(se, 12, 9)) {
                if (goingNorth) {
                	takeThenGo(midEastCS);
                	updateSwitch(midEast, true);
                }
            }

            if (passedSensor(se, 15, 7)) {
                if (goingSouth()) {
                	takeThenGo(midEastCS);
                	updateSwitch(north, false);
                }
            }

            if (passedSensor(se, 19, 7)) {
                if (goingNorth) {
                	checkAndSwitch(northStart, north, "north");
                }
            }

            if (passedSensor(se, 16, 8)) {
                if (goingSouth()) {
                	takeThenGo(midEastCS);
                	updateSwitch(north, true);
                }
            }

            if (passedSensor(se, 17, 9)) {
                if (goingSouth()) {
                    checkAndSwitch(midUpperSection, midEast, "midEastCS");
                }
            }

            if (passedSensor(se, 7, 9)) {
                if (goingSouth()) {
                	takeThenGo(midWestCS);
                	updateSwitch(midWest, true);
                }
            }

            if (passedSensor(se, 1, 11)) {
                if (goingSouth()) {
                    checkAndSwitch(start, south, "startCS");
                }
            }

            if (passedSensor(se, 5, 11)) {
                if (goingNorth) {
                	takeThenGo(midWestCS);
                	updateSwitch(south, true);
                          }
            }

            if (passedSensor(se, 3, 13)) {
                if (goingNorth) {
                	takeThenGo(midWestCS);
                	updateSwitch(south, false);
                }
            }

            if (passedSensor(se, 1, 9)) {
                if (goingNorth) {
                    checkAndSwitch(midUpperSection, midWest, "midUpperCS");
                }
            }
            if (passedSensor(se, 14, 10)) {
                if (goingNorth) {
                	takeThenGo(midEastCS);
                	updateSwitch(midEast, false);
                } 
            }
        }

		private void handleIntersection(SensorEvent se) throws InterruptedException {
			if (passedSensor(se, 10, 7)) {
                if (goingNorth) {
                    if (!northIntersection.tryAcquire()) {
                        halt();
                        northIntersection.acquire();
                        go();
                    }
                }
            }

            if (passedSensor(se, 9, 8)) {
                if (goingNorth) {
                    if (!northIntersection.tryAcquire()) {
                        halt();
                        northIntersection.acquire();
                        go();
                    }
                }
            }
            if (passedSensor(se, 8, 5)) {
                if (goingSouth()) {
                    if (!northIntersection.tryAcquire()) {
                        halt();
                        northIntersection.acquire();
                        go();
                    }
                }
            }
            if (passedSensor(se, 6, 7)) {
                if (goingSouth()) {
                    if (!northIntersection.tryAcquire()) {
                        halt();
                        northIntersection.acquire();
                        go();
                    }
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
        
        private void takeThenGo(Semaphore cs) throws InterruptedException {
            if (!cs.tryAcquire()) {
                halt();
                cs.acquire();
                go();
            }
        }

        private void shortestPathSwitches(SensorEvent se) throws CommandException, InterruptedException {
            if (passedSensor(se, 15, 5) && goingNorth) {
                switchDirection();
            }
            if (passedSensor(se, 14, 11) && !goingNorth) {
                switchDirection();
            }
            if (passedSensor(se, 14, 13) && !goingNorth) {
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






