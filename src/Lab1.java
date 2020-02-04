import TSim.*;

import java.util.concurrent.Semaphore;


public class Lab1 {

    private final Switch north = new Switch(17, 7, true);
    private final Switch midWest = new Switch(4, 9, false);
    private final Switch midEast = new Switch(15, 9, true);
    private final Switch south = new Switch(3, 11, false);
    
    private final Semaphore southStart = new Semaphore(1);
    private final Semaphore midWestCS = new Semaphore(1);
    private final Semaphore midUpperSection = new Semaphore(1);
    private final Semaphore midEastCS = new Semaphore(1);
    private final Semaphore northStart = new Semaphore(1);
    private final Semaphore northIntersection = new Semaphore(1);

    public Lab1(int speed1, int speed2) {
        Thread train1 = new Thread(new Train(speed1, 1, false));
        Thread train2 = new Thread(new Train(speed2, 2, true));

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
            try {
            	// Train 2 starts on a critical section and must therefore acquire the semaphore on start
            	if (id == 2) southStart.acquire();
            	
            	// Initialize with the speed provided in constructor
                tsi.setSpeed(id, speed);
                
                // Runs simulation forever (or if an exception is thrown)
                runTrainSimulation();
            } catch (InterruptedException | CommandException e) {
                e.printStackTrace();
            }
        }

        private void runTrainSimulation() throws CommandException, InterruptedException {
            while (true) {
                SensorEvent se = tsi.getSensor(this.id);
                updateOnActive(se);
                releaseOnInactive(se);
                shortestPathSwitches(se);
            }
        }

        private void releaseOnInactive(SensorEvent se) {
        	releaseIfNeededFor(se, 5, 11, true, southStart);
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
			if (passedSensorOnInactive(se, x, y)) {
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

        private void updateOnActive(SensorEvent se) throws InterruptedException, CommandException {

            handleIntersection(se);

            if (passedSensorOnActive(se, 5, 10)) {
                if (goingSouth()) {
                	takeThenGo(midWestCS);
                	updateSwitch(midWest, false);
                }
            }

            if (passedSensorOnActive(se, 12, 9)) {
                if (goingNorth) {
                	takeThenGo(midEastCS);
                	updateSwitch(midEast, true);
                }
            }

            if (passedSensorOnActive(se, 15, 7)) {
                if (goingSouth()) {
                	takeThenGo(midEastCS);
                	updateSwitch(north, false);
                }
            }

            if (passedSensorOnActive(se, 19, 7)) {
                if (goingNorth) {
                	checkAndSwitch(northStart, north, "north");
                }
            }

            if (passedSensorOnActive(se, 16, 8)) {
                if (goingSouth()) {
                	takeThenGo(midEastCS);
                	updateSwitch(north, true);
                }
            }

            if (passedSensorOnActive(se, 17, 9)) {
                if (goingSouth()) {
                    checkAndSwitch(midUpperSection, midEast, "midEastCS");
                }
            }

            if (passedSensorOnActive(se, 7, 9)) {
                if (goingSouth()) {
                	takeThenGo(midWestCS);
                	updateSwitch(midWest, true);
                }
            }

            if (passedSensorOnActive(se, 1, 11)) {
                if (goingSouth()) {
                    checkAndSwitch(southStart, south, "startCS");
                }
            }

            if (passedSensorOnActive(se, 5, 11)) {
                if (goingNorth) {
                	takeThenGo(midWestCS);
                	updateSwitch(south, true);
                          }
            }

            if (passedSensorOnActive(se, 3, 13)) {
                if (goingNorth) {
                	takeThenGo(midWestCS);
                	updateSwitch(south, false);
                }
            }

            if (passedSensorOnActive(se, 1, 9)) {
                if (goingNorth) {
                    checkAndSwitch(midUpperSection, midWest, "midUpperCS");
                }
            }
            if (passedSensorOnActive(se, 14, 10)) {
                if (goingNorth) {
                	takeThenGo(midEastCS);
                	updateSwitch(midEast, false);
                } 
            }
        }

		private void handleIntersection(SensorEvent se) throws InterruptedException, CommandException {
			if (passedSensorOnActive(se, 10, 7)) {
                if (goingNorth) {
                    if (!northIntersection.tryAcquire()) {
                        halt();
                        northIntersection.acquire();
                        go();
                    }
                }
            }

            if (passedSensorOnActive(se, 9, 8)) {
                if (goingNorth) {
                    if (!northIntersection.tryAcquire()) {
                        halt();
                        northIntersection.acquire();
                        go();
                    }
                }
            }
            if (passedSensorOnActive(se, 8, 5)) {
                if (goingSouth()) {
                    if (!northIntersection.tryAcquire()) {
                        halt();
                        northIntersection.acquire();
                        go();
                    }
                }
            }
            if (passedSensorOnActive(se, 6, 7)) {
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
        
        private void takeThenGo(Semaphore cs) throws InterruptedException, CommandException {
            if (!cs.tryAcquire()) {
                halt();
                cs.acquire();
                go();
            }
        }

        private void shortestPathSwitches(SensorEvent se) throws CommandException, InterruptedException {
            if (passedSensorOnActive(se, 15, 5) && goingNorth) {
                switchDirection();
            }
            if (passedSensorOnActive(se, 14, 11) && !goingNorth) {
                switchDirection();
            }
            if (passedSensorOnActive(se, 14, 13) && !goingNorth) {
                switchDirection();
            }
            if (passedSensorOnActive(se, 14, 3) && goingNorth) {
                switchDirection();
            }
        }

        private void go() throws CommandException {
                tsi.setSpeed(id, goingForward ? initialspeed : -initialspeed);
        }

        private void halt() throws CommandException {
                tsi.setSpeed(id, 0);
        }

        private void switchDirection() throws CommandException, InterruptedException {
            tsi.setSpeed(this.id, 0);
            Thread.sleep(2000);
            goingForward = !goingForward;
            go();
            this.goingNorth = !goingNorth;
        }

        private boolean passedSensorOnActive(SensorEvent se, int x, int y) {
            return samePos(se, x, y) && se.getStatus() == SensorEvent.ACTIVE;
        }

        private boolean passedSensorOnInactive(SensorEvent se, int x, int y) {
            return samePos(se, x, y) && se.getStatus() == SensorEvent.INACTIVE;
        }
        
        private boolean samePos(SensorEvent se, int x, int y) {
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






