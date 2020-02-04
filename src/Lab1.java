import TSim.*;

import java.util.concurrent.Semaphore;


public class Lab1 {

    private final Switch north 		= new Switch(17, 7, true);
    private final Switch midWest 	= new Switch(4, 9, false);
    private final Switch midEast 	= new Switch(15, 9, true);
    private final Switch south 		= new Switch(3, 11, false);
    
    private final Semaphore southStartCS 		= new Semaphore(1);
    private final Semaphore midWestCS 			= new Semaphore(1);
    private final Semaphore midUpperSectionCS 	= new Semaphore(1);
    private final Semaphore midEastCS 			= new Semaphore(1);
    private final Semaphore northStartCS 		= new Semaphore(1);
    private final Semaphore northIntersectionCS = new Semaphore(1);

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
            	if (id == 2) southStartCS.acquire();
            	
            	// Initialize with the speed provided in constructor
                tsi.setSpeed(id, speed);
                
                // Runs simulation forever (or if an exception is thrown)
                while (true) {
                    runTrainSimulation();
                }
            } catch (InterruptedException | CommandException e) {
                e.printStackTrace();
            }
        }

        private void runTrainSimulation() throws CommandException, InterruptedException {
            	// Will wait for a new event to be emitted, then proceed with actions below
                SensorEvent se = tsi.getSensor(this.id);
                                
                // Performs necessary updates if train made any switch ACTIVE
                updateOnActive(se);
                
                // Performs necessary updates if train made any switch INACTIVE
                releaseOnInactive(se);
                
                // Will stop, wait and switch direction if train reached a station
                handleTrainReachedStation(se);
        }

        /**
         * Handles all releases of semaphores. They are only released if sensor becomes inactive.
         * @param se
         */
        private void releaseOnInactive(SensorEvent se) {
        	releaseIfNeededFor(se, 5, 11, true, southStartCS);
        	releaseIfNeededFor(se, 15, 8, false, northStartCS);
        	
        	releaseIfNeededFor(se, 12, 9, true, midUpperSectionCS);
        	releaseIfNeededFor(se, 7, 9, false, midUpperSectionCS);
        	
        	releaseIfNeededFor(se, 10, 7, false, northIntersectionCS);
        	releaseIfNeededFor(se, 9, 8, false, northIntersectionCS);
        	releaseIfNeededFor(se, 8, 5, true, northIntersectionCS);
        	releaseIfNeededFor(se, 6, 7, true, northIntersectionCS);

        	releaseIfNeededFor(se, 14, 10, false, midEastCS);
        	releaseIfNeededFor(se, 12, 9, false, midEastCS);
        	releaseIfNeededFor(se, 15, 8, true, midEastCS);
        	releaseIfNeededFor(se, 15, 7, true, midEastCS);

        	releaseIfNeededFor(se, 6, 10, true, midWestCS);
        	releaseIfNeededFor(se, 7, 9, true, midWestCS);
        	releaseIfNeededFor(se, 5, 11, false, midWestCS);
        	releaseIfNeededFor(se, 3, 13, false, midWestCS);
        }
        
        /**
         * Checks the given sensor event, x, y and if the train is headed the direction asked for (going north or south), 
         * if so the semaphore will be released.
         * @param se
         * @param x
         * @param y
         * @param forGoingNorth
         * @param sem
         */
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

    	/**
    	 * Given a sensor event it will perform any needed actions for trains to go correctly.
    	 * @param se
    	 * @throws InterruptedException
    	 * @throws CommandException
    	 */
        private void updateOnActive(SensorEvent se) throws InterruptedException, CommandException {

            handleIntersection(se);

            if (passedSensorOnActive(se, 6, 10)) {
                if (goingSouth()) {
                	takeThenGo(midWestCS, midWest, false);
                }
            }

            if (passedSensorOnActive(se, 12, 9)) {
                if (goingNorth) {
                	takeThenGo(midEastCS, midEast, true);
                }
            }

            if (passedSensorOnActive(se, 15, 7)) {
                if (goingSouth()) {
                	System.out.println("TAKE THEN GO 15 7");
                	takeThenGo(midEastCS, north, false);
                }
            }

            if (passedSensorOnActive(se, 19, 7)) {
                if (goingNorth) {
                	checkAndSwitch(northStartCS, north, "north");
                }
            }

            if (passedSensorOnActive(se, 15, 8)) {
                if (goingSouth()) {
                	takeThenGo(midEastCS, north, true);
                }
            }

            if (passedSensorOnActive(se, 17, 9)) {
                if (goingSouth()) {
                    checkAndSwitch(midUpperSectionCS, midEast, "midEastCS");
                }
            }

            if (passedSensorOnActive(se, 7, 9)) {
                if (goingSouth()) {
                	takeThenGo(midWestCS, midWest, true);
                }
            }

            if (passedSensorOnActive(se, 1, 11)) {
                if (goingSouth()) {
                    checkAndSwitch(southStartCS, south, "startCS");
                }
            }

            if (passedSensorOnActive(se, 5, 11)) {
                if (goingNorth) {
                	takeThenGo(midWestCS, south, true);
                          }
            }

            if (passedSensorOnActive(se, 3, 13)) {
                if (goingNorth) {
                	takeThenGo(midWestCS, south, false);
                }
            }

            if (passedSensorOnActive(se, 1, 9)) {
                if (goingNorth) {
                    checkAndSwitch(midUpperSectionCS, midWest, "midUpperCS");
                }
            }
            if (passedSensorOnActive(se, 14, 10)) {
                if (goingNorth) {
                	takeThenGo(midEastCS, midEast, false);
                } 
            }
        }

        /**
         * Handles the only intersection that needs logic to sync trains.
         * @param se
         * @throws InterruptedException
         * @throws CommandException
         */
		private void handleIntersection(SensorEvent se) throws InterruptedException, CommandException {        
            if (goingNorth) {
            	handlePathIntoIntersection(se,  10, 7);
            	handlePathIntoIntersection(se,  9, 8);
            }
            
            if (goingSouth()) {
            	handlePathIntoIntersection(se, 8, 5);
                handlePathIntoIntersection(se, 6, 7);
            }
		}

		private void handlePathIntoIntersection(SensorEvent se, int x, int y) throws CommandException, InterruptedException {
			if (passedSensorOnActive(se, x, y)) {
                    if (!northIntersectionCS.tryAcquire()) {
                        halt();
                        northIntersectionCS.acquire();
                        go();
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
        
        private void takeThenGo(Semaphore cs, Switch s, boolean shortestPath) throws InterruptedException, CommandException {
            if (!cs.tryAcquire()) {
            	System.out.println("halting");
                halt();
                cs.acquire();
            	System.out.println("acquired");
                System.out.println("updated switch");
                updateSwitch(s, shortestPath);
                go();
                return;
            }
            updateSwitch(s, shortestPath);
        }

        private void handleTrainReachedStation(SensorEvent se) throws CommandException, InterruptedException {
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






