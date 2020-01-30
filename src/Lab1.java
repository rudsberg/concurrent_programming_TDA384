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
    private final Semaphore midWestCS = new Semaphore(1);
    private final Semaphore intersectionMidWest = new Semaphore(1);
    private final Semaphore midUpperSection = new Semaphore(1);
    private final Semaphore midEastCS = new Semaphore(1);

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
        	
        	// Hantera goingNorth
        		// if can take then take
        		// else (1) 

            try {
                SensorEvent se = tsi.getSensor(this.id);
                if (passedSensor(se, 14, 9)) {
                	if (goingSouth()) {
                    	midEastCS.release();
                	} else {
                		if (!midEastCS.tryAcquire()) {
                			System.out.println("midEastCS not acquired, train id: " + id);
                			halt();
                			midEastCS.acquire();
                			System.out.println("midEastCS acquired, train id: " + id);
                			go();
                		} else {
                			System.out.println("midEastCS acquired, train id: " + id);
                		}
                	}
                    updateSwitch(midEast, true);
                }
                if (passedSensor(se, 16, 7)) {
                	if (goingSouth()) {
                    	midEastCS.acquire();
                	}
                    // TODO
                    updateSwitch(north, false);
        			System.out.println("midEastCS acquired, train id: " + id);

                }

                if (passedSensor(se, 16, 9)) {
                    updateSwitch(midEast, true);
                }
                if (passedSensor(se, 5, 9)) {
                	if (goingNorth) {
            			System.out.println("midWestCS released, train id: " + id);
                		midWestCS.release();
                	} else {
                		if (!midWestCS.tryAcquire()) {
                			System.out.println("midWestCS not acquired, train id: " + id);
                			halt();
                			midWestCS.acquire();
                  			System.out.println("midWestCS acquired, train id: " + id);
                			go();
                		} else {
                			System.out.println("midWestCS acquired, train id: " + id);
                		}
                		updateSwitch(midWest, true);
                	}
                }
                if (passedSensor(se, 2, 11)) {
                	if (goingSouth()) {
                		if (!start.tryAcquire()) {
                			updateSwitch(south, false);
                			// TODO: maybe take bottom path sema
                		} else {
                			System.out.println("start acquired, train id: " + id);
                			updateSwitch(south, true);
                		}
                	} 
                }
                if (passedSensor(se, 4, 11)) {
                	if (goingNorth) {
	                	if (!midWestCS.tryAcquire()) {
                			System.out.println("southWestCS not acquired, train id: " + id);
	                		halt();
	                		midWestCS.acquire();
                			System.out.println("southWestCS acquired, train id: " + id);
	                		go();
	                	} else {
                			System.out.println("southWestCS acquired, train id: " + id);
	                	}
                		updateSwitch(south, true);
            			System.out.println("start released, train id: " + id);
	                	start.release();
                	} else {
            			System.out.println("southWestCS released, train id: " + id);
                		midWestCS.release();
                	}
                }
                updateSwitch(midEast, true);
                if (passedSensor(se, 3, 12)) {
                	if (goingNorth) {
                		if (!midWestCS.tryAcquire()) {
                			System.out.println("southWestCS not acquired, train id: " + id);
                			halt();
                			midWestCS.acquire();
                			System.out.println("southWestCS acquired, train id: " + id);
                    		go();
                		} else {
                			System.out.println("southWestCS acquired, train id: " + id);
                		}
                		updateSwitch(south, false);
                		// TODO: release sem of bottom path to station (works probably only to have sema for standard track)
                	} else {
            			System.out.println("southWestCS released, train id: " + id);
                		midWestCS.release();
                		// TODO: take sem of bottom path  (works probably only to have sema for standard track)
                	}
                }

                if (passedSensor(se, 3, 9)) {
                    if (goingNorth) {
            			if (!midUpperSection.tryAcquire()) {
                			System.out.println("midUpperSection not acquired, train id: " + id);
            				updateSwitch(midWest, false);
            			} else {
                			System.out.println("midUpperSection acquired, train id: " + id);
            				updateSwitch(midWest, true);
            			}
                    }
                }
                if (passedSensor(se, 15, 10)) {
                    if (!midEastCS.tryAcquire()) {
            			System.out.println("midEastCS not acquired, train id: " + id);
                    	halt();
                    	midEastCS.acquire();
            			System.out.println("midEastCS acquired, train id: " + id);
                    	go();
                    } else {
            			System.out.println("midEastCS acquired, train id: " + id);
                    }
                    updateSwitch(midEast, false);
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






