import TSim.*;

import java.util.concurrent.Semaphore;

public class Lab1 {

	private final Switch north = new Switch(17, 7);
	private final Switch midWest = new Switch(4, 9);
	private final Switch midEast = new Switch(15, 9);
	private final Switch south = new Switch(3, 11);

	private final Semaphore southStartCS = new Semaphore(1);
	private final Semaphore midWestCS = new Semaphore(1);
	private final Semaphore midUpperSectionCS = new Semaphore(1);
	private final Semaphore midEastCS = new Semaphore(1);
	private final Semaphore northStartCS = new Semaphore(1);
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
				// Train 2 starts on a critical section and must therefore acquire the semaphore
				// on start
				if (id == 2)
					southStartCS.acquire();

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
				// Will wait for a new event to be emitted, then proceed with actions below
				SensorEvent se = tsi.getSensor(this.id);

				// Performs necessary updates if train made any switch ACTIVE
				updateOnActive(se);

				// Performs necessary updates if train made any switch INACTIVE
				releaseOnInactive(se);

				// Will stop, wait and switch direction if train reached a station
				handleTrainReachedStation(se);
			}
		}

        /**
         * Handles all releases of semaphores. They are only released if sensor becomes inactive.
         */
        private void releaseOnInactive(SensorEvent se) {
        	releaseIfNeededFor(se, 6, 11, true, southStartCS);
        	releaseIfNeededFor(se, 15, 8, false, northStartCS);
        	
        	releaseIfNeededFor(se, 12, 9, true, midUpperSectionCS);
        	releaseIfNeededFor(se, 7, 9, false, midUpperSectionCS);
        	
        	releaseIfNeededFor(se, 11, 7, false, northIntersectionCS);
        	releaseIfNeededFor(se, 10, 8, false, northIntersectionCS);
        	releaseIfNeededFor(se, 8, 5, true, northIntersectionCS);
        	releaseIfNeededFor(se, 6, 6, true, northIntersectionCS);

        	releaseIfNeededFor(se, 13, 10, false, midEastCS);
        	releaseIfNeededFor(se, 12, 9, false, midEastCS);
        	releaseIfNeededFor(se, 15, 8, true, midEastCS);
        	releaseIfNeededFor(se, 14, 7, true, midEastCS);

        	releaseIfNeededFor(se, 6, 10, true, midWestCS);
        	releaseIfNeededFor(se, 7, 9, true, midWestCS);
        	releaseIfNeededFor(se, 6, 11, false, midWestCS);
        	releaseIfNeededFor(se, 4, 13, false, midWestCS);
        }
        
        /**
         * Checks the given sensor event, x, y and if the train is headed the direction asked for (going north or south), 
         * if so the semaphore will be released.
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
		 * Given a sensor event it will perform any needed actions for trains to go
		 * correctly.
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

			if (passedSensorOnActive(se, 14, 7)) {
				if (goingSouth()) {
					takeThenGo(midEastCS, north, false);
				}
			}

			if (passedSensorOnActive(se, 19, 8)) {
				if (goingNorth) {
					checkAndSwitch(northStartCS, north);
				}
			}

			if (passedSensorOnActive(se, 15, 8)) {
				if (goingSouth()) {
					takeThenGo(midEastCS, north, true);
				}
			}

			if (passedSensorOnActive(se, 17, 9)) {
				if (goingSouth()) {
					checkAndSwitch(midUpperSectionCS, midEast);
				}
			}

			if (passedSensorOnActive(se, 7, 9)) {
				if (goingSouth()) {
					takeThenGo(midWestCS, midWest, true);
				}
			}

			if (passedSensorOnActive(se, 1, 10)) {
				if (goingSouth()) {
					checkAndSwitch(southStartCS, south);
				}
			}

			if (passedSensorOnActive(se, 6, 11)) {
				if (goingNorth) {
					takeThenGo(midWestCS, south, true);
				}
			}

            if (passedSensorOnActive(se, 4, 13)) {
                if (goingNorth) {
                	takeThenGo(midWestCS, south, false);
                }
            }

            if (passedSensorOnActive(se, 1, 9)) {
                if (goingNorth) {
                    checkAndSwitch(midUpperSectionCS, midWest);
                }
            }
            if (passedSensorOnActive(se, 13, 10)) {
                if (goingNorth) {
                	takeThenGo(midEastCS, midEast, false);
                } 
            }
        }

		/**
		 * Handles the only intersection that needs logic to sync trains.
		 */
		private void handleIntersection(SensorEvent se) throws InterruptedException, CommandException {
			if (goingNorth) {
				handlePathIntoIntersection(se, 11, 7);
				handlePathIntoIntersection(se, 10, 8);
			}

			if (goingSouth()) {
				handlePathIntoIntersection(se, 8, 5);
				handlePathIntoIntersection(se, 6, 6);
			}
		}

		/**
		 * Will check that the intersection is available and wait if it is not.
		 */
		private void handlePathIntoIntersection(SensorEvent se, int x, int y)
				throws CommandException, InterruptedException {
			if (passedSensorOnActive(se, x, y)) {
				if (!northIntersectionCS.tryAcquire()) {
					halt();
					northIntersectionCS.acquire();
					go();
				}
			}
		}

		/**
		 * Tries to enter the shortest path if it is available, else take longer path.
		 * Enters new section if it is not a section where only one train can be
		 * present. Used where trains can take to different paths, for example in the
		 * middle where they can overtake each other.
		 */
		private void checkAndSwitch(Semaphore sem, Switch s) {
			if (!sem.tryAcquire()) {
				updateSwitch(s, false);
			} else {
				updateSwitch(s, true);
			}
		}

		/**
		 * Used when section to enter is a critical section. Because if it is acquired
		 * it will stop, wait for it to be acquired and then update the switch and start
		 * again.
		 */
		private void takeThenGo(Semaphore cs, Switch s, boolean shortestPath)
				throws InterruptedException, CommandException {
			if (!cs.tryAcquire()) {
				halt();
				cs.acquire();
				updateSwitch(s, shortestPath);
				go();
			} else {
				updateSwitch(s, shortestPath);
			}
		}

		/**
		 * Switches direction of train if it has reached any of the stations heading in
		 * the direction of the station
		 */
		private void handleTrainReachedStation(SensorEvent se) throws CommandException, InterruptedException {
			if (passedSensorOnActive(se, 15, 5) && goingNorth || passedSensorOnActive(se, 14, 11) && !goingNorth
					|| passedSensorOnActive(se, 14, 13) && !goingNorth
					|| passedSensorOnActive(se, 14, 3) && goingNorth) {
				switchDirection();
			}
		}

		/**
		 * Starts the train with the speed provided in constructor, i.e. the initial
		 * speed.
		 */
		private void go() throws CommandException {
			tsi.setSpeed(id, goingForward ? initialspeed : -initialspeed);
		}

		/**
		 * Stops the train.
		 */
		private void halt() throws CommandException {
			tsi.setSpeed(id, 0);
		}

		/**
		 * Switches the direction of the train. To be used when it reach a station.
		 */
		private void switchDirection() throws CommandException, InterruptedException {
			tsi.setSpeed(this.id, 0);
			Thread.sleep(2000);
			goingForward = !goingForward;
			go();
			this.goingNorth = !goingNorth;
		}

		/**
		 * Checks if train passes a given x y coordinate and if the event is active.
		 */
		private boolean passedSensorOnActive(SensorEvent se, int x, int y) {
			return samePos(se, x, y) && se.getStatus() == SensorEvent.ACTIVE;
		}

		/**
		 * Checks if train passes a given x y coordinate and if the event is inactive.
		 */
		private boolean passedSensorOnInactive(SensorEvent se, int x, int y) {
			return samePos(se, x, y) && se.getStatus() == SensorEvent.INACTIVE;
		}

		private boolean samePos(SensorEvent se, int x, int y) {
			return se.getXpos() == x && se.getYpos() == y;
		}

		/**
		 * Updates switch with respect to the shortest path of the train. If the train
		 * travels north or south the logic is different.
		 */
		private void updateSwitch(Switch s, boolean shortestPath) {
			if (goingNorth) {
				updateNorthGoingSwitches(s, shortestPath);
			} else {
				updateSouthGoingSwitches(s, shortestPath);
			}
		}

		/**
		 * Updates switch when train is traveling south.
		 */
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

		/**
		 * Updates switch when train is traveling south.
		 */
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

/**
 * Class to encapsulate a switch position and switch actions.
 */
class Switch {
	private final int x;
	private final int y;

	Switch(int x, int y) {
		this.x = x;
		this.y = y;
	}

	void setSwitchLeft() {
		try {
			TSimInterface.getInstance().setSwitch(x, y, TSimInterface.SWITCH_LEFT);
		} catch (CommandException e) {
			// e.printStackTrace();
		}
	}

	void setSwitchRight() {
		try {
			TSimInterface.getInstance().setSwitch(x, y, TSimInterface.SWITCH_RIGHT);
		} catch (CommandException e) {
			// e.printStackTrace();
		}

	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Switch))
			return false;
		Switch s = (Switch) obj;
		return this.x == s.x && this.y == s.y;
	}
}
