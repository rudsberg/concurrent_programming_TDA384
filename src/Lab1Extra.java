import TSim.*;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Lab1Extra {

	private final Switch north = new Switch(17, 7);
	private final Switch midWest = new Switch(4, 9);
	private final Switch midEast = new Switch(15, 9);
	private final Switch south = new Switch(3, 11);

	private final Track midEastTrack = new Track();
	private final Track southStartTrack = new Track();
	private final Track midWestTrack = new Track();
	private final Track midUpperTrack = new Track();
	private final Track northStartTrack = new Track();
	private final Track northIntersectionTrack = new Track();

	public Lab1Extra(int speed1, int speed2) {
		Thread train1 = new Thread(new Train(speed1, 1, false));
		Thread train2 = new Thread(new Train(speed2, 2, true));

		train1.start();
		train2.start();
	}

	/**
	 * Track is a critical section where only one train may be inside at a time. 
	 */
	class Track {
		private final Lock lock = new ReentrantLock();
		private final Condition occupied = lock.newCondition();
		private boolean isOccupied = false;

		/**
		 * Enters a track. If it occupied the train will stop and then go to into WAITING
		 * state until it is woken up by another train leaving the track. If it is not 
		 * occupied it will simply enter the track.
		 */
		void enter(Train t) throws CommandException {
			lock.lock();
			try {
				while (isOccupied) {
					t.halt();
					occupied.await();
				}
				isOccupied = true;
			} catch (InterruptedException e) {
				e.printStackTrace();
			} finally {
				lock.unlock();
			}
		}

		/**
		 * Leaves the track and signals any waiting train that is currently waiting to enter.
		 */
		void leave() {
			lock.lock();
			try {
				isOccupied = false;
				occupied.signal();
			} finally {
				lock.unlock();
			}
		}

		/**
		 * If track is already occupied by another train. This can for example be used by another train 
		 * that only needs to know the status of the track in order to determine what route to take 
		 * (if it not only has one option to take).
		 */
		boolean isOccupied() {
			return this.isOccupied;
		}
	}

	public class Train implements Runnable {
		private final TSimInterface tsi = TSimInterface.getInstance();
		private int id;
		private int speed;
		private final int initialspeed;
		private boolean goingNorth;
		private boolean goingForward = true;

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
					southStartTrack.enter(this);

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
				// Will wait for a new sensor event to be emitted for this train, then proceed
				// with actions below
				SensorEvent se = tsi.getSensor(this.id);

				// Performs necessary updates if train made any switch ACTIVE
				updateOnActive(se);

				// Performs necessary updates if train made any switch INACTIVE
				leaveOnInactive(se);

				// Will stop, wait and switch direction if train reached a station
				handleTrainReachedStation(se);
			}
		}

		/**
		 * If train passes a sensor in a particular direction that becomes inactive and
		 * it was previously in a CS, the train will leave the given monitor.
		 */
		private void leaveOnInactive(SensorEvent se) {
			if (passedSensorOnInactive(se, 6, 11) && goingNorth) {
				southStartTrack.leave();
			}
			if (passedSensorOnInactive(se, 15, 8) && goingSouth()) {
				northStartTrack.leave();
			}

			if (passedSensorOnInactive(se, 12, 9) && goingNorth || passedSensorOnInactive(se, 7, 9) && goingSouth()) {
				midUpperTrack.leave();
			}

			if (passedSensorOnInactive(se, 11, 7) && goingSouth() || passedSensorOnInactive(se, 10, 8) && goingSouth()
					|| passedSensorOnInactive(se, 8, 5) && goingNorth
					|| passedSensorOnInactive(se, 6, 6) && goingNorth) {
				northIntersectionTrack.leave();
			}

			if (passedSensorOnInactive(se, 15, 8) && goingNorth || passedSensorOnInactive(se, 14, 7) && goingNorth
					|| passedSensorOnInactive(se, 13, 10) && goingSouth()
					|| passedSensorOnInactive(se, 12, 9) && goingSouth()) {
				midEastTrack.leave();
			}

			if (passedSensorOnInactive(se, 6, 10) && goingNorth || passedSensorOnInactive(se, 7, 9) && goingNorth
					|| passedSensorOnInactive(se, 6, 11) && goingSouth()
					|| passedSensorOnInactive(se, 4, 13) && goingSouth()) {
				midWestTrack.leave();
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
					midWestTrack.enter(this);
					takeThenGo(midWest, false);
				}
			}

			if (passedSensorOnActive(se, 12, 9)) {
				if (goingNorth) {
					midEastTrack.enter(this);
					takeThenGo(midEast, true);
				}
			}

			if (passedSensorOnActive(se, 14, 7)) {
				if (goingSouth()) {
					midEastTrack.enter(this);
					takeThenGo(north, false);
				}
			}

			if (passedSensorOnActive(se, 19, 8)) {
				if (goingNorth) {
					if (northStartTrack.isOccupied()) {
						updateSwitch(north, false);
					} else {
						updateSwitch(north, true);
						northStartTrack.enter(this);
					}
				}
			}

			if (passedSensorOnActive(se, 15, 8)) {
				if (goingSouth()) {
					midEastTrack.enter(this);
					takeThenGo(north, true);
				}
			}

			if (passedSensorOnActive(se, 18, 9)) {
				if (goingSouth()) {
					if (midUpperTrack.isOccupied()) {
						updateSwitch(midEast, false);
					} else {
						updateSwitch(midEast, true);
						midUpperTrack.enter(this);
					}
				}
			}

			if (passedSensorOnActive(se, 7, 9)) {
				if (goingSouth()) {
					midWestTrack.enter(this);
					takeThenGo(midWest, true);
				}
			}

			if (passedSensorOnActive(se, 1, 10)) {
				if (goingSouth()) {
					if (southStartTrack.isOccupied) {
						updateSwitch(south, false);
					} else {
						southStartTrack.enter(this);
						updateSwitch(south, true);
					}
				}
			}

			if (passedSensorOnActive(se, 6, 11)) {
				if (goingNorth) {
					midWestTrack.enter(this);
					takeThenGo(south, true);
				}
			}

			if (passedSensorOnActive(se, 4, 13)) {
				if (goingNorth) {
					midWestTrack.enter(this);
					takeThenGo(south, false);
				}
			}

			if (passedSensorOnActive(se, 1, 9)) {
				if (goingNorth) {
					if (midUpperTrack.isOccupied()) {
						updateSwitch(midWest, false);
					} else {
						updateSwitch(midWest, true);
						midUpperTrack.enter(this);
					}
				}
			}
			if (passedSensorOnActive(se, 13, 10)) {
				if (goingNorth) {
					midEastTrack.enter(this);
					takeThenGo(midEast, false);
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
				northIntersectionTrack.enter(this);
				go();
			}
		}

		/**
		 * Simply updates the switch according to shortest path and the starts train.
		 */
		private void takeThenGo(Switch s, boolean shortestPath) throws CommandException {
			updateSwitch(s, shortestPath);
			go();
		}

		/**
		 * Switches direction of train if it has reached any of the stations heading in
		 * the direction of the station
		 */
		private void handleTrainReachedStation(SensorEvent se) throws CommandException, InterruptedException {
			if (passedSensorOnActive(se, 15, 5) && goingNorth || passedSensorOnActive(se, 14, 11) && goingSouth()
					|| passedSensorOnActive(se, 14, 13) && goingSouth()
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
