import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.ArrayList;
import java.util.List;



public class Lab1 {

	List<Semaphore> semaphores          = new ArrayList();
	List<SensorObject> sensors          = new ArrayList();
	private final TSimInterface tsi = TSimInterface.getInstance();
	private final Switch north   = new Switch(17,7);
	private final Switch midWest = new Switch(4,9);
	private final Switch midEast = new Switch(15,9);
	private final Switch south   = new Switch(3,11);

	public Lab1(int speed1, int speed2) {

		setupSensors();
		//setupSemaphores();

		Train train1 = new Train(speed1,1,false);
		Train train2 = new Train(speed2,2,true);
		// loop 
		train1.run();
		train2.run();
		System.out.print("test");

		while(true /* TODO */) {
			
			// Depending on app state change the following accordingly:
			// 1) Train, stop or go
			// 2) Switches, left or right 

			// These change depending on
			// - Semaphores. Each sema is connected between each pair of sensors that are connected. 

			// Trains and semaphores
			// - if train owns a semaphore and passes the next sensor it must release the semaphore
			// 


			//read sensors and act accordingly

			//This gets us the coordinates for the sensor, therefore we can probably use the coordinates in a data
			//structure to know which sensor we are dealing with. Direction might not be that easy.
			//checkSensors(sensorevent);

			updateSensors(train1.getID(),train1.goingNorth);
			updateSensors(train2.getID(),train2.goingNorth);

		}



	}

	private void updateSensors(int trainID,boolean goingNorth) {
		try {
			SensorEvent se = tsi.getSensor(trainID);
			System.out.println(se.getXpos() + " " + se.getYpos());
			
			if(true ) {//se.getXpos() == 14 && se.getYpos() == 9 && goingNorth) {
				midEast.setSwitchLeft();
			}
			
			
			
		} catch (CommandException | InterruptedException e) {
			e.printStackTrace();
			System.exit(1);
			System.out.print("hej");

		}
	}

	private void setupSensors() {
		sensors.add(new SensorObject(16,7,1));
		//sensors.add(new SensorObject(17,7,1));


	}



	private void addSemaphores( List<Semaphore> semaphores) {
		semaphores.add(new Semaphore(1)); // NE crossroads into CS.
		semaphores.add(new Semaphore(1));


	}

	public class Train extends Thread {
		TSimInterface tsi = TSimInterface.getInstance();
		int id;
		int speed;
		boolean goingNorth;
		List <Semaphore> holdingsemaphores = new ArrayList();


		public Train (int speed, int id,boolean goingNorth) {
			this.speed = speed;
			this.id = id;
			this.goingNorth = goingNorth;
		}
		@Override
		public void run() {

			try {
				tsi.setSpeed(id,speed);
			}  catch (CommandException e) {
				e.printStackTrace();    // or only e.getMessage() for the error
				System.exit(1);
			}
		}
		
		public int getID() {
			return id;
		}
		public boolean isGoingNorth() {
			return goingNorth;
		}

	}

	public class SensorObject {
		private final Position pos;
		private int status;
		SensorObject(int x, int y, int status){
			this.pos = new Position(x, y);
			this.status = status;
		}
		public int getX() {
			return this.pos.getX();
		}
		public  int getY() {
			return this.pos.getY();
		}
		public  int getStatus() {
			return this.status;
		}

	}

	private class Switch {
		private final Position pos;
		private int status;

		Switch(int x, int y){
			this.pos = new Position(x, y);
			this.status =0;
		}

		void setSwitchLeft(){
			try {
				TSimInterface.getInstance().setSwitch(pos.x, pos.y, TSimInterface.SWITCH_LEFT);
			} catch (CommandException e) {
				e.printStackTrace();
			}
		}
		void setSwitchRight(){
			try {
				TSimInterface.getInstance().setSwitch(pos.x, pos.y, TSimInterface.SWITCH_RIGHT);
			} catch (CommandException e) {
				e.printStackTrace();
			}
		}


	}

	class Position {
		final int x;
		final int y;

		Position(int x, int y) {
			this.x = x;
			this.y = y;
		}

		public  int getX() {
			return this.x;
		}
		public  int getY() {
			return this.y;
		}
	}

}




