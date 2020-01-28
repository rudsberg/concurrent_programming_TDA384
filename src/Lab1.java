import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.ArrayList;
import java.util.List;
import TSim.SensorEvent;



public class Lab1 {
	
	List<Semaphore> semaphores          = new ArrayList();
	List<SensorObject> sensors          = new ArrayList();
	List<Integer> switches              = new ArrayList();

  public Lab1(int speed1, int speed2) {
    	
    	setupSensors();
    	setupSwitches();
    	//setupSemaphores();
  
    	Thread train1 = new Train(speed1,1);
    	Thread train2 = new Train(speed2,2);
    	train1.run();
    	train2.run();
    	
    
    }
  
  private void setupSensors() {
	sensors.add(new SensorObject(16,7,1));
	//sensors.add(new SensorObject(17,7,1));

	
}
  
  private void setupSwitches() {
	/*  switches.add(17,7);//  North
	  switches.add(15,9);//  Mideast
	  switches.add(4,9); //  Midwest
	  switches.add(3,11); // South*/
  }

private void addSemaphores( List<Semaphore> semaphores) {
	  semaphores.add(new Semaphore(1)); // NE crossroads into CS.
	  semaphores.add(new Semaphore(1));
	  
	 
  }
  
  public class Train extends Thread{
	  TSimInterface tsi = TSimInterface.getInstance();
	  int id;
	  int speed;
	  boolean direction;
	  List <Semaphore> holdingsemaphores = new ArrayList();
	  
	  
	  public Train (int speed, int id) {
		  this.speed = speed;
		  this.id = id;
	  }
	  @Override
	  public void run() {
		  
		  try {
			  tsi.setSpeed(id,speed);
			  while(!this.isInterrupted()) {
				  //read sensors and act accordingly
				  SensorEvent sensorevent = tsi.getSensor(this.id);
				  //This gets us the coordinates for the sensor, therefore we can probably use the coordinates in a data
				  //structure to know which sensor we are dealing with. Direction might not be that easy.
				  checkSensors(sensorevent);
				  
				  
				  if(sensorevent.getXpos() == 16 && sensorevent.getYpos()==7 && sensorevent.getStatus() == sensorevent.ACTIVE) {
					//  tsi.setSwitch(17, 7, 0);
					  	 tsi.setSpeed(this.id,0);
						 tsi.setSpeed(2, 15);
						 tsi.setSwitch(15, 9, 0);
				  }
				  System.out.print("x:" + sensorevent.getXpos() + "y:" + sensorevent.getYpos());
				  
			  }
			  
			  
		  }  catch (CommandException | InterruptedException e) {
		      e.printStackTrace();    // or only e.getMessage() for the error
		      System.exit(1);
		    }
	  }
	private void checkSensors(SensorEvent sensorevent) {
		// might be a switch case here which checks what sensor changed status and then tells the train to
		// act accordingly.
		
		
	}
	  
	  
  }

  public class SensorObject {
	  int x;
	  int y;
	  int status;
	  SensorObject(int x, int y, int status){
		  this.x = x;
		  this.y = y;
		  this.status = status;
	  }
	  public  int getX() {
		  return this.x;
	  }
	  public  int getY() {
		  return this.y;
	  }
	  public  int getStatus() {
		  return this.status;
	  }

  }
 
  
  	

}




