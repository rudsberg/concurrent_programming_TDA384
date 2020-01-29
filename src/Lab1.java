import TSim.*;

import java.util.concurrent.Semaphore;
import java.util.ArrayList;
import java.util.List;


public class Lab1 {

    List<Semaphore> semaphores = new ArrayList();
    private final TSimInterface tsi = TSimInterface.getInstance();
    private final Switch north = new Switch(17, 7,true);
    private final Switch midWest = new Switch(4, 9,false);
    private final Switch midEast = new Switch(15, 9,true);
    private final Switch south = new Switch(3, 11,false);
    private final Semaphore semaphore  = new Semaphore(1);

    public Lab1(int speed1, int speed2) {

        //setupSemaphores();

        Thread train1 = new Train(speed1, 1, false);
        Thread train2 = new Train(speed2, 2, true);
        try {
            semaphore.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

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
        boolean goingNorth;
        List<Semaphore> holdingsemaphores = new ArrayList();
        boolean ack;


        public Train(int speed, int id, boolean goingNorth) {
            this.speed = speed;
            this.id = id;
            this.goingNorth = goingNorth;
        }

        @Override
        public void run() {

            try {
                tsi.setSpeed(id, speed);
                while (!this.isInterrupted()) {
                    updateSensors();
                }
            } catch (CommandException e) {

                e.printStackTrace();
            }
        }

        private void updateSensors() {

            try {
                SensorEvent se = tsi.getSensor(this.id);
                if (passedSensor(se,14,9)) {
                    updateSwitch(midEast);
                }
                if (passedSensor(se, 16, 7)) {
                    updateSwitch(north);
                }

                if(passedSensor(se,16,9) ){
                    updateSwitch(midEast);
                }
                if(passedSensor(se,2,12) ){
                    updateSwitch(south);
                    
                }
                if (passedSensor(se, 15, 5) && goingNorth) {
                    switchDirection();
                }
                if(passedSensor(se,16,11) && !goingNorth){
                    switchDirection();
                }
                if(passedSensor(se,15,13) && !goingNorth){
                    switchDirection();
                }
                if(passedSensor(se,14,3) && goingNorth){
                    switchDirection();
                }

            } catch (CommandException | InterruptedException e) {
                e.printStackTrace();
            }
        }

        private void switchDirection() throws CommandException, InterruptedException {
            int currentSpeed = this.speed;
            System.out.println("Current speed " + speed);
            tsi.setSpeed(this.id, 0);
            Thread.sleep(2000);
            tsi.setSpeed(this.id, goingNorth ? -currentSpeed : currentSpeed);
            this.goingNorth = !goingNorth;
        }

        private boolean passedSensor(SensorEvent se, int x, int y) {
            return se.getXpos() == x && se.getYpos() == y;
        }

        private void updateSwitch(Switch s) {
            if(goingNorth){
                updateNorthGoingSwitches(s);
            }else{
                updateSouthGoingSwitches(s);
            }
        }

        private void updateSouthGoingSwitches(Switch s) {

            if(s.equals(north)){
                s.setSwitchRight();
            }else if(s.equals(midEast)){
                s.setSwitchRight();
            }else if(s.equals(midWest)){
                s.setSwitchRight();
            }else{
                s.setSwitchRight();
            }
        }


        private void updateNorthGoingSwitches(Switch s) {
            if(s.equals(north)){
                s.setSwitchLeft();
            }else if(s.equals(midEast)){
                s.setSwitchRight();
            }else if(s.equals(midWest)){
                s.setSwitchLeft();
            }else{
                s.setSwitchLeft();
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
        this.leftTurn= leftTurn;
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

    boolean isLeftTurn(){
        return leftTurn;
    }

    boolean isRightTurn(){
        return !leftTurn;
    }

    @Override
    public boolean equals(Object obj) {
        if(!(obj instanceof Switch)) return  false;
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






