package org.zaluum.drivers.robotis
import scala.collection.mutable.Buffer
import Robotis._
/**
 * Ported from 
 * http://code.google.com/p/ua-ros-pkg/source/browse/trunk/arrg/ua_drivers/ax12_driver_core/src/ax12_driver_core/
 *
 */
class AX12(port:String, baud:Int = 57600){
	var ser : Robotis  = null 
	def start {
		ser = new Robotis(port,baud)
	}
	def stop {
		if (ser!=null) 
			ser.stop
		ser = null
	}
	def enterTossMode() = ser.enterTossMode()
  def test_bit(number : Int, offset: Int) = (number & (1 << offset)) !=0
  def hi(i:Int) = (i >> 8) & 0xff
  def lo(i:Int) = i & 0xff
  def lohi(i:Int) : (Int,Int) = (lo(i),hi(i))
  def slohi(i:Int) : (Int,Int) = { 
  	if (i >=0)
  		lohi(i)
  	else 
  		(lo(1023-i),hi(1023-i))
  }
  def int(lo:Int, hi: Int ) = lo + (hi<<8)
  def sint(lo:Int,hi:Int) = {
    val s = int(lo,hi)
    if (s>1023)
      1023-s
    else s
  }
  
  def err(data : Buffer[Int], msg:String) : Buffer[Int] = {
		if (data.length<5) throw new Exception("Error message too short " + msg)
		else {
			val ec = data(4)
			def check(mask:Int,s:String) {
				if ((ec & mask)!=0) 
					throw new Exception(s + " " + msg)
			}
			check(AX_INSTRUCTION_ERROR,"AX_INSTRUCTION_ERROR")
  		check(AX_OVERLOAD_ERROR,"AX_OVERLOAD_ERROR")
  		check(AX_CHECKSUM_ERROR,"AX_CHECKSUM_ERROR")
  		check(AX_RANGE_ERROR,"AX_RANGE_ERROR")
  		check(AX_OVERHEATING_ERROR,"AX_OVERHEATING_ERROR")
  		check(AX_ANGLE_LIMIT_ERROR,"AX_ANGLE_LIMIT_ERROR")
  		check(AX_INPUT_VOLTAGE_ERROR,"AX_INPUT_VOLTAGE_ERROR")
  		data
		}
	}
  
  def ping(servoId : Int) = err(ser.ping_servo(servoId),"Pinging " + servoId)
   
	/**
      Set the servo with servoId to the specified goal speed.
      Speed can be negative only if the dynamixel is in "freespin" mode.
  */
  def set_servo_speed(servoId:Int, speed:Int)={
      // split speed into 2 bytes
  		val (loVal,hiVal) = slohi(speed)
      // set two register values with low and high byte for the speed
      val response = ser.write_to_servo(servoId, AX_GOAL_SPEED_L,
                                           Buffer(loVal, hiVal))
      err(response, "Writing servo "+servoId+" speed " +  speed)
  }
  /** 
      Set the servo with servoId to the specified goal position.
      Position value must be positive.
  */
  def set_servo_position(servoId : Int, position:Int)={
      // split position into 2 bytes
  		val (loVal,hiVal) = lohi(position)
      // set two register values with low and high byte for the position
      val response = ser.write_to_servo(servoId, AX_GOAL_POSITION_L,
                                           Buffer(loVal, hiVal))
      err(response, "Setting servo with id "+servoId+" to position " + position)
  }
  /**
      Set the servo with servoId to specified position and speed.
      Speed can be negative only if the dynamixel is in "freespin" mode.
   */
  def set_servo_position_and_speed(servoId : Int, position : Int, speed : Int)={
      // split speed into 2 bytes
  		val (loSpeedVal, hiSpeedVal) = slohi(speed)
  		// split position into 2 bytes
  		val (loPositionVal, hiPositionVal) = lohi(position)
      val response = ser.write_to_servo(servoId, AX_GOAL_POSITION_L,
                                           Buffer(loPositionVal, hiPositionVal,
                                            loSpeedVal, hiSpeedVal))
      err(response, "setting servo with id "+ servoId+" to position "+position+" and speed "+speed) 
  }
  /**
    Set different speeds for multiple servos.
    Should be called as such:
  	set_multi_servo_speeds( ( (id1, speed1), (id2, speed2), (id3, speed3) ) )
  */
  def set_multi_servo_speeds(speeds:Buffer[(Int,Int)])={
      // prepare value tuples for call to syncwrite
      val writeableVals = Buffer[Int]()
      for ((sid,speed) <- speeds){
        // split speed into 2 bytes
      	val (loVal,hiVal) = slohi(speed)
        writeableVals ++= Buffer(sid, loVal, hiVal) 
      }
      // use sync write to broadcast multi servo message
      ser.sync_write_to_servos(AX_GOAL_SPEED_L, 3, writeableVals)
  }
  /**
      Set different positions for multiple servos.
      Should be called as such:
      set_multi_servo_positions( ( (id1, position1), (id2, position2), (id3, position3) ) )
   */
  def set_multi_servo_positions(values : Buffer[(Int,Int)]){
    // prepare value tuples for call to syncwrite
    val writeableVals = Buffer[Int]()
    for ((sid, position)<- values){
      // split position into 2 bytes
      val (loVal, hiVal) = lohi(position)
      writeableVals ++= Buffer(sid, loVal, hiVal) 
    }
    // use sync write to broadcast multi servo message
    ser.sync_write_to_servos(AX_GOAL_POSITION_L,3, writeableVals)
  }
  /**
    Set different positions and speeds for multiple servos.
    Should be called as such:
    set_multi_servo_speeds( ( (id1, position1, speed1), (id2, position2, speed2), (id3, position3, speed3) ) )
  */
  def set_multi_servo_positions_and_speeds(values : Buffer[(Int,Int,Int)]){
    // prepare value tuples for call to syncwrite
    val writeableVals = Buffer[Int]()
    for ((sid,position,speed) <- values){
      // split speed into 2 bytes
    	val (loSpeedVal, hiSpeedVal) = slohi(speed)
      // split position into 2 bytes
      val (loPositionVal, hiPositionVal) = lohi(position)
      writeableVals ++= Buffer(sid, loPositionVal, hiPositionVal, loSpeedVal, hiSpeedVal) 
    }
    // use sync write to broadcast multi servo message
    ser.sync_write_to_servos(AX_GOAL_POSITION_L,5, writeableVals)
  }
  /**
    Set the min and max angle of rotation limits.
    NOTE: the absolute min is 0 and the absolute max is 300
   */
  def set_min_max_angle_limits(servoId : Int , minAngle : Int, maxAngle : Int)={
      val (loMinVal,hiMinVal) = lohi(minAngle)
      val (loMaxVal,hiMaxVal) = lohi(maxAngle)
      // set 4 register values with low and high bytes for min and max angles
      val response = ser.write_to_servo(servoId, AX_CW_ANGLE_LIMIT_L,
                                    Buffer(loMinVal, hiMinVal, loMaxVal, hiMaxVal))
      err(response, " when setting servo with id "+servoId+" to min angle "+minAngle+
      			" and max angle " + maxAngle)
  }
  /**
    Sets the value of the torque enabled register to 1 or 0. When the
  	torque is disabled the servo can be moved manually while the motor is
  	still powered.
  */
  def set_torque_enabled(servoId : Int, enabled:Boolean)={
      val response = ser.write_to_servo(servoId, AX_TORQUE_ENABLE,
                                           Buffer(if (enabled)1 else 0))
      err (response, " when setting servo with id "+servoId+" to torque_enabled " + enabled)
  }

  /**
    Method to set multiple servos torque enabled.
    Should be called as such:
  	set_multi_servos_to_torque_enabled( (id1, id2, id3), True)
  */
  def set_multi_servos_to_torque_enabled(servoIds : Buffer[Int], enabled:Boolean){
      // prepare value tuples for call to syncwrite
      val writeableVals = Buffer[Int]()
      for (sid <- servoIds)
        writeableVals ++= Buffer(sid, if (enabled) 1 else 0)
      // use sync write to broadcast multi servo message
      ser.sync_write_to_servos(AX_TORQUE_ENABLE,2, writeableVals)
  }
  
  /**
  	Reads the servo's speed value from its registers.
   */
  def get_servo_speed(servoId : Int)={
	  val response = ser.read_from_servo(servoId, AX_PRESENT_SPEED_L, 2)
	  err(response, " when getting speed of servo with id "+servoId)
	  sint( response(5) , response(6))
  }
  /** 
   *  Reads the servo's position value from its registers. """
   */
  def get_servo_position(servoId:Int)={
      val response = ser.read_from_servo(servoId, AX_PRESENT_POSITION_L, 2)
      err(response," when getting position of servo with id " + servoId)
      int (response(5), response(6))
  }
  /**
    Returns the min and max angle limits from the specified servo.
  */
  def get_min_max_angle_limits(servoId:Int)={
      // read in 4 consecutive bytes starting with low value of clockwise angle limit
      val response = ser.read_from_servo(servoId, AX_CW_ANGLE_LIMIT_L, 4)
      err(response, "when getting min/max angle limits of servo with id " + servoId)
      // extract data valus from the raw data
      (int(response(5), response(6)), int(response(7), response(8)))
  }
  
  /**
      Returns the position, speed, load, voltage, and temperature values
      from the specified servo.
   */
  def get_servo_feedback(servoId : Int) = {
      // read in 8 consecutive bytes starting with low value for position
      val response = ser.read_from_servo(servoId, AX_PRESENT_POSITION_L, 11)
      err(response, " when getting feedback from servo with id " + servoId)
      // extract data values from the raw data
      val position = int (response(5), response(6))
      val speed = sint(response(7), response(8))
      val load_raw = int(response(9), response(10))
      val load_direction = if (test_bit(load_raw, 10)) 1 else 0
      val load = (load_raw & 0x3FF)
      val sload = if (load_direction==1) -load else load
      val voltage = response(11)
      val temperature = response(12)
      val moving = response(15)
      RobotisFeedback (servoId, position, speed, sload, voltage, temperature, moving)          
  }
  
}
case class RobotisFeedback(id : Int, position : Int, speed : Int, sload: Int, voltage: Int, temperature: Int, moving: Int)