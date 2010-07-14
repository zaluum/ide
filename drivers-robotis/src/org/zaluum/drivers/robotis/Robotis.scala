package org.zaluum.drivers.robotis
import gnu.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
object Robotis {
	val AX_MODEL_NUMBER_L = 0
	val AX_MODOEL_NUMBER_H = 1
	val AX_VERSION = 2
	val AX_ID = 3
  val AX_BAUD_RATE = 4
  val AX_RETURN_DELAY_TIME = 5
  val AX_CW_ANGLE_LIMIT_L = 6
  val AX_CW_ANGLE_LIMIT_H = 7
  val AX_CCW_ANGLE_LIMIT_L = 8
  val AX_CCW_ANGLE_LIMIT_H = 9
  val AX_SYSTEM_DATA2 = 10
  val AX_LIMIT_TEMPERATURE = 11
  val AX_DOWN_LIMIT_VOLTAGE = 12
  val AX_UP_LIMIT_VOLTAGE = 13
  val AX_MAX_TORQUE_L = 14
  val AX_MAX_TORQUE_H = 15
  val AX_RETURN_LEVEL = 16
  val AX_ALARM_LED = 17
  val AX_ALARM_SHUTDOWN = 18
  val AX_OPERATING_MODE = 19
  val AX_DOWN_CALIBRATION_L = 20
  val AX_DOWN_CALIBRATION_H = 21
  val AX_UP_CALIBRATION_L = 22
  val AX_UP_CALIBRATION_H = 23
  val AX_TORQUE_ENABLE = 24
  val AX_LED = 25
  val AX_CW_COMPLIANCE_MARGIN = 26
  val AX_CCW_COMPLIANCE_MARGIN = 27
  val AX_CW_COMPLIANCE_SLOPE = 28
  val AX_CCW_COMPLIANCE_SLOPE = 29
  val AX_GOAL_POSITION_L = 30
  val AX_GOAL_POSITION_H = 31
  val AX_GOAL_SPEED_L = 32
  val AX_GOAL_SPEED_H = 33
  val AX_TORQUE_LIMIT_L = 34
  val AX_TORQUE_LIMIT_H = 35
  val AX_PRESENT_POSITION_L = 36
  val AX_PRESENT_POSITION_H = 37
  val AX_PRESENT_SPEED_L = 38
  val AX_PRESENT_SPEED_H = 39
  val AX_PRESENT_LOAD_L = 40
  val AX_PRESENT_LOAD_H = 41
  val AX_PRESENT_VOLTAGE = 42
  val AX_PRESENT_TEMPERATURE = 43
  val AX_REGISTERED_INSTRUCTION = 44
  val AX_PAUSE_TIME = 45
  val AX_MOVING = 46
  val AX_LOCK = 47
  val AX_PUNCH_L = 48
  val AX_PUNCH_H = 49
  
  // Status Return Levels
  val AX_RETURN_NONE = 0
  val AX_RETURN_READ = 1
  val AX_RETURN_ALL = 2
  
  // Instruction Set
  val AX_PING = 1
  val AX_READ_DATA = 2
  val AX_WRITE_DATA = 3
  val AX_REG_WRITE = 4
  val AX_ACTION = 5
  val AX_RESET = 6
  val AX_SYNC_WRITE = 131
  
  // # Broadcast Constant
  val AX_BROADCAST = 254
  
  // # Error Codes
  val AX_INSTRUCTION_ERROR = 64
  val AX_OVERLOAD_ERROR = 32
  val AX_CHECKSUM_ERROR = 16
  val AX_RANGE_ERROR = 8
  val AX_OVERHEATING_ERROR = 4
  val AX_ANGLE_LIMIT_ERROR = 2
  val AX_INPUT_VOLTAGE_ERROR = 1
  val AX_NO_ERROR = 0
  
  //# Static parameters
  val AX_RANGE = 300
  val AX_TICKS = 1024
  val AX_MAX_POSITION = 1023
  val AX_RAW_DEG_RATIO = 3.41333333333333333333     //# how many ticks in degree
  val AX_DEG_RAW_RATIO = 0.29296875                 //# how many degrees in a tick
  val AX_MAX_SPEED_DEG = 684.0                      //# degrees per second
  
  // Control Table Constants
  val AX_GOAL_POSITION = 1
  val AX_GOAL_SPEED = 2
  val AX_TORQUE_EN = 3
}
class Robotis(val port:String) {
	import Robotis._
	def calcChecksum(data:Buffer[Int]) = {
	  var sum = 0
	  for (i <- 2 until data.length-1)
	    sum+= data(i)
	  255 - (sum % 256)
	}
	
  val portIdentifier = CommPortIdentifier.getPortIdentifier(port);
  val (serialPort,in,out) = 
  	if (portIdentifier.isCurrentlyOwned)
      throw new Exception("Error: Port is currently in use")
    else {
    	portIdentifier.open(this.getClass().getName(),2000) match {
      	case serialPort : SerialPort => 
          serialPort.setSerialPortParams(57600,SerialPort.DATABITS_8,SerialPort.STOPBITS_1,SerialPort.PARITY_NONE);

          serialPort.enableReceiveTimeout(100)
          
          //serialPort.notifyOnDataAvailable(true)
          val in = serialPort.getInputStream();
          val out = serialPort.getOutputStream();
          (serialPort,in,out)
      	case _ => 
      		throw new Exception ("Serial port not found")
    	}
    }     
	def stop {
		in.close()
		out.close()
		serialPort.close()
	}
  def chr(i:Int) : Byte = (i & 0xFF).asInstanceOf[Byte]
  def uint(b:Int) : Int = 0xFF & b
  
  def readData : Buffer[Int] = {
    val data = Buffer[Int]()
    data.append(uint(in.read())) // 0 read 0xFF
    if (data(0) != 0xff) error("bad header" + data(0)) 
    data.append(uint(in.read)) // 1 read 0xFF
    if (data(1) != 0xff) error("bad header")
    data.append(uint(in.read)) // 2 read id
    data.append(uint(in.read)) // 3 read length
    for (i <- 0 until data(3)) 
      data.append(uint(in.read))
    //println ("read" + (data.map(_.toHexString) mkString(" ")))
    val readChecksum = uint(data.last)
    val calc = calcChecksum(data)
    if (calc!= (data.last.asInstanceOf[Int] & 0xFF))
      error ("checksum error: read " + readChecksum + " calc " + calc )
    data
  }
  def enterTossMode() {
    out.write('t')
    out.write('\n')
    out.flush
    //println("toss mode on")
  }
  def writePacket(servoId: Int, instruction: Int, params: Buffer[Int]){
    import scala.collection.mutable.ArrayBuilder
     //Number of bytes following standard header (0xFF, 0xFF, id, length)
    val length = 1 + params.length +1
    val packet = ArrayBuilder.make[Byte]
    packet.sizeHint(5)
    packet ++= Array(chr(0xff),chr(0xff),chr(servoId),chr(length),chr(instruction))
    val checksum = 255 - ((servoId + length + instruction + params.sum) % 256)
    packet ++= params map {chr(_)}
    packet += chr(checksum)
    val p = packet.result
    //in.read
    //println("writing " + ((p map {_.toHexString}) mkString(" ")))
    out.write(p)
    out.flush
  }
  def writeRead(servoId:Int, instruction:Int,params:Buffer[Int]) = {
    writePacket(servoId,instruction,params)
    // wait for response packet from AX-12+
    Thread.sleep(0,500000)
    val l = readData
    l
  }
	/**
	 *  Read "size" bytes of data from servo with "servoId" starting at the
	 *  register with "address". "address" is an integer between 0 and 49. It is
	 *  recommended to use the constants in module ax12_const for readability.
	 *  	To read the position from servo with id 1, the method should be called
	 *  	like:
	 *  		read_from_servo(1, AX_GOAL_POSITION_L, 2)
   */
	def read_from_servo(servoId : Int , address : Int, size: Int) : Buffer[Int] = {
    writePacket(servoId, AX_READ_DATA, Buffer(address,size))
    // wait
    Thread.sleep(0,500000)
    // read response
    readData
	}
	
  /**
   *  Write the values from the "data" list to the servo with "servoId"
	  starting with data[0] at "address", continuing through data[n-1] at
	  "address" + (n-1), where n = len(data). "address" is an integer between
	  0 and 49. It is recommended to use the constants in module ax12_const
	  for readability. "data" is a list/tuple of integers.
	
	  To set servo with id 1 to position 276, the method should be called
	  like:
		  read_from_servo(1, AX_GOAL_POSITION_L, (20, 1))
	 */
	def write_to_servo(servoId : Int , address : Int , toWrite : Buffer[Int]) = {
	  writeRead(servoId, AX_WRITE_DATA, Buffer(address) ++ toWrite)
	}
	
  /** Ping the servo with "servoId". This causes the servo to return a
      "status packet". This can tell us if the servo is attached and powered,
      and if so, if there is any errors.
        
      To ping the servo with id 1 to position 276, the method should be called
      like:
      ping_servo(1)
  */
	def ping_servo(servoId:Int)={ 
	  writeRead(servoId, AX_PING, Buffer())
	}
	/** Use Broadcast message to send multiple servos instructions at the
     same time. No "status packet" will be returned from any servos.
     "address" is an integer between 0 and 49. It is recommended to use the
     constants in module ax12_const for readability. "data" is a tuple of
     tuples. Each tuple in "data" must contain the servo id followed by the
     data that should be written from the starting address. The amount of
     data can be as long as needed.
     
     To set servo with id 1 to position 276 and servo with id 2 to position
     550, the method should be called like:
       sync_write_to_servos(AX_GOAL_POSITION_L, ( (1, 20, 1), (2 ,38, 2) ))
   */
	def sync_write_to_servos(address:Int, packetLen : Int , data:Buffer[Int]){
	  writePacket(AX_BROADCAST, AX_SYNC_WRITE, Buffer(address,data.size) ++ data)
	}
}