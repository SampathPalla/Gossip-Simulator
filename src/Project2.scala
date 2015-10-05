/**
 * Created by SampathYadav on 10/4/2015.
 */
import akka.actor._
import scala.math._
import scala.util.Random
import scala.concurrent.duration._

sealed trait Message

//MASTER AND NODE CASES
case class GossipMessage (network_node:ActorRef, selectedAction:String, noOfNodes:Int) extends Message
case class Initialize (loop_id:Int, neighbors:List[Int], network_nodes:List[ActorRef], master:ActorRef) extends Message
case class Terminate (id:Int, gossipCount:Int, start_time:Long) extends Message

//GOSSIP CLASS
case class Action_Gossip (id:Int, start_time:Long) extends Message
// PUSH SUM CLASS
case class Action_PushSum (sum:Double, weight:Double, start_time:Long) extends Message




object Project2 extends App{

  var noOfNodes = 0                //NUMBER OF INPUT NODES
  var selectedTopolgy =  ""       //THIS IS AN INPUT STRING WHICH ACCEPTS ONE OF THE FOLLOWING STRINGS : FULL, LINE, 3D, IMP3D
  var selectedAction = ""         //THIS IS AN INPUT STRING WHICH ACCEPTS ONE OF THE FOLLOWING STRINGS : GOSSIP, PUSHSUM

  if(args.length == 3)
    {
      noOfNodes = args(0).toInt
      selectedTopolgy = args(1).toString()
      selectedAction = args(2).toString()

      selectedTopolgy = selectedTopolgy.toUpperCase()
      selectedAction = selectedAction.toUpperCase()

        val system = ActorSystem("GossipSystem")
        var master : ActorRef =  system.actorOf(Props[Master], name = "master")
        var node : List[ActorRef] = Nil
        var neighbor_loop : Int = 0;
        var loop : Int = 0
        var rows : Double = 0

      println("SELECTED TOPOLGY : " + selectedTopolgy)

      if(selectedTopolgy == "FULL" || selectedTopolgy == "LINE")
      {
        while (loop < noOfNodes) {
          node ::= system.actorOf(Props[Nodes])
          loop += 1;
        }

        if (selectedTopolgy == "FULL") {
          while (neighbor_loop < node.length) {
            var neighbors: List[Int] = Nil
            var i: Int = 0
            while (i < node.length) {

              //FINDING NEIGHBORS FOR FULL GRID (ALL NODES ARE NEIGHBORS TO ALL NODES EXCEPT ITSELF)
              if (i != neighbor_loop) {
                neighbors ::= i
              }
              i += 1
            }
            node(neighbor_loop) ! Initialize(neighbor_loop, neighbors, node, master)
            neighbor_loop += 1
          }
        }
        else if (selectedTopolgy == "LINE") {
          while (neighbor_loop < node.length) {
            var neighbors: List[Int] = Nil

            //FINDING NEIGHBORS FOR LINE GRID
            if (neighbor_loop > 0)
              neighbors ::= (neighbor_loop - 1)
            if (neighbor_loop < (node.length - 1))
              neighbors ::= (neighbor_loop + 1)
            node(neighbor_loop) ! Initialize(neighbor_loop, neighbors, node, master)
            neighbor_loop += 1
          }
        }
      }
      else if(selectedTopolgy == "3D" || selectedTopolgy == "IMP3D")
      {
        var neighbors : List[Int] = Nil
        var RandomNeighbor : Int = -1
        var left : Double = 0
        var right : Double = 0
        var front : Double = 0
        var back : Double = 0
        var top : Double = 0
        var bottom : Double = 0

        //FIND THE NEAREST PERFECT CUBE
        noOfNodes= perfCube(noOfNodes)
        rows = math.cbrt(noOfNodes.toDouble)
        while (loop < noOfNodes){
          node ::= system.actorOf(Props[Nodes])
          loop += 1;
        }

        if(selectedTopolgy == "3D")
        {
          while(neighbor_loop < node.length){
            left = neighbor_loop - 1
            right = neighbor_loop + 1
            front = neighbor_loop - (rows * rows)
            back = neighbor_loop + (rows * rows)
            top = neighbor_loop - rows
            bottom = neighbor_loop + rows

            //FINDING NEIGHBORS FOR 3D GRID
            if (top >= 0)
              neighbors ::= top.toInt
            if (bottom < noOfNodes)
              neighbors ::= bottom.toInt
            if (front >= 0)
              neighbors ::= front.toInt
            if (back < noOfNodes)
              neighbors ::= back.toInt
            if (neighbor_loop%rows > 0)
              neighbors ::= left.toInt
            if (neighbor_loop%rows < (rows-1))
              neighbors ::= right.toInt

            node(neighbor_loop) !  Initialize(neighbor_loop,  neighbors, node , master)
            neighbor_loop += 1
          }
        }
        else if(selectedTopolgy == "IMP3D")
        {
          while(neighbor_loop < node.length){
            left = neighbor_loop - 1
            right = neighbor_loop + 1
            front = neighbor_loop - (rows * rows)
            back = neighbor_loop + (rows * rows)
            top = neighbor_loop - rows
            bottom = neighbor_loop + rows

            //FINDING NEIGHBORS FOR IMPERFECT 3D GRID
            if (top >= 0)
              neighbors ::= top.toInt
            if (bottom < noOfNodes)
              neighbors ::= bottom.toInt
            if (front >= 0)
              neighbors ::= front.toInt
            if (back < noOfNodes)
              neighbors ::= back.toInt
            if (neighbor_loop%rows > 0)
              neighbors ::= left.toInt
            if (neighbor_loop%rows < (rows-1))
              neighbors ::= right.toInt

            while(RandomNeighbor == -1){
              RandomNeighbor = Random.nextInt(node.length)
              for(i <- neighbors){
                if(RandomNeighbor == i){
                  RandomNeighbor = -1
                }
              }
            }
            neighbors ::= (RandomNeighbor)
            node(neighbor_loop) !  Initialize(neighbor_loop,  neighbors, node, master)
            neighbor_loop += 1
          }
        }
      }
      else
        {
          println("THIS TOPOLOGY DOES NOT EXIST. PLEASE SELECT ONE OF THE FOLLOWING:")
          println("1. FULL")
          println("2. LINE")
          println("3. 3D")
          println("4. IMP3D")
          System.exit(1)
        }

      def perfCube (num_Nodes : Int): Int = {
        var cube:Int = num_Nodes
        while(math.cbrt(cube.toDouble)%1 != 0){
          cube += 1
        }
        return cube
      }
      master ! GossipMessage(node(0), selectedAction, noOfNodes)
    }
  else
    {
      println("WRONG INPUT TYPE")
      println("PLEASE ENTER THE INPUT AS : scala Project2 noOfNodes topology action")
      System.exit(1)
    }

  //MASTER CLASS
  class Master extends Actor{
    var Total_Nodes : Int = 0
    var Nodes_Visited : List[Int] = Nil
    def receive={
      case GossipMessage (network_node:ActorRef, selectedAction:String, noOfNodes:Int) => {
        Total_Nodes = noOfNodes
        var start_time :Long = System.currentTimeMillis

        println("RUNNING SELECTED ACTION: '" + selectedAction)
        if(selectedAction == "GOSSIP"){
          network_node ! Action_Gossip (-1, start_time)
        }
        else
        if(selectedAction == "PUSHSUM"){
          network_node ! Action_PushSum (0, 1, start_time)
        }
        else{
          println("THIS ALGORITHM DOES NOT EXIST. PLEASE SELECT ONE OF THE FOLLOWING:")
          println("1. GOSSIP")
          println("2. PUSHSUM(WITHOUT SPACES)")
          System.exit(1)
        }

      }
      case Terminate (id:Int, gossipCount:Int, start_time:Long) => {

        var flag:Int = 1
        var i:Int = 0
        while(i < Nodes_Visited.length) {
          if(Nodes_Visited(i) == id) {
            flag = 0
          }
          i += 1
        }
        if (flag == 1) {
          Nodes_Visited ::= id
        }
        if(Nodes_Visited.length == Total_Nodes) {
          println("TIME = " + (System.currentTimeMillis - start_time) + "ms")
          context.system.shutdown()
          System.exit(1)
        }
      }
      case _ => println("Hi")
    }
  }

  //NODE CLASS
  class Nodes extends Actor{
    var master:ActorRef = null
    var neighbors_List:List[Int] = Nil
    var RumourCount:Int = 0
    var Delta_Difference:Int = 0
    var LoopID:Int = 0
    var node:List[ActorRef] = Nil
    var network_nodes:List[Int] =Nil
    var node_with_rumour:List[Int] = Nil
    var sum:Double = 0
    var weight:Double = 0
    var next_round_timer: Cancellable = _
    val system = context.system
    var RandomNeighbor:Int = 0

    def receive={
      case Initialize(loop_id:Int,  neighbors:List[Int], network_nodes:List[ActorRef], master_ref:ActorRef) => {
        neighbors_List = neighbors_List ::: neighbors
        LoopID = loop_id
        master = master_ref
        sum = loop_id
        node = network_nodes
      }
      case Action_Gossip(callerId:Int, start_time:Long) => {
        if (RumourCount < 10) {

          if (callerId != LoopID) {
            RumourCount += 1
            master ! Terminate(LoopID, RumourCount, start_time)
          }

          RandomNeighbor = Random.nextInt(neighbors_List.length)
          node(neighbors_List(RandomNeighbor)) ! Action_Gossip(LoopID, start_time)
          self ! Action_Gossip(LoopID, start_time)

        }
        else {
          context.stop(self)
        }
      }
      case Action_PushSum(new_sum:Double, new_weight:Double, start_time:Long) => {

        RumourCount += 1
        var oldratio:Double = sum/weight

        sum += new_sum
        weight += new_weight
        sum = sum/2
        weight = weight/2
        var newratio:Double = sum/weight

        if ((RumourCount == 1) || (Math.abs((oldratio-newratio)) > math.pow(10, -10))) {

          Delta_Difference=0
          RandomNeighbor = Random.nextInt(neighbors_List.length)
          node(neighbors_List(RandomNeighbor)) ! Action_PushSum(sum, weight, start_time)

        } else {

          Delta_Difference += 1
          if (Delta_Difference > 3) {
            println("SUM =" + newratio)
            println("TIME = " + (System.currentTimeMillis-start_time) + "ms")
            System.exit(1)
          } else {
            RandomNeighbor = Random.nextInt(neighbors_List.length)
            node(neighbors_List(RandomNeighbor)) ! Action_PushSum(sum, weight, start_time)
          }
        }
      }
      case _ => println("BYE")
    }
  }
}
