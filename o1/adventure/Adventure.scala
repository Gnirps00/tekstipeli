package o1.adventure


/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure {

  /** The title of the adventure game. */
  val title = "A Palace Adventure"

  private val start = new Area("Bedroom", "You are somewhere in the old palace. There are a lot of books on the bookshelf.", false, "", true)
  private val secretRoom = new Area("Secret Room", "You've found a secret room!! You can see also an another door in east.", true, "", true)
  private val finalRoom = new Area("Final Room", "There seems to be a board with question...", true, "The master of Merlin is:\n目覚める(2)赤(1)と共に(3)と共に(4)青(3)赤(1)", false, true, true)
  private val northCorridor = new Area("North Corridor", "You are at a corridor in the middle of palace. The corridor continues to South and there are also rooms beside the corridor.\nAnd you can see also a board with some sentences.", false, 
                              "The way to North opens, when you use the old golden key.", false, false, false, true)
  private val room1 = new Area("Room", "You are in a normal room. It's quite cold here.")
  private val southCorridor = new Area("South Corridor", "You are at a corridor in the middle of palace. The corridor continues to North and there are also rooms beside the corridor.", false)
  private val room2 = new Area("Merlin's Room", "You are in a glorious room. It's quite clean here. There is a paper with some text.", false, 
                              "赤、青、緑の宝玉と共に私は目覚める")
  private val kitchen = new Area("Kitchen", "You are in a kitchen. There is a stairs to go down.")
  private val basement = new Area("Basement", "You are in a basement. There is a man sleeping on the bed. You can see 3 holes.", false, "", false, false, false, false, true, 3)
  private val bigRoom = new Area("Big Room", "You are in a big room. You can see a man standing nearby the door.")
  private val garden = new Area("Garden", "You are in a garden. You can see a flower garden on the east of the garden, but there is a stream between them so you can't go there on foot.", false, "", true)
  private val flowerGarden = new Area("Flower Garden", "You are in a small flower garden. There are a lot of colorful flowers.", true)
  private val teleportRoom = new Area("Teleport Room", "You are in a teleport room. There is a teleport machine to teleport to the real world.", true)
  private val destination = teleportRoom

  start.setNeighbors(Vector("north" -> secretRoom, "east" -> northCorridor))
  secretRoom.setNeighbors(Vector("east" -> finalRoom, "south" -> start))
  finalRoom.setNeighbors(Vector("north" -> teleportRoom, "south" -> northCorridor, "west" -> secretRoom   ))
  northCorridor.setNeighbors(Vector("north" -> finalRoom, "east" -> room1, "south" -> southCorridor, "west" -> start))
  southCorridor.setNeighbors(Vector("north" -> northCorridor, "east" -> kitchen,   "south" -> bigRoom, "west" -> room2))
  room1.setNeighbors(Vector("west" -> northCorridor))
  room2.setNeighbors(Vector("east" -> southCorridor, "south" -> bigRoom))
  kitchen.setNeighbors(Vector("west" -> southCorridor, "down" -> basement))
  basement.setNeighbors(Vector("up" -> kitchen))
  bigRoom.setNeighbors(Vector("north" -> southCorridor, "south" -> garden, "west" -> room2))
  garden.setNeighbors(Vector("north" -> bigRoom, "east" -> flowerGarden))
  flowerGarden.setNeighbors(Vector("west" -> garden))
  teleportRoom.setNeighbors(Vector("South" -> finalRoom))

  private val blueGem = new Gem("blue gem", "It's a blue gem. Looks beautiful.")
  private val redGem = new Gem("red gem", "It's a red gem. Looks beautiful.")
  private val greenGem = new Gem("green gem", "It's a green gem. Looks beautiful.")
  private val key = new Key("key", "It's an old golden key.\nWhat it was doing in the flower garden, you have no idea.")
  private val dictionary = new Item("dictionary", "It's a japanese-english dictionary. 目覚める -> wake up, 宝玉 -> gem, 私 -> I, 赤 -> red, 青 -> blue, 緑 -> green, と共に -> with.")
  
  private val merlin = new Person("merlin", "There is a flower garden in east of the garden. I've hidden a key for the final room there. And to open the way to the flower garden, you have to use spell 'Abrakadabra direction'.", false)
  private val steward = new Person("steward", "My master used to study Japanese when he was young...", true)
  
  /**
   *  Add items to areas
   */
  room1.addItem(blueGem)
  southCorridor.addItem(greenGem)
  bigRoom.addItem(redGem)
  flowerGarden.addItem(key)
  start.addItem(dictionary)
  
  /**
   *  Add people to area
   */
  basement.addPerson(merlin)
  bigRoom.addPerson(steward)

  /** The character that the player controls in the game. */
  val player = new Player(start)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 70


  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.player.location == this.destination

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit || this.player.isDeadWithGem || this.player.answeredWrong || this.player.usedWrong

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "You are somehow lost in the palace. Find the final room, and solve the problem to go back home."


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "You've solved the problem and you are back in the real world! Well done!"
    else if (this.turnCount == this.timeLimit)
      "Oh no! Time's up. You blacked out!.\nGame over!"
    else if (this.player.isDeadWithGem)
      "The energy of gem burned you!\nGame over!"
    else if (this.player.answeredWrong)
      "Merlin angered and killed you!\nGame over!"
    else if(this.player.usedWrong)
      "Answering outside the final room angered steward and killed you!\nGame over!"
    else  // game over due to player quitting
      "Quitter!"
  }


  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    if (outcomeReport.isDefined) {
      this.turnCount += 1
    }
    outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
  }


}

