package o1.adventure

import scala.collection.mutable.Map


/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private val items = Map[String, Item]()
  private var deadWithGem = false
  private var wrongAnswer = false
  private var wrongPlace = false


  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the current location of the player. */
  def location = this.currentLocation
  
  /** Determines if the player has dead by using the gem.*/
  def isDeadWithGem = this.deadWithGem
  
  /** Determines if the player has dead by answering wrong.*/
  def answeredWrong = this.wrongAnswer
  
  /** Determines if the player has dead by answering at wrong place.*/
  def usedWrong = this.wrongPlace


  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) = {
    val destination = this.location.neighbor(direction)
    if (destination.isDefined && destination.forall(!_.isClosed) && destination.forall(!_.locked)) {
      this.currentLocation = destination.getOrElse(this.currentLocation)
      "You go " + direction + "."
    } else {
      "You can't go " + direction + "."
    }
  }


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() = {
    "You rest for a while. Better get a move on, though."
  }


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }
  
  def get(itemName: String): String = {
    this.location.removeItem(itemName) match {
      case Some(item) => 
        this.items += itemName -> item
        "You pick up the " + itemName + "."
      case None => "There is no " + itemName + " here to pick up."
    }
  }
  
  /*def drop(itemName: String): String = {
    if(this.has(itemName)) {
      this.location.addItem(this.items(itemName))
      this.items -= itemName
      "You drop the " + itemName + "."
    } else {
      "You don't have that!"
    }
  }
  * 
  */
  
  def examine(itemName: String): String = {
    if(this.has(itemName)) {
       "You look closely at the " + itemName + ".\n" + this.items(itemName).description
    } else {
      "If you want to examine something, you need to pick it up first."
    }
  }
  
  def has(itemName: String): Boolean = {
    this.items.contains(itemName)
  }
  
  def inventory: String = {
    if(this.items.isEmpty) {
      "You are empty-handed."
    } else {
      "You are carrying:\n" + this.items.keys.mkString("\n")
    }
  }
  
  /**
   *  When given a correct direction and player is in specific area, new way opens to the direction
   */
  def abrakadabra(direction: String): String = {
    val destination = this.currentLocation.neighbor(direction)
    if(this.currentLocation.abra) {
      destination.foreach(_.locked = false)
      this.open(destination)
    } else {
      "You can't use that spell here!!"
    }
  }
  
  /** This command is to be used in the final room to open final door. 
   *  If player answers wrong or answers at wrong place, he/she dies.
   */
  def answer(guess: String): String = {
    if(this.currentLocation.finallock) {
      if(guess == this.currentLocation.correctAnswer) {
        this.currentLocation.open
        "Correct answer!\nThe final door in north opened!"
      } else {
        this.wrongAnswer = true
        "Wrong answer!"
      }
    } else {
      this.wrongPlace = true
      "It's forbitten to use that spell here!!"
    }
  }
  
  /**
   *  Opens a new way by changing area's isClosed boolean value 
   */
  def open(way: Option[Area]) = {
    val wasClosed = way.forall(_.isClosed)
    way.foreach(_.isClosed = false)
    if(way.exists(!_.isClosed) && wasClosed) "New way opened!!" else "But nothing happened..."
  }
  
  /**
   *  Returns a text in the area, if there is any text to read.
   */
  def read: String = {
    val text = this.currentLocation.text
    if(!text.isEmpty()) text else "There are no text to read."
  }
  
  /**
   *  Returns a person's speech, if there is any person in the area.
   *  And if there is a person in the area, but is sleeping(is not awake), then returns just zzz.
   */
  def talk(personName: String): String = {
    val optionPerson = this.currentLocation.person(personName)
    if(optionPerson.isDefined && optionPerson.forall(_.isAwake)) {
      optionPerson.get.name + ": " + optionPerson.get.speech
    } else if(optionPerson.isDefined && optionPerson.forall(!_.isAwake)){
      optionPerson.get.name + ": zzz"
    } else {
      "Such a person isn't here!"
    }
  }
  
  /** This command is to be used in the basement to wake up Merlin. 
   *  If player sets something else than gems or sets at wrong place it doesn't do anything.
   */
  def set(itemName: String): String = {
    if(this.currentLocation.canSet){
      if(this.has(itemName)) {
        val item = this.items.get(itemName).get
        item match {
          case gem: Gem => 
            this.items -= itemName
            this.currentLocation.addGem(gem)
            if(this.currentLocation.isFull) {
              this.currentLocation.wakeUp
              this.currentLocation.allPeople + " woke up!"
            } else {
              this.currentLocation.allPeople + " is still sleeping..."
            }
          case _ => 
            "You can only set Gems!!"
        }
      } else {
        "You don't have that item."
      }
    } else {
      "You can't set anything here!!"
    }
  }
  
  
  
  /** This command is to be used in the north corridor to open final room. 
   *  If player uses gem, he/she dies. Using something else doesn't do anything.
   */
  def use(itemName: String): String = {
    val optionItem = this.items.get(itemName)
    if(optionItem.isDefined) {
      val item = optionItem.get
      item match {
        case key: Key => 
          if(this.currentLocation.useKey) {
            val lockedAreas = this.currentLocation.lockedAreas
            this.currentLocation.unlock
            this.currentLocation.open
            this.currentLocation.useKey = false
            this.items -= itemName
            "The door(s) of " + lockedAreas + " opened!"
          } else {
            "There are no rooms locked."
          }
        case gem: Gem =>
          this.deadWithGem = true
          "The gem started to emit energy!!"
        case _ => "You can only use either key or gems."
      }
    } else {
      "If you want to use something, you need to pick it up first."
    }
  }
  
  
  
  /** This command shows all commands in this game besides 'abrakadabra', which is a kind of secret command.
   */
  def help: String = {
    """Commands:
      go 'direction' = Player goes to given direction, if possible. directions are north, east, south, west, up and down.
      rest = Causes the player to rest for a short while (this has no substantial effect in game terms).
      quit = Quits the game.
      inventory = Shows all items player has.
      get 'item name' = Picks up item from current location, if there are any.
      examine 'item name' = Shows the detail of given item, if player has the item.
      read = Reads text from current location, if there are any. (If you want to read a dictionary, use examine.)
      talk 'person's name' = Talks with given person, if there are any person in current location and is awake.
      set 'item name' = Sets given item to current location.
      use 'item name' = Uses given item, if player has it.
      answer 'your answer' = Use this command only in the final room to clear the game, if you use it outside the final room..."""
  }


  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name


}


