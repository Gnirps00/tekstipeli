package o1.adventure

import scala.collection.mutable.Map

/** The class `Area` represents locations in a text adventure game world. A game world
  * consists of areas. In general, an "area" can be pretty much anything: a room, a building,
  * an acre of forest, or something completely different. What different areas have in
  * common is that players can be located in them and that they can have exits leading to
  * other, neighboring areas. An area also has a name and a description.
  * @param name         the name of the area
  * @param description  a basic description of the area (typically not including information about items)
  * @param isClosed			determines if the area is closed by spell
  * @param text					the text that can be found in the area, this can be a hint for clearing the game
  * @param abra					determines if the spell 'abrakadabra' can be used in the area
  * @param finallock		determines if the player can answer to the final problem
  * @param locked				determines if the area is locked by key
  * @param useKey				determines if the player can use the key in the area
  * @param canSet				determines if the player can set any item in the area
  * @param maxSet				maximum number of items to be able to set to the area */
class Area(
    var name: String,
    var description: String,
    var isClosed: Boolean = false,
    val text: String = "",
    val abra: Boolean = false,
    val finallock: Boolean = false,
    var locked: Boolean = false,
    var useKey: Boolean = false,
    var canSet: Boolean = false,
    val maxSet: Int = 0) {

  private val neighbors = Map[String, Area]()
  private val items = Map[String, Item]()
  private val people = Map[String, Person]()
  private val gems = Map[String, Gem]()
  
  val correctAnswer = "arthur"


  /** Returns the area that can be reached from this area by moving in the given direction. The result
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)


  /** Adds an exit from this area to the given area. The neighboring area is reached by moving in
    * the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area) = {
    this.neighbors += direction -> neighbor
  }


  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling
    * the `setNeighbor` method on each of the given direction--area pairs.
    * @param exits  contains pairs consisting of a direction and the neighboring area in that direction
    * @see [[setNeighbor]] */
  def setNeighbors(exits: Vector[(String, Area)]) = {
    this.neighbors ++= exits
  }


  /** Returns a multi-line description of the area as a player sees it. This includes a basic
    * description of the area as well as information about exits and items. The return
    * value has the form "DESCRIPTION\n\nExits available: DIRECTIONS SEPARATED BY SPACES".
    * The directions are listed in an arbitrary order. */
  def fullDescription = {
    val descriptionWithName = this.name + ":\n" + this.description
    val exitList = "\n\nExits available: " + this.neighbors.keys.mkString(" ")
    val itemList = "\n\nYou see here(items): " + this.items.keys.mkString(" ")
    val personList = "\n\nYou see here(people): " + this.people.keys.mkString(" ")
    if(this.items.isEmpty && this.people.isEmpty) {
      descriptionWithName + exitList
    } else if(this.people.isEmpty && !this.items.isEmpty){
      descriptionWithName + itemList + exitList
    } else if(this.items.isEmpty && !this.people.isEmpty){
      descriptionWithName + personList + exitList
    } else {
      descriptionWithName + personList + itemList + exitList
    }
  }
  
  def addItem(item: Item): Unit = {
    this.items += item.name -> item
  }
  
  def contains(itemName: String): Boolean = {
    this.items.contains(itemName)
  }
  
  def removeItem(itemName: String): Option[Item] = {
    this.items.remove(itemName)
  }
  
  /** Adds a person to this area. */
  def addPerson(person: Person) = {
    this.people += person.name -> person
  }
  
  /** Returns a person with given name. */
  def person(personName: String) = this.people.get(personName)
  
  /** Adds a gem to this area, if it's not full.
   */
  def addGem(gem: Gem) = {
    if(!this.isFull) {
      this.gems += gem.name -> gem
      "You set the " + gem.name + "."
    } else {
      "There are no space to set anymore!"
    }
  }
  
  /** Determines if more gems are able to be set to this area.
   */
  def isFull: Boolean = {
    this.gems.size == this.maxSet
  }
  
  /** Wakes up all the people in this area.
   */
  def wakeUp = {
    this.people.foreach(_._2.isAwake = true)
  }
  
  /** Returns a string of all the people in this area.
   */
  def allPeople = this.people.keys.mkString(", ")
  
  /** Unlocks all the neighbors of this area. 'Unlocking' means opening the door by using key.
   */
  def unlock = this.neighbors.foreach(_._2.locked = false)
  
  /** Returns all neighboring areas that are locked.
   */
  def lockedAreas = this.neighbors.filter(_._2.locked).map(_._2.name).mkString(", ")
  
  /** Opens all the neighbors of this area. 'Opening' means opening the door by using spell.
   */
  def open = this.neighbors.foreach(_._2.isClosed = false)


  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)



}
