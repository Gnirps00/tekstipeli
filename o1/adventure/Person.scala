package o1.adventure

/**
 *  The class Person represents a person in an area.
 *  The class has as parameter it's name, speech and isAwake.
 *  @param name			the name of the person
 *  @param speech 	a string the person says when talking with it
 *  @param isAwake	shows whether the person is awake or not
 */

class Person(val name: String, val speech: String, var isAwake: Boolean) {
  
  override def toString = this.name
  
}