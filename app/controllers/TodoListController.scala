package controllers

import javax.inject.{Inject, Singleton}
import models.{NewTodoListItem, TodoListItem}
import play.api.libs.json._
import play.api.mvc.{BaseController, ControllerComponents}

import scala.collection.mutable

@Singleton
class TodoListController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {

  private val todoList = new mutable.ListBuffer[TodoListItem]()
  todoList += TodoListItem(1, "test", true)
  todoList += TodoListItem(2, "some other value", false)

  implicit val todoListJson = Json.format[TodoListItem]

  def getAll() = Action {
    if (todoList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(todoList))
    }
  }

  def getById(itemId: Long) = Action {
    val foundItem = todoList.find(_.id == itemId)
    foundItem match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }

  def markAsDone(itemId: Long) = Action {
    val foundItem = todoList.find(_.id == itemId)
    foundItem match {
      case Some(item) => {
        val newItem = item.copy(isItDone = true)
        todoList += newItem
        todoList -= item
        Ok(Json.toJson(newItem))
      }
      case None => NotFound
    }
  }

  def deleteAllDone() = Action {
    todoList.filterInPlace(!_.isItDone)
    Accepted
  }

  implicit val newTodoListJson = Json.format[NewTodoListItem]

  def addNewItem() = Action { implicit request =>
    val todoListItem: Option[NewTodoListItem] =
      request.body.asJson.flatMap(
        Json.fromJson[NewTodoListItem](_).asOpt
      )

    todoListItem match {
      case Some(newItem) =>
        val nextId = todoList.map(_.id).max + 1
        val toBeAdded = TodoListItem(nextId, newItem.description, false)
        todoList += toBeAdded
        Created(Json.toJson(toBeAdded))
      case None =>
        BadRequest
    }
  }
}