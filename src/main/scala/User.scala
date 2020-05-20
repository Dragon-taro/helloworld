case class User(id: UserId, name: UserName, auth: UserAuth)

case class UserId(value: Int)

case class UserName(value: String) {
  def isEmpty(): Boolean = {
    value == ""
  }
}

case class UserAuth(userId: String, password: String)
