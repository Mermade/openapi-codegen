import connexion
import six

IO.OpenAPI.Model.Default  # noqa: E501
from IO.OpenAPI import util


def createUser(body):  # noqa: E501
    """

     # noqa: E501

    :param body: Created user object
    :type body: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        body = object.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def createUsersWithArrayInput(body):  # noqa: E501
    """

     # noqa: E501

    :param body: List of user object
    :type body: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        body = object.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def createUsersWithListInput(body):  # noqa: E501
    """

     # noqa: E501

    :param body: List of user object
    :type body: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        body = object.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def loginUser(username, password):  # noqa: E501
    """

     # noqa: E501

    :param username: The user name for login
    :type username: 
    :param password: The password for login in clear text
    :type password: 

    :rtype: 
    """
    return 'do some magic!'


def logoutUser():  # noqa: E501
    """

     # noqa: E501


    :rtype: None
    """
    return 'do some magic!'


def getUserByName(username):  # noqa: E501
    """

     # noqa: E501

    :param username: The name that needs to be fetched. Use user1 for testing. 
    :type username: 

    :rtype: 
    """
    return 'do some magic!'


def updateUser(username, body):  # noqa: E501
    """

     # noqa: E501

    :param username: name that need to be updated
    :type username: 
    :param body: Updated user object
    :type body: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        body = object.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def deleteUser(username):  # noqa: E501
    """

     # noqa: E501

    :param username: The name that needs to be deleted
    :type username: 

    :rtype: None
    """
    return 'do some magic!'
