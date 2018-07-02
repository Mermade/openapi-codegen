import connexion
import six

IO.OpenAPI.Model.Default  # noqa: E501
from IO.OpenAPI import util


def getInventory():  # noqa: E501
    """

     # noqa: E501


    :rtype: 
    """
    return 'do some magic!'


def placeOrder(body):  # noqa: E501
    """

     # noqa: E501

    :param body: order placed for purchasing the pet
    :type body: dict | bytes

    :rtype: 
    """
    if connexion.request.is_json:
        body = object.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def getOrderById(orderId):  # noqa: E501
    """

     # noqa: E501

    :param orderId: ID of pet that needs to be fetched
    :type orderId: 

    :rtype: 
    """
    return 'do some magic!'


def deleteOrder(orderId):  # noqa: E501
    """

     # noqa: E501

    :param orderId: ID of the order that needs to be deleted
    :type orderId: 

    :rtype: None
    """
    return 'do some magic!'
