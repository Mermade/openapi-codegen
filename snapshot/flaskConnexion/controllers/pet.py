import connexion
import six

IO.OpenAPI.Model.Default  # noqa: E501
from IO.OpenAPI import util


def addPet(body):  # noqa: E501
    """

     # noqa: E501

    :param body: Pet object that needs to be added to the store
    :type body: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        body = object.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def updatePet(body):  # noqa: E501
    """

     # noqa: E501

    :param body: Pet object that needs to be added to the store
    :type body: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        body = object.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def findPetsByStatus(status):  # noqa: E501
    """

     # noqa: E501

    :param status: Status values that need to be considered for filter
    :type status: 

    :rtype: 
    """
    return 'do some magic!'


def findPetsByTags(tags):  # noqa: E501
    """

     # noqa: E501

    :param tags: Tags to filter by
    :type tags: 

    :rtype: 
    """
    return 'do some magic!'


def getPetById(petId):  # noqa: E501
    """

     # noqa: E501

    :param petId: ID of pet to return
    :type petId: 

    :rtype: 
    """
    return 'do some magic!'


def updatePetWithForm(petId, body=None):  # noqa: E501
    """

     # noqa: E501

    :param petId: ID of pet that needs to be updated
    :type petId: 
    :param body: 
    :type body: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        body = object.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def deletePet(petId, api_key=None):  # noqa: E501
    """

     # noqa: E501

    :param petId: Pet id to delete
    :type petId: 
    :param api_key: 
    :type api_key: 

    :rtype: None
    """
    return 'do some magic!'


def uploadFile(petId, body=None):  # noqa: E501
    """

     # noqa: E501

    :param petId: ID of pet to update
    :type petId: 
    :param body: 
    :type body: dict | bytes

    :rtype: 
    """
    if connexion.request.is_json:
        body = object.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'
