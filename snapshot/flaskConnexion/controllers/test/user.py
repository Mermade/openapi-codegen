# coding: utf-8

from __future__ import absolute_import

from flask import json
from six import BytesIO

IO.OpenAPI.Model.Default  # noqa: E501
from IO.OpenAPI.test import BaseTestCase


class TestuserApi(BaseTestCase):
    """userApi integration test stubs"""

    def test_createUser(self):
        """Test case for createUser

        Create user
        """
        body = {"id":0,"username":"string","firstName":"string","lastName":"string","email":"string","password":"string","phone":"string","userStatus":0}
        response = self.client.open(
            '/user',
            method='POST',
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_createUsersWithArrayInput(self):
        """Test case for createUsersWithArrayInput

        Creates list of users with given input array
        """
        body = [{"id":0,"username":"string","firstName":"string","lastName":"string","email":"string","password":"string","phone":"string","userStatus":0}]
        response = self.client.open(
            '/user/createWithArray',
            method='POST',
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_createUsersWithListInput(self):
        """Test case for createUsersWithListInput

        Creates list of users with given input array
        """
        body = [{"id":0,"username":"string","firstName":"string","lastName":"string","email":"string","password":"string","phone":"string","userStatus":0}]
        response = self.client.open(
            '/user/createWithList',
            method='POST',
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_loginUser(self):
        """Test case for loginUser

        Logs user into the system
        """
        query_string = [('username', "string"),
                        ('password', "pa$$word")]
        response = self.client.open(
            '/user/login',
            method='GET',
            content_type='application/json',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_logoutUser(self):
        """Test case for logoutUser

        Logs out current logged in user session
        """
        response = self.client.open(
            '/user/logout',
            method='GET',
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_getUserByName(self):
        """Test case for getUserByName

        Get user by user name
        """
        response = self.client.open(
            '/user/{username}'.format(username="string"),
            method='GET',
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_updateUser(self):
        """Test case for updateUser

        Updated user
        """
        body = {"id":0,"username":"string","firstName":"string","lastName":"string","email":"string","password":"string","phone":"string","userStatus":0}
        response = self.client.open(
            '/user/{username}'.format(username="string"),
            method='PUT',
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_deleteUser(self):
        """Test case for deleteUser

        Delete user
        """
        response = self.client.open(
            '/user/{username}'.format(username="string"),
            method='DELETE',
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    import unittest
    unittest.main()
