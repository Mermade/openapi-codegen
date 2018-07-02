# coding: utf-8

from __future__ import absolute_import

from flask import json
from six import BytesIO

IO.OpenAPI.Model.Default  # noqa: E501
from IO.OpenAPI.test import BaseTestCase


class TestpetApi(BaseTestCase):
    """petApi integration test stubs"""

    def test_addPet(self):
        """Test case for addPet

        Add a new pet to the store
        """
        body = {"id":0,"category":{"id":0,"name":"string"},"name":"doggie","photoUrls":["string"],"tags":[{"id":0,"name":"string"}],"status":"available"}
        response = self.client.open(
            '/pet',
            method='POST',
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_updatePet(self):
        """Test case for updatePet

        Update an existing pet
        """
        body = {"id":0,"category":{"id":0,"name":"string"},"name":"doggie","photoUrls":["string"],"tags":[{"id":0,"name":"string"}],"status":"available"}
        response = self.client.open(
            '/pet',
            method='PUT',
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_findPetsByStatus(self):
        """Test case for findPetsByStatus

        Finds Pets by status
        """
        query_string = [('status', ["available"])]
        response = self.client.open(
            '/pet/findByStatus',
            method='GET',
            content_type='application/json',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_findPetsByTags(self):
        """Test case for findPetsByTags

        Finds Pets by tags
        """
        query_string = [('tags', ["string"])]
        response = self.client.open(
            '/pet/findByTags',
            method='GET',
            content_type='application/json',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_getPetById(self):
        """Test case for getPetById

        Find pet by ID
        """
        response = self.client.open(
            '/pet/{petId}'.format(petId=0),
            method='GET',
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_updatePetWithForm(self):
        """Test case for updatePetWithForm

        Updates a pet in the store with form data
        """
        body = {"name":"string","status":"string"}
        response = self.client.open(
            '/pet/{petId}'.format(petId=0),
            method='POST',
            data=json.dumps(body),
            content_type='application/x-www-form-urlencoded')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_deletePet(self):
        """Test case for deletePet

        Deletes a pet
        """
        headers = [('api_key', "string")]
        response = self.client.open(
            '/pet/{petId}'.format(petId=0),
            method='DELETE',
            headers=headers,
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_uploadFile(self):
        """Test case for uploadFile

        uploads an image
        """
        body = "string"
        response = self.client.open(
            '/pet/{petId}/uploadImage'.format(petId=0),
            method='POST',
            data=json.dumps(body),
            content_type='application/octet-stream')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    import unittest
    unittest.main()
