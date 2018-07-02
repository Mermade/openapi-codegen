# coding: utf-8

from __future__ import absolute_import

from flask import json
from six import BytesIO

IO.OpenAPI.Model.Default  # noqa: E501
from IO.OpenAPI.test import BaseTestCase


class TeststoreApi(BaseTestCase):
    """storeApi integration test stubs"""

    def test_getInventory(self):
        """Test case for getInventory

        Returns pet inventories by status
        """
        response = self.client.open(
            '/store/inventory',
            method='GET',
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_placeOrder(self):
        """Test case for placeOrder

        Place an order for a pet
        """
        body = {"id":0,"petId":0,"quantity":0,"shipDate":"2018-07-02T10:01:51Z","status":"placed","complete":false}
        response = self.client.open(
            '/store/order',
            method='POST',
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_getOrderById(self):
        """Test case for getOrderById

        Find purchase order by ID
        """
        response = self.client.open(
            '/store/order/{orderId}'.format(orderId=1),
            method='GET',
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_deleteOrder(self):
        """Test case for deleteOrder

        Delete purchase order by ID
        """
        response = self.client.open(
            '/store/order/{orderId}'.format(orderId=1),
            method='DELETE',
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    import unittest
    unittest.main()
