import * as api from './api/api';
import * as angular from 'angular';

const apiModule = angular.module('api', [])
.service('petApi', api.petApi)
.service('storeApi', api.storeApi)
.service('userApi', api.userApi)

export default apiModule;
